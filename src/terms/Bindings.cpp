/*
 * This file is part of KnowRob, please consult
 * https://github.com/knowrob/knowrob for license details.
 */

#include "knowrob/terms/Bindings.h"
#include "knowrob/terms/Unifier.h"
#include "knowrob/knowrob.h"
#include "knowrob/formulas/CompoundFormula.h"
#include "knowrob/formulas/ModalFormula.h"
#include "knowrob/formulas/Implication.h"
#include "knowrob/formulas/Negation.h"
#include "knowrob/formulas/Conjunction.h"
#include "knowrob/formulas/Disjunction.h"
#include "knowrob/terms/Function.h"
#include "knowrob/integration/python/utils.h"
#include "knowrob/integration/python/converter/dict.h"
#include "knowrob/Logger.h"

using namespace knowrob;

Bindings::Bindings(const std::map<std::shared_ptr<Variable>, TermPtr> &mapping) {
	for (const auto &pair: mapping) {
		set(pair.first, pair.second);
	}
}

bool Bindings::operator==(const Bindings &other) const {
	if (mapping_.size() != other.mapping_.size()) {
		return false;
	}

	for (auto &x: mapping_) {
		auto &val1 = x.second.second;
		auto &val2 = other.get(x.first);
		if (val1 && val2) {
			if (!(*val1 == *val2)) return false;
		} else if (val1 || val2) {
			return false;
		}
	}

	return true;
}

void Bindings::operator+=(const Bindings &other) {
	for (auto &x: other.mapping_) {
		set(x.second.first, x.second.second);
	}
}

void Bindings::set(const std::shared_ptr<Variable> &var, const TermPtr &term) {
	mapping_.insert({var->name(), {var, term}});
}

bool Bindings::contains(std::string_view varName) const {
	return mapping_.find(varName) != mapping_.end();
}

const TermPtr &Bindings::get(std::string_view varName) const {
	static const TermPtr null_term;

	auto it = mapping_.find(varName);
	if (it != mapping_.end()) {
		return it->second.second;
	} else {
		return null_term;
	}
}

std::shared_ptr<Atomic> Bindings::getAtomic(std::string_view varName) const {
	static const std::shared_ptr<Atomic> null_term;

	auto term = get(varName);
	if (term && term->termType() == TermType::ATOMIC) {
		return std::static_pointer_cast<Atomic>(term);
	} else {
		return null_term;
	}
}

size_t Bindings::hash() const {
	auto seed = static_cast<size_t>(0);

	for (const auto &item: mapping_) {
		// Compute the hash of the key using the in-built hash function for string.
		hashCombine(seed, std::hash<std::string_view>{}(item.first));

		auto value_hash = static_cast<size_t>(0);
		if (item.second.second) {
			value_hash = item.second.second->hash();
		}
		hashCombine(seed, value_hash);
	}

	return seed;
}

std::shared_ptr<const Bindings> Bindings::emptyBindings() {
	static const auto empty = std::make_shared<Bindings>();
	return empty;
}

bool Bindings::isConsistentWith(const Bindings &other) const {
	const Map *map_small;
	const Map *map_large;
	if (mapping_.size() < other.mapping_.size()) {
		map_small = &mapping_;
		map_large = &other.mapping_;
	} else {
		map_small = &other.mapping_;
		map_large = &mapping_;
	}

	for (const auto &pair: *map_small) {
		auto it = map_large->find(pair.first);
		if (it != map_large->end()) {
			auto t0 = pair.second.second;
			auto t1 = it->second.second;
			Unifier sigma(t0, t1);
			if (!sigma.exists()) {
				return false;
			}
		}
	}

	return true;
}

bool Bindings::unifyWith(const Bindings &other) {
	for (const auto &pair: other.mapping_) {
		auto it = mapping_.find(pair.first);
		if (it == mapping_.end()) {
			// new variable instantiation
			mapping_.insert(pair);
		} else {
			// variable has already an instantiation, need to unify
			TermPtr t0 = it->second.second;
			TermPtr t1 = pair.second.second;

			// t0 and t1 are not syntactically equal -> compute a unifier
			Unifier sigma(t0, t1);
			if (sigma.exists()) {
				// a unifier exists
				it->second.second = sigma.apply();
			} else {
				// no unifier exists
				return false;
			}
		}
	}

	return true;
}

void Bindings::write(std::ostream &os) const {
	uint32_t i = 0;
	os << '{';
	for (const auto &pair: mapping_) {
		if (i++ > 0) os << ',';
		os << pair.first << ':' << ' ' << (*pair.second.second);
	}
	os << '}';
}

template<class T>
std::shared_ptr<T> applyCompoundBindings( //NOLINT(misc-no-recursion)
		const std::shared_ptr<T> &phi,
		const Bindings &bindings) {
	std::vector<FormulaPtr> formulae;
	bool hasNewFormula = false;
	for (const auto &f: phi->formulae()) {
		auto f1 = applyBindings(f, bindings);
		if (f1 != f) {
			hasNewFormula = true;
		}
		formulae.push_back(applyBindings(f, bindings));
	}
	if (!hasNewFormula) {
		return phi;
	} else {
		return std::make_shared<T>(formulae);
	}
}

namespace knowrob {
	FormulaPtr applyBindings(const FormulaPtr &phi, const Bindings &bindings) { //NOLINT(misc-no-recursion)
		if (phi->isGround()) {
			return phi;
		}
		switch (phi->type()) {
			case FormulaType::MODAL: {
				auto modal = std::static_pointer_cast<ModalFormula>(phi);
				auto inner = applyBindings(modal->modalFormula(), bindings);
				if (inner == modal->modalFormula()) {
					return modal;
				} else {
					return std::make_shared<ModalFormula>(modal->modalOperator(), inner);
				}
			}
			case FormulaType::IMPLICATION: {
				auto implication = std::static_pointer_cast<Implication>(phi);
				auto antecedent = applyBindings(implication->antecedent(), bindings);
				auto consequent = applyBindings(implication->consequent(), bindings);
				if (antecedent == implication->antecedent() && consequent == implication->consequent()) {
					return implication;
				} else {
					return std::make_shared<Implication>(antecedent, consequent);
				}
			}
			case FormulaType::NEGATION: {
				auto negation = std::static_pointer_cast<Negation>(phi);
				auto negated = applyBindings(negation->negatedFormula(), bindings);
				if (negated == negation->negatedFormula()) {
					return negation;
				} else {
					return std::make_shared<Negation>(negated);
				}
			}
			case FormulaType::CONJUNCTION:
				return applyCompoundBindings<Conjunction>(std::static_pointer_cast<Conjunction>(phi), bindings);
			case FormulaType::DISJUNCTION:
				return applyCompoundBindings<Disjunction>(std::static_pointer_cast<Disjunction>(phi), bindings);
			case FormulaType::PREDICATE: {
				auto predicate = std::static_pointer_cast<Predicate>(phi);
				auto args = predicate->arguments();
				auto newArgs = std::vector<TermPtr>(args.size());
				auto hasNewArg = false;
				for (uint32_t i = 0; i < args.size(); i++) {
					auto arg = applyBindings(args[i], bindings);
					if (arg != args[i]) {
						hasNewArg = true;
					}
					newArgs[i] = arg;
				}
				if (!hasNewArg) {
					return predicate;
				} else {
					return std::make_shared<Predicate>(predicate->functor(), newArgs);
				}
			}
		}
		return phi;
	}

	TermPtr applyBindings(const TermPtr &t, const Bindings &bindings) { //NOLINT
		if (t->isGround()) {
			return t;
		}
		switch (t->termType()) {
			case TermType::ATOMIC:
				return t;
			case TermType::VARIABLE: {
				auto var = std::static_pointer_cast<Variable>(t);
				auto term = bindings.get(var->name());
				if (term) {
					return term;
				} else {
					return var;
				}
			}
			case TermType::FUNCTION: {
				auto function = std::static_pointer_cast<Function>(t);
				auto args = function->arguments();
				auto newArgs = std::vector<TermPtr>(args.size());
				auto hasNewArg = false;
				for (uint32_t i = 0; i < args.size(); i++) {
					auto arg = applyBindings(args[i], bindings);
					if (arg != args[i]) {
						hasNewArg = true;
					}
					newArgs[i] = arg;
				}
				if (!hasNewArg) {
					return function;
				} else {
					return std::make_shared<Function>(function->functor(), newArgs);
				}
			}
		}
		return t;
	}
}

namespace knowrob::py {
	TermPtr applyBindings_t(const TermPtr &t, const Bindings &bindings) {
		return applyBindings(t, bindings);
	}

	FormulaPtr applyBindings_phi(const FormulaPtr &phi, const Bindings &bindings) {
		return applyBindings(phi, bindings);
	}

	template<>
	void createType<Bindings>() {
		using namespace boost::python;

		class_<Bindings, std::shared_ptr<Bindings>>("Bindings", init<>())
				.def(init<std::map<VariablePtr, TermPtr>>())
				.def("__eq__", &Bindings::operator==)
				.def("__iter__", range(&Bindings::begin, &Bindings::end))
				.def("__len__", &Bindings::size)
				.def("__hash__", &Bindings::hash)
				.def("empty", &Bindings::empty)
				.def("set", &Bindings::set)
				.def("get", &Bindings::get, return_value_policy<copy_const_reference>())
				.def("contains", &Bindings::contains)
				.def("unifyWith", &Bindings::unifyWith)
				.def("isConsistentWith", &Bindings::isConsistentWith);

		// define global functions
		def("applyBindings", applyBindings_t);
		def("applyBindings", applyBindings_phi);
		// Allow implicit conversion from shared_ptr<Bindings> to shared_ptr<const Bindings>
		register_ptr_to_python<std::shared_ptr<const Bindings> >();
		implicitly_convertible<std::shared_ptr<Bindings>, std::shared_ptr<const Bindings> >();
		// Allow conversion from python dict to std::map
		dict_map_converter<std::shared_ptr<Variable>,std::shared_ptr<Term>>::register_from_python_converter();
	}
}
