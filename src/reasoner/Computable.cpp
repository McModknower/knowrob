/*
 * This file is part of KnowRob, please consult
 * https://github.com/knowrob/knowrob for license details.
 */

#include <knowrob/reasoner/Computable.h>

using namespace knowrob;

Computable::Computable(const FirstOrderLiteral &literal, const std::vector<DefiningReasoner> &reasonerList)
: FirstOrderLiteral(literal), reasonerList_(reasonerList) {}

Computable::Computable(const PredicatePtr &predicate, bool isNegative,
					   const std::vector<DefiningReasoner> &reasonerList)
: FirstOrderLiteral(predicate, isNegative), reasonerList_(reasonerList) {}

namespace knowrob {
	ComputablePtr applyBindings(const ComputablePtr &lit, const Bindings &bindings) {
		auto predicate = std::static_pointer_cast<Predicate>(applyBindings(lit->predicate(), bindings));
		if (predicate != lit->predicate()) {
			return std::make_shared<Computable>(predicate, lit->isNegated(), lit->reasonerList());
		}
		else {
			return lit;
		}
	}
} // knowrob
