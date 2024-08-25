/*
 * This file is part of KnowRob, please consult
 * https://github.com/knowrob/knowrob for license details.
 */

#include "knowrob/triples/GraphBuiltin.h"
#include "knowrob/integration/python/utils.h"
#include "knowrob/terms/Numeric.h"

using namespace knowrob;

static inline bool isSmaller(const TermPtr &a, const TermPtr &b) {
	if (a->isNumeric() && b->isNumeric()) {
		auto a_numeric = std::static_pointer_cast<Numeric>(a);
		auto b_numeric = std::static_pointer_cast<Numeric>(b);
		return a_numeric->asDouble() < b_numeric->asDouble();
	}
	return false;
}

static inline bool isGreater(const TermPtr &a, const TermPtr &b) {
	if (a->isNumeric() && b->isNumeric()) {
		auto a_numeric = std::static_pointer_cast<Numeric>(a);
		auto b_numeric = std::static_pointer_cast<Numeric>(b);
		return a_numeric->asDouble() > b_numeric->asDouble();
	}
	return false;
}

bool GraphBuiltin::apply(const std::shared_ptr<Bindings> &bindings) const {
	switch (builtinType()) {
		case GraphBuiltinType::Bind: {
			auto valueTerm = applyBindings(arguments_[0], *bindings);
			bindings->set(bindVar(), valueTerm);
			return true;
		}
		case GraphBuiltinType::Min: {
			auto a = applyBindings(arguments_[0], *bindings);
			auto b = applyBindings(arguments_[1], *bindings);
			if (a->isGround() && b->isGround()) {
				if (isSmaller(a, b)) {
					bindings->set(bindVar(), a);
				} else {
					bindings->set(bindVar(), b);
				}
			}
			return true;
		}
		case GraphBuiltinType::Max: {
			auto a = applyBindings(arguments_[0], *bindings);
			auto b = applyBindings(arguments_[1], *bindings);
			if (a->isGround() && b->isGround()) {
				if (isGreater(a, b)) {
					bindings->set(bindVar(), a);
				} else {
					bindings->set(bindVar(), b);
				}
			}
			return true;
		}
		case GraphBuiltinType::Less: {
			auto a = applyBindings(arguments_[0], *bindings);
			auto b = applyBindings(arguments_[1], *bindings);
			return isSmaller(a, b);
		}
		case GraphBuiltinType::LessOrEqual: {
			auto a = applyBindings(arguments_[0], *bindings);
			auto b = applyBindings(arguments_[1], *bindings);
			return isSmaller(a, b) || (*a == *b);
		}
		case GraphBuiltinType::Greater: {
			auto a = applyBindings(arguments_[0], *bindings);
			auto b = applyBindings(arguments_[1], *bindings);
			return isGreater(a, b);
		}
		case GraphBuiltinType::GreaterOrEqual: {
			auto a = applyBindings(arguments_[0], *bindings);
			auto b = applyBindings(arguments_[1], *bindings);
			return isGreater(a, b) || (*a == *b);
		}
		case GraphBuiltinType::Equal: {
			auto a = applyBindings(arguments_[0], *bindings);
			auto b = applyBindings(arguments_[1], *bindings);
			return *a == *b;
		}
		case GraphBuiltinType::NotEqual: {
			auto a = applyBindings(arguments_[0], *bindings);
			auto b = applyBindings(arguments_[1], *bindings);
			return *a != *b;
		}
	}
	return true;
}

namespace knowrob::py {
	template<>
	void createType<GraphBuiltin>() {
		using namespace boost::python;

		class_<GraphBuiltin, bases<GraphTerm, Function>, std::shared_ptr<GraphBuiltin>, boost::noncopyable>
				("GraphBuiltin", no_init)
				.def("setOptional", &GraphBuiltin::setOptional)
				.def("isOptional", &GraphBuiltin::isOptional)
				.def("bind", &GraphBuiltin::bind).staticmethod("bind")
				.def("min", &GraphBuiltin::min).staticmethod("min")
				.def("max", &GraphBuiltin::max).staticmethod("max")
				.def("less", &GraphBuiltin::less).staticmethod("less")
				.def("lessOrEqual", &GraphBuiltin::lessOrEqual).staticmethod("lessOrEqual")
				.def("greater", &GraphBuiltin::greater).staticmethod("greater")
				.def("greaterOrEqual", &GraphBuiltin::greaterOrEqual).staticmethod("greaterOrEqual")
				.def("equal", &GraphBuiltin::equal).staticmethod("equal")
				.def("notEqual", &GraphBuiltin::notEqual).staticmethod("notEqual");
	}
}
