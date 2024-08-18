/*
 * This file is part of KnowRob, please consult
 * https://github.com/knowrob/knowrob for license details.
 */

#include <knowrob/formulas/SimpleConjunction.h>
#include "knowrob/formulas/Negation.h"
#include "knowrob/integration/python/utils.h"

using namespace knowrob;

std::vector<FormulaPtr> literalsToFormulae(const std::vector<FirstOrderLiteralPtr> &literals) {
	std::vector<FormulaPtr> formulae;
	for (const auto &literal: literals) {
		if (literal->isNegated()) {
			formulae.push_back(std::make_shared<Negation>(literal->predicate()));
		} else {
			formulae.push_back(literal->predicate());
		}
	}
	return formulae;
}

SimpleConjunction::SimpleConjunction(const std::vector<FirstOrderLiteralPtr> &literals)
		: Conjunction(literalsToFormulae(literals)), literals_(literals) {
}

SimpleConjunction::SimpleConjunction(const FirstOrderLiteralPtr &literal)
		: SimpleConjunction(std::vector<FirstOrderLiteralPtr>{literal}) {
}

namespace knowrob::py {
	template<>
	void createType<SimpleConjunction>() {
		using namespace boost::python;
		class_<SimpleConjunction, std::shared_ptr<SimpleConjunction>, bases<Conjunction>>
				("SimpleConjunction", init<const std::vector<FirstOrderLiteralPtr> &>()).
				def("literals", &SimpleConjunction::literals, return_value_policy<copy_const_reference>());
	}
}
