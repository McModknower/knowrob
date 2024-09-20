/*
 * This file is part of KnowRob, please consult
 * https://github.com/knowrob/knowrob for license details.
 */

#include <knowrob/reasoner/RDFGoal.h>

#include <utility>
#include "knowrob/integration/python/utils.h"

using namespace knowrob;

static SimpleConjunctionPtr makeSimpleConjunction(const std::vector<TriplePatternPtr> &literals) {
	std::vector<FirstOrderLiteralPtr> firstOrderLiterals;
	firstOrderLiterals.reserve(literals.size());
	for (const auto &literal : literals) {
		firstOrderLiterals.push_back(literal);
	}
	return std::make_shared<SimpleConjunction>(firstOrderLiterals);
}

RDFGoal::RDFGoal(const std::vector<TriplePatternPtr> &literals, const Goal &goal)
		: Goal(makeSimpleConjunction(literals), goal), rdfLiterals_(literals) {
}

RDFGoal::RDFGoal(const std::vector<TriplePatternPtr> &literals, const QueryContextPtr &ctx)
		: Goal(makeSimpleConjunction(literals), ctx), rdfLiterals_(literals) {
}

RDFGoal::RDFGoal(const TriplePatternPtr &literal, const QueryContextPtr &ctx)
		: Goal(literal, ctx), rdfLiterals_({literal}) {
}

namespace knowrob::py {
	template<>
	void createType<RDFGoal>() {
		using namespace boost::python;

		class_<RDFGoal, std::shared_ptr<RDFGoal>, bases<Goal>>
				("RDFGoal", init<const std::vector<TriplePatternPtr> &, QueryContextPtr>())
				.def(init<const TriplePatternPtr &, QueryContextPtr>())
				.def("rdfLiterals", &RDFGoal::rdfLiterals, return_value_policy<copy_const_reference>());
	}
}

