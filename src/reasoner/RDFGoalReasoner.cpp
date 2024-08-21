/*
 * This file is part of KnowRob, please consult
 * https://github.com/knowrob/knowrob for license details.
 */

#include <knowrob/reasoner/RDFGoalReasoner.h>
#include "knowrob/integration/python/utils.h"

using namespace knowrob;

bool RDFGoalReasoner::evaluate(GoalPtr query) {
	std::vector<FramedTriplePatternPtr> rdfLiterals;
	for (auto &lit : query->formula()->literals()) {
		auto rdfLit = std::dynamic_pointer_cast<FramedTriplePattern>(lit);
		if (rdfLit) {
			rdfLiterals.push_back(rdfLit);
		} else {
			rdfLiterals.push_back(std::make_shared<FramedTriplePattern>(lit->predicate(), lit->isNegated()));
		}
	}
	auto rdfGoal = std::make_shared<RDFGoal>(rdfLiterals, *query);
	return evaluateRDF(rdfGoal);
}

namespace knowrob::py {

	// this struct is needed because Reasoner has pure virtual methods
	struct RDFGoalReasonerWrap : public RDFGoalReasoner, boost::python::wrapper<RDFGoalReasoner> {
		explicit RDFGoalReasonerWrap(PyObject *p) : self(p), RDFGoalReasoner() {}

		bool initializeReasoner(const PropertyTree &config) override {
			return call_method<bool>(self, "initializeReasoner", config);
		}

		bool evaluateRDF(RDFGoalPtr query) override {
			return call_method<bool>(self, "evaluateRDF", query);
		}

	private:
		PyObject *self;
	};

	template<>
	void createType<RDFGoalReasoner>() {
		using namespace boost::python;

		// export the GoalDrivenReasoner class
		class_<RDFGoalReasoner, std::shared_ptr<RDFGoalReasonerWrap>, bases<GoalDrivenReasoner>, boost::noncopyable>
				("RDFGoalReasoner", init<>())
						// methods that must be implemented by reasoner plugins
				.def("evaluateRDF", &RDFGoalReasonerWrap::evaluateRDF);
	}
}
