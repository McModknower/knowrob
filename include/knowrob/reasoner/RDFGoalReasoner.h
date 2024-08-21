/*
 * This file is part of KnowRob, please consult
 * https://github.com/knowrob/knowrob for license details.
 */

#ifndef KNOWROB_RDF_GOAL_REASONER_H
#define KNOWROB_RDF_GOAL_REASONER_H

#include "GoalDrivenReasoner.h"
#include "RDFGoal.h"

namespace knowrob {
	/**
	 * A reasoner that evaluates RDF goals.
	 */
	class RDFGoalReasoner : public GoalDrivenReasoner {
	public:
		RDFGoalReasoner() : GoalDrivenReasoner() {}

		virtual ~RDFGoalReasoner() = default;

		/**
		 * Evaluate an RDF goal.
		 * @param query the goal to evaluate.
		 * @return true if the goal is satisfied, false otherwise.
		 */
		virtual bool evaluateRDF(RDFGoalPtr query) = 0;

		// override GoalDrivenReasoner
		bool evaluate(GoalPtr query) override;
	};

} // knowrob

#endif //KNOWROB_RDF_GOAL_REASONER_H
