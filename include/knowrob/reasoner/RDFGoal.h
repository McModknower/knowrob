/*
 * This file is part of KnowRob, please consult
 * https://github.com/knowrob/knowrob for license details.
 */

#ifndef KNOWROB_RDF_GOAL_H
#define KNOWROB_RDF_GOAL_H

#include "Goal.h"

namespace knowrob {
	/**
	 * An RDF goal.
	 */
	class RDFGoal : public Goal {
	public:
		/**
		 * @param formula a formula.
		 * @param ctx a query context.
		 */
		explicit RDFGoal(const std::vector<FramedTriplePatternPtr> &literals, const QueryContextPtr &ctx = DefaultQueryContext());

		/**
		 * @param literal a literal.
		 * @param ctx a query context.
		 */
		explicit RDFGoal(const FramedTriplePatternPtr &literal, const QueryContextPtr &ctx = DefaultQueryContext());

		RDFGoal(const std::vector<FramedTriplePatternPtr> &literals, const Goal &goal);

		~RDFGoal() override = default;

		/**
		 * @return the RDF literals.
		 */
		auto &rdfLiterals() const { return rdfLiterals_; }

	protected:
		const std::vector<FramedTriplePatternPtr> rdfLiterals_;
	};

	using RDFGoalPtr = std::shared_ptr<RDFGoal>;

} // knowrob

#endif //KNOWROB_RDF_GOAL_H
