/*
 * This file is part of KnowRob, please consult
 * https://github.com/knowrob/knowrob for license details.
 */

#ifndef KNOWROB_COMPUTABLE_H
#define KNOWROB_COMPUTABLE_H

#include "knowrob/formulas/FirstOrderLiteral.h"
#include "GoalDrivenReasoner.h"

namespace knowrob {
	/**
	 * A computable first-order literal that can be evaluated by a goal-driven reasoner.
	 * @see GoalDrivenReasoner
	 */
	class Computable : public FirstOrderLiteral {
	public:
		/**
		 * Create a computable first-order literal.
		 * @param literal the first-order literal.
		 * @param reasonerList the list of reasoners.
		 */
		Computable(const FirstOrderLiteral &literal,
				   const std::vector<std::shared_ptr<GoalDrivenReasoner>> &reasonerList)
				: FirstOrderLiteral(literal), reasonerList_(reasonerList) {}

		/**
		 * @return the list of reasoners.
		 */
		const auto &reasonerList() const { return reasonerList_; }

	protected:
		std::vector<std::shared_ptr<GoalDrivenReasoner>> reasonerList_;
	};

	using ComputablePtr = std::shared_ptr<Computable>;

} // knowrob

#endif //KNOWROB_COMPUTABLE_H
