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
				   const std::vector<DefiningReasoner> &reasonerList);

		/**
		 * Create a computable first-order literal.
		 * @param predicate the predicate of the literal.
		 * @param isNegative true if the literal is negated.
		 * @param reasonerList the list of reasoners that define the predicate.
		 */
		Computable(const PredicatePtr &predicate, bool isNegative,
				   const std::vector<DefiningReasoner> &reasonerList);

		/**
		 * @return the list of reasoners.
		 */
		const auto &reasonerList() const { return reasonerList_; }

	protected:
		std::vector<DefiningReasoner> reasonerList_;
	};

	using ComputablePtr = std::shared_ptr<Computable>;

	/**
	 * Apply a substitution to a computable.
	 * @param lit the computable literal.
	 * @param bindings the substitution.
	 * @return the literal with the substitution applied.
	 */
	ComputablePtr applyBindings(const ComputablePtr &lit, const Bindings &bindings);

} // knowrob

#endif //KNOWROB_COMPUTABLE_H
