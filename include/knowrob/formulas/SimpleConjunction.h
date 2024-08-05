/*
 * This file is part of KnowRob, please consult
 * https://github.com/knowrob/knowrob for license details.
 */

#ifndef KNOWROB_SIMPLE_CONJUNCTION_H
#define KNOWROB_SIMPLE_CONJUNCTION_H

#include "Conjunction.h"
#include "FirstOrderLiteral.h"

namespace knowrob {
	/**
	 * A conjunction of literals.
	 */
	class SimpleConjunction : public Conjunction {
	public:
		/**
		 * @param literals sequence of literals in conjunction.
		 */
		explicit SimpleConjunction(const std::vector<FirstOrderLiteralPtr> &literals);

		/**
		 * @param literal a literal.
		 */
		explicit SimpleConjunction(const FirstOrderLiteralPtr &literal);

		/**
		 * @return sequence of literals in conjunction.
		 */
		auto &literals() const { return literals_; }

	protected:
		std::vector<FirstOrderLiteralPtr> literals_;
	};

	using SimpleConjunctionPtr = std::shared_ptr<SimpleConjunction>;

} // knowrob

#endif //KNOWROB_SIMPLE_CONJUNCTION_H
