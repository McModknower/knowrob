/*
 * This file is part of KnowRob, please consult
 * https://github.com/knowrob/knowrob for license details.
 */

#ifndef KNOWROB_GOAL_H_
#define KNOWROB_GOAL_H_

#include <ostream>
#include <utility>
#include <knowrob/semweb/TriplePattern.h>
#include <knowrob/queries/TokenBuffer.h>
#include <knowrob/queries/Answer.h>
#include "knowrob/formulas/SimpleConjunction.h"

namespace knowrob {
	/**
	 * A query that can be submitted to a reasoner.
	 */
	class Goal : public Query {
	public:
		/**
		 * @param formula a formula.
		 * @param ctx a query context.
		 */
		explicit Goal(SimpleConjunctionPtr formula, QueryContextPtr ctx = DefaultQueryContext());

		/**
		 * @param literal a literal.
		 * @param ctx a query context.
		 */
		explicit Goal(const FirstOrderLiteralPtr &literal, QueryContextPtr ctx = DefaultQueryContext());

		/**
		 * @param formula a formula.
		 * @param goal a goal.
		 */
		Goal(SimpleConjunctionPtr formula, const Goal &goal);

		~Goal() override;

		/**
		 * @return the literal.
		 */
		auto &formula() const { return formula_; }

		/**
		 * Pushes an answer into the output channel.
		 * @param answer an answer.
		 */
		void push(const AnswerPtr &answer);

		/**
		 * Pushes a positive answer into the output channel.
		 * @param bindings the bindings under which the query is true.
		 */
		void push(const BindingsPtr &bindings);

		/**
		 * Indicates that the evaluation has finished.
		 */
		void finish() { outputChannel_->push(EndOfEvaluation::get()); }

		/**
		 * @return the answer buffer.
		 */
		auto &answerBuffer() const { return answerBuffer_; }

		/**
		 * @return the output channel.
		 */
		auto &outputChannel() const { return outputChannel_; }

	protected:
		std::shared_ptr<const QueryContext> ctx_;
		std::shared_ptr<TokenBuffer> answerBuffer_;
		std::shared_ptr<TokenStream::Channel> outputChannel_;
		std::shared_ptr<SimpleConjunction> formula_;

		// Override Query
		void write(std::ostream &os) const override { os << *formula_; }
	};

	using GoalPtr = std::shared_ptr<Goal>;
}

#endif //KNOWROB_GOAL_H_
