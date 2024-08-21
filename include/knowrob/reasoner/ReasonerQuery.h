/*
 * This file is part of KnowRob, please consult
 * https://github.com/knowrob/knowrob for license details.
 */

#ifndef KNOWROB_REASONER_QUERY_H_
#define KNOWROB_REASONER_QUERY_H_

#include <ostream>
#include <utility>
#include <knowrob/triples/FramedTriplePattern.h>
#include <knowrob/queries/TokenBuffer.h>
#include <knowrob/queries/Answer.h>
#include "knowrob/formulas/SimpleConjunction.h"

namespace knowrob {
	/**
	 * A query that can be submitted to a reasoner.
	 */
	class ReasonerQuery : public Query {
	public:
		/**
		 * @param formula a formula.
		 * @param ctx a query context.
		 */
		explicit ReasonerQuery(SimpleConjunctionPtr formula, QueryContextPtr ctx = DefaultQueryContext());

		/**
		 * @param literal a literal.
		 * @param ctx a query context.
		 */
		explicit ReasonerQuery(const FirstOrderLiteralPtr &literal, QueryContextPtr ctx = DefaultQueryContext());

		~ReasonerQuery() override;

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

	protected:
		std::shared_ptr<const QueryContext> ctx_;
		std::shared_ptr<TokenBuffer> answerBuffer_;
		std::shared_ptr<TokenStream::Channel> outputChannel_;
		std::shared_ptr<SimpleConjunction> formula_;

		// Override Query
		void write(std::ostream &os) const override { os << *formula_; }
	};

	using ReasonerQueryPtr = std::shared_ptr<ReasonerQuery>;
}

#endif //KNOWROB_REASONER_QUERY_H_
