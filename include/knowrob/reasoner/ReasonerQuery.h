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

namespace knowrob {
	/**
	 */
	class ReasonerQuery : public Query {
	public:
		/**
		 * @param ctx the query context.
		 */
		explicit ReasonerQuery(FramedTriplePatternPtr literal, QueryContextPtr ctx = DefaultQueryContext());

		~ReasonerQuery() override;

		/**
		 * @return the literal.
		 */
		auto &literal() const { return literal_; }

		/**
		 * Pushes an answer into the output channel.
		 * @param answer an answer.
		 */
		void push(const AnswerPtr &answer) { outputChannel_->push(answer); }

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
		std::shared_ptr<FramedTriplePattern> literal_;

		// Override Query
		void write(std::ostream &os) const override { os << *literal_; }
	};

	using ReasonerQueryPtr = std::shared_ptr<ReasonerQuery>;
}

#endif //KNOWROB_REASONER_QUERY_H_
