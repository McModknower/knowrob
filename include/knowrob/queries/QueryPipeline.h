/*
 * This file is part of KnowRob, please consult
 * https://github.com/knowrob/knowrob for license details.
 */

#ifndef KNOWROB_QUERY_PIPELINE_H
#define KNOWROB_QUERY_PIPELINE_H

#include "memory"
#include "vector"
#include "TokenStream.h"
#include "knowrob/formulas/DependencyGraph.h"
#include "knowrob/semweb/GraphPathQuery.h"
#include "ConjunctiveQuery.h"
#include "knowrob/reasoner/Computable.h"

namespace knowrob {
	class KnowledgeBase;

	/**
	 * Holds a reference to pipeline stages during execution,
	 * and stops each stage on destruction ensuring that none of them
	 * continues broadcasting messages.
	 */
	class QueryPipeline {
	public:
		/**
		 * Create a query pipeline for the given formula.
		 * @param kb the knowledge base to query.
		 * @param phi the formula to query.
		 * @param ctx the query context.
		 */
		QueryPipeline(const std::shared_ptr<KnowledgeBase> &kb, const FormulaPtr &phi, const QueryContextPtr &ctx);

		/**
		 * Create a query pipeline for the given conjunctive query.
		 * @param kb the knowledge base to query.
		 * @param conjunctiveQuery the query to execute.
		 */
		QueryPipeline(const std::shared_ptr<KnowledgeBase> &kb, const ConjunctiveQueryPtr &conjunctiveQuery);

		~QueryPipeline();

		/**
		 * Stream the last stage of the pipeline into the given stage.
		 * @param stage the stage to stream the last stage into.
		 */
		void operator>>(const std::shared_ptr<TokenStream> &stage);

		/**
		 * After creation of the pipeline, messages are buffered until this is called.
		 */
		void stopBuffering();

	protected:
		std::vector<std::shared_ptr<TokenStream>> initialStages_;
		std::shared_ptr<TokenBroadcaster> finalStage_;
		std::shared_ptr<TokenBuffer> bufferStage_;

		void addInitialStage(const std::shared_ptr<TokenStream> &stage);

		static std::vector<ComputablePtr> createComputationSequence(
				const std::shared_ptr<KnowledgeBase> &kb,
				const std::list<DependencyNodePtr> &dependencyGroup);

		static void createComputationPipeline(
				const std::shared_ptr<KnowledgeBase> &kb,
				std::vector<ComputablePtr> &computableLiterals,
				const std::shared_ptr<TokenBroadcaster> &pipelineInput,
				const std::shared_ptr<TokenBroadcaster> &pipelineOutput,
				const QueryContextPtr &ctx);
	};

	/**
	 * A buffer that holds a reference to a query pipeline.
	 * This is used such that the underlying pipeline is destroyed once the
	 * user drops the reference to the buffer.
	 */
	class AnswerBuffer_WithReference : public TokenBuffer {
	public:
		explicit AnswerBuffer_WithReference(const std::shared_ptr<QueryPipeline> &pipeline)
				: TokenBuffer(), pipeline_(pipeline) {}

	protected:
		std::shared_ptr<QueryPipeline> pipeline_;
	};
}


#endif //KNOWROB_QUERY_PIPELINE_H
