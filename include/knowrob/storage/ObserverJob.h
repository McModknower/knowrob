/*
 * This file is part of KnowRob, please consult
 * https://github.com/knowrob/knowrob for license details.
 */

#ifndef KNOWROB_OBSERVER_JOB_H
#define KNOWROB_OBSERVER_JOB_H

#include <memory>
#include "ObserverManager.h"
#include "knowrob/triples/GraphBuiltin.h"
#include "knowrob/triples/GraphUnion.h"
#include "knowrob/triples/GraphSequence.h"

namespace knowrob {

	class ObserverJob {
	public:
		ObserverJob(const std::shared_ptr<ObserverManager> &manager,
			const GraphQueryPtr &query,
			const AnswerHandler &callback);

		~ObserverJob();

		void processInsertion(const TripleContainerPtr &triples);

		void stop();

	protected:
		std::shared_ptr<ObserverManager> manager_;
		GraphQueryPtr query_;
		AnswerHandler callback_;

		struct Node {
			std::shared_ptr<GraphPattern> pattern;
			std::vector<std::shared_ptr<GraphBuiltin>> builtins;
			std::vector<std::shared_ptr<Node>> children;
			std::vector<Node*> parents;
			std::map<size_t, AnswerPtr> answers;
		};
		std::vector<std::shared_ptr<Node>> nodes_;

		using NodeParents = std::vector<std::shared_ptr<Node>>;
		NodeParents terminalNodes_;

		NodeParents createGraph(const std::shared_ptr<GraphTerm> &term, const NodeParents &parents);

		std::shared_ptr<ObserverJob::Node> createNode(const std::shared_ptr<GraphPattern> &pattern);

		void initializeNode(const std::shared_ptr<Node> &node);
		void initializeNode(const std::shared_ptr<Node> &node, const AnswerPtr &answer);

		GraphQueryPtr makeQuery(const std::vector<Node*> &reverseSequence);

		bool matches(const Node &node, const FramedTriple &triple);

		void insert(std::shared_ptr<Node> &node, const FramedTriple &triple);
	};

} // knowrob

#endif //KNOWROB_OBSERVER_JOB_H
