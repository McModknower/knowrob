/*
 * This file is part of KnowRob, please consult
 * https://github.com/knowrob/knowrob for license details.
 */

#ifndef KNOWROB_OBSERVER_JOB_H
#define KNOWROB_OBSERVER_JOB_H

#include <memory>
#include "ObserverManager.h"
#include "knowrob/semweb/GraphBuiltin.h"
#include "knowrob/semweb/GraphUnion.h"
#include "knowrob/semweb/GraphSequence.h"

namespace knowrob {
	/**
	 * An observer job is a query that is observed by an observer manager.
	 */
	class ObserverJob {
	public:
		/**
		 * Create an observer job.
		 * @param manager the observer manager.
		 * @param query the query to observe.
		 * @param callback the callback to invoke when the query matches.
		 */
		ObserverJob(const std::shared_ptr<ObserverManager> &manager,
					const GraphQueryPtr &query,
					const BindingsHandler &callback);

		~ObserverJob();

		/**
		 * Stop the observer job.
		 */
		void stop();

		/**
		 * Process the insertion of triples.
		 * @param triples the triples to insert.
		 */
		void processInsertion(const TripleContainerPtr &triples);

		/**
		 * Process the removal of triples.
		 * @param triples the triples to remove.
		 */
		void processRemoval(const TripleContainerPtr &triples);

		/**
		 * @return the observer manager.
		 */
		auto &manager() const { return manager_; }

	protected:
		std::shared_ptr<ObserverManager> manager_;
		GraphQueryPtr query_;
		BindingsHandler callback_;

		struct Node {
			std::shared_ptr<GraphPattern> pattern;
			std::vector<std::shared_ptr<GraphBuiltin>> builtins;
			std::vector<std::shared_ptr<Node>> children;
			std::vector<Node *> parents;
			std::map<size_t, BindingsPtr> solutions;
		};
		std::vector<std::shared_ptr<Node>> nodes_;

		using NodeParents = std::vector<std::shared_ptr<Node>>;
		NodeParents terminalNodes_;

		NodeParents createGraph(const std::shared_ptr<GraphTerm> &term, const NodeParents &parents);

		std::shared_ptr<ObserverJob::Node> createNode(const std::shared_ptr<GraphPattern> &pattern);

		void initializeNode(const std::shared_ptr<Node> &node);

		void initializeNode(const std::shared_ptr<Node> &node, const BindingsPtr &bindings);

		GraphQueryPtr makeQuery(const std::vector<Node *> &reverseSequence);

		GraphQueryPtr makeAtomicQuery(const std::shared_ptr<Node> &node, const BindingsPtr &bindings);

		bool matches(const Node &node, const Triple &triple);

		void remove(const std::shared_ptr<Node> &node, const Triple &triple);

		void insert(const std::shared_ptr<Node> &node, const Triple &triple);

		void doInsert(const std::shared_ptr<Node> &node, const BindingsPtr &newBindings);

		void doInsert(const std::shared_ptr<Node> &node,
			const BindingsPtr &parentBindings,
			const BindingsPtr &nodeBindings);

		BindingsPtr applyBuiltins(const std::shared_ptr<Node> &node, const BindingsPtr &bindings);
	};

} // knowrob

#endif //KNOWROB_OBSERVER_JOB_H
