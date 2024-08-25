/*
 * This file is part of KnowRob, please consult
 * https://github.com/knowrob/knowrob for license details.
 */

#ifndef KNOWROB_OBSERVER_MANAGER_H
#define KNOWROB_OBSERVER_MANAGER_H

#include <memory>
#include <knowrob/triples/GraphQuery.h>
#include <knowrob/storage/Transaction.h>
#include "Observer.h"

namespace knowrob {

	class ObserverManager : public std::enable_shared_from_this<ObserverManager> {
	public:
		ObserverManager();
		~ObserverManager();

		ObserverPtr observe(const GraphQueryPtr &query, const AnswerHandler &callback);

		void stopObservation(const ObserverPtr &observer);

		void insert(const TripleContainerPtr &triples);

		void remove(const TripleContainerPtr &triples);

		/**
		 * Block the current thread until all queued transactions have been processed.
		 */
		void sync();

		void query(const GraphQueryPtr &query, const AnswerHandler &callback);

	protected:
		struct Impl;
		std::unique_ptr<Impl> impl_;

		void run();

		void processTransaction(const std::shared_ptr<transaction::Transaction> &transaction);
	};

	using ObserverManagerPtr = std::shared_ptr<ObserverManager>;

} // knowrob

#endif //KNOWROB_OBSERVER_MANAGER_H
