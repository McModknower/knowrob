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
	/**
	 * An observer manager is responsible for managing observers.
	 */
	class ObserverManager : public std::enable_shared_from_this<ObserverManager> {
	public:
		/**
		 * Create an observer manager.
		 * @param backend the backend to observe.
		 */
		explicit ObserverManager(const QueryableBackendPtr &backend);

		~ObserverManager();

		/**
		 * Observe a query.
		 * @param query the query to observe.
		 * @param callback the callback to invoke when the query matches.
		 * @return the observer.
		 */
		ObserverPtr observe(const GraphQueryPtr &query, const BindingsHandler &callback);

		/**
		 * Stop observing a query.
		 * @param observer the observer to stop.
		 */
		void stopObservation(const Observer &observer);

		/**
		 * Insert triples.
		 * @param triples the triples to insert.
		 */
		void insert(const TripleContainerPtr &triples);

		/**
		 * Remove triples.
		 * @param triples the triples to remove.
		 */
		void remove(const TripleContainerPtr &triples);

		/**
		 * Query the backend.
		 * @param query the query to execute.
		 * @param callback the callback to invoke with the results.
		 */
		void query(const GraphQueryPtr &query, const BindingsHandler &callback);

	protected:
		struct Impl;
		std::unique_ptr<Impl> impl_;
		QueryableBackendPtr backend_;

		void run();
	};

	using ObserverManagerPtr = std::shared_ptr<ObserverManager>;

} // knowrob

#endif //KNOWROB_OBSERVER_MANAGER_H
