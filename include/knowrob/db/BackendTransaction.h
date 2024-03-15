/*
 * This file is part of KnowRob, please consult
 * https://github.com/knowrob/knowrob for license details.
 */

#ifndef KNOWROB_DATA_TRANSACTION_H
#define KNOWROB_DATA_TRANSACTION_H

#include "memory"
#include "knowrob/ThreadPool.h"
#include "knowrob/db/DataBackend.h"
#include "knowrob/triples/TripleContainer.h"
#include "DefinedBackend.h"

namespace knowrob::transaction {
	/**
	 * Baseclass for transactions.
	 * A set of backends can be added to a transaction. When a transaction is committed,
	 * the triple is committed to all backends. If a backend does not support the context
	 * of the triple, the triple is reified and the reified triples are committed to the backend.
	 */
	class Transaction {
	public:
		Transaction(const std::shared_ptr<semweb::Vocabulary> &vocabulary,
					const std::shared_ptr<semweb::ImportHierarchy> &importHierarchy)
				: vocabulary_(vocabulary), importHierarchy_(importHierarchy) {}

		/**
		 * Adds a backend to the transaction.
		 * @param backend the backend to add.
		 */
		void addBackend(const std::shared_ptr<DefinedBackend> &backend) { backends_.push_back(backend); }

		/**
		 * Commits a triple to all backends.
		 * @param triple the triple to commit.
		 * @return true if the triple was committed to all backends, false otherwise.
		 */
		bool commit(const FramedTriple &triple);

		/**
		 * Commits a set of triples to all backends.
		 * @param triples the triples to commit.
		 * @return true if the triples were committed to all backends, false otherwise.
		 */
		bool commit(const semweb::TripleContainerPtr &triples);

	protected:
		std::shared_ptr<semweb::Vocabulary> vocabulary_;
		std::shared_ptr<semweb::ImportHierarchy> importHierarchy_;
		std::vector<std::shared_ptr<DefinedBackend>> backends_;

		virtual bool commit(const FramedTriple &triple, const DataBackendPtr &backend) = 0;

		virtual bool commit(const semweb::TripleContainerPtr &triples, const DataBackendPtr &backend) = 0;

		virtual void updateVocabulary(const FramedTriple &triple) = 0;

		static std::shared_ptr<ThreadPool::Runner> createTripleWorker(
				const semweb::TripleContainerPtr &triples,
				const std::function<void(const FramedTriplePtr &)> &fn);
	};

	/**
	 * A data insertion transaction.
	 */
	class Insert : public Transaction {
	public:
		Insert(const std::shared_ptr<semweb::Vocabulary> &vocabulary,
			   const std::shared_ptr<semweb::ImportHierarchy> &importHierarchy)
				: Transaction(vocabulary, importHierarchy) {}

	protected:
		bool commit(const FramedTriple &triple, const DataBackendPtr &backend) override;

		bool commit(const semweb::TripleContainerPtr &triples, const DataBackendPtr &backend) override;

		void updateVocabulary(const FramedTriple &triple) override;
	};

	/**
	 * A data removal transaction.
	 */
	class Remove : public Transaction {
	public:
		Remove(const std::shared_ptr<semweb::Vocabulary> &vocabulary,
			   const std::shared_ptr<semweb::ImportHierarchy> &importHierarchy)
				: Transaction(vocabulary, importHierarchy) {}

	protected:
		bool commit(const FramedTriple &triple, const DataBackendPtr &backend) override;

		bool commit(const semweb::TripleContainerPtr &triples, const DataBackendPtr &backend) override;

		void updateVocabulary(const FramedTriple &triple) override;
	};
}

#endif //KNOWROB_DATA_TRANSACTION_H