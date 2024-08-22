/*
 * This file is part of KnowRob, please consult
 * https://github.com/knowrob/knowrob for license details.
 */

#ifndef KNOWROB_REASONER_MANAGER_H_
#define KNOWROB_REASONER_MANAGER_H_

#include "knowrob/plugins/PluginManager.h"
#include "knowrob/storage/StorageManager.h"
#include "knowrob/KnowledgeBase.h"
#include "knowrob/reasoner/GoalDrivenReasoner.h"
#include "knowrob/reasoner/DataDrivenReasoner.h"

namespace knowrob {
	/**
	 * Manages a set of available reasoning subsystems.
	 */
	class ReasonerManager : public PluginManager<Reasoner> {
	public:
		/**
		 * Create a new reasoner manager.
		 * @param kb the knowledge base associated with this manager.
		 * @param backendManager the backend manager associated with this manager.
		 */
		ReasonerManager(KnowledgeBase *kb, const std::shared_ptr<StorageManager> &backendManager);

		~ReasonerManager() override;

		/**
		 * @return the knowledge base associated with this manager.
		 */
		auto kb() const { return kb_; }

		/**
		 * @return the backend manager associated with this manager.
		 */
		auto backendManager() const { return backendManager_; }

		/**
		 * @return the goal-driven reasoners defined by this manager.
		 */
		auto &goalDriven() const { return goalDriven_; }

		/**
		 * @return the data-driven reasoners defined by this manager.
		 */
		auto &dataDriven() const { return dataDriven_; }

		/**
		 * @param indicator a predicate indicator.
		 * @return the reasoners that define the given relation.
		 */
		std::vector<GoalDrivenReasonerPtr> getReasonerForRelation(const PredicateIndicator &indicator) const;

		/**
		 * Return the backend associated with a reasoner if any.
		 * @param reasoner a defined reasoner.
		 * @return a backend or a null reference.
		 */
		std::shared_ptr<Storage> getReasonerStorage(const std::shared_ptr<NamedReasoner> &reasoner);

		// override PluginManager
		std::shared_ptr<NamedReasoner> loadPlugin(const boost::property_tree::ptree &config) override;

		// override PluginManager
		std::shared_ptr<NamedReasoner>
		addPlugin(std::string_view reasonerID, PluginLanguage language, const std::shared_ptr<Reasoner> &reasoner) override;

		/**
		 * Evaluate a query using a goal-driven reasoner.
		 * @param reasoner the reasoner to use.
		 * @param literals the query to evaluate.
		 * @param ctx the query context.
		 * @return a buffer that can be used to retrieve the results of the query.
		 */
		static TokenBufferPtr evaluateQuery(
				const GoalDrivenReasonerPtr &reasoner,
				const std::vector<FirstOrderLiteralPtr> &literals,
				const QueryContextPtr &ctx);

	private:
		KnowledgeBase *kb_;
		std::shared_ptr<StorageManager> backendManager_;
		// maps reasoner to their backends
		std::map<std::string_view, StoragePtr, std::less<>> reasonerBackends_;
		std::map<std::string_view, DataDrivenReasonerPtr> dataDriven_;
		std::map<std::string_view, GoalDrivenReasonerPtr> goalDriven_;

		void setReasonerStorage(const std::shared_ptr<NamedPlugin<Reasoner>> &plugin, const std::shared_ptr<Storage> &dataBackend);

		void initPlugin(const std::shared_ptr<NamedReasoner> &namedReasoner);

		static bool initializeReasoner(const std::shared_ptr<NamedReasoner> &namedReasoner, PropertyTree &config);
	};
}

// a macro for static registration of a reasoner type.
// reasoner types registered with this macro are builtin reasoners that are not
// loaded from a plugin.
#define KNOWROB_BUILTIN_REASONER(Name, Type) class Type ## _Registration{ static bool isRegistered; }; \
        bool Type ## _Registration::isRegistered = knowrob::PluginManager<knowrob::Reasoner>::addFactory<Type>(Name);

#endif //KNOWROB_REASONER_MANAGER_H_
