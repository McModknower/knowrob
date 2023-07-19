/*
 * Copyright (c) 2022, Daniel Beßler
 * All rights reserved.
 *
 * This file is part of KnowRob, please consult
 * https://github.com/knowrob/knowrob for license details.
 */

#ifndef KNOWROB_BACKEND_MANAGER_H_
#define KNOWROB_BACKEND_MANAGER_H_

#include <map>
#include <mutex>
#include <boost/property_tree/ptree.hpp>
#include "TypedKnowledgeGraphFactory.h"
#include "KnowledgeGraphPlugin.h"
#include "DefinedKnowledgeGraph.h"
#include "KnowledgeGraph.h"

namespace knowrob {
	/**
	 * Manages a set of available knowledgeGraph subsystems.
	 */
	class KnowledgeGraphManager {
	public:
		explicit KnowledgeGraphManager(const std::shared_ptr<ThreadPool> &threadPool);
        ~KnowledgeGraphManager();

        /**
         * @param managerID the ID of a knowledgeGraph manager
         * @return the knowledgeGraph manager, or nullptr if ID is unknown
         */
        static KnowledgeGraphManager* getManager(uint32_t managerID);

		/**
		 * Add a knowledgeGraph factory to the manager.
		 * Note that factories for shared libraries are created on the fly, and thus
		 * do not need to be added manually.
		 * @param typeName the name of the knowledgeGraph type
		 * @param factory a knowledgeGraph factory
		 */
		static bool addFactory(const std::string &typeName, const std::shared_ptr<KnowledgeGraphFactory> &factory);

		template<class T> static bool addFactory(const std::string &typeName)
		{ return addFactory(typeName, std::make_shared<TypedKnowledgeGraphFactory<T>>(typeName)); }

		/**
		 * Load a new backend instance into the knowledgeGraph manager.
		 * The type of the knowledgeGraph is determined based on either the value of
		 * "type" or "lib" in the property tree root. The tree is further used
		 * to generate a backend configuration used by the created knowledgeGraph.
		 * Backend factories for libraries are created on the fly, the ones
		 * for built-in backend types need to be added to the knowledgeGraph manager before.
		 * @param config a property tree holding a knowledgeGraph configuration
		 */
		void loadKnowledgeGraph(const boost::property_tree::ptree &config);

		/**
		 * @param backendID a knowledgeGraph ID string.
		 * @return a knowledgeGraph instance or a null pointer reference.
		 */
		std::shared_ptr<DefinedKnowledgeGraph> getKnowledgeGraphWithID(const std::string &backendID);

        /**
         * Add a knowledgeGraph to this manager.
         * @reasoner a defined knowledgeGraph.
         */
        std::shared_ptr<DefinedKnowledgeGraph> addKnowledgeGraph(
                const std::string &reasonerID, const std::shared_ptr<KnowledgeGraph> &backend);

        /**
         * @return map of all knowledgeGraph defined by this manager.
         */
        const auto& knowledgeGraphPool() const  { return knowledgeGraphPool_; }

        auto managerID() const  { return managerID_; }

	private:
        std::shared_ptr<ThreadPool> threadPool_;
		// maps backend type name to factory used to create instances of that type
		static std::map<std::string, std::shared_ptr<KnowledgeGraphFactory>> backendFactories_;
        // maps backend id to manager
        static std::map<uint32_t, KnowledgeGraphManager*> backendManagers_;
        // counts number of initialized managers
        static uint32_t managerIDCounter_;
        // mutex used to interact with static variables
        std::mutex staticMutex_;
		// pool of all backend instances created via this manager
		// maps backend ID to backend instance.
		std::map<std::string, std::shared_ptr<DefinedKnowledgeGraph>> knowledgeGraphPool_;
		// maps plugin names to factories used to create backend instances
		std::map<std::string, std::shared_ptr<KnowledgeGraphPlugin>> loadedPlugins_;
		// a counter used to generate unique IDs
		uint32_t backendIndex_;
        // an identifier for this manager
        uint32_t managerID_;

		std::shared_ptr<KnowledgeGraphPlugin> loadBackendPlugin(const std::string &path);

        /**
         * Remove a reasoner from this manager.
         * @reasoner a reasoner.
         */
        void removeBackend(const std::shared_ptr<DefinedKnowledgeGraph> &reasoner);
	};

	// a macro for static registration of a reasoner type.
	// reasoner types registered with this macro are builtin reasoners that are not
	// loaded from a plugin.
	#define KNOWROB_BUILTIN_BACKEND(Name,Type) class Type ## _Registration{ static bool isRegistered; }; \
		bool Type ## _Registration::isRegistered = BackendManager::addBackendFactory<Type>(Name);
}

#endif //KNOWROB_BACKEND_MANAGER_H_