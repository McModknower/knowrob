/*
 * Copyright (c) 2022, Daniel Beßler
 * All rights reserved.
 *
 * This file is part of KnowRob, please consult
 * https://github.com/knowrob/knowrob for license details.
 */

#ifndef KNOWROB_BACKEND_PLUGIN_H_
#define KNOWROB_BACKEND_PLUGIN_H_

#include <string>
#include <memory>
#include "KnowledgeGraphFactory.h"
#include "KnowledgeGraph.h"

namespace knowrob {
	/**
	 * A backend factory that uses a knowledgeGraph shared library
	 * for creation of knowledgeGraph instances.
	 * Backend plugins are usually defined in shared libraries
	 * through the BACKEND_PLUGIN macro.
	 * They need to expose a set of functions that are used as
	 * an entry point for loading the plugin.
	 */
	class KnowledgeGraphPlugin : public KnowledgeGraphFactory {
	public:
		/**
		 * @param dllPath the name or path of the shared library.
		 */
		explicit KnowledgeGraphPlugin(std::string dllPath);

		~KnowledgeGraphPlugin() override;

		/**
		 * Cannot be copy-assigned.
		 */
		KnowledgeGraphPlugin(const KnowledgeGraphPlugin&) = delete;

		/**
		 * @return true if the shared library was loaded successfully.
		 */
		bool isLoaded();

		/**
		 * Try loading the shared library from filesystem.
		 * Note that, on unix-based systems, the LD_LIBRARY_PATH environment
		 * variable is used to locate the library.
		 * @return true on success.
		 */
		bool loadDLL();

		// Override BackendFactory
		std::shared_ptr<KnowledgeGraph> createKnowledgeGraph(const std::string &backendID) override;

		// Override BackendFactory
		const std::string& name() const override {  return name_; };

	protected:
		const std::string dllPath_;
		std::string name_;
		// handle of opened library
		void *handle_;
		// a factory function used to create new instances of a backend.
		std::shared_ptr<KnowledgeGraph> (*create_)(const std::string &backendID);
		// a function that returns the name of the plugin
		char* (*get_name_)();
	};
}

/**
 * Define a KnowledgeGraph knowledgeGraph plugin.
 * The macro generates two functions that are used as entry points for
 * loading the plugin.
 * First, a factory function is defined that creates instances of @classType.
 * This will only work when @classType has a single argument constructor that
 * accepts a string as argument (the knowledgeGraph instance ID).
 * Second, a function is generated that exposes the plugin name.
 * @classType the type of the knowledgeGraph, must be a subclass of KnowledgeGraph
 * @pluginName a plugin identifier, e.g. the name of the knowledgeGraph type.
 */
#define BACKEND_PLUGIN(classType, pluginName) extern "C" { \
		std::shared_ptr<knowrob::KnowledgeGraph> knowrob_createKnowledgeGraph(const std::string &backendID) \
			{ return std::make_shared<classType>(backendID); } \
		const char* knowrob_getPluginName() { return pluginName; } }

#endif //KNOWROB_BACKEND_PLUGIN_H_