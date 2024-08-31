/*
 * This file is part of KnowRob, please consult
 * https://github.com/knowrob/knowrob for license details.
 */

#ifndef KNOWROB_PROLOG_TESTS_H_
#define KNOWROB_PROLOG_TESTS_H_

#include <gtest/gtest.h>

#include "PrologReasoner.h"
#include "knowrob/reasoner/ReasonerManager.h"
#include "knowrob/Logger.h"
#include "knowrob/knowrob.h"

namespace knowrob {
	/**
	 * A baseclass for prolog test fixtures.
	 */
	class PrologTestsBase : public testing::Test {
	protected:
		static void runPrologTests(
				const std::shared_ptr<knowrob::PrologReasoner> &reasoner,
				const std::string &target);
	};

	template<class ReasonerType, class BackendType>
	class PrologTests : public PrologTestsBase {
	protected:
		static std::shared_ptr<ReasonerType> reasoner_;
		static std::shared_ptr<KnowledgeBase> kb_;
		static std::shared_ptr<BackendType> db_;

		static std::shared_ptr<BackendType> createBackend(const std::string &name, const std::shared_ptr<KnowledgeBase> &kb) {
			auto db = std::make_shared<BackendType>();
			kb->backendManager()->addPlugin(name, PluginLanguage::CPP, db);
			db->initializeBackend(knowrob::PropertyTree());
			return db;
		}

		static std::shared_ptr<ReasonerType> createReasoner(const std::string &name, const std::shared_ptr<KnowledgeBase> &kb, const std::shared_ptr<BackendType> &db) {
			auto r = std::make_shared<ReasonerType>();
			kb->reasonerManager()->addPlugin(name, PluginLanguage::CPP, r);
			r->setStorage(db);
			r->initializeReasoner(knowrob::PropertyTree());
			return r;
		}

		// Per-test-suite set-up.
		static void SetUpTestSuite() {
			// Initialize the reasoner
			try {
				reasoner();
			} catch (std::exception &e) {
				FAIL() << "SetUpTestSuite failed: " << e.what();
			}
		}

		// Per-test-suite tear-down.
		static void TearDownTestSuite() {
			kb_ = nullptr;
			db_ = nullptr;
			reasoner_ = nullptr;
		}

		static void runTests(const std::string &t) {
			try {
				runPrologTests(reasoner(), t);
			} catch (std::exception &e) {
				FAIL() << "runTests failed: " << e.what();
			}
		}

		static std::shared_ptr<ReasonerType> reasoner() {
			if(!reasoner_) {
				std::stringstream ss;
				ss << "prolog_";
				insertUnique(ss);

				kb_ = KnowledgeBase::create();
				db_ = createBackend(ss.str(), kb_);
				reasoner_ = createReasoner(ss.str(), kb_, db_);

				kb_->loadCommon();
				kb_->init();
			}
			return reasoner_;
		}
	};

	template <class ReasonerType, class BackendType>
	std::shared_ptr<ReasonerType> PrologTests<ReasonerType,BackendType>::reasoner_;

	template <class ReasonerType, class BackendType>
	std::shared_ptr<KnowledgeBase> PrologTests<ReasonerType,BackendType>::kb_;

	template <class ReasonerType, class BackendType>
	std::shared_ptr<BackendType> PrologTests<ReasonerType,BackendType>::db_;
}

#endif //KNOWROB_PROLOG_TESTS_H_
