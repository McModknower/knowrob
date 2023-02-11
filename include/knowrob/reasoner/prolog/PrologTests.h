/*
 * Copyright (c) 2022, Daniel Beßler
 * All rights reserved.
 *
 * This file is part of KnowRob, please consult
 * https://github.com/knowrob/knowrob for license details.
 */

#ifndef KNOWROB_PROLOG_TESTS_H_
#define KNOWROB_PROLOG_TESTS_H_

#include <gtest/gtest.h>

#include "PrologReasoner.h"

namespace knowrob {
	/**
	 * A baseclass for prolog test fixtures.
	 */
	class PrologTestsBase: public testing::Test {
	protected:
		static void runPrologTests(const std::shared_ptr<knowrob::PrologReasoner> &reasoner,
								   const std::string &target);
	};

	template <class T> class PrologTests: public PrologTestsBase {
	protected:
		// Per-test-suite set-up.
		static void SetUpTestSuite() { reasoner();  }

		static void runTests(const std::string &t) { runPrologTests(reasoner(), t); }

		static std::shared_ptr<T> reasoner() {
			static std::shared_ptr<T> r;
			static int reasonerIndex_=0;
			if(!r) {
				std::stringstream ss;
				ss << "prolog" << reasonerIndex_++;
				r = std::make_shared<T>(ss.str());
				r->loadConfiguration(knowrob::ReasonerConfiguration());
			}
			return r;
		}
	};
}

#endif //KNOWROB_PROLOG_TESTS_H_
