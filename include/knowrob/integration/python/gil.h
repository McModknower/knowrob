/*
 * This file is part of KnowRob, please consult
 * https://github.com/knowrob/knowrob for license details.
 */

#ifndef KNOWROB_PY_GIL_H
#define KNOWROB_PY_GIL_H

#include <boost/python.hpp>

namespace knowrob::py {
	/**
	 * RAII class to release the GIL.
	 * This is useful when executing long running C++ code that does not need the GIL,
	 * i.e. because it does not interact with the Python interpreter.
	 */
	struct no_gil {
	public:
		no_gil() { state_ = PyEval_SaveThread(); }

		~no_gil() { PyEval_RestoreThread(state_); }

	private:
		PyThreadState *state_;
	};

	/**
	 * RAII class to acquire the GIL.
	 * This is needed when executing C++ code that interacts with the Python interpreter.
	 */
	class gil_lock {
	public:
		gil_lock() { state_ = PyGILState_Ensure(); }

		~gil_lock() { PyGILState_Release(state_); }

	private:
		PyGILState_STATE state_;
	};
}

#endif //KNOWROB_PY_GIL_H
