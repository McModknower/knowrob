/*
 * This file is part of KnowRob, please consult
 * https://github.com/knowrob/knowrob for license details.
 */

#include <knowrob/Logger.h>
#include <knowrob/ThreadPool.h>
#include <knowrob/integration/python/gil.h>
#include <knowrob/integration/python/converter/shared_ptr.h>

#define KNOWROB_PY_USE_SHARED_PTR_DELETER_QUEUE

namespace boost::python::converter {
	void shared_ptr_deleter::operator()(void const *) {
		// This is needed to release the Python GIL when the shared_ptr is deleted.
		// Otherwise, if the shared_ptr is deleted in a C++ thread,
		// Python will crash because the GIL is not acquired.
		// There was also a debate about adding this to boost which was not really conclusive
		// ite seems.
		// @see https://github.com/boostorg/python/pull/11

#ifdef KNOWROB_PY_USE_SHARED_PTR_DELETER_QUEUE
		static knowrob::ThreadPool gil_pool(1);
		// release the shared_ptr such that we can call reset in a worker thread
		auto released_ptr = owner.release();
		// a lambda worker that will lock the GIL and delete the pointer.
		// note that calling reset in the current thread directly may cause deadlocks.
		auto runner = std::make_shared<knowrob::ThreadPool::LambdaRunner>(
			[released_ptr](const knowrob::ThreadPool::LambdaRunner::StopChecker &) {
				knowrob::py::gil_lock gil;
				handle<> ptr_handle(released_ptr);
				ptr_handle.reset();
			});
		gil_pool.pushWork(runner, [](const std::exception &e) {
			KB_WARN("an exception occurred when deleting ptr: {}.", e.what());
			throw;
		});
#else
		knowrob::py::gil_lock gil;
		owner.reset();
#endif
	}
}

namespace boost {
	// This is needed for boost::python to work with std::shared_ptr.
	// @see https://stackoverflow.com/questions/46435509/boostpython-stdshared-ptr-to-stdshared-ptr
	template<class T>
	T *get_pointer(std::shared_ptr<T> p) { return p.get(); }
}
