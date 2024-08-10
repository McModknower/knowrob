/*
 * This file is part of KnowRob, please consult
 * https://github.com/knowrob/knowrob for license details.
 */

#ifndef KNOWROB_PY_CONVERTER_SHARED_PTR_H
#define KNOWROB_PY_CONVERTER_SHARED_PTR_H

#include <boost/python.hpp>
#include <memory>
#include <knowrob/integration/python/gil.h>

namespace boost::python::converter {
	void shared_ptr_deleter::operator()(void const*) {
		// This is needed to release the Python GIL when the shared_ptr is deleted.
		// Otherwise, if the shared_ptr is deleted in a C++ thread,
		// Python will crash because the GIL is not acquired.
		// There was also a debate about adding this to boost which was not really conclusive
		// ite seems.
		// @see https://github.com/boostorg/python/pull/11
		knowrob::py::gil_lock gil;
		owner.reset();
	}
}

namespace boost {
	// This is needed for boost::python to work with std::shared_ptr.
	// @see https://stackoverflow.com/questions/46435509/boostpython-stdshared-ptr-to-stdshared-ptr
	template<class T>
	T *get_pointer(std::shared_ptr<T> p) { return p.get(); }
}

#endif //KNOWROB_PY_CONVERTER_SHARED_PTR_H
