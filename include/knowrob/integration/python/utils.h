/*
 * This file is part of KnowRob, please consult
 * https://github.com/knowrob/knowrob for license details.
 */

#ifndef KNOWROB_PY_UTILS_H
#define KNOWROB_PY_UTILS_H

#include <boost/python.hpp>
#include <filesystem>
#include "PythonError.h"
#include "gil.h"
#include "with.h"

namespace knowrob::py {
	// call a method of a python object
	template<typename R, typename... Args> R call_method(PyObject *self, const char *method, Args... args) {
		try {
			return boost::python::call_method<R>(self, method, boost::python::object(args)...);
		} catch(const boost::python::error_already_set&) {
			throw PythonError();
		}
	}

	/**
	 * Call a function and translate boost::python::error_already_set exceptions into PythonError.
	 * @param goal the function to call.
	 */
	template<typename R> R call(const std::function<R()>& goal) {
		try {
			return goal();
		} catch(const boost::python::error_already_set&) {
			throw PythonError();
		}
	}

	/**
	 * Same as call but locks the GIL before performing the call.
	 * @param goal the function to call.
	 */
	template<typename R> R call_with_gil(const std::function<R()>& goal) {
		py::gil_lock lock;
		return call(goal);
	}

	/**
	 * A wrapper class to call a Python object.
	 */
	class PyObj_wrap {
	public:
		explicit PyObj_wrap(boost::python::object self)
			: self_(std::move(self)) {}

		~PyObj_wrap() {
			gil_lock _lock;
			self_ = boost::python::object();
		}

		void operator()() {
			gil_lock _lock;
			self_();
		}

	protected:
		boost::python::object self_;
	};

	/**
	 * Resolve a module path to a file path.
	 * @param modulePath the module path.
	 * @return the file path.
	 */
	std::string resolveModulePath(std::string_view modulePath);

	/**
	 * Make sure that a Python file at path can be imported.
	 * @param modulePath the path to add.
	 * @return the import string needed to import the module.
	 */
	std::string addToSysPath(const std::filesystem::path& modulePath);

	/**
	 * A template function to create a new type in Python.
	 * @tparam T The C++ type to map into Python.
	 */
	template <typename T> void createType();
}

#endif //KNOWROB_PY_UTILS_H
