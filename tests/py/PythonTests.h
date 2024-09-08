/*
 * This file is part of KnowRob, please consult
 * https://github.com/knowrob/knowrob for license details.
 */

#ifndef KNOWROB_PYTHON_TESTS_H
#define KNOWROB_PYTHON_TESTS_H

#include <gtest/gtest.h>
#include <boost/python/import.hpp>
#include "knowrob/integration/python/utils.h"
#include "knowrob/Logger.h"

namespace python = boost::python;
using namespace knowrob;

class PythonTests : public testing::Test {
protected:
	static python::object test_module;
	static python::object knowrob_module;
	static python::object AssertionError;

	// Per-test-suite set-up.
	static void SetUpTestSuite(std::string_view module_name);

	// Per-test-suite tear-down.
	static void TearDownTestSuite();

	static python::object do_call(std::string_view file, uint32_t line, std::string_view method_name,
								  const std::function<python::object(python::object &)> &gn);

	static python::object
	call(std::string_view file, uint32_t line, std::string_view method_name, const python::object &args...);

	static python::object call(std::string_view file, uint32_t line, std::string_view method_name);
};

#define PYTHON_TEST_CALL0(method_name, ...) {  \
    py::gil_lock lock;            \
    call(__FILE__, __LINE__, method_name, __VA_ARGS__); } \
    static_assert(true, "")
#define PYTHON_TEST_CALL1(method_name) {  \
    py::gil_lock lock;                    \
    call(__FILE__, __LINE__, method_name); } \
    static_assert(true, "")

#endif //KNOWROB_PYTHON_TESTS_H
