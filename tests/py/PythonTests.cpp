/*
 * This file is part of KnowRob, please consult
 * https://github.com/knowrob/knowrob for license details.
 */

#include "PythonTests.h"

python::object PythonTests::test_module;
python::object PythonTests::knowrob_module;

void PythonTests::SetUpTestSuite(std::string_view module_name) {
	try {
		py::call_with_gil<void>([&] {
			// make sure the knowrob module is loaded, without it conversion of types won't work.
			knowrob_module = python::import("knowrob");
			test_module = python::import(module_name.data());
		});
	} catch (const std::exception &e) {
		KB_ERROR("Failed to set up test suite. {}", e.what());
	}
}

void PythonTests::TearDownTestSuite() {
	try {
		py::call_with_gil<void>([&] {
			test_module = {};
			knowrob_module = {};
		});
	} catch (const std::exception &e) {
		KB_ERROR("Failed to tear down test suite. {}", e.what());
	}
}

python::object PythonTests::do_call(std::string_view file, uint32_t line, std::string_view method_name,
									const std::function<python::object(python::object &)> &gn) {
	EXPECT_FALSE(test_module.is_none());
	if (test_module.is_none()) { return {}; }

	python::object fn = test_module.attr(method_name.data());
	EXPECT_FALSE(fn.is_none());
	if (fn.is_none()) { return {}; }

	try {
		return py::call<python::object>([&] { return gn(fn); });
	} catch (const PythonError &err) {
		GTEST_MESSAGE_AT_(file.data(), line, method_name.data(), testing::TestPartResult::kNonFatalFailure)
				<< err.what();
	}
	return {};
}

python::object
PythonTests::call(std::string_view file, uint32_t line, std::string_view method_name, const python::object &args...) {
	return do_call(
			file, line, method_name,
			[&](python::object &fn) { return fn(args); });
}

python::object PythonTests::call(std::string_view file, uint32_t line, std::string_view method_name) {
	return do_call(
			file, line, method_name,
			[&](python::object &fn) { return fn(); });
}
