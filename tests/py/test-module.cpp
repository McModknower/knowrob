/*
 * This file is part of KnowRob, please consult
 * https://github.com/knowrob/knowrob for license details.
 */

#include "PythonTests.h"
#include <knowrob/terms/Atom.h>
#include <boost/python/extract.hpp>
#include "knowrob/terms/String.h"
#include "knowrob/semweb/Triple.h"

namespace python = boost::python;
using namespace knowrob;

class BoostPythonTests : public PythonTests {
protected:
	// Per-test-suite set-up.
	static void SetUpTestSuite() {
		PythonTests::SetUpTestSuite("tests.py.test_boost_python");
	}
};

#define EXPECT_CONVERTIBLE_TO_PY(x) EXPECT_NO_THROW( EXPECT_FALSE( \
    py::call_with_gil<bool>([&]{ return boost::python::object(x).is_none(); })))

TEST_F(BoostPythonTests, atom_to_python) {
	// test that we can create a term in C++ and pass it to Python.
	auto atom = Atom::Tabled("hello");
	// terms can be passed by reference or by value
	EXPECT_CONVERTIBLE_TO_PY(atom);
	EXPECT_CONVERTIBLE_TO_PY(*atom);
	// pass atom into Python code and inspect it there
	EXPECT_NO_THROW(PYTHON_TEST_CALL0("atom_to_python", python::object(atom)));
	EXPECT_NO_THROW(PYTHON_TEST_CALL0("atom_to_python", python::object(*atom)));
}

TEST_F(BoostPythonTests, string_copy_from_python) {
	py::gil_lock lock;
	python::object s = call(__FILE__, __LINE__, "string_copy_from_python");
	EXPECT_FALSE(boost::python::object(s).is_none());
	auto extracted = boost::python::extract<String>(s);
	EXPECT_TRUE(extracted.check());
	if (extracted.check()) {
		const auto &str = extracted();
		EXPECT_EQ(str.stringForm(), "hello");
	}
}

TEST_F(BoostPythonTests, modify_triple_in_python) {
	// test that we can create a triple in C++ and pass it to Python where it is modified.
	// Note: it might not be safe to pass a FramedTripleView into Python to
	//       fill it with string data as the Python strings might be garbage collected
	//       at some point after returning the call.
	auto triple = std::make_shared<FramedTripleCopy>(
			"hello", "knows", "world");
	// pass triple into Python code and modify it there
	EXPECT_NO_THROW(PYTHON_TEST_CALL0("modify_triple_in_python", python::object(triple)));
	EXPECT_EQ(triple->subject(), "olleh");
	EXPECT_EQ(triple->predicate(), "swonk");
}

TEST_F(BoostPythonTests, optionals) {
	std::optional<std::string_view> opt_str_view;
	std::optional<XSDType> opt_xsd_type;
	std::optional<double> opt_double;
	EXPECT_NO_THROW(PYTHON_TEST_CALL0("optional_is_none", python::object(opt_str_view)));
	EXPECT_NO_THROW(PYTHON_TEST_CALL0("optional_is_none", python::object(opt_xsd_type)));
	EXPECT_NO_THROW(PYTHON_TEST_CALL0("optional_is_none", python::object(opt_double)));
	opt_xsd_type = XSDType::STRING;
	EXPECT_NO_THROW(PYTHON_TEST_CALL0("optional_is_not_none", python::object(opt_xsd_type)));
	EXPECT_NO_THROW(PYTHON_TEST_CALL0("set_xsd_optional", python::object(opt_xsd_type)));
	// value is copied, so the original value should not change
	EXPECT_EQ(opt_xsd_type, XSDType::STRING);
}

TEST_F(BoostPythonTests, connective_formula_in_python) {
	EXPECT_NO_THROW(PYTHON_TEST_CALL1("connective_formulas"));
}

TEST_F(BoostPythonTests, answer_queue_in_python) {
	EXPECT_NO_THROW(PYTHON_TEST_CALL1("answer_queue"));
}

TEST_F(BoostPythonTests, read_settings_from_dict) {
	EXPECT_NO_THROW(PYTHON_TEST_CALL1("read_settings_from_dict"));
}

TEST_F(BoostPythonTests, create_bindings) {
	EXPECT_NO_THROW(PYTHON_TEST_CALL1("create_bindings"));
}

TEST_F(BoostPythonTests, handle_property_tree) {
	EXPECT_NO_THROW(PYTHON_TEST_CALL1("handle_property_tree"));
}

TEST_F(BoostPythonTests, kb_positive_query) {
	std::string testfile = "tests/settings/kb-test.json";
	EXPECT_NO_THROW(PYTHON_TEST_CALL0("kb_positive_query", python::object(testfile)));
}

TEST_F(BoostPythonTests, kb_negative_query) {
	std::string testfile = "tests/settings/kb-test.json";
	EXPECT_NO_THROW(PYTHON_TEST_CALL0("kb_negative_query", python::object(testfile)));
}

TEST_F(BoostPythonTests, kb_dont_know_query) {
	std::string testfile = "tests/settings/kb-test.json";
	EXPECT_NO_THROW(PYTHON_TEST_CALL0("kb_dont_know_query", python::object(testfile)));
}

TEST_F(BoostPythonTests, kb_assert) {
	std::string testfile = "tests/settings/kb-test.json";
	EXPECT_NO_THROW(PYTHON_TEST_CALL0("kb_assert", python::object(testfile)));
}
