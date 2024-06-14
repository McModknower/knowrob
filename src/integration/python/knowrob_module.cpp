/*
 * This file is part of KnowRob, please consult
 * https://github.com/knowrob/knowrob for license details.
 */
// Check if MODULENAME is defined, and if not, define it with default "kb"
#ifndef MODULENAME
#define MODULENAME knowrob
#endif

#include <iostream>
#include <functional>
#include <future>
#include <memory>
#include <boost/python.hpp>
#include <boost/python/suite/indexing/vector_indexing_suite.hpp>

#include "knowrob/Logger.h"
#include "knowrob/terms/ListTerm.h"
#include "knowrob/terms/Numeric.h"
#include "knowrob/terms/Blank.h"
#include "knowrob/terms/String.h"
#include "knowrob/terms/IRIAtom.h"
#include "knowrob/formulas/Formula.h"
#include "knowrob/queries/QueryContext.h"
#include "knowrob/queries/QueryParser.h"
#include "knowrob/reasoner/Reasoner.h"
#include "knowrob/KnowledgeBase.h"
#include "knowrob/integration/python/converter.h"
#include "knowrob/integration/python/utils.h"
#include "knowrob/knowrob.h"
#include "knowrob/integration/InterfaceUtils.h"

using namespace knowrob;

static inline void register_common_types() {
	py::createType<Logger>();
	py::createType<Perspective>();
	py::createType<ModalOperator>();
	py::createType<PropertyTree>();
}

static inline void register_term_types() {
	py::createType<RDFNode>();
	py::createType<Term>();
	py::createType<Variable>();
	py::createType<Function>();
	py::createType<ListTerm>();
	py::createType<Atomic>();
	py::createType<Atom>();
	py::createType<Blank>();
	py::createType<IRIAtom>();
	py::createType<XSDAtomic>();
	py::createType<String>();
	py::createType<Numeric>();
	py::createType<Bindings>();
	// allow conversion between std::vector and python::list for Term objects.
	typedef std::vector<TermPtr> TermList;
	py::custom_vector_from_seq<TermPtr>();
	boost::python::class_<TermList>("TermList").def(boost::python::vector_indexing_suite<TermList, true>());
}

static inline void register_formula_types() {
	py::createType<Formula>();
	// allow conversion between std::vector and python::list for Formula objects.
	typedef std::vector<FormulaPtr> FormulaList;
	py::custom_vector_from_seq<FormulaPtr>();
	boost::python::class_<FormulaList>("FormulaList").def(boost::python::vector_indexing_suite<FormulaList, true>());
}

static inline void register_triple_types() {
	py::createType<FramedTriple>();
	py::createType<FramedTriplePattern>();
	py::createType<GraphSelector>();
	py::createType<TripleContainer>();
	// allow conversion between std::vector and python::list for FramedTriple objects.
	typedef std::vector<std::shared_ptr<FramedTriple>> TripleList;
	py::custom_vector_from_seq<std::shared_ptr<FramedTriple>>();
	boost::python::class_<TripleList>("TripleList").def(boost::python::vector_indexing_suite<TripleList, true>());
}

static void InitKnowledgeBaseWrapper(boost::python::list py_argv) {
	int argc = boost::python::len(py_argv);
	std::vector<char *> argv;

	for (int i = 0; i < argc; ++i) {
		std::string arg = boost::python::extract<std::string>(py_argv[i]);
		argv.push_back(arg.data());
	}

	// Call the actual InitKnowledgeBase function with the converted arguments
	knowrob::InitKnowledgeBase(argc, argv.data());
}

void InitKnowledgeBaseFromSysArgv() {
	using namespace boost::python;
	object sys = import("sys");
	list py_argv = extract<list>(sys.attr("argv"));
	// Add a default program name if sys.argv is empty or its first element is an empty string (seems to happen if
	// the python code is run without any arguments from the interpreter).
	if (len(py_argv) == 0 ||
		(len(py_argv) > 0 && extract<std::string>(py_argv[0]).check() && extract<std::string>(py_argv[0])().empty())) {
		py_argv[0] = "knowrob";
	}

	InitKnowledgeBaseWrapper(py_argv);
}

BOOST_PYTHON_MODULE (MODULENAME) {
	using namespace boost::python;
	using namespace knowrob::py;

	// convert std::string_view to python::str and vice versa.
	register_string_view_converter();

	/////////////////////////////////////////////////////
	// mappings for KnowRob types
	/////////////////////////////////////////////////////
	register_common_types();
	register_term_types();
	register_formula_types();
	register_triple_types();

	createType<TokenStream>();
	createType<QueryContext>();
	createType<QueryParser>();
	createType<DataSource>();
	createType<DataSourceHandler>();
	createType<Storage>();
	createType<Reasoner>();
	createType<KnowledgeBase>();
	createType<InterfaceUtils>();

	/////////////////////////////////////////////////////
	// mappings for optionals used in the structs above
	/////////////////////////////////////////////////////
	// Note: At the moment each optional must be listed individually in the module declaration.
	//       It would be nice if this could be avoided...
	python_optional<XSDType>();
	python_optional<std::string_view>();
	python_optional<double>();
	python_optional<PerspectivePtr>();

	/////////////////////////////////////////////////////
	// mappings for static functions
	def("InitKnowledgeBaseWithArgs", &InitKnowledgeBaseWrapper, "Initialize the Knowledge Base with arguments.");
	def("InitKnowledgeBase", &InitKnowledgeBaseFromSysArgv, "Initialize the Knowledge Base using sys.argv.");

}
