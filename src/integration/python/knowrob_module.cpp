/*
 * This file is part of KnowRob, please consult
 * https://github.com/knowrob/knowrob for license details.
 */

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
#include "knowrob/storage/redland/RedlandModel.h"
#include "knowrob/integration/prolog/PrologBackend.h"
#include "knowrob/storage/mongo/MongoKnowledgeGraph.h"

#ifndef MODULENAME
#define MODULENAME knowrob
#endif

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
	// allow conversion between std::vector and python::list for FirstOrderLiteral objects.
	typedef std::vector<std::shared_ptr<FirstOrderLiteral>> LiteralList;
	py::custom_vector_from_seq<std::shared_ptr<FirstOrderLiteral>>();
	boost::python::class_<LiteralList>("LiteralList").def(boost::python::vector_indexing_suite<LiteralList, true>());
}

static inline void register_triple_types() {
	py::createType<Triple>();
	py::createType<FramedTriplePattern>();
	py::createType<GraphSelector>();
	py::createType<TripleContainer>();
	// allow conversion between std::vector and python::list for FramedTriple objects.
	typedef std::vector<std::shared_ptr<Triple>> TripleList;
	py::custom_vector_from_seq<std::shared_ptr<Triple>>();
	boost::python::class_<TripleList>("TripleList").def(boost::python::vector_indexing_suite<TripleList, true>());
}

BOOST_PYTHON_MODULE (MODULENAME) {
	using namespace boost::python;
	using namespace knowrob::py;

	// convert std::string_view to python::str and vice versa.
	register_string_view_converter();
	register_pair_converter();
	register_dict_to_map_converter();
	register_list_converter();

	/////////////////////////////////////////////////////
	// mappings for KnowRob types
	/////////////////////////////////////////////////////
	register_common_types();
	register_term_types();
	register_formula_types();
	register_triple_types();

	knowrob::py::staticKnowRobModuleInit();
	createType<TokenStream>();
	createType<QueryContext>();
	createType<QueryParser>();
	createType<DataSource>();
	createType<DataSourceHandler>();
	createType<Storage>();
	createType<Reasoner>();
	createType<KnowledgeBase>();
	createType<InterfaceUtils>();

	// register builtin storage types.
	// without it seems the to-python conversion is not mapping to QueryableStorage but only Storage type!
	class_<RedlandModel, std::shared_ptr<RedlandModel>, bases<QueryableStorage>, boost::noncopyable>
			("RedlandModel", init<>());
	class_<PrologBackend, std::shared_ptr<PrologBackend>, bases<QueryableStorage>, boost::noncopyable>
			("PrologBackend", init<>());
	class_<MongoKnowledgeGraph, std::shared_ptr<MongoKnowledgeGraph>, bases<QueryableStorage>, boost::noncopyable>
			("MongoKnowledgeGraph", init<>());

	/////////////////////////////////////////////////////
	// mappings for optionals used in the structs above
	/////////////////////////////////////////////////////
	// Note: At the moment each optional must be listed individually in the module declaration.
	//       It would be nice if this could be avoided...
	python_optional<XSDType>();
	python_optional<std::string_view>();
	python_optional<double>();
	python_optional<PerspectivePtr>();
}
