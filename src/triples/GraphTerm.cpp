/*
 * This file is part of KnowRob, please consult
 * https://github.com/knowrob/knowrob for license details.
 */

#include <boost/python/suite/indexing/vector_indexing_suite.hpp>
#include "knowrob/triples/GraphTerm.h"
#include "knowrob/integration/python/utils.h"
#include "knowrob/triples/GraphSequence.h"
#include "knowrob/triples/GraphUnion.h"
#include "knowrob/triples/GraphPattern.h"
#include "knowrob/integration/python/converter/vector.h"
#include "knowrob/triples/GraphBuiltin.h"

using namespace knowrob;

namespace std {
	std::ostream &operator<<(std::ostream &os, const knowrob::GraphTerm &t) {
		t.write(os);
		return os;
	}
}

namespace knowrob::py {
	template<>
	void createType<GraphTerm>() {
		using namespace boost::python;

		enum_<GraphTermType>("GraphTermType")
		        .value("Sequence", GraphTermType::Sequence)
		        .value("Union", GraphTermType::Union)
		        .value("Pattern", GraphTermType::Pattern)
		        .value("Builtin", GraphTermType::Builtin)
		        .export_values();

		class_<GraphTerm, std::shared_ptr<GraphTerm>, boost::noncopyable>
		        ("GraphTerm", no_init)
		        .def("isPattern", &GraphTerm::isPattern)
		        .def("isBuiltin", &GraphTerm::isBuiltin)
		        .def("termType", &GraphTerm::termType);

		class_<GraphPattern, bases<GraphTerm>, std::shared_ptr<GraphPattern>, boost::noncopyable>
		        ("GraphPattern", init<FramedTriplePatternPtr>())
		        .def(init<const TermPtr&, const TermPtr&, const TermPtr&>())
		        .def("value", &GraphPattern::value, return_value_policy<copy_const_reference>());

		createType<GraphUnion>();
		createType<GraphSequence>();
		createType<GraphBuiltin>();

		// allow conversion between std::vector and python::list for FirstOrderLiteral objects.
		typedef std::vector<std::shared_ptr<GraphTerm>> GraphTermList;
		py::custom_vector_from_seq<std::shared_ptr<GraphTerm>>();
		boost::python::class_<GraphTermList>("GraphTermList").def(boost::python::vector_indexing_suite<GraphTermList, true>());
	}
}
