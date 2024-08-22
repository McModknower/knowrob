/*
 * This file is part of KnowRob, please consult
 * https://github.com/knowrob/knowrob for license details.
 */

#include "knowrob/triples/GraphBuiltin.h"
#include "knowrob/integration/python/utils.h"

using namespace knowrob;

namespace knowrob::py {
	template<>
	void createType<GraphBuiltin>() {
		using namespace boost::python;

		class_<GraphBuiltin, bases<GraphTerm, Function>, std::shared_ptr<GraphBuiltin>, boost::noncopyable>
		        ("GraphBuiltin", no_init)
		        .def("setOptional", &GraphBuiltin::setOptional)
		        .def("isOptional", &GraphBuiltin::isOptional)
		        .def("bind", &GraphBuiltin::bind).staticmethod("bind")
		        .def("min", &GraphBuiltin::min).staticmethod("min")
		        .def("max", &GraphBuiltin::max).staticmethod("max")
		        .def("less", &GraphBuiltin::less).staticmethod("less")
		        .def("lessOrEqual", &GraphBuiltin::lessOrEqual).staticmethod("lessOrEqual")
		        .def("greater", &GraphBuiltin::greater).staticmethod("greater")
		        .def("greaterOrEqual", &GraphBuiltin::greaterOrEqual).staticmethod("greaterOrEqual")
		        .def("equal", &GraphBuiltin::equal).staticmethod("equal")
		        .def("notEqual", &GraphBuiltin::notEqual).staticmethod("notEqual");
	}
}
