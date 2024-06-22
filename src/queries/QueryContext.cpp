/*
 * This file is part of KnowRob, please consult
 * https://github.com/knowrob/knowrob for license details.
 */

#include "knowrob/queries/QueryContext.h"
#include "knowrob/integration/python/utils.h"

using namespace knowrob;

namespace knowrob::py {
	template<>
	void createType<QueryContext>() {
		using namespace boost::python;
		class_<QueryContext, std::shared_ptr<QueryContext>, boost::noncopyable>
				("QueryContext", init<int>())
				.def(init<const QueryContext &, const ModalOperatorPtr &>())
				.def_readwrite("queryFlags", &QueryContext::queryFlags)
				.def_readwrite("modalIteration", &QueryContext::modalIteration)
				.def_readwrite("selector", &QueryContext::selector);

		// QueryContextPtr uses `const QueryContext` which currently requires
		// a custom converter to be defined in order to be used in Python.
		register_ptr_to_python< std::shared_ptr< const QueryContext > >();
		implicitly_convertible< std::shared_ptr< QueryContext >, std::shared_ptr< const QueryContext > >();
	}
}
