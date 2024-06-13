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

		enum_<QueryFlag>("QueryFlag")
		        .value("QUERY_FLAG_ALL_SOLUTIONS", QUERY_FLAG_ALL_SOLUTIONS)
				.value("QUERY_FLAG_ONE_SOLUTION", QUERY_FLAG_ONE_SOLUTION)
				.value("QUERY_FLAG_PERSIST_SOLUTIONS", QUERY_FLAG_PERSIST_SOLUTIONS)
				.value("QUERY_FLAG_UNIQUE_SOLUTIONS", QUERY_FLAG_UNIQUE_SOLUTIONS)
				.export_values();

		class_<QueryContext, std::shared_ptr<QueryContext>>
				("QueryContext", init<int>())
				.def(init<const QueryContext &, const ModalOperatorPtr &>())
				.def_readwrite("queryFlags", &QueryContext::queryFlags)
				.def_readwrite("modalIteration", &QueryContext::modalIteration)
				.def_readwrite("selector", &QueryContext::selector);
	}
}
