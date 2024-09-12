/*
 * This file is part of KnowRob, please consult
 * https://github.com/knowrob/knowrob for license details.
 */

#include "knowrob/queries/ConjunctiveQuery.h"
#include "knowrob/semweb/GraphPattern.h"
#include "knowrob/formulas/Conjunction.h"
#include "knowrob/integration/python/utils.h"

using namespace knowrob;

ConjunctiveQuery::ConjunctiveQuery(const FirstOrderLiteralPtr &query, const QueryContextPtr &ctx)
		: Query(ctx),
		  formula_(std::make_shared<SimpleConjunction>(query)) {
}

ConjunctiveQuery::ConjunctiveQuery(const std::vector<FirstOrderLiteralPtr> &query, const QueryContextPtr &ctx)
		: Query(ctx),
		  formula_(std::make_shared<SimpleConjunction>(query)) {
}

void ConjunctiveQuery::write(std::ostream &os) const {
	os << *formula_;
}

namespace knowrob::py {
	template<>
	void createType<ConjunctiveQuery>() {
		using namespace boost::python;

		createType<GraphTerm>();

		class_<ConjunctiveQuery, std::shared_ptr<ConjunctiveQuery>, boost::noncopyable>
				("GraphQuery", init<const FirstOrderLiteralPtr &, const QueryContextPtr &>())
				.def(init<const std::vector<FirstOrderLiteralPtr> &, const QueryContextPtr &>())
				.def("formula", &ConjunctiveQuery::formula, return_value_policy<copy_const_reference>())
				.def("literals", &ConjunctiveQuery::literals, return_value_policy<copy_const_reference>());
	}
}
