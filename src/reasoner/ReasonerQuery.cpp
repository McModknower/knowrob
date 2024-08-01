/*
 * This file is part of KnowRob, please consult
 * https://github.com/knowrob/knowrob for license details.
 */

#include "knowrob/reasoner/ReasonerQuery.h"
#include "knowrob/integration/python/utils.h"

using namespace knowrob;

ReasonerQuery::ReasonerQuery(FramedTriplePatternPtr literal, QueryContextPtr ctx)
		: Query(ctx),
		  literal_(std::move(literal)),
		  ctx_(std::move(ctx)),
		  answerBuffer_(std::make_shared<TokenBuffer>()),
		  outputChannel_(TokenStream::Channel::create(answerBuffer_)) {}

ReasonerQuery::~ReasonerQuery() {
	outputChannel_->close();
}

namespace knowrob::py {
	template<>
	void createType<ReasonerQuery>() {
		using namespace boost::python;

		class_<ReasonerQuery, std::shared_ptr<ReasonerQuery>, boost::noncopyable>
				("ReasonerQuery", init<FramedTriplePatternPtr, QueryContextPtr>())
				.def("literal", &ReasonerQuery::literal, return_value_policy<copy_const_reference>())
				.def("answerBuffer", &ReasonerQuery::answerBuffer, return_value_policy<copy_const_reference>())
				.def("ctx", &Query::ctx, return_value_policy<copy_const_reference>())
				.def("push", &ReasonerQuery::push);
	}
}
