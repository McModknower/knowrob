/*
 * This file is part of KnowRob, please consult
 * https://github.com/knowrob/knowrob for license details.
 */

#include "knowrob/reasoner/ReasonerQuery.h"
#include "knowrob/integration/python/utils.h"
#include "knowrob/queries/AnswerYes.h"

using namespace knowrob;

ReasonerQuery::ReasonerQuery(SimpleConjunctionPtr formula, QueryContextPtr ctx)
		: Query(ctx),
		  formula_(std::move(formula)),
		  ctx_(std::move(ctx)),
		  answerBuffer_(std::make_shared<TokenBuffer>()),
		  outputChannel_(TokenStream::Channel::create(answerBuffer_)) {}

ReasonerQuery::ReasonerQuery(const FirstOrderLiteralPtr &literal, QueryContextPtr ctx)
		: Query(ctx),
		  formula_(std::make_shared<SimpleConjunction>(literal)),
		  ctx_(std::move(ctx)),
		  answerBuffer_(std::make_shared<TokenBuffer>()),
		  outputChannel_(TokenStream::Channel::create(answerBuffer_)) {}

ReasonerQuery::~ReasonerQuery() {
	outputChannel_->close();
}

void ReasonerQuery::push(const AnswerPtr &answer) {
	outputChannel_->push(answer);
}

void ReasonerQuery::push(const BindingsPtr &bindings) {
	auto yes = std::make_shared<AnswerYes>(bindings);
	for (const auto &lit : formula_->literals()) {
		auto instance = applyBindings(lit->predicate(), *bindings);
		yes->addGrounding(std::static_pointer_cast<Predicate>(instance), lit->isNegated());
	}
	push(yes);
}

namespace knowrob::py {
	template<>
	void createType<ReasonerQuery>() {
		using namespace boost::python;

		using Push1 = void (ReasonerQuery::*)(const AnswerPtr &);
		using Push2 = void (ReasonerQuery::*)(const BindingsPtr &);

		class_<ReasonerQuery, std::shared_ptr<ReasonerQuery>, boost::noncopyable>
				("ReasonerQuery", init<FramedTriplePatternPtr, QueryContextPtr>())
				.def("formula", &ReasonerQuery::formula, return_value_policy<copy_const_reference>())
				.def("answerBuffer", &ReasonerQuery::answerBuffer, return_value_policy<copy_const_reference>())
				.def("ctx", &Query::ctx, return_value_policy<copy_const_reference>())
				.def("push", static_cast<Push1>(&ReasonerQuery::push))
				.def("push", static_cast<Push2>(&ReasonerQuery::push));
	}
}
