/*
 * This file is part of KnowRob, please consult
 * https://github.com/knowrob/knowrob for license details.
 */

#include "knowrob/reasoner/Goal.h"
#include "knowrob/integration/python/utils.h"
#include "knowrob/queries/AnswerYes.h"
#include "knowrob/reasoner/RDFGoal.h"

using namespace knowrob;

Goal::Goal(SimpleConjunctionPtr formula, const Goal &goal)
		: Query(goal.ctx_),
		  formula_(std::move(formula)),
		  ctx_(goal.ctx_),
		  answerBuffer_(goal.answerBuffer_),
		  outputChannel_(goal.outputChannel_) {}

Goal::Goal(SimpleConjunctionPtr formula, QueryContextPtr ctx)
		: Query(ctx),
		  formula_(std::move(formula)),
		  ctx_(std::move(ctx)),
		  answerBuffer_(std::make_shared<TokenBuffer>()),
		  outputChannel_(TokenStream::Channel::create(answerBuffer_)) {}

Goal::Goal(const FirstOrderLiteralPtr &literal, QueryContextPtr ctx)
		: Query(ctx),
		  formula_(std::make_shared<SimpleConjunction>(literal)),
		  ctx_(std::move(ctx)),
		  answerBuffer_(std::make_shared<TokenBuffer>()),
		  outputChannel_(TokenStream::Channel::create(answerBuffer_)) {}

Goal::~Goal() {
	outputChannel_->close();
}

void Goal::push(const AnswerPtr &answer) {
	outputChannel_->push(answer);
}

void Goal::push(const BindingsPtr &bindings) {
	auto yes = std::make_shared<AnswerYes>(bindings);
	for (const auto &lit : formula_->literals()) {
		auto instance = applyBindings(lit->predicate(), *bindings);
		yes->addGrounding(std::static_pointer_cast<Predicate>(instance), lit->isNegated());
	}
	push(yes);
}

namespace knowrob::py {
	template<>
	void createType<Goal>() {
		using namespace boost::python;

		using Push1 = void (Goal::*)(const AnswerPtr &);
		using Push2 = void (Goal::*)(const BindingsPtr &);

		class_<Goal, std::shared_ptr<Goal>, boost::noncopyable>
				("Goal", init<FramedTriplePatternPtr, QueryContextPtr>())
				.def("formula", &Goal::formula, return_value_policy<copy_const_reference>())
				.def("answerBuffer", &Goal::answerBuffer, return_value_policy<copy_const_reference>())
				.def("ctx", &Query::ctx, return_value_policy<copy_const_reference>())
				.def("push", static_cast<Push1>(&Goal::push))
				.def("push", static_cast<Push2>(&Goal::push));
		createType<RDFGoal>();
	}
}
