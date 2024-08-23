#include <gtest/gtest.h>

#include "KnowledgeBaseTest.h"
#include "knowrob/queries/QueryParser.h"
#include "knowrob/queries/QueryError.h"
#include "knowrob/formulas/Negation.h"
#include "knowrob/terms/String.h"
#include "knowrob/formulas/Disjunction.h"
#include "knowrob/formulas/Conjunction.h"
#include "knowrob/formulas/ModalFormula.h"
#include "knowrob/queries/AnswerYes.h"
#include "knowrob/reasoner/ReasonerManager.h"

using namespace knowrob;
using namespace knowrob::modals;

#define KB_TEST_SETTINGS_FILE "tests/settings/kb-test.json"

std::shared_ptr<knowrob::KnowledgeBase> KnowledgeBaseTest::kb_;
std::shared_ptr<knowrob::IRIAtom> KnowledgeBaseTest::Fred_;
std::shared_ptr<knowrob::IRIAtom> KnowledgeBaseTest::Ernest_;
std::shared_ptr<knowrob::IRIAtom> KnowledgeBaseTest::Lea_;
std::shared_ptr<knowrob::IRIAtom> KnowledgeBaseTest::Rex_;
std::shared_ptr<knowrob::IRIAtom> KnowledgeBaseTest::hasSibling_;
std::shared_ptr<knowrob::IRIAtom> KnowledgeBaseTest::hasNumber_;
std::shared_ptr<knowrob::IRIAtom> KnowledgeBaseTest::hasAncestor_;
std::shared_ptr<knowrob::IRIAtom> KnowledgeBaseTest::p_;
std::shared_ptr<knowrob::IRIAtom> KnowledgeBaseTest::q_;
std::shared_ptr<knowrob::Variable> KnowledgeBaseTest::varX_;
std::shared_ptr<knowrob::Variable> KnowledgeBaseTest::varY_;
std::shared_ptr<knowrob::Variable> KnowledgeBaseTest::varZ_;
std::shared_ptr<knowrob::Variable> KnowledgeBaseTest::varNum_;

// a reasoner that defines a single fact
class TestReasoner : public knowrob::GoalDrivenReasoner {
public:
	const std::string p_;
	const std::string s_;
	const std::string o_;
	TestReasoner(const std::string_view &p, const std::string_view &s, const std::string_view &o)
	: knowrob::GoalDrivenReasoner(), p_(p), s_(s), o_(o) {
		define(PredicateIndicator(p_, 2));
	}

	bool initializeReasoner(const PropertyTree &cfg) override { return true; }

	bool evaluate(GoalPtr query) override {
		auto &phi = query->formula();
		auto &literals = phi->literals();
		if(literals.size() != 1) {
			KB_ERROR("TestReasoner: expected a single literal");
			return false;
		}
		auto &literal = literals[0];
		auto &p = literal->predicate();
		EXPECT_EQ(p->arity(), 2);

		auto &subjectTerm = p->arguments()[0];
		auto &objectTerm = p->arguments()[1];
		bool succeed = true;
		if(subjectTerm->isGround()) {
			succeed = (*subjectTerm == IRIAtom(s_));
		}
		if(objectTerm->isGround()) {
			succeed = succeed && (*objectTerm == IRIAtom(o_));
		}

		if(succeed) {
			auto bindings = std::make_shared<Bindings>();
			if(!subjectTerm->isGround()) {
				auto v = *subjectTerm->variables().begin();
				bindings->set(std::make_shared<Variable>(v), IRIAtom::Tabled(s_));
			}
			if(!objectTerm->isGround()) {
				auto v = *objectTerm->variables().begin();
				bindings->set(std::make_shared<Variable>(v), IRIAtom::Tabled(o_));
			}
			auto answer = std::make_shared<AnswerYes>(bindings);
			query->push(answer);
		}

		return true;
	}
};

void KnowledgeBaseTest::SetUpTestSuite() {
	// initialize a KB, setup database backend for testing, insert tmp data on which queries can be evaluated
	kb_ = KnowledgeBase::create(KB_TEST_SETTINGS_FILE);

	Fred_   = IRIAtom::Tabled(QueryParser::parseRawAtom("swrl_test:Fred"));
	Ernest_ = IRIAtom::Tabled(QueryParser::parseRawAtom("swrl_test:Ernest"));
	Lea_    = IRIAtom::Tabled(QueryParser::parseRawAtom("swrl_test:Lea"));
	Rex_    = IRIAtom::Tabled(QueryParser::parseRawAtom("swrl_test:Rex"));
	hasSibling_  = IRIAtom::Tabled(QueryParser::parseRawAtom("swrl_test:hasSibling"));
	hasAncestor_  = IRIAtom::Tabled(QueryParser::parseRawAtom("swrl_test:hasAncestor"));
	hasNumber_  = IRIAtom::Tabled(QueryParser::parseRawAtom("swrl_test:hasNumber"));
	p_  = IRIAtom::Tabled(QueryParser::parseRawAtom("p"));
	q_  = IRIAtom::Tabled(QueryParser::parseRawAtom("q"));
	varX_ = std::make_shared<Variable>("X");
	varY_ = std::make_shared<Variable>("Y");
	varZ_ = std::make_shared<Variable>("Z");
	varNum_ = std::make_shared<Variable>("Num");

	kb_->reasonerManager()->addPlugin(
		"r1", PluginLanguage::CPP, std::make_shared<TestReasoner>("p", Ernest_->stringForm(), "x"));
	kb_->reasonerManager()->addPlugin(
		"r2", PluginLanguage::CPP, std::make_shared<TestReasoner>("q", "x", "y"));
}

static std::vector<BindingsPtr> lookup(const FormulaPtr &formula, const QueryContextPtr &ctx) {
	auto answerStream = KnowledgeBaseTest::kb_->submitQuery(formula, ctx);
	auto answerQueue = answerStream->createQueue();
	std::vector<BindingsPtr> out;
	while(true) {
		auto solution = answerQueue->pop_front();
		if(solution->indicatesEndOfEvaluation()) break;
		if(solution->tokenType() == TokenType::ANSWER_TOKEN) {
			auto answer = std::static_pointer_cast<const Answer>(solution);
			if(answer->isPositive()) {
				auto answerYes = std::static_pointer_cast<const AnswerYes>(answer);
				out.push_back(answerYes->substitution());
			}
		}
	}
	return out;
}

static std::vector<BindingsPtr> lookupAll(const std::string &queryString) {
	auto ctx = std::make_shared<QueryContext>(QUERY_FLAG_ALL_SOLUTIONS);
    return lookup(QueryParser::parse(queryString), ctx);
}

static std::vector<BindingsPtr> lookupAll(const FormulaPtr &p) {
	auto ctx = std::make_shared<QueryContext>(QUERY_FLAG_ALL_SOLUTIONS);
    return lookup(p, ctx);
}

static std::vector<BindingsPtr> lookupOne(const std::string &queryString) {
	auto ctx = std::make_shared<QueryContext>(QUERY_FLAG_ONE_SOLUTION);
    return lookup(QueryParser::parse(queryString), ctx);
}

static BindingsPtr lookupOne(const FormulaPtr &p) {
	auto ctx = std::make_shared<QueryContext>(QUERY_FLAG_ONE_SOLUTION);
    auto substitutions = lookup(p, ctx);
    if(substitutions.empty()) {
    	return {};
    }
    else {
    	return substitutions[0];
    }
}

static bool containsAnswer(const std::vector<BindingsPtr> &answers, const std::string &key, const TermPtr &value) {
	for (auto &x: answers) {
		if (x->contains(key)) {
			auto actual = x->get(key);
			if (*value == *actual) {
				return true;
			}
		}
	}

	return false;
}

#define EXPECT_ONLY_SOLUTION(phi, sol) { \
	auto sols = lookupAll(phi);              \
	EXPECT_EQ(sols.size(),1);               \
	if(sols.size()==1) EXPECT_EQ(*sols[0], sol); }

#define EXPECT_NO_SOLUTION(phi) EXPECT_EQ(lookupAll(phi).size(),0)

TEST_F(KnowledgeBaseTest, undefinedNamespace) {
	EXPECT_THROW(lookupAll("undefined:hasSibling(swrl_test:Fred, X)"), QueryError);
}

TEST_F(KnowledgeBaseTest, atomic_EDB) {
	EXPECT_ONLY_SOLUTION(
		// the query formula:
		(*hasSibling_)(Fred_, varX_),
		// the expected solution as substitution mapping:
		Bindings({{varX_, Ernest_}}))
	EXPECT_ONLY_SOLUTION(
			(*hasSibling_)(varX_, Ernest_),
			Bindings({{varX_, Fred_}}))
	EXPECT_ONLY_SOLUTION(
			(*hasSibling_)(Fred_, Ernest_),
			Bindings())
	// negative case without any solution:
	EXPECT_NO_SOLUTION((*hasSibling_)(Lea_, varX_));
}

TEST_F(KnowledgeBaseTest, conjunctive_EDB) {
	EXPECT_ONLY_SOLUTION(
	(*hasSibling_)(Fred_, varX_) & (*hasNumber_)(varX_, varNum_),
	Bindings({
			{varNum_, std::make_shared<String>("123456")},
			{varX_, Ernest_}
		}))
}

TEST_F(KnowledgeBaseTest, disjunctive_EDB) {
	auto sols = lookupAll(
		(*hasSibling_)(Fred_, varX_) |
		(*hasAncestor_)(Fred_, varX_));
	EXPECT_EQ(sols.size(), 2);
	EXPECT_TRUE(containsAnswer(sols, "X", Ernest_));
	EXPECT_TRUE(containsAnswer(sols, "X", Rex_));
}

TEST_F(KnowledgeBaseTest, complex_EDB) {
	EXPECT_ONLY_SOLUTION(
	((*hasSibling_)(Fred_, varX_) | (*hasAncestor_)(Fred_, varX_)) & (*hasSibling_)(varX_, varY_),
	Bindings({
			{varX_, Ernest_},
			{varY_, Fred_}
		}))
}

TEST_F(KnowledgeBaseTest, negated_EDB) {
	EXPECT_ONLY_SOLUTION(
			~(*hasSibling_)(Lea_, varX_),
			Bindings())
	EXPECT_NO_SOLUTION(~(*hasSibling_)(Fred_, varX_));
}

TEST_F(KnowledgeBaseTest, negated_IDB) {
	EXPECT_ONLY_SOLUTION(
			~(*p_)(Lea_, varX_),
			Bindings())
	EXPECT_NO_SOLUTION(~(*p_)(Ernest_, varX_));
}

TEST_F(KnowledgeBaseTest, negatedComplex_EDB) {
	// Rex is an ancestor of Fred who does not have a sibling
	EXPECT_ONLY_SOLUTION(
		(*hasAncestor_)(Fred_, varX_) & ~(*hasSibling_)(varX_, varY_),
		Bindings({{varX_, Rex_}}))
}

TEST_F(KnowledgeBaseTest, atomic_IDB) {
	EXPECT_ONLY_SOLUTION(
			(*p_)(Ernest_, varX_),
			Bindings({{varX_, IRIAtom::Tabled("x")}}))
	EXPECT_ONLY_SOLUTION(
			(*q_)(IRIAtom::Tabled("x"), varX_),
			Bindings({{varX_, IRIAtom::Tabled("y")}}))
	EXPECT_NO_SOLUTION((*p_)(IRIAtom::Tabled("x"), varX_));
}

TEST_F(KnowledgeBaseTest, mixed_EDB_IDB) {
	EXPECT_ONLY_SOLUTION(
	(*hasSibling_)(Fred_, varX_) & (*p_)(varX_, varY_),
	Bindings({
			{varX_, Ernest_},
			{varY_, IRIAtom::Tabled("x")}
		}))
}

TEST_F(KnowledgeBaseTest, IDB_interaction) {
	EXPECT_ONLY_SOLUTION(
	(*p_)(varX_, varY_) & (*q_)(varY_, varZ_),
	Bindings({
			{varX_, Ernest_},
			{varY_, IRIAtom::Tabled("x")},
			{varZ_, IRIAtom::Tabled("y")}
		}))
}

TEST_F(KnowledgeBaseTest, modal_EDB_K) {
	EXPECT_ONLY_SOLUTION(
			K((*hasSibling_)(Fred_, varX_)),
			Bindings({{varX_, Ernest_}}))
}

TEST_F(KnowledgeBaseTest, modal_EDB_B) {
	EXPECT_ONLY_SOLUTION(
			B((*hasSibling_)(Fred_, varX_)),
			Bindings({{varX_, Ernest_}}))
}
