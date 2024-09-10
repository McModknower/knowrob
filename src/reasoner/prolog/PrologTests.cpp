/*
 * This file is part of KnowRob, please consult
 * https://github.com/knowrob/knowrob for license details.
 */

#include <boost/algorithm/string/replace.hpp>
#include "knowrob/Logger.h"
#include "knowrob/terms/ListTerm.h"
#include "knowrob/terms/OptionList.h"
#include "knowrob/reasoner/prolog/PrologTests.h"
#include "knowrob/integration/prolog/PrologBackend.h"
#include "knowrob/queries/QueryParser.h"

using namespace knowrob;

static std::string unescapeString(const std::string &input) {
	std::string result = input;
	boost::replace_all(result, "\\n", "\n");
	boost::replace_all(result, "\\t", "\t");
	return result;
}

void PrologTestsBase::runPrologTests(
		const std::shared_ptr<knowrob::PrologReasoner> &reasoner,
		const std::string &target) {
	bool hasResult = false;
	int numTests = 0;
	int numFailedTests = 0;

	for (const auto &t: reasoner->runTests(PrologEngine::getPrologPath(target))) {
		hasResult = true;
		numTests += 1;

		// each result is a term element/3
		ASSERT_EQ(t->termType(), TermType::FUNCTION);
		auto *pElem = (Function *) t.get();
		ASSERT_EQ(*pElem->functor(), *Atom::Tabled("element"));
		ASSERT_EQ(pElem->arity(), 3);
		ASSERT_EQ(*(pElem->arguments()[0]), *Atom::Tabled("testcase"));

		// the second argument has the form:
		// [name=::string, file=::string, line=::int, time=::double]
		OptionList trace(pElem->arguments()[1]);
		ASSERT_TRUE(trace.contains("file"));
		ASSERT_TRUE(trace.contains("line"));
		ASSERT_TRUE(trace.contains("name"));
		const auto &name = trace.getString("name", "");
		const char *file = trace.getString("file", "").data();
		const long line = trace.getLong("line", 0);

		// the third argument is a list of failures.
		auto *failureList = (ListTerm *) pElem->arguments()[2].get();
		if (!failureList->isNIL()) {
			numFailedTests += 1;
		}
		for (const auto &failureTerm: (*failureList)) {
			ASSERT_EQ(failureTerm->termType(), TermType::FUNCTION);
			// each failure is a term element/3
			auto *errElem = (Function *) failureTerm.get();
			ASSERT_EQ(*errElem->functor(), *Atom::Tabled("element"));
			ASSERT_EQ(errElem->arity(), 3);
			ASSERT_EQ(*(errElem->arguments()[0]), *Atom::Tabled("failure"));

			OptionList errOpts(errElem->arguments()[1]);
			ASSERT_TRUE(errOpts.contains("type"));
			ASSERT_TRUE(errOpts.contains("message"));

			std::ostringstream message_os;
			message_os << errOpts.getString("type", "");
			message_os << ": ";
			message_os << errOpts.getString("message", "");
			auto message = message_os.str();

			std::ostringstream summary_os;
			summary_os << "test: ";
			summary_os << name;
			auto summary = summary_os.str();

			// the second argument has the form: [ type=error|failure, message='...' ]
			// the third argument is the exact same message, which is a bit strange.
			//ADD_FAILURE_AT(file,line) << message;
			GTEST_MESSAGE_AT_(file, line, summary.c_str(), \
                              testing::TestPartResult::kNonFatalFailure) << unescapeString(message);
		}
	}
	EXPECT_TRUE(hasResult);
	KB_INFO1(target.c_str(), 1, "[plunit] {}/{} tests succeeded for target '{}'.", (numTests - numFailedTests),
			 numTests, target);
}

namespace knowrob::testing {
	class PrologReasonerTests : public PrologTests<knowrob::PrologReasoner, knowrob::PrologBackend> {
	protected:
		static std::string getPath(const std::string &filename) {
			return std::filesystem::path("reasoner") / "prolog" / filename;
		}

		static std::vector<BindingsPtr> lookup(const SimpleConjunctionPtr &formula) {
			auto ctx = std::make_shared<QueryContext>(QUERY_FLAG_ALL_SOLUTIONS);
			auto query = std::make_shared<Goal>(formula, ctx);
			reasoner()->evaluate(query);

			auto answerQueue = query->answerBuffer()->createQueue();
			std::vector<BindingsPtr> out;
			while (!answerQueue->empty()) {
				auto solution = answerQueue->pop_front();
				if (solution->indicatesEndOfEvaluation()) break;
				if (solution->tokenType() == TokenType::ANSWER_TOKEN) {
					auto answer = std::static_pointer_cast<const Answer>(solution);
					if (answer->isPositive()) {
						auto answerYes = std::static_pointer_cast<const AnswerYes>(answer);
						out.push_back(answerYes->substitution());
					}
				}
			}
			return out;
		}
	};
}
using namespace knowrob::testing;

#define EXPECT_ONLY_SOLUTION(phi, sol) { \
    auto sols = lookup(phi);              \
    EXPECT_EQ(sols.size(),1);               \
    if(sols.size()==1) { EXPECT_EQ(*sols[0], sol); } }

#define EXPECT_NO_SOLUTION(phi) EXPECT_EQ(lookupAll(phi).size(),0)

TEST_F(PrologReasonerTests, simple_conjunction) {
	auto p1 = QueryParser::parsePredicate("atom_concat(a,b,AB)");
	auto p2 = QueryParser::parsePredicate("atom_concat(AB,c,ABC)");
	auto queryFormula = std::make_shared<SimpleConjunction>(std::vector<FirstOrderLiteralPtr>{
			std::make_shared<FirstOrderLiteral>(p1, false),
			std::make_shared<FirstOrderLiteral>(p2, false)
	});
	EXPECT_ONLY_SOLUTION(
	// the query formula:
			queryFormula,
	// the expected solution as substitution mapping:
			Bindings({
							 {std::make_shared<Variable>("AB"),  Atom::Tabled("ab")},
							 {std::make_shared<Variable>("ABC"), Atom::Tabled("abc")}
					 }))
}

// register test cases of prolog files in this directory (pl or plt file extension)
TEST_F(PrologReasonerTests, semweb) { runTests(getPath("semweb.pl")); }
