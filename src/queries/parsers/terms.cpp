/*
 * This file is part of KnowRob, please consult
 * https://github.com/knowrob/knowrob for license details.
 */

#include <boost/spirit/include/phoenix.hpp>
#include "knowrob/queries/parsers/terms.h"
#include "knowrob/queries/parsers/strings.h"
#include "knowrob/queries/parsers/common.h"
#include "knowrob/terms/String.h"
#include "knowrob/terms/Numeric.h"
#include "knowrob/terms/Variable.h"
#include "knowrob/terms/ListTerm.h"
#include "knowrob/terms/Function.h"
#include "knowrob/terms/IRIAtom.h"
#include "knowrob/terms/Blank.h"

#define RETURN_TERM_RULE(expr) static TermRule r(expr); return r
#define RETURN_ATOM_RULE(expr) static AtomRule r(expr); return r
#define RETURN_VAR_RULE(expr) static VariableRule r(expr); return r
#define RETURN_FUNCTION_RULE(expr) static FunctionRule r(expr); return r

static knowrob::AtomPtr makeAtom(std::string_view stringForm) {
	switch (knowrob::rdfNodeTypeGuess(stringForm)) {
		case knowrob::RDFNodeType::IRI:
			return knowrob::IRIAtom::Tabled(stringForm);
		case knowrob::RDFNodeType::BLANK:
			return knowrob::Blank::Tabled(stringForm);
		case knowrob::RDFNodeType::LITERAL:
			break;
	}
	return knowrob::Atom::Tabled(stringForm);
}

static knowrob::AtomPtr makeIRI(std::string_view stringForm) {
	return knowrob::IRIAtom::Tabled(stringForm);
}

static knowrob::TermPtr makeXSD(std::string_view value, const knowrob::AtomPtr &typeIRI) {
	return knowrob::XSDAtomic::create(value, typeIRI->stringForm());
}

static std::vector<knowrob::TermPtr> createTermVector2(const knowrob::TermPtr &a, const knowrob::TermPtr &b) {
	return {a, b};
}

namespace knowrob::parsers::terms {
	using namespace knowrob::parsers::str;
	namespace qi = boost::spirit::qi;

	AtomRule &iri() {
		RETURN_ATOM_RULE(str::iri()[qi::_val = boost::phoenix::bind(&makeIRI, qi::_1)]);
	}

	AtomRule &atom_regular() {
		RETURN_ATOM_RULE(str::atom()[qi::_val = boost::phoenix::bind(&makeAtom, qi::_1)]);
	}

	AtomRule &atom() {
		RETURN_ATOM_RULE(iri() | atom_regular());
	}

	TermRule &string() {
		RETURN_TERM_RULE(double_quotes()[qi::_val = ptr_<String>()(qi::_1)]);
	}

	TermRule &number() {
		RETURN_TERM_RULE(qi::double_[qi::_val = ptr_<Double>()(qi::_1)]);
	}

	TermRule &blank() {
		RETURN_TERM_RULE(str::blank()[qi::_val = ptr_<String>()(qi::_1)]);
	}

	TermRule &xsd() {
		RETURN_TERM_RULE((xsd_value() >> "^^" >> iri())[qi::_val = boost::phoenix::bind(&makeXSD, qi::_1, qi::_2)]);
	}

	TermRule &atomic() {
		RETURN_TERM_RULE(atom() | string() | xsd() | number());
	}

	VariableRule &var_upper() {
		RETURN_VAR_RULE(str::upper_prefix()[qi::_val = ptr_<Variable>()(qi::_1)]);
	}

	VariableRule &var_question() {
		RETURN_VAR_RULE('?' >> str::lower_prefix()[qi::_val = ptr_<Variable>()(qi::_1)]);
	}

	VariableRule &var() {
		RETURN_VAR_RULE(var_upper() | var_question());
	}

	TermRule &atomic_list() {
		RETURN_TERM_RULE(('[' >> (atomic() % ',') >> ']')[qi::_val = ptr_<ListTerm>()(qi::_1)]);
	}

	TermRule &nil() {
		RETURN_TERM_RULE(qi::attr(ListTerm::nil()));
	}

	TermRule &key_value_pair() {
		static auto equalFunctor = Atom::Tabled("=");
		RETURN_TERM_RULE((atom() >> '=' >> atomic())
						 [qi::_val = ptr_<Function>()(equalFunctor,
													  boost::phoenix::bind(&createTermVector2, qi::_1, qi::_2))]);
	}

	TermRule &option() {
		RETURN_TERM_RULE(key_value_pair() | atomic() | blank());
	}

	TermRule &options() {
		RETURN_TERM_RULE(('[' >> (option() % ',') >> ']')[qi::_val = ptr_<ListTerm>()(qi::_1)]);
	}

	TermRule &options_or_nil() {
		RETURN_TERM_RULE(options() | nil());
	}

	struct term_and_function_parsers {
		term_and_function_parsers() {
			function = ((atom() >> '(' >> (term % ',') >> ')') [qi::_val = ptr_<Function>()(qi::_1, qi::_2)]);
			term %= function | var() | atomic() | atomic_list();
		}
		TermRule term;
		FunctionRule function;
	};

	auto &term_and_function() {
		static term_and_function_parsers p;
		return p;
	}

	TermRule &term() {
		RETURN_TERM_RULE(term_and_function().term);
	}

	FunctionRule &function() {
		RETURN_FUNCTION_RULE(term_and_function().function);
	}
}
