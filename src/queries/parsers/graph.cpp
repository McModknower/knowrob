/*
 * This file is part of KnowRob, please consult
 * https://github.com/knowrob/knowrob for license details.
 */

#include <boost/spirit/include/phoenix.hpp>
#include "knowrob/queries/parsers/common.h"
#include "knowrob/queries/parsers/graph.h"
#include "knowrob/queries/parsers/terms.h"
#include "knowrob/queries/parsers/strings.h"
#include "knowrob/Logger.h"
#include "knowrob/semweb/GraphPattern.h"
#include "knowrob/semweb/GraphBuiltin.h"

#define RETURN_GRAPH_PAT_RULE(expr) static GraphTermRule r(expr); return r

using namespace knowrob;

namespace knowrob::parsers::graph {
	using namespace knowrob::parsers::terms;
	namespace qi = boost::spirit::qi;

	GraphTermRule &pattern() {
		RETURN_GRAPH_PAT_RULE((iri() >> '(' >> term() >> ',' >> term() >> ')')
		                      [qi::_val = ptr_<GraphPattern>()(qi::_2, qi::_1, qi::_3)]);
	}

	struct graph_parsers_struct {
		graph_parsers_struct() {
			using namespace boost::phoenix;

			term %= disjunction | brackets;
			brackets %= ('(' >>term >> ')');

			disjunction = (((conjunction | brackets)
					>> (qi::char_(';') | qi::char_('|'))
					>> (disjunction | brackets))
								 [qi::_val = (qi::_1 | qi::_3)]
								 | conjunction[qi::_val = qi::_1]);
			conjunction = (((atomic | brackets)
					>> (qi::char_(',') | qi::char_('&'))
					>> (conjunction | brackets))
								 [qi::_val = (qi::_1 & qi::_3)]
								 | atomic[qi::_val = qi::_1]);

			atomic %= pattern() | builtin;
			builtin %= less | greater | leq | geq | eq | neq | bindCmd | min | max;
			auto &t = terms::term();
			less =    (t >> '<'  >> t) [qi::_val = bind(&GraphBuiltin::less, qi::_1, qi::_2)];
			greater = (t >> '>'  >> t) [qi::_val = bind(&GraphBuiltin::greater, qi::_1, qi::_2)];
			leq =     (t >> "<=" >> t) [qi::_val = bind(&GraphBuiltin::lessOrEqual, qi::_1, qi::_2)];
			geq =     (t >> ">=" >> t) [qi::_val = bind(&GraphBuiltin::greaterOrEqual, qi::_1, qi::_2)];
			eq =      (t >> '='  >> t) [qi::_val = bind(&GraphBuiltin::equal, qi::_1, qi::_2)];
			neq =     (t >> "!=" >> t) [qi::_val = bind(&GraphBuiltin::notEqual, qi::_1, qi::_2)];

			min = ((qi::string("min") >> '(' >> t >> ',' >> t >> ')' >> "as" >> terms::var())
				[qi::_val = bind(&GraphBuiltin::min, qi::_4, qi::_2, qi::_3)]);
			max = ((qi::string("max") >> '(' >> t >> ',' >> t >> ')' >> "as" >> terms::var())
				[qi::_val = bind(&GraphBuiltin::max, qi::_4, qi::_2, qi::_3)]);
			bindCmd = ((qi::string("bind") >> '(' >> t >>  ')' >> "as" >> terms::var())
				[qi::_val = bind(&GraphBuiltin::bind, qi::_3, qi::_2)]);
		}

		GraphTermRule term;
		GraphTermRule brackets;
		GraphTermRule disjunction, conjunction;
		GraphTermRule atomic;
		GraphTermRule builtin;
		GraphTermRule less, greater, leq, geq, eq, neq, bindCmd, min, max;
	};

	auto &parsers() {
		static graph_parsers_struct p;
		return p;
	}

	GraphTermRule &graphTerm() {
		return parsers().term;
	}
}
