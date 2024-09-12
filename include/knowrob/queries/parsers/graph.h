/*
 * This file is part of KnowRob, please consult
 * https://github.com/knowrob/knowrob for license details.
 */

#ifndef KNOWROB_GRAPH_PARSERS_H
#define KNOWROB_GRAPH_PARSERS_H

#include <boost/spirit/include/qi.hpp>
#include "knowrob/semweb/GraphTerm.h"
#include "knowrob/semweb/GraphPattern.h"

namespace knowrob::parsers::graph {
	using GraphTermRule = boost::spirit::qi::rule<std::string::const_iterator, std::shared_ptr<GraphTerm>(), boost::spirit::ascii::space_type>;
	using GraphPatternRule = boost::spirit::qi::rule<std::string::const_iterator, std::shared_ptr<GraphPattern>(), boost::spirit::ascii::space_type>;

	/**
	 * @return a parser for formulas.
	 */
	GraphTermRule &graphTerm();
}

#endif //KNOWROB_GRAPH_PARSERS_H
