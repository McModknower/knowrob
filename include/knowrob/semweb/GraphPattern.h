/*
 * This file is part of KnowRob, please consult
 * https://github.com/knowrob/knowrob for license details.
 */

#ifndef KNOWROB_GRAPH_PATTERN_H
#define KNOWROB_GRAPH_PATTERN_H

#include "knowrob/semweb/GraphTerm.h"
#include "knowrob/semweb/FramedTriplePattern.h"

namespace knowrob {
	/**
	 * A triple pattern that appears in a graph query.
	 */
	class GraphPattern : public GraphTerm {
	public:
		explicit GraphPattern(FramedTriplePatternPtr pattern)
				: GraphTerm(GraphTermType::Pattern),
				  pattern_(std::move(pattern)) {}

		explicit GraphPattern(const TermPtr &subject, const TermPtr &predicate, const TermPtr &object)
				: GraphTerm(GraphTermType::Pattern) ,
				  pattern_(std::make_shared<FramedTriplePattern>(subject, predicate, object)) {}

		/**
		 * @return the triple pattern.
		 */
		const auto &value() const { return pattern_; }

		void write(std::ostream &os) const override { os << *pattern_; }

	protected:
		FramedTriplePatternPtr pattern_;
	};
} // knowrob

#endif //KNOWROB_GRAPH_PATTERN_H