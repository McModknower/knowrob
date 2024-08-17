/*
 * This file is part of KnowRob, please consult
 * https://github.com/knowrob/knowrob for license details.
 */

#ifndef KNOWROB_REDUNDANT_ANSWER_FILTER_H
#define KNOWROB_REDUNDANT_ANSWER_FILTER_H

#include "set"
#include "TokenBroadcaster.h"

namespace knowrob {

	class RedundantAnswerFilter : public TokenBroadcaster {
	public:
		RedundantAnswerFilter() = default;

	protected:
		std::set<std::size_t> previousAnswers_;

		// Override QueryResultStream
		void push(const TokenPtr &tok) override;
	};

} // knowrob

#endif //KNOWROB_REDUNDANT_ANSWER_FILTER_H
