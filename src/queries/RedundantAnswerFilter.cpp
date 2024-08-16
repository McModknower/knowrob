/*
 * This file is part of KnowRob, please consult
 * https://github.com/knowrob/knowrob for license details.
 */

#include "knowrob/queries/RedundantAnswerFilter.h"
#include "knowrob/knowrob.h"

using namespace knowrob;

void RedundantAnswerFilter::push(const TokenPtr &tok) {
	auto msgHash = tok->hash();

	uint32_t count;
	{
		std::lock_guard<std::mutex> lock(mtx_);
		count = previousAnswers_.count(msgHash);
			previousAnswers_.insert(msgHash);
	}

	if (count == 0) {
		TokenBroadcaster::push(tok);
	}
}
