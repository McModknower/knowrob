/*
 * This file is part of KnowRob, please consult
 * https://github.com/knowrob/knowrob for license details.
 */

#include "knowrob/queries/TokenBuffer.h"
#include "knowrob/queries/TokenQueue.h"
#include "knowrob/Logger.h"
#include "knowrob/integration/python/utils.h"
#include "knowrob/queries/AnswerDontKnow.h"
#include <gtest/gtest.h>

using namespace knowrob;

TokenBuffer::TokenBuffer()
		: TokenBroadcaster(), isBuffering_(true) {}

void TokenBuffer::stopBuffering() {
	if (!isBuffering_) return;
	TokenPtr next;

	while (true) {
		{
			// acquire lock, pop first element and lift the lock before pushing to the broadcaster.
			// this is done such that other threads may still push to the buffer while we are broadcasting.
			std::lock_guard<std::mutex> lock(bufferMutex_);
			if (buffer_.empty()) {
				isBuffering_ = false;
				break;
			} else {
				next = buffer_.front();
				buffer_.pop_front();
			}
		}
		TokenBroadcaster::push(next);
	}
}

std::shared_ptr<TokenQueue> TokenBuffer::createQueue() {
	auto queue = std::make_shared<TokenQueue>();
	addSubscriber(Channel::create(queue));
	stopBuffering();
	return queue;
}

void TokenBuffer::push(const TokenPtr &tok) {
	{
		std::lock_guard<std::mutex> lock(bufferMutex_);
		if (isBuffering_) {
			buffer_.push_back(tok);
			return;
		}
	}
	TokenBroadcaster::push(tok);
}

namespace knowrob::py {
	template<>
	void createType<TokenBuffer>() {
		using namespace boost::python;
		class_<TokenBuffer, std::shared_ptr<TokenBuffer>, bases<TokenBroadcaster>, boost::noncopyable>
				("TokenBuffer", init<>())
				.def("stopBuffering", &TokenBuffer::stopBuffering)
				.def("createQueue", &TokenBuffer::createQueue);
	}
}

TEST(TokenBuffer, twoTokens) {
	auto out = std::make_shared<TokenBuffer>();
	auto channel = TokenStream::Channel::create(out);
	auto dontKnow = std::make_shared<AnswerDontKnow>();
	channel->push(dontKnow);
	channel->push(EndOfEvaluation::get());
	auto queue = out->createQueue();
	std::vector<TokenPtr> tokens;
	while(!queue->empty()) {
		auto tok = queue->pop_front();
		tokens.push_back(tok);
	}
	ASSERT_EQ(tokens.size(), 2);
	ASSERT_EQ(tokens[0], dontKnow);
	ASSERT_EQ(tokens[1], EndOfEvaluation::get());
}
