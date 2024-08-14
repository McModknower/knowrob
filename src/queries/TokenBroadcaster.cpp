/*
 * This file is part of KnowRob, please consult
 * https://github.com/knowrob/knowrob for license details.
 */

#include <knowrob/queries/TokenBroadcaster.h>
#include "knowrob/Logger.h"
#include "knowrob/integration/python/utils.h"

using namespace knowrob;

TokenBroadcaster::TokenBroadcaster()
		: TokenStream() {}

TokenBroadcaster::~TokenBroadcaster() {
	if (isOpened()) {
		for (auto &x: subscribers_) {
			x->push(EndOfEvaluation::get());
		}
	}
}

void TokenBroadcaster::addSubscriber(const std::shared_ptr<Channel> &subscriber) {
	std::lock_guard<std::mutex> lock(mtx_);
	subscribers_.push_back(subscriber);
}

void TokenBroadcaster::removeSubscriber(const std::shared_ptr<Channel> &subscriber) {
	std::lock_guard<std::mutex> lock(mtx_);
	subscribers_.remove(subscriber);
}

void TokenBroadcaster::push(const TokenPtr &tok) {
	pushToBroadcast(tok);
}

void TokenBroadcaster::pushToBroadcast(const TokenPtr &tok) {
	// broadcast the query result to all subscribers.
	// for now only allow one broadcast at a time: if multiple
	// broadcasts are performed, there are all sorts of concurrency problems
	// for stages in query pipelines, so we better avoid it for now.
	// Also protect the list of subscribers with a mutex.
	std::lock_guard<std::mutex> lock(mtx_);
	for (auto &x: subscribers_) {
		x->push(tok);
	}
}

namespace knowrob {
	void operator>>(const std::shared_ptr<TokenBroadcaster> &a,
					const std::shared_ptr<TokenStream> &b) {
		a->addSubscriber(TokenStream::Channel::create(b));
	}
}

namespace knowrob::py {
	template<>
	void createType<TokenBroadcaster>() {
		using namespace boost::python;
		class_<TokenBroadcaster, std::shared_ptr<TokenBroadcaster>, bases<TokenStream>, boost::noncopyable>
				("TokenBroadcaster", init<>())
				.def("addSubscriber", &TokenBroadcaster::addSubscriber)
				.def("removeSubscriber", &TokenBroadcaster::removeSubscriber);
	}
}
