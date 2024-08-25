/*
 * This file is part of KnowRob, please consult
 * https://github.com/knowrob/knowrob for license details.
 */

#include <thread>
#include <queue>
#include <mutex>
#include <atomic>
#include "knowrob/storage/ObserverManager.h"
#include "knowrob/storage/ObserverJob.h"

using namespace knowrob;

struct ObserverManager::Impl {
	Impl() = default;
	~Impl() = default;
	std::thread thread_;
	std::queue<TripleContainerPtr> insertionQueue_;
	std::vector<std::shared_ptr<ObserverJob>> jobs_;
	std::mutex transactionMutex_;
	std::mutex jobMutex_;
	std::condition_variable transactionCondition_;
	std::condition_variable syncCondition_;
	std::atomic<bool> running_{true};
};

ObserverManager::ObserverManager()
	: impl_(std::make_unique<Impl>()) {
	impl_->thread_ = std::thread(&ObserverManager::run, this);
}

ObserverManager::~ObserverManager() {
	impl_->running_ = false;
	{
		std::lock_guard<std::mutex> lock(impl_->transactionMutex_);
		while (!impl_->transactions_.empty()) {
			impl_->transactions_.pop();
		}
	}
	impl_->thread_.join();
}

ObserverPtr ObserverManager::observe(const GraphQueryPtr &query, const AnswerHandler &callback) {
	auto job = std::make_shared<ObserverJob>(shared_from_this(), query, callback);
	{
		std::lock_guard<std::mutex> lock(impl_->jobMutex_);
		impl_->jobs_.push_back(job);
	}
	return std::make_unique<Observer>(job);
}

void ObserverManager::stopObservation(const ObserverPtr &observer) {
	std::lock_guard<std::mutex> lock(impl_->jobMutex_);
	impl_->jobs_.erase(observer);
}

void ObserverManager::processTransaction(const std::shared_ptr<transaction::Transaction> &transaction) {
	std::lock_guard<std::mutex> lock(impl_->jobMutex_);
	for (auto &job : impl_->jobs_) {
		job->processTransaction(transaction);
	}
}

void ObserverManager::event(const std::shared_ptr<transaction::Transaction> &transaction) {
	// push transaction to queue
	{
		std::lock_guard<std::mutex> lock(impl_->transactionMutex_);
		impl_->transactions_.push(transaction);
	}
	// notify thread
	impl_->transactionCondition_.notify_one();
}

void ObserverManager::query(const GraphQueryPtr &query, const AnswerHandler &callback) {
	xxx;
}

void ObserverManager::sync() {
	// TODO: implement
	//std::unique_lock<std::mutex> lock(impl_->transactionMutex_);
	//impl_->syncCondition_.wait(lock, [this] { return impl_->transactions_.empty(); });
}

void ObserverManager::run() {
	while (impl_->running_) {
		std::shared_ptr<transaction::Transaction> transaction;
		{
			std::unique_lock<std::mutex> lock(impl_->transactionMutex_);
			// TODO: what about running flag here?
			impl_->transactionCondition_.wait(lock, [this] { return !impl_->transactions_.empty(); });
			transaction = impl_->transactions_.front();
			impl_->transactions_.pop();
		}
		// TODO: implement
		//{
		//	std::lock_guard<std::mutex> lock(impl_->transactionMutex_);
		//	if(impl_->transactions_.empty()) {
		//		impl_->syncCondition_.notify_one();
		//	}
		//}
		processTransaction(transaction);
	}
	KB_DEBUG("ObserverManager thread stopped");
}
