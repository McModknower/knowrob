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

	enum class Mode {
		INSERT,
		REMOVE
	};

	std::thread thread_;

	std::condition_variable syncCondition_;
	std::mutex syncMutex_;

	std::queue<std::pair<Mode,TripleContainerPtr>> queue_;
	std::mutex queueMutex_;
	std::condition_variable queueCondition_;

	std::vector<std::shared_ptr<ObserverJob>> jobs_;
	std::mutex jobMutex_;

	std::atomic<bool> running_{true};
};

ObserverManager::ObserverManager(const QueryableBackendPtr &backend)
		: backend_(backend), impl_(std::make_unique<Impl>()) {
	impl_->thread_ = std::thread(&ObserverManager::run, this);
}

ObserverManager::~ObserverManager() {
	{
		std::lock_guard<std::mutex> lock(impl_->queueMutex_);
		while (!impl_->queue_.empty()) {
			impl_->queue_.pop();
		}
	}
	{
		std::lock_guard<std::mutex> lock(impl_->jobMutex_);
		impl_->jobs_.clear();
	}
	impl_->running_ = false;
	impl_->queueCondition_.notify_one();
	if (impl_->thread_.joinable()) impl_->thread_.join();
}

void ObserverManager::query(const GraphQueryPtr &query, const BindingsHandler &callback) {
	backend_->query(query, callback);
}

ObserverPtr ObserverManager::observe(const GraphQueryPtr &query, const BindingsHandler &callback) {
	auto job = std::make_shared<ObserverJob>(shared_from_this(), query, callback);
	{
		std::lock_guard<std::mutex> lock(impl_->jobMutex_);
		impl_->jobs_.push_back(job);
	}
	return std::make_shared<Observer>(job);
}

void ObserverManager::synchronize() {
	{
		std::lock_guard<std::mutex> lock(impl_->queueMutex_);
		if (impl_->queue_.empty()) return;
	}
	{
		std::unique_lock<std::mutex> lock(impl_->syncMutex_);
		impl_->syncCondition_.wait(lock, [this] { return impl_->queue_.empty(); });
	}
}

void ObserverManager::stopObservation(const Observer &observer) {
	std::lock_guard<std::mutex> lock(impl_->jobMutex_);
	for (auto it = impl_->jobs_.begin(); it != impl_->jobs_.end(); ++it) {
		auto &job = *it;
		if (job.get() == observer.job().get()) {
			impl_->jobs_.erase(it);
			break;
		}
	}
}

void ObserverManager::insert(const TripleContainerPtr &triples) {
	{
		std::lock_guard<std::mutex> lock(impl_->queueMutex_);
		impl_->queue_.push({Impl::Mode::INSERT, triples});
	}
	impl_->queueCondition_.notify_one();
}

void ObserverManager::remove(const TripleContainerPtr &triples) {
	{
		std::lock_guard<std::mutex> lock(impl_->queueMutex_);
		impl_->queue_.push({Impl::Mode::REMOVE, triples});
	}
	impl_->queueCondition_.notify_one();
}

void ObserverManager::run() {
	impl_->running_ = true;

	while (impl_->running_) {
		std::pair<Impl::Mode,TripleContainerPtr> next;
		{
			std::unique_lock<std::mutex> lock(impl_->queueMutex_);
			impl_->queueCondition_.wait(lock, [this] { return !impl_->running_ || !impl_->queue_.empty(); });
			if (!impl_->running_) {
				break;
			}
			next = impl_->queue_.front();
		}
		{
			std::lock_guard<std::mutex> lock(impl_->jobMutex_);
			if (next.first == Impl::Mode::INSERT) {
				for (auto &job: impl_->jobs_) {
					job->processInsertion(next.second);
				}
			} else {
				for (auto &job: impl_->jobs_) {
					job->processRemoval(next.second);
				}
			}
		}
		{
			std::unique_lock<std::mutex> lock(impl_->queueMutex_);
			impl_->queue_.pop();
			if (impl_->queue_.empty()) {
				impl_->syncCondition_.notify_all();
			}
		}
	}
}
