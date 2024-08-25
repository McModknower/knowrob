/*
 * This file is part of KnowRob, please consult
 * https://github.com/knowrob/knowrob for license details.
 */

#include "knowrob/storage/Observer.h"
#include "knowrob/storage/ObserverJob.h"

using namespace knowrob;

Observer::Observer(const std::shared_ptr<ObserverJob> &job)
		: job_(job) {}

Observer::~Observer() {
	stopObservation();
	job_ = nullptr;
}

void Observer::stopObservation() {
	if (job_) {
		auto manager = job_->manager();
		manager->stopObservation(*this);
		job_->stop();
	}
}
