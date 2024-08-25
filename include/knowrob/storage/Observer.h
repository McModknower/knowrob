/*
 * This file is part of KnowRob, please consult
 * https://github.com/knowrob/knowrob for license details.
 */

#ifndef KNOWROB_OBSERVER_H
#define KNOWROB_OBSERVER_H

#include <memory>

namespace knowrob {
	// Forward declaration
	class ObserverJob;

	/**
	 * An observer is a handle to an observation job.
	 */
	class Observer {
	public:
		/**
		 * Create an observer.
		 * @param job the job to observe.
		 */
		explicit Observer(const std::shared_ptr<ObserverJob> &job);

		~Observer();

		/**
		 * Stop the observation.
		 */
		void stopObservation();

		/**
		 * @return the job being observed.
		 */
		auto &job() const { return job_; }

	protected:
		std::shared_ptr<ObserverJob> job_;
	};

	using ObserverPtr = std::shared_ptr<Observer>;

} // knowrob

#endif //KNOWROB_OBSERVER_H
