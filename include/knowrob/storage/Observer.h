/*
 * This file is part of KnowRob, please consult
 * https://github.com/knowrob/knowrob for license details.
 */

#ifndef KNOWROB_OBSERVER_H
#define KNOWROB_OBSERVER_H

#include <memory>

namespace knowrob {
	class ObserverJob;

	class Observer {
	public:
		explicit Observer(const std::shared_ptr<ObserverJob> &job);
		~Observer();

		void stopObservation();

	protected:
		std::shared_ptr<ObserverJob> job_;
	};

	using ObserverPtr = std::unique_ptr<Observer>;

} // knowrob

#endif //KNOWROB_OBSERVER_H
