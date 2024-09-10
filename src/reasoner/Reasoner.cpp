/*
 * This file is part of KnowRob, please consult
 * https://github.com/knowrob/knowrob for license details.
 */

#include "knowrob/Logger.h"
#include "knowrob/reasoner/Reasoner.h"
#include "knowrob/reasoner/ReasonerManager.h"
#include "knowrob/reasoner/ReasonerError.h"
#include "knowrob/reasoner/GoalDrivenReasoner.h"
#include "knowrob/reasoner/DataDrivenReasoner.h"
#include "knowrob/integration/python/utils.h"

using namespace knowrob;

Reasoner::Reasoner() :
	reasonerManager_(nullptr),
	reasonerLanguage_(PluginLanguage::CPP)
{
	static const auto undefinedName = std::make_shared<Atom>("undefined");
	t_reasonerName_ = undefinedName;
}

ReasonerManager &Reasoner::reasonerManager() const {
	if (reasonerManager_) {
		return *reasonerManager_;
	} else {
		throw ReasonerError("No ReasonerManager has been assigned to the reasoner.");
	}
}

namespace knowrob {
	class ReasonerTask : public ThreadPool::Runner {
	public:
		explicit ReasonerTask(const std::function<void()> &fn) : fn_(fn) {}

		void run() override { fn_(); }

	protected:
		std::function<void()> fn_;
	};
}

void Reasoner::pushWork(const std::function<void(void)> &fn) {
	auto runner = std::make_shared<ReasonerTask>(fn);
	DefaultThreadPool()->pushWork(runner, [](const std::exception &e) {
		KB_ERROR("Error in reasoner worker thread: {}", e.what());
	});
}

namespace knowrob::py {
	// this struct is needed because Reasoner has pure virtual methods
	struct ReasonerWrap : public Reasoner, boost::python::wrapper<Reasoner> {
		explicit ReasonerWrap(PyObject *p) : Reasoner(), self(p) {}

		bool initializeReasoner(const PropertyTree &config) override {
			return call_method<bool>(self, "initializeReasoner", config);
		}

	private:
		PyObject *self;
	};

	template<>
	void createType<Reasoner>() {
		using namespace boost::python;
		class_<Reasoner, std::shared_ptr<ReasonerWrap>, bases<DataSourceHandler>, boost::noncopyable>
				("Reasoner", init<>())
				.def("pushWork", +[](Reasoner &x, object &fn) {
					auto fn_wrap = std::make_shared<PyObj_wrap>(fn);
					x.pushWork([fn_wrap] {
						(*fn_wrap)();
					});
				})
				.def("reasonerLanguage", &Reasoner::reasonerLanguage)
				.def("storage", &Reasoner::storage)
						// methods that must be implemented by reasoner plugins
				.def("initializeReasoner", &ReasonerWrap::initializeReasoner);
		py::createType<DataDrivenReasoner>();
		py::createType<GoalDrivenReasoner>();
	}
}
