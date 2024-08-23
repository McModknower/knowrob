/*
 * This file is part of KnowRob, please consult
 * https://github.com/knowrob/knowrob for license details.
 */

#include "knowrob/reasoner/GoalDrivenReasoner.h"
#include "knowrob/integration/python/utils.h"
#include "knowrob/integration/python/gil.h"
#include "knowrob/reasoner/RDFGoalReasoner.h"

using namespace knowrob;

bool GoalDrivenReasoner::hasFeature(GoalDrivenReasonerFeature feature) const {
	return (features_ & static_cast<int>(feature)) != 0;
}

void GoalDrivenReasoner::enableFeature(GoalDrivenReasonerFeature feature) {
	features_ = features_ | static_cast<int>(feature);
}

void ReasonerRunner::run() {
	if (reasoner->reasonerLanguage() == PluginLanguage::PYTHON) {
		// If the reasoner uses Python code, then we must make sure that the GIL is acquired
		// in the current thread before calling the reasoner.
		// Note that due to Python's GIL, only one thread can execute Python code at a time
		// but Python should do switching between threads automatically.
		py::gil_lock acquire;
		run_();
	} else {
		run_();
	}
}

void ReasonerRunner::run_() {
	if (!reasoner->evaluate(query)) {
		KB_WARN("Reasoner {} produced 'false' in query evaluation for query: {}",
				*reasoner->reasonerName(), *query->formula());
	}
	query->finish();
}

namespace knowrob::py {
	// this struct is needed because Reasoner has pure virtual methods
	struct GoalDrivenReasonerWrap : public GoalDrivenReasoner, boost::python::wrapper<GoalDrivenReasoner> {
		explicit GoalDrivenReasonerWrap(PyObject *p) : self(p), GoalDrivenReasoner() {}

		bool initializeReasoner(const PropertyTree &config) override {
			return call_method<bool>(self, "initializeReasoner", config);
		}

		bool evaluate(GoalPtr query) override {
			return call_method<bool>(self, "evaluate", query);
		}

	private:
		PyObject *self;
	};

	template<>
	void createType<GoalDrivenReasoner>() {
		using namespace boost::python;

		using Define1 = void (GoalDrivenReasoner::*)(const PredicateIndicator &);
		using Define2 = void (GoalDrivenReasoner::*)(const IRIAtomPtr &);

		// export the GoalDrivenReasonerFeature enum
		enum_<GoalDrivenReasonerFeature>("GoalDrivenReasonerFeature")
				.value("SupportsSimpleConjunctions", GoalDrivenReasonerFeature::SupportsSimpleConjunctions)
				.value("SupportsExtensionalGrounding", GoalDrivenReasonerFeature::SupportsExtensionalGrounding);

		// export the GoalDrivenReasoner class
		class_<GoalDrivenReasoner, std::shared_ptr<GoalDrivenReasonerWrap>, bases<Reasoner>, boost::noncopyable>
				("GoalDrivenReasoner", init<>())
				.def("hasFeature", &GoalDrivenReasoner::hasFeature)
				.def("enableFeature", &GoalDrivenReasoner::enableFeature)
				.def("isRelationDefined", &GoalDrivenReasoner::isRelationDefined)
				.def("define", static_cast<Define1>(&GoalDrivenReasoner::define))
				.def("define", static_cast<Define2>(&GoalDrivenReasoner::define))
				.def("undefine", &GoalDrivenReasoner::undefine)
						// methods that must be implemented by reasoner plugins
				.def("evaluate", &GoalDrivenReasonerWrap::evaluate);

		// export sub-types
		createType<Goal>();
		createType<RDFGoalReasoner>();
	}
}
