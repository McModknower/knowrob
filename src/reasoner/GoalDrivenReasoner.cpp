/*
 * This file is part of KnowRob, please consult
 * https://github.com/knowrob/knowrob for license details.
 */

#include "knowrob/reasoner/GoalDrivenReasoner.h"
#include "knowrob/integration/python/utils.h"

using namespace knowrob;

void ReasonerRunner::run() {
	if (!reasoner->evaluateQuery(query)) {
		KB_WARN("Reasoner {} produced 'false' in query evaluation for query: {}",
				 *reasoner->reasonerName(), *query->literal());
	}
	query->finish();
}

namespace knowrob::py {
	// this struct is needed because Reasoner has pure virtual methods
	struct GoalDrivenReasonerWrap : public GoalDrivenReasoner, boost::python::wrapper<GoalDrivenReasoner> {
		explicit GoalDrivenReasonerWrap(PyObject *p) : self(p), GoalDrivenReasoner() {}

		void setDataBackend(const StoragePtr &backend) override {
			call_method<void>(self, "setDataBackend", backend);
		}

		bool initializeReasoner(const PropertyTree &config) override {
			return call_method<bool>(self, "initializeReasoner", config);
		}

		bool evaluateQuery(ReasonerQueryPtr query) override {
			return call_method<bool>(self, "evaluateQuery", query);
		}

	private:
		PyObject *self;
	};

	template<>
	void createType<GoalDrivenReasoner>() {
		using namespace boost::python;
		class_<GoalDrivenReasoner, std::shared_ptr<GoalDrivenReasonerWrap>, bases<Reasoner>, boost::noncopyable>
				("GoalDrivenReasoner", init<>())
				.def("isRelationDefined", &GoalDrivenReasoner::isRelationDefined)
				.def("defineRelation", &GoalDrivenReasoner::defineRelation)
				.def("unDefineRelation", &GoalDrivenReasoner::unDefineRelation)
						// methods that must be implemented by reasoner plugins
				.def("evaluateQuery", &GoalDrivenReasonerWrap::evaluateQuery);

		createType<ReasonerQuery>();
	}
}
