#include "knowrob/DataSourceHandler.h"
#include "knowrob/integration/python/utils.h"

using namespace knowrob;

void
DataSourceHandler::addDataHandler(const std::string &format, const std::function<bool(const DataSourcePtr &)> &fn) {
	dataSourceHandler_[format] = fn;
}

bool DataSourceHandler::loadDataSource(const DataSourcePtr &dataSource) {
	if (dataSource->format().empty()) {
		return loadDataSourceWithUnknownFormat(dataSource);
	} else {
		auto it = dataSourceHandler_.find(dataSource->format());
		if (it != dataSourceHandler_.end()) {
			return it->second(dataSource);
		} else {
			return false;
		}
	}
}

bool DataSourceHandler::hasDataHandler(const DataSourcePtr &dataSource) const {
	return dataSourceHandler_.find(dataSource->format()) != dataSourceHandler_.end();
}

namespace knowrob::py {
	template<>
	void createType<DataSourceHandler>() {
		using namespace boost::python;
		class_<DataSourceHandler, std::shared_ptr<DataSourceHandler>>("DataSourceHandler", init<>())
				.def("addDataHandler", +[]
						(DataSourceHandler &x, const std::string &format, object &fn) {
					// when KnowRob calls the data handler, it does not hold the GIL
					// so we need to acquire it before calling fn.
					x.addDataHandler(format, [fn](const DataSourcePtr &dataSource) {
						gil_lock lock;
						return fn(dataSource);
					});
				})
				.def("loadDataSource", &DataSourceHandler::loadDataSource);
	}
}