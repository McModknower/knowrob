/*
 * This file is part of KnowRob, please consult
 * https://github.com/knowrob/knowrob for license details.
 */

#ifndef KNOWROB_PY_CONVERTER_LIST_H
#define KNOWROB_PY_CONVERTER_LIST_H

#include <boost/python.hpp>
#include <list>

namespace knowrob::py {
	/** handling of std::list, map to Python list. */
	template <typename T>
	struct list_to_pylist
	{
		static PyObject* convert(const std::list<T>& list)
		{
			boost::python::list result;
			for(const auto& elem : list)
			{
				result.append(boost::python::object(elem));
			}
			return boost::python::incref(result.ptr());
		}
	};

	void register_list_converter() {
		boost::python::to_python_converter<std::list<std::shared_ptr<knowrob::DataSource>>, list_to_pylist<std::shared_ptr<knowrob::DataSource>>>();
	}
}

#endif //KNOWROB_PY_CONVERTER_LIST_H
