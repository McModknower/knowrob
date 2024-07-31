
#ifndef KNOWROB_PY_CONVERTER_PYTHON_TO_MAP_H
#define KNOWROB_PY_CONVERTER_PYTHON_TO_MAP_H

#include <boost/python.hpp>
#include <unordered_map>
#include <boost/any.hpp>
#include <stdexcept>

namespace knowrob::py {

	// Function to convert Python object to boost::any
	boost::any python_to_boost_any(const boost::python::object& obj) {
		using namespace boost::python;

		if (extract<int>(obj).check()) {
			return boost::any(extract<int>(obj)());
		} else if (extract<double>(obj).check()) {
			return boost::any(extract<double>(obj)());
		} else if (extract<std::string>(obj).check()) {
			return boost::any(extract<std::string>(obj)());
		} else {
			// Add more type checks as needed
			PyErr_SetString(PyExc_TypeError, "Unsupported type in Python object");
			throw_error_already_set();
		}

		return boost::any(); // This will never be reached
	}

	// Function to convert Python dict to std::unordered_map<std::string, boost::any>
	std::unordered_map<std::string, boost::any> dict_to_map(const boost::python::dict& py_dict) {
		std::unordered_map<std::string, boost::any> map;
		boost::python::list keys = py_dict.keys();
		for (int i = 0; i < len(keys); ++i) {
			std::string key = boost::python::extract<std::string>(keys[i]);
			boost::python::object value = py_dict[keys[i]];
			map[key] = python_to_boost_any(value);
		}
		return map;
	}

	// Converter from Python dict to C++ std::unordered_map<std::string, boost::any>
	struct DictToMapConverter {
		static void* convertible(PyObject* obj_ptr) {
			if (!PyDict_Check(obj_ptr)) return 0;
			return obj_ptr;
		}

		static void construct(PyObject* obj_ptr, boost::python::converter::rvalue_from_python_stage1_data* data) {
			void* storage = ((boost::python::converter::rvalue_from_python_storage<std::unordered_map<std::string, boost::any>>*)data)->storage.bytes;
			new (storage) std::unordered_map<std::string, boost::any>(dict_to_map(boost::python::dict(boost::python::borrowed(obj_ptr))));
			data->convertible = storage;
		}
	};

	// Register the converter in the module initialization function
	void register_dict_to_map_converter() {
		boost::python::converter::registry::push_back(
				&DictToMapConverter::convertible,
				&DictToMapConverter::construct,
				boost::python::type_id<std::unordered_map<std::string, boost::any>>());
	}
}

#endif //KNOWROB_PY_CONVERTER_PYTHON_TO_MAP_H
