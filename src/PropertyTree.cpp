/*
 * This file is part of KnowRob, please consult
 * https://github.com/knowrob/knowrob for license details.
 */

#include <boost/property_tree/json_parser.hpp>
#include "knowrob/PropertyTree.h"
#include "knowrob/terms/Function.h"
#include "knowrob/integration/python/utils.h"
#include "knowrob/terms/String.h"
#include "knowrob/terms/ListTerm.h"
#include <iostream>
#include <utility>



using namespace knowrob;

PropertyTree::PropertyTree()
		: ptree_(nullptr),
		  delimiter_(".") {}

PropertyTree::PropertyTree(std::shared_ptr<const boost::property_tree::ptree> ptree)
		: ptree_(std::move(ptree)),
		  delimiter_(".") {
	init();
}

PropertyTree::PropertyTree(const std::string_view &json_str)
		: PropertyTree() {
	std::istringstream ss(json_str.data());
	boost::property_tree::ptree tree;
	boost::property_tree::read_json(ss, tree);
	// assign the variable with a new memory allocation as shared_ptr
	ptree_ = std::make_shared<boost::property_tree::ptree>(tree);
	init();
}

void PropertyTree::init() {
	static const std::string formatDefault = {};

	// process list of data sources that should be imported into the reasoner backend.
	auto data_sources = ptree_->get_child_optional("data-sources");
	if (data_sources) {
		for (const auto &pair: data_sources.value()) {
			auto &subtree = pair.second;
			URI dataSourceURI(subtree);
			auto dataFormat = subtree.get("format", formatDefault);
			auto source = std::make_shared<DataSource>(dataSourceURI, dataFormat, DataSourceType::UNSPECIFIED);
			dataSources_.push_back(source);
		}
	}
}

TermPtr PropertyTree::get(std::string_view key, const TermPtr &defaultValue) {
	return get_value_recursive(*ptree_, std::string(key));
}

TermPtr PropertyTree::get_value_recursive(const boost::property_tree::ptree& node, const std::string& path) {
	if (path.empty()) {
		try {
			return std::make_shared<String>(node.get_value<std::string>());
		} catch (const boost::property_tree::ptree_bad_data&) {
			throw std::runtime_error("The found child is not a leaf node");
		}
	}

	// Find the position of the first dot or bracket
	size_t dot_pos = path.find('.');
	size_t bracket_pos = path.find('[');
	size_t next_pos = (dot_pos == std::string::npos) ? bracket_pos : ((bracket_pos == std::string::npos) ? dot_pos : std::min(dot_pos, bracket_pos));

	// Extract the current key
	std::string key = (next_pos == std::string::npos) ? path : path.substr(0, next_pos);
	std::string remaining_path = (next_pos == std::string::npos) ? "" : path.substr(next_pos);

	// Check for array indexing
	if (!remaining_path.empty() && remaining_path[0] == '[') {
		size_t end_bracket_pos = remaining_path.find(']');
		if (end_bracket_pos == std::string::npos) {
			throw std::runtime_error("Invalid path syntax: unmatched '['");
		}

		// Extract the index inside the brackets
		std::string index_str = remaining_path.substr(1, end_bracket_pos - 1);
		size_t index = std::stoi(index_str);
		remaining_path = remaining_path.substr(end_bracket_pos + 1);

		// Remove leading dot if it exists
		if (!remaining_path.empty() && remaining_path[0] == '.') {
			remaining_path = remaining_path.substr(1);
		}

		// Find the child array and access the element at the specified index
		try {
			const boost::property_tree::ptree& child_array = node.get_child(key);
			auto it = child_array.begin();
			std::advance(it, index);
			if (it == child_array.end()) {
				throw std::out_of_range("Index out of range");
			}

			// Recursively call with the selected child and remaining path
			return get_value_recursive(it->second, remaining_path);
		} catch (const boost::property_tree::ptree_bad_path&) {
			throw std::runtime_error("Invalid path (array): '" + key + "'");
		}
	} else {
		// Normal key, recursively call with the next child
		try {
			// Remove leading dot if it exists
			if (!remaining_path.empty() && remaining_path[0] == '.') {
				remaining_path = remaining_path.substr(1);
			}
			// Attempt to fetch the child node
			const boost::property_tree::ptree& child_node = node.get_child(key);
			// Recursively call with the selected child and remaining path
			return get_value_recursive(child_node, remaining_path);
		} catch (const boost::property_tree::ptree_bad_path&) {
			throw std::runtime_error("Invalid path (key): '" + key + "'");
		}
	}
}

TermPtr PropertyTree::createKeyTerm(std::string_view key) const {
	TermPtr last_key, next_key;

	size_t pos_start = 0, pos_end, delim_len = delimiter_.length();
	std::string_view token;
	std::vector<std::string_view> res;

	while ((pos_end = key.find(delimiter_, pos_start)) != std::string::npos) {
		token = key.substr(pos_start, pos_end - pos_start);
		pos_start = pos_end + delim_len;
		next_key = Atom::Tabled(token);
		if (last_key) {
			last_key = std::make_shared<Function>(Function(delimiter_, {last_key, next_key}));
		} else {
			last_key = next_key;
		}
		res.push_back(token);
	}
	if (!last_key) {
		last_key = Atom::Tabled(key);
	}

	return last_key;
}

namespace knowrob::py {
	template<>
	void createType<PropertyTree>() {
		using namespace boost::python;
		class_<PropertyTree, std::shared_ptr<PropertyTree>>("PropertyTree", init<>())
				.def(init<const std::string &>())
				.def("__iter__", range(&PropertyTree::begin, &PropertyTree::end))
				.def("get", &PropertyTree::get)
				.def("dataSources", &PropertyTree::dataSources, return_value_policy<copy_const_reference>());
	}
}
