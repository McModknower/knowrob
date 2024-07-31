/*
 * This file is part of KnowRob, please consult
 * https://github.com/knowrob/knowrob for license details.
 */

#ifndef KNOWROB_PROPERTY_TREE_H_
#define KNOWROB_PROPERTY_TREE_H_

#include <list>
#include <map>
#include <memory>
#include <optional>
#include <boost/property_tree/ptree.hpp>
#include "knowrob/terms/Term.h"
#include "knowrob/ontologies/DataSource.h"

namespace knowrob {
	/**
	 * Properties of a reasoner.
	 */
	class PropertyTree {
	public:
		PropertyTree();

		/**
		 * Load a reasoner configuration from a property tree.
		 * @param ptree a property tree.
		 */
		explicit PropertyTree(std::shared_ptr<const boost::property_tree::ptree> ptree);

		/**
		 * Load a reasoner configuration from a JSON string.
		 * @param json_str a JSON string.
		 */
		explicit PropertyTree(const std::string_view &json_str);

		/**
		 * Initialize the property tree.
		 */
		void init();

		/**
		 * Access the underlying boost property tree.
		 * @return the underlying property tree.
		 */
		auto operator->() const { return ptree_; }

		/**
		 * Get a value from the property tree. The keys can contain dots to access nested values and
		 * square brackets to access array elements. E.g. a call could have this form:
		 *
		 * get("key1.key2[0].key3", defaultValue).
		 *
		 * The result needs to be a leaf node in the property tree, else (or in case no value is found)
		 * an exception is thrown.
		 *
		 * @param key a key.
		 * @param defaultValue a default value.
		 * @return the value associated with the key.
		 */
		TermPtr get(std::string_view key, const TermPtr &defaultValue);

		/**
		 * Generate a term from a key string.
		 * @param key a key
		 * @return a term representing the key.
		 */
		TermPtr createKeyTerm(std::string_view key) const;

		/**
		 * @return the begin iterator of the key-value pairs.
		 */
		auto begin() const { return properties_.begin(); }

		/**
		 * @return the end iterator of the key-value pairs.
		 */
		auto end() const { return properties_.end(); }

		/**
		 * @return the list of data sources that should be imported into the reasoner backend.
		 */
		auto &dataSources() const { return dataSources_; }

		/**
		 * @return the property tree used to generate this configuration.
		 */
		auto ptree() const { return ptree_; }

	private:
		std::map<std::string, TermPtr> properties_;
		std::list<std::shared_ptr<DataSource>> dataSources_;
		std::shared_ptr<const boost::property_tree::ptree> ptree_;
		std::string delimiter_;

		/**
		 * Get a value from a property tree recursively.
		 * @param node a property tree node.
		 * @param path a path to the value.
		 * @return a term representing the value.
		 */
		TermPtr get_value_recursive(const boost::property_tree::ptree &node, const std::string &path);
	};
}

#endif //KNOWROB_PROPERTY_TREE_H_
