/*
 * This file is part of KnowRob, please consult
 * https://github.com/knowrob/knowrob for license details.
 */

#ifndef KNOWROB_URI_H
#define KNOWROB_URI_H

#include <string>
#include <optional>
#include <boost/property_tree/ptree.hpp>

namespace knowrob {
	/**
	 * A URI is a uniform resource identifier.
	 */
	class URI {
	public:
		/**
		 * @param path the path of the URI.
		 */
		explicit URI(std::string_view path);

		/**
		 * @param property_tree the property tree to construct the URI from.
		 */
		explicit URI(const boost::property_tree::ptree &property_tree);

		/**
		 * @param path the path of the URI.
		 * @param protocol the protocol of the URI.
		 * @param host the host of the URI.
		 * @param port the port of the URI.
		 */
		URI(std::string path, std::string protocol, std::string host, int port);

		/**
		 * Resolves a URI string.
		 * @param uriString the URI string to resolve.
		 * @return the resolved URI string.
		 */
		static std::string resolve(const std::string_view &uriString);

		/**
		 * @return the home path of the current user.
		 */
		static std::optional<std::string> getHomePath(void);

		/**
		 * @return URI string of this data source.
		 */
		const auto &operator()() const { return uri_; }

		/**
		 * @return string identifier of the data format.
		 */
		const std::string &path() const { return path_; }

	protected:
		std::string uri_;
		std::string path_;
		boost::optional<std::string> protocol_;
		boost::optional<std::string> host_;
		boost::optional<int> port_;

		void updateURI();
	};
}

#endif //KNOWROB_URI_H
