/*
 * This file is part of KnowRob, please consult
 * https://github.com/knowrob/knowrob for license details.
 */

#ifndef KNOWROB_DEFINED_BACKEND_H_
#define KNOWROB_DEFINED_BACKEND_H_

#include <string>
#include <map>
#include <memory>
#include "DataBackend.h"

namespace knowrob {
	/**
	 * A backend with a name managed by the backend manager.
	 */
	class DefinedBackend {
	public:
		/**
		 * @param name the name of the backend, unique within manager
		 * @param backend the KG instance
		 */
		DefinedBackend(std::string name, const std::shared_ptr<DataBackend> &backend)
		: name_(std::move(name)), backend_(backend) {}

		/**
		 * @return the backend instance
		 */
		auto& operator()() const { return backend_; }

		/**
		 * @return the backend instance
		 */
		auto& backend() const { return backend_; }

		/**
		 * @return the backend name.
		 */
		auto& name() const { return name_; }

		std::optional<std::string> getVersionOfOrigin(std::string_view origin) const {
			auto it = originVersions_.find(origin.data());
			if (it != originVersions_.end()) {
				return it->second;
			}
			return std::nullopt;
		}

		void setVersionOfOrigin(std::string_view origin, std::string_view version) {
			originVersions_[origin.data()] = version.data();
		}

	protected:
		const std::string name_;
		const std::shared_ptr<DataBackend> backend_;
		std::map<std::string, std::string> originVersions_;
	};
}

#endif //KNOWROB_DEFINED_BACKEND_H_
