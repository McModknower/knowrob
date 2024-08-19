/*
 * This file is part of KnowRob, please consult
 * https://github.com/knowrob/knowrob for license details.
 */

#include <set>
#include <list>
#include "knowrob/integration/python/utils.h"
#include "knowrob/URI.h"
#include "knowrob/Logger.h"

namespace knowrob::py {
	std::string resolveModulePath(std::string_view modulePath) {
		// guess if '/' or '.' is used as delimiter
		if (modulePath.find('/') != std::string::npos) {
			return URI::resolve(modulePath);
		} else {
			// replace '.' with '/', assuming dots do not appear in directory names.
			std::string modulePath_withSlash(modulePath);
			std::replace(modulePath_withSlash.begin(), modulePath_withSlash.end(), '.', '/');
			return URI::resolve(modulePath_withSlash);
		}
	}

	std::string addToSysPath(const std::filesystem::path& modulePath) {
		static std::set<std::filesystem::path> moduleDirectories;

		auto topmostPythonPath = modulePath.parent_path();
		std::list<std::string> modulePathParts;
		modulePathParts.push_front(modulePath.stem().string());
		while(!topmostPythonPath.empty() && std::filesystem::exists(topmostPythonPath / "__init__.py")) {
			modulePathParts.push_front(topmostPythonPath.stem().string());
			topmostPythonPath = topmostPythonPath.parent_path();
		}
		std::stringstream ss;
		for (auto it = modulePathParts.begin(); it != modulePathParts.end(); ++it) {
			ss << *it;
			if (it != --modulePathParts.end()) {
				ss << ".";
			}
		}
		auto relativeModulePath = ss.str();

		// make sure that the module directory is only added once
		if (moduleDirectories.count(topmostPythonPath) == 0) {
			moduleDirectories.insert(topmostPythonPath);
			// >>> sys.path.append(moduleDir)
			auto py_sys = boost::python::import("sys");
			auto py_path = py_sys.attr("path");
			auto sysPathAppend = py_path.attr("append");
			sysPathAppend(topmostPythonPath.string());
			KB_DEBUG("[python] Added '{}' to sys.path.", topmostPythonPath.string().c_str());
		}

		return relativeModulePath;
	}
}
