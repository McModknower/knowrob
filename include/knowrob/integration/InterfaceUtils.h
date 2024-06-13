/*
 * Copyright (c) 2024, Sascha Jongebloed
 * All rights reserved.
 *
 * This file is part of KnowRob, please consult
 * https://github.com/knowrob/knowrob for license details.
 */

#ifndef KNOWROB_INTERFACEUTILS_H
#define KNOWROB_INTERFACEUTILS_H

#include <knowrob/KnowledgeBase.h>
#include "knowrob/formulas/Formula.h"

namespace knowrob {
	/**
	 * Utility function for interfaces
	 */
	class InterfaceUtils {
	public:
		/**
		 * Load the settings from the default file or the file
		 * specified in the environment variable KNOWROB_SETTINGS.
		 *
		 * @return the settings.
		 */
		static boost::property_tree::ptree loadSettings();

		/**
		 * Assert a list of statements.
		 *
		 * @param args the statements.
		 * @return true if all statements are true.
		 */
		static bool assertStatements(KnowledgeBase &kb_, const std::vector<FormulaPtr> &args);

		/**
		 * Apply a modality to a formula.
		 *
		 * @param options the options.
		 * @param phi the formula.
		 * @return the formula with the modality applied.
		 */
		static FormulaPtr applyModality(const std::unordered_map<std::string, boost::any> &options, FormulaPtr phi);

	};

}

#endif //KNOWROB_INTERFACEUTILS_H

