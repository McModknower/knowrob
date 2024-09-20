/*
 * This file is part of KnowRob, please consult
 * https://github.com/knowrob/knowrob for license details.
 */

#ifndef KNOWROB_UN_REIFICATION_CONTAINER_H
#define KNOWROB_UN_REIFICATION_CONTAINER_H

#include <map>
#include "knowrob/semweb/TripleContainer.h"

namespace knowrob {
	/**
	 * A container that reverses the reification of input triples.
	 */
	class UnReificationContainer : public TripleContainer {
	public:
		/**
		 * Add a triple to the container.
		 * @param triple the triple to add.
		 */
		void add(const Triple &triple);

		// Override TripleContainer
		ConstGenerator cgenerator() const override;

	protected:
		std::map<std::string_view, TriplePtr> triples_;

		TriplePtr &getUnReifiedTriple(std::string_view subject);
	};

} // knowrob

#endif //KNOWROB_UN_REIFICATION_CONTAINER_H
