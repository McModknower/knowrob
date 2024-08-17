/*
 * This file is part of KnowRob, please consult
 * https://github.com/knowrob/knowrob for license details.
 */

#ifndef KNOWROB_QUERY_FLAG_H
#define KNOWROB_QUERY_FLAG_H

namespace knowrob {
	/**
	 * Flags for controlling query evaluation.
	 */
	enum QueryFlag {
		/** Query all solutions. */
		QUERY_FLAG_ALL_SOLUTIONS = 1 << 0,
		/** Query only one solution. */
		QUERY_FLAG_ONE_SOLUTION = 1 << 1,
		/** Persist solutions in the data base. */
		QUERY_FLAG_PERSIST_SOLUTIONS = 1 << 2,
		/** Filter redundant solutions. */
		QUERY_FLAG_UNIQUE_SOLUTIONS = 1 << 3,
	};
}

#endif //KNOWROB_QUERY_FLAG_H
