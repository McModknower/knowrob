/*
 * This file is part of KnowRob, please consult
 * https://github.com/knowrob/knowrob for license details.
 */

#ifndef KNOWROB_TRIPLE_FORMAT_H
#define KNOWROB_TRIPLE_FORMAT_H

#include "string_view"

namespace knowrob::semweb {
	/**
	 * Used to indicate the file format when loading triple data.
	 */
	enum TripleFormat {
		RDF_XML,
		RDFA,
		TRIG,
		GRDDL,
		TURTLE,
		N_TRIPLES
	};

	/**
	 * @param format the string representation of the format
	 * @return the TripleFormat
	 */
	TripleFormat tripleFormatFromString(std::string_view format);

	/**
	 * @param format the TripleFormat
	 * @return the string representation of the format
	 */
	std::string_view tripleFormatToString(TripleFormat format);

	/**
	 * @param format the TripleFormat
	 * @return the MIME type of the format
	 */
	std::string_view tripleFormatMimeType(TripleFormat format);

	/**
	 * @param format the string representation of the format
	 * @return true if the format is a valid triple format
	 */
	bool isTripleFormatString(std::string_view format);
}

#endif //KNOWROB_TRIPLE_FORMAT_H
