/*
 * This file is part of KnowRob, please consult
 * https://github.com/knowrob/knowrob for license details.
 */

#ifndef KNOWROB_PRINTABLE_H
#define KNOWROB_PRINTABLE_H

#include <ostream>
#include <sstream>
#include <fmt/format.h>

namespace knowrob {

	/**
	 * Interface for objects that can be printed to a stream.
	 */
	class Printable {
	public:
		virtual ~Printable() = default;

		/**
		 * Print this object to a stream.
		 * @param os the stream to print to.
		 */
		virtual void write(std::ostream &os) const = 0;

		/**
		 * Format this object as a string.
		 * @return the string representation.
		 */
		virtual std::string format() const {
			std::stringstream ss;
			write(ss);
			return ss.str();
		}
	};

	/**
	 * Print a printable object to a stream.
	 * @param os the stream to print to.
	 * @param printable the object to print.
	 * @return the stream.
	 */
	inline std::ostream &operator<<(std::ostream &os, const Printable &printable) {
		printable.write(os);
		return os;
	}
}

//#if FMT_VERSION >= 90000
//template <> struct fmt::formatter<knowrob::Printable> : fmt::ostream_formatter {};
//#endif

#endif //KNOWROB_PRINTABLE_H
