/*
 * This file is part of KnowRob, please consult
 * https://github.com/knowrob/knowrob for license details.
 */

#include "knowrob/semweb/Triple.h"
#include "knowrob/integration/python/utils.h"
#include "knowrob/Logger.h"

using namespace knowrob;

template<typename T>
T xsdConv(std::string_view str) {
	T result;
	std::istringstream(str.data()) >> result;
	return result;
}

template<typename T>
T xsdConvFixed(std::string_view str) {
	T result;
	std::istringstream(str.data()) >> std::fixed >> result;
	return result;
}

bool xsdConvBool(std::string_view str) {
	bool result;
	std::istringstream(str.data()) >> std::boolalpha >> result;
	return result;
}

void Triple::setXSDValue(std::string_view v, XSDType type) {
	switch (type) {
		case XSDType::STRING:
			setStringValue(v);
			break;
		case XSDType::DOUBLE:
			setDoubleValue(xsdConvFixed<double>(v));
			break;
		case XSDType::FLOAT:
			setDoubleValue(xsdConvFixed<float>(v));
			break;
		case XSDType::NON_NEGATIVE_INTEGER:
		case XSDType::INTEGER:
			setIntValue(xsdConv<int>(v));
			break;
		case XSDType::LONG:
			setLongValue(xsdConv<long>(v));
			break;
		case XSDType::SHORT:
			setShortValue(xsdConv<short>(v));
			break;
		case XSDType::UNSIGNED_LONG:
			setUnsignedLongValue(xsdConv<unsigned long>(v));
			break;
		case XSDType::UNSIGNED_INT:
			setUnsignedIntValue(xsdConv<unsigned int>(v));
			break;
		case XSDType::UNSIGNED_SHORT:
			setUnsignedShortValue(xsdConv<unsigned short>(v));
			break;
		case XSDType::BOOLEAN:
			setBooleanValue(xsdConvBool(v));
			break;
		case XSDType::LAST:
			KB_ERROR("Invalid XSD type");
			break;
	}
}

std::string Triple::createStringValue() const {
	static const auto a_true = "true";
	static const auto a_false = "false";

	if (isObjectIRI() || isObjectBlank()) {
		return std::string(valueAsString());
	}

	if (xsdType()) {
		switch (xsdType().value()) {
			case XSDType::DOUBLE: {
				std::ostringstream os;
				os << std::fixed << valueAsDouble();
				return os.str();
			}
			case XSDType::FLOAT: {
				std::ostringstream os;
				os << std::fixed << valueAsFloat();
				return os.str();
			}
			case XSDType::NON_NEGATIVE_INTEGER:
			case XSDType::INTEGER:
				return std::to_string(valueAsInt());
			case XSDType::LONG:
				return std::to_string(valueAsLong());
			case XSDType::SHORT:
				return std::to_string(valueAsShort());
			case XSDType::UNSIGNED_LONG:
				return std::to_string(valueAsUnsignedLong());
			case XSDType::UNSIGNED_INT:
				return std::to_string(valueAsUnsignedInt());
			case XSDType::UNSIGNED_SHORT:
				return std::to_string(valueAsUnsignedShort());
			case XSDType::BOOLEAN:
				return valueAsBoolean() ? a_true : a_false;
			case XSDType::STRING:
				return std::string(valueAsString());
			case XSDType::LAST:
				break;
		}
	}

	return "null";
}

bool Triple::operator<(const Triple &other) const {
	if (graph() != other.graph()) {
		return graph() < other.graph();
	}
	if (perspective() != other.perspective()) {
		return perspective() < other.perspective();
	}
	if (subject() != other.subject()) {
		return subject() < other.subject();
	}
	if (predicate() != other.predicate()) {
		return predicate() < other.predicate();
	}
	if (xsdType() != other.xsdType()) {
		return xsdType() < other.xsdType();
	}
	if (xsdType()) {
		switch (xsdType().value()) {
			case XSDType::STRING:
				if (valueAsString() != other.valueAsString()) {
					return valueAsString() < other.valueAsString();
				} else {
					break;
				}
			case XSDType::DOUBLE:
				if (valueAsDouble() != other.valueAsDouble()) {
					return valueAsDouble() < other.valueAsDouble();
				} else {
					break;
				}
			case XSDType::FLOAT:
				if (valueAsFloat() != other.valueAsFloat()) {
					return valueAsFloat() < other.valueAsFloat();
				} else {
					break;
				}
			case XSDType::NON_NEGATIVE_INTEGER:
			case XSDType::INTEGER:
				if (valueAsInt() != other.valueAsInt()) {
					return valueAsInt() < other.valueAsInt();
				} else {
					break;
				}
			case XSDType::BOOLEAN:
				if (valueAsBoolean() != other.valueAsBoolean()) {
					return valueAsBoolean() < other.valueAsBoolean();
				} else {
					break;
				}
			case XSDType::LONG:
				if (valueAsLong() != other.valueAsLong()) {
					return valueAsLong() < other.valueAsLong();
				} else {
					break;
				}
			case XSDType::SHORT:
				if (valueAsShort() != other.valueAsShort()) {
					return valueAsShort() < other.valueAsShort();
				} else {
					break;
				}
			case XSDType::UNSIGNED_LONG:
				if (valueAsUnsignedLong() != other.valueAsUnsignedLong()) {
					return valueAsUnsignedLong() < other.valueAsUnsignedLong();
				} else {
					break;
				}
			case XSDType::UNSIGNED_INT:
				if (valueAsUnsignedInt() != other.valueAsUnsignedInt()) {
					return valueAsUnsignedInt() < other.valueAsUnsignedInt();
				} else {
					break;
				}
			case XSDType::UNSIGNED_SHORT:
				if (valueAsUnsignedShort() != other.valueAsUnsignedShort()) {
					return valueAsUnsignedShort() < other.valueAsUnsignedShort();
				} else {
					break;
				}
			case XSDType::LAST:
				KB_ERROR("Invalid XSD type");
				break;
		}
	} else if (valueAsString() != other.valueAsString()) {
		return valueAsString() < other.valueAsString();
	}
	if (isUncertain() != other.isUncertain()) {
		return isUncertain() < other.isUncertain();
	}
	if (isOccasional() != other.isOccasional()) {
		return isOccasional() < other.isOccasional();
	}
	if (begin() != other.begin()) {
		return begin() < other.begin();
	}
	if (end() != other.end()) {
		return end() < other.end();
	}
	if (confidence() != other.confidence()) {
		return confidence() < other.confidence();
	}
	return false;
}

bool Triple::operator==(const Triple &other) const {
	if (subject() != other.subject()) return false;
	if (predicate() != other.predicate()) return false;
	if (graph(), other.graph()) return false;
	if (perspective(), other.perspective()) return false;
	if (isUncertain() != other.isUncertain()) return false;
	if (isOccasional() != other.isOccasional()) return false;
	if (begin() != other.begin()) return false;
	if (end() != other.end()) return false;
	if (confidence() != other.confidence()) return false;
	if (xsdType() != other.xsdType()) return false;
	if (xsdType()) {
		switch (xsdType().value()) {
			case XSDType::STRING:
				if (valueAsString() != other.valueAsString()) return false;
				break;
			case XSDType::DOUBLE:
				if (valueAsDouble() != other.valueAsDouble()) return false;
				break;
			case XSDType::FLOAT:
				if (valueAsFloat() != other.valueAsFloat()) return false;
				break;
			case XSDType::BOOLEAN:
				if (valueAsBoolean() != other.valueAsBoolean()) return false;
				break;
			case XSDType::NON_NEGATIVE_INTEGER:
			case XSDType::INTEGER:
				if (valueAsInt() != other.valueAsInt()) return false;
				break;
			case XSDType::LONG:
				if (valueAsLong() != other.valueAsLong()) return false;
				break;
			case XSDType::SHORT:
				if (valueAsShort() != other.valueAsShort()) return false;
				break;
			case XSDType::UNSIGNED_LONG:
				if (valueAsUnsignedLong() != other.valueAsUnsignedLong()) return false;
				break;
			case XSDType::UNSIGNED_INT:
				if (valueAsUnsignedInt() != other.valueAsUnsignedInt()) return false;
				break;
			case XSDType::UNSIGNED_SHORT:
				if (valueAsUnsignedShort() != other.valueAsUnsignedShort()) return false;
				break;
			case XSDType::LAST:
				KB_ERROR("Invalid XSD type");
				break;
		}
	} else if (valueAsString() != other.valueAsString()) {
		return false;
	}
	return true;
}

bool Triple::mergeFrame(const Triple &other) {
	bool sameBegin = begin() == other.begin();
	bool sameEnd = end() == other.end();
	bool sameTime = sameBegin && sameEnd;

	if (sameTime) {
		// occasional can switch to always if both triples have the same time
		setIsOccasional(isOccasional() && other.isOccasional());
		// same for uncertain vs. certain
		setIsUncertain(isUncertain() && other.isUncertain());
		// plus we can take the max confidence
		if (other.confidence().has_value() && confidence().has_value()) {
			setConfidence(std::max(confidence().value(), other.confidence().value()));
		} else if (!other.confidence().has_value()) {
			confidence_ = std::nullopt;
		}
	} else {
		// either both triples must be occasional or neither
		if (isOccasional() != other.isOccasional()) return false;
		// same for uncertain vs. certain
		if (isUncertain() != other.isUncertain()) return false;
		// also confidence must match in order to merge different time frames
		if (confidence() != other.confidence()) return false;
		// finally we can merge time frame
		if (isOccasional()) {
			if (begin() && other.begin()) {
				setBegin(std::max(begin().value(), other.begin().value()));
			} else if (other.begin()) {
				setBegin(other.begin().value());
			}
			if (end() && other.end()) {
				setEnd(std::min(end().value(), other.end().value()));
			} else if (other.end()) {
				setEnd(other.end().value());
			}
		} else {
			if (begin() && other.begin()) {
				setBegin(std::min(begin().value(), other.begin().value()));
			} else if (!other.begin().has_value()) {
				begin_ = std::nullopt;
			}
			if (end() && other.end()) {
				setEnd(std::max(end().value(), other.end().value()));
			} else if (!other.end().has_value()) {
				end_ = std::nullopt;
			}
		}
	}
	return true;
}

void Triple::write(std::ostream &os) const {
	os << '(';
	os << subject() << ',' << ' ';
	os << predicate() << ',' << ' ';
	if (isObjectIRI() || isObjectBlank()) {
		os << valueAsString();
	} else {
		os << createStringValue();
	}
	if (graph()) {
		os << ',' << " g=" << graph().value();
	}
	if (perspective()) {
		os << ',' << " p=" << perspective().value();
	}
	if (isOccasional()) {
		os << ',' << " o";
	}
	if (isUncertain()) {
		os << ',' << " u";
	}
	if (begin()) {
		os << ',' << " b=" << begin().value();
	}
	if (end()) {
		os << ',' << " e=" << end().value();
	}
	os << ')';
}

namespace knowrob::py {
	// this struct is needed because Triple has pure virtual methods
	struct TripleWrap : public Triple, boost::python::wrapper<Triple> {
		explicit TripleWrap(PyObject *p) : Triple(), self(p) {}

		void setSubject(std::string_view subject) override { call_method<void>(self, "setSubject", subject); }

		void setPredicate(std::string_view predicate) override { call_method<void>(self, "setPredicate", predicate); }

		void setObjectIRI(std::string_view object) override { call_method<void>(self, "setObjectIRI", object); }

		void setSubjectBlank(std::string_view str) override { call_method<void>(self, "setSubjectBlank", str); }

		void setObjectBlank(std::string_view str) override { call_method<void>(self, "setObjectBlank", str); }

		std::string_view subject() const override { return call_method<std::string_view>(self, "subject"); }

		std::string_view predicate() const override { return call_method<std::string_view>(self, "predicate"); }

		void setGraph(std::string_view graph) override { call_method<void>(self, "setGraph", graph); }

		void setPerspective(std::string_view perspective) override {
			call_method<void>(self, "setPerspective", perspective);
		}

		std::optional<std::string_view> graph() const override {
			return call_method<std::optional<std::string_view>>(self, "graph");
		}

		std::optional<std::string_view> perspective() const override {
			return call_method<std::optional<std::string_view>>(self, "perspective");
		}

		std::string_view valueAsString() const override { return call_method<std::string_view>(self, "valueAsString"); }

	private:
		PyObject *self;
	};

	template<>
	void createType<Triple>() {
		using namespace boost::python;
		class_<Triple, std::shared_ptr<TripleWrap>, boost::noncopyable>
				("Triple", no_init)
				.def("__eq__", &Triple::operator==)
				.def("isObjectIRI", &Triple::isObjectIRI)
				.def("isSubjectIRI", &Triple::isSubjectIRI)
				.def("isObjectBlank", &Triple::isObjectBlank)
				.def("isSubjectBlank", &Triple::isSubjectBlank)
				.def("isXSDLiteral", &Triple::isXSDLiteral)
				.def("setSubject", pure_virtual(&Triple::setSubject))
				.def("setPredicate", pure_virtual(&Triple::setPredicate))
				.def("setSubjectBlank", pure_virtual(&Triple::setSubjectBlank))
				.def("setObjectIRI", pure_virtual(&Triple::setObjectIRI))
				.def("setObjectBlank", pure_virtual(&Triple::setObjectBlank))
				.def("valueAsString", pure_virtual(&Triple::valueAsString))
				.def("createStringValue", &Triple::createStringValue)
				.def("setXSDValue", &Triple::setXSDValue)
				.def("xsdTypeIRI", &Triple::xsdTypeIRI)
				.def("setGraph", pure_virtual(&Triple::setGraph))
				.def("setPerspective", pure_virtual(&Triple::setPerspective))
				.def("setIsOccasional", &Triple::setIsOccasional)
				.def("setIsUncertain", &Triple::setIsUncertain)
				.def("setBegin", &Triple::setBegin)
				.def("setEnd", &Triple::setEnd)
				.def("setConfidence", &Triple::setConfidence)
				.def("xsdType", &Triple::xsdType)
				.def("subject", pure_virtual(&Triple::subject))
				.def("predicate", pure_virtual(&Triple::predicate))
				.def("graph", pure_virtual(&Triple::graph))
				.def("perspective", pure_virtual(&Triple::perspective))
				.def("isOccasional", &Triple::isOccasional)
				.def("isUncertain", &Triple::isUncertain)
				.def("begin", &Triple::begin)
				.def("end", &Triple::end)
				.def("confidence", &Triple::confidence);
		class_<TripleCopy, std::shared_ptr<TripleCopy>, bases<Triple>>
				("TripleCopy", init<>())
				.def(init<std::string_view, std::string_view, std::string_view>());
		class_<TripleView, std::shared_ptr<TripleView>, bases<Triple>>
				("TripleView", init<>())
				.def(init<std::string_view, std::string_view, std::string_view>());
		class_<TriplePtr>("TriplePtr", init<>())
				.def("get", &TriplePtr::get, return_value_policy<reference_existing_object>());
	}
}
