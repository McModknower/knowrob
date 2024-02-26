#include "knowrob/semweb/FramedTriple.h"
#include "knowrob/py/utils.h"

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

void FramedTriple::setXSDValue(std::string_view v, XSDType type) {
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

std::string FramedTriple::createStringValue() const {
	static const auto a_true = "true";
	static const auto a_false = "false";

	if (isObjectIRI() || isObjectBlank()) {
		return std::string(valueAsString());
	}

	if (xsdType()) {
		switch (xsdType().value()) {
			case XSDType::DOUBLE:
				return (std::ostringstream() << std::fixed << valueAsDouble()).str();
			case XSDType::FLOAT:
				return (std::ostringstream() << std::fixed << valueAsFloat()).str();
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

static inline bool c_str_equal(const char *a, const char *b) {
	if (!a) {
		return (!b);
	} else if (!b) {
		return false;
	} else {
		return std::string_view(a) == std::string_view(b);
	}
}

bool FramedTriple::operator<(const FramedTriple &other) const {
	if (graph() != other.graph()) {
		return graph() < other.graph();
	}
	if (agent() != other.agent()) {
		return agent() < other.agent();
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
	if (temporalOperator() != other.temporalOperator()) {
		return temporalOperator() < other.temporalOperator();
	}
	if (epistemicOperator() != other.epistemicOperator()) {
		return epistemicOperator() < other.epistemicOperator();
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

bool FramedTriple::operator==(const FramedTriple &other) const {
	if (subject() != other.subject()) return false;
	if (predicate() != other.predicate()) return false;
	if (graph(), other.graph()) return false;
	if (agent(), other.agent()) return false;
	if (temporalOperator() != other.temporalOperator()) return false;
	if (epistemicOperator() != other.epistemicOperator()) return false;
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

namespace knowrob::py {
	// this struct is needed because FramedTriple has pure virtual methods
	struct FramedTripleWrap : public FramedTriple, boost::python::wrapper<FramedTriple> {
		explicit FramedTripleWrap(PyObject *p) : self(p), FramedTriple() {}

		void setSubject(std::string_view subject) override { call_method<void>(self, "setSubject", subject); }

		void setPredicate(std::string_view predicate) override { call_method<void>(self, "setPredicate", predicate); }

		void setObjectIRI(std::string_view object) override { call_method<void>(self, "setObjectIRI", object); }

		void setSubjectBlank(std::string_view str) override { call_method<void>(self, "setSubjectBlank", str); }

		void setObjectBlank(std::string_view str) override { call_method<void>(self, "setObjectBlank", str); }

		std::string_view subject() const override { return call_method<std::string_view>(self, "subject"); }

		std::string_view predicate() const override { return call_method<std::string_view>(self, "predicate"); }

		void setGraph(std::string_view graph) override { call_method<void>(self, "setGraph", graph); }

		void setAgent(std::string_view agent) override { call_method<void>(self, "setAgent", agent); }

		std::optional<std::string_view> graph() const override {
			return call_method<std::optional<std::string_view>>(self, "graph");
		}

		std::optional<std::string_view> agent() const override {
			return call_method<std::optional<std::string_view>>(self, "agent");
		}

		std::string_view valueAsString() const override { return call_method<std::string_view>(self, "valueAsString"); }

	private:
		PyObject *self;
	};

	template<>
	void createType<FramedTriple>() {
		using namespace boost::python;
		class_<FramedTriple, std::shared_ptr<FramedTripleWrap>, boost::noncopyable>
				("FramedTriple", no_init)
				.def("__eq__", &FramedTriple::operator==)
				.def("isObjectIRI", &FramedTriple::isObjectIRI)
				.def("isSubjectIRI", &FramedTriple::isSubjectIRI)
				.def("isObjectBlank", &FramedTriple::isObjectBlank)
				.def("isSubjectBlank", &FramedTriple::isSubjectBlank)
				.def("isXSDLiteral", &FramedTriple::isXSDLiteral)
				.def("setSubject", pure_virtual(&FramedTriple::setSubject))
				.def("setPredicate", pure_virtual(&FramedTriple::setPredicate))
				.def("setSubjectBlank", pure_virtual(&FramedTriple::setSubjectBlank))
				.def("setObjectIRI", pure_virtual(&FramedTriple::setObjectIRI))
				.def("setObjectBlank", pure_virtual(&FramedTriple::setObjectBlank))
				.def("valueAsString", pure_virtual(&FramedTriple::valueAsString))
				.def("createStringValue", &FramedTriple::createStringValue)
				.def("setXSDValue", &FramedTriple::setXSDValue)
				.def("xsdTypeIRI", &FramedTriple::xsdTypeIRI)
				.def("setGraph", pure_virtual(&FramedTriple::setGraph))
				.def("setAgent", pure_virtual(&FramedTriple::setAgent))
				.def("setTemporalOperator", &FramedTriple::setTemporalOperator)
				.def("setEpistemicOperator", &FramedTriple::setEpistemicOperator)
				.def("setBegin", &FramedTriple::setBegin)
				.def("setEnd", &FramedTriple::setEnd)
				.def("setConfidence", &FramedTriple::setConfidence)
				.def("xsdType", &FramedTriple::xsdType)
				.def("subject", pure_virtual(&FramedTriple::subject))
				.def("predicate", pure_virtual(&FramedTriple::predicate))
				.def("graph", pure_virtual(&FramedTriple::graph))
				.def("agent", pure_virtual(&FramedTriple::agent))
				.def("temporalOperator", &FramedTriple::temporalOperator)
				.def("epistemicOperator", &FramedTriple::epistemicOperator)
				.def("begin", &FramedTriple::begin)
				.def("end", &FramedTriple::end)
				.def("confidence", &FramedTriple::confidence);
		class_<FramedTripleCopy, std::shared_ptr<FramedTripleCopy>, bases<FramedTriple>>
				("FramedTriple", init<>())
				.def(init<std::string_view, std::string_view, std::string_view>());
		class_<FramedTripleView, std::shared_ptr<FramedTripleView>, bases<FramedTriple>>
				("FramedTripleView", init<>())
				.def(init<std::string_view, std::string_view, std::string_view>());
		class_<FramedTriplePtr>("FramedTriplePtr", init<>())
				.def("get", &FramedTriplePtr::get, return_value_policy<reference_existing_object>());
	}
}