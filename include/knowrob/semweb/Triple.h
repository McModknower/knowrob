/*
 * This file is part of KnowRob, please consult
 * https://github.com/knowrob/knowrob for license details.
 */

#ifndef KNOWROB_FRAMED_TRIPLE_H
#define KNOWROB_FRAMED_TRIPLE_H

#include "optional"
#include "variant"
#include "knowrob/terms/XSDType.h"
#include "knowrob/Printable.h"

namespace knowrob {
	/**
	 * A triple with additional information about its origin, epistemic and temporal context.
	 * This is an abstract class to support both std::string and std::string_view for triple data,
	 * however, at the cost of having some virtual methods.
	 */
	class Triple : public Printable {
	public:
		explicit Triple()
				: isOccasional_(false), isUncertain_(false), xsdType_(std::nullopt) {}

		explicit Triple(XSDType xsdType)
				: isOccasional_(false), isUncertain_(false), xsdType_(xsdType) {}

		virtual ~Triple() = default;

		/**
		 * Merge the frame of the triple with another triple.
		 * @param other another triple.
		 */
		bool mergeFrame(const Triple &other);

		/**
		 * @return true if the object of the triple is a XSD literal.
		 */
		bool isXSDLiteral() const { return xsdType_.has_value(); }

		/**
		 * @return true if the subject of the triple is a blank node.
		 */
		bool isSubjectBlank() const { return isBlank(subject()); }

		/**
		 * @return true if the object of the triple is a blank node.
		 */
		bool isObjectBlank() const { return !isXSDLiteral() && isBlank(valueAsString()); }

		/**
		 * @return true if the object of the triple is a IRI.
		 */
		bool isObjectIRI() const { return !isXSDLiteral() && !isBlank(valueAsString()); }

		/**
		 * @return true if the subject of the triple is a IRI.
		 */
		bool isSubjectIRI() const { return !isSubjectBlank(); }

		/**
		 * @return the object type of the triple.
		 */
		auto xsdType() const { return xsdType_; }

		/**
		 * @return the IRI of the XSD type of the object of the triple.
		 */
		auto xsdTypeIRI() const { return xsdTypeToIRI(xsdType_ ? *xsdType_ : XSDType::STRING); }

		/**
		 * Assign a XSD value to the object of the triple.
		 * @param v the string form of a XSD value.
		 * @param type the XSD type of the value.
		 */
		void setXSDValue(std::string_view v, XSDType type);

		/**
		 * Construct string form of the object of the triple.
		 * @return the string form of the object.
		 */
		std::string createStringValue() const;

		/**
		 * @return the subject of the triple.
		 */
		virtual std::string_view subject() const = 0;

		/**
		 * @return the predicate of the triple.
		 */
		virtual std::string_view predicate() const = 0;

		/**
		 * Read the object part of the triple as a string.
		 * @return the object as a double.
		 */
		virtual std::string_view valueAsString() const = 0;

		/**
		 * Read the object part of the triple as a float.
		 * @return the object as a float.
		 */
		virtual float valueAsFloat() const = 0;

		/**
		 * Read the object part of the triple as a double.
		 * @return the object as a double.
		 */
		virtual double valueAsDouble() const = 0;

		/**
		 * Read the object part of the triple as a long.
		 * @return the object as a long.
		 */
		virtual long valueAsLong() const = 0;

		/**
		 * Read the object part of the triple as a int.
		 * @return the object as a int.
		 */
		virtual int valueAsInt() const = 0;

		/**
		 * Read the object part of the triple as a boolean.
		 * @return the object as a boolean.
		 */
		virtual bool valueAsBoolean() const = 0;

		/**
		 * Read the object part of the triple as a short.
		 * @return the object as a short.
		 */
		virtual short valueAsShort() const = 0;

		/**
		 * Read the object part of the triple as a unsigned long.
		 * @return the object as a unsigned long.
		 */
		virtual unsigned long valueAsUnsignedLong() const = 0;

		/**
		 * Read the object part of the triple as a unsigned int.
		 * @return the object as a unsigned int.
		 */
		virtual unsigned int valueAsUnsignedInt() const = 0;

		/**
		 * Read the object part of the triple as a unsigned short.
		 * @return the object as a unsigned short.
		 */
		virtual unsigned short valueAsUnsignedShort() const = 0;

		/**
		 * @return the graph of the triple.
		 */
		virtual std::optional<std::string_view> graph() const = 0;

		/**
		 * @return the perspective of the triple.
		 */
		virtual std::optional<std::string_view> perspective() const = 0;

		/**
		 * @param subject the subject of the triple.
		 */
		virtual void setSubject(std::string_view subject) = 0;

		/**
		 * @param predicate the predicate of the triple.
		 */
		virtual void setPredicate(std::string_view predicate) = 0;

		/**
		 * @param object the object of the triple.
		 */
		virtual void setObjectIRI(std::string_view object) = 0;

		/**
		 * @param str a blank node identifier for the subject of the triple.
		 */
		virtual void setSubjectBlank(std::string_view str) = 0;

		/**
		 * @param str a blank node identifier for the object of the triple.
		 */
		virtual void setObjectBlank(std::string_view str) = 0;

		/**
		 * Assign a string value to the object of the triple.
		 * @param v the string value to assign.
		 */
		virtual void setStringValue(std::string_view v) = 0;

		/**
		 * Assign a double value to the object of the triple.
		 * @param v the double value to assign.
		 */
		virtual void setDoubleValue(double v) = 0;

		/**
		 * Assign a float value to the object of the triple.
		 * @param v the float value to assign.
		 */
		virtual void setFloatValue(float v) = 0;

		/**
		 * Assign a int value to the object of the triple.
		 * @param v the int value to assign.
		 */
		virtual void setIntValue(int v) = 0;

		/**
		 * Assign a boolean value to the object of the triple.
		 * @param v the boolean value to assign.
		 */
		virtual void setBooleanValue(bool v) = 0;

		/**
		 * Assign a long value to the object of the triple.
		 * @param v the long value to assign.
		 */
		virtual void setLongValue(long v) = 0;

		/**
		 * Assign a short value to the object of the triple.
		 * @param v the short value to assign.
		 */
		virtual void setShortValue(short v) = 0;

		/**
		 * Assign a unsigned int value to the object of the triple.
		 * @param v the unsigned int value to assign.
		 */
		virtual void setUnsignedIntValue(unsigned int v) = 0;

		/**
		 * Assign a unsigned long value to the object of the triple.
		 * @param v the unsigned long value to assign.
		 */
		virtual void setUnsignedLongValue(unsigned long v) = 0;

		/**
		 * Assign a unsigned short value to the object of the triple.
		 * @param v the unsigned short value to assign.
		 */
		virtual void setUnsignedShortValue(unsigned short v) = 0;

		/**
		 * @param graph the graph of the triple.
		 */
		virtual void setGraph(std::string_view graph) = 0;

		/**
		 * @param perspective the perspective of the triple.
		 */
		virtual void setPerspective(std::string_view perspective) = 0;

		/**
		 * @return true if the triple is occasionally true.
		 */
		bool isOccasional() const { return isOccasional_; }

		/**
		 * @return true if the triple is uncertain.
		 */
		bool isUncertain() const { return isUncertain_; }

		/**
		 * @return the begin of the triple.
		 */
		auto begin() const { return begin_; }

		/**
		 * @return the end of the triple.
		 */
		auto end() const { return end_; }

		/**
		 * @return the confidence of the triple.
		 */
		auto confidence() const { return confidence_; }

		/**
		 * @param isOccasional true if the triple is occasionally true.
		 */
		void setIsOccasional(bool isOccasional) { isOccasional_ = isOccasional; }

		/**
		 * @param isUncertain true if the triple is uncertain.
		 */
		void setIsUncertain(bool isUncertain) { isUncertain_ = isUncertain; }

		/**
		 * @param begin the begin of the triple.
		 */
		void setBegin(double begin) { begin_ = begin; }

		/**
		 * @param end the end of the triple.
		 */
		void setEnd(double end) { end_ = end; }

		/**
		 * @param confidence the confidence of the triple.
		 */
		void setConfidence(double confidence) { confidence_ = confidence; }

		/**
		 * Reset the triple to its initial state.
		 */
		virtual void reset() = 0;

		/**
		 * @param other another triple.
		 * @return true if the two triples are equal.
		 */
		bool operator==(const Triple &other) const;

		/**
		 * @param other another triple.
		 * @return true if this triple is less than the other triple.
		 */
		bool operator<(const Triple &other) const;

		// Override Printable
		void write(std::ostream &os) const override;

	protected:
		bool isOccasional_;
		bool isUncertain_;
		std::optional<double> begin_;
		std::optional<double> end_;
		std::optional<double> confidence_;
		std::optional<XSDType> xsdType_;

		static bool isBlank(std::string_view str) {
			return str.empty() || str[0] == '_';
		}
	};

	/**
	 * A Triple with a specified string type for the triple data.
	 * @tparam StringType the string type for the triple data.
	 */
	template<class StringType>
	class TripleTemplate : public Triple {
	protected:
		using TypedObject = std::variant<StringType,
				float,
				double,
				long,
				int,
				short,
				bool,
				unsigned long,
				unsigned int,
				unsigned short>;
	public:
		TripleTemplate()
				: Triple() {}

		TripleTemplate(std::string_view subject_iri, std::string_view predicate_iri)
				: Triple(),
				  subject_(subject_iri),
				  predicate_(predicate_iri) {
			xsdType_ = std::nullopt;
		}

		TripleTemplate(std::string_view subject_iri, std::string_view predicate_iri, std::string_view object_iri)
				: Triple(),
				  subject_(subject_iri),
				  predicate_(predicate_iri) {
			object_ = StringType(object_iri);
			xsdType_ = std::nullopt;
		}

		template<typename ValT>
		TripleTemplate(std::string_view subject, std::string_view predicate, const ValT &object, XSDType type)
				: Triple(),
				  subject_(subject),
				  predicate_(predicate) {
			if (type == XSDType::STRING) {
				object_ = StringType(object);
				xsdType_ = type;
			} else {
				set(object, type);
			}
		}

		explicit TripleTemplate(const Triple &other)
				: Triple() {
			subject_ = other.subject();
			predicate_ = other.predicate();
			isOccasional_ = other.isOccasional();
			isUncertain_ = other.isUncertain();
			xsdType_ = other.xsdType();
			graph_ = other.graph();
			perspective_ = other.perspective();
			begin_ = other.begin();
			end_ = other.end();
			confidence_ = other.confidence();

			if (!xsdType_) {
				object_ = StringType(other.valueAsString());
			} else {
				switch (*xsdType_) {
					case XSDType::STRING:
						object_ = StringType(other.valueAsString());
						break;
					case XSDType::FLOAT:
						object_ = other.valueAsFloat();
						break;
					case XSDType::DOUBLE:
						object_ = other.valueAsDouble();
						break;
					case XSDType::LONG:
						object_ = other.valueAsLong();
						break;
					case XSDType::NON_NEGATIVE_INTEGER:
					case XSDType::INTEGER:
						object_ = other.valueAsInt();
						break;
					case XSDType::BOOLEAN:
						object_ = other.valueAsBoolean();
						break;
					case XSDType::SHORT:
						object_ = other.valueAsShort();
						break;
					case XSDType::UNSIGNED_LONG:
						object_ = other.valueAsUnsignedLong();
						break;
					case XSDType::UNSIGNED_INT:
						object_ = other.valueAsUnsignedInt();
						break;
					case XSDType::UNSIGNED_SHORT:
						object_ = other.valueAsUnsignedShort();
						break;
					case XSDType::LAST:
						break;
				}
			}
		}

		// Override Triple
		void setSubject(std::string_view subject) override { subject_ = subject; }

		// Override Triple
		void setPredicate(std::string_view predicate) override { predicate_ = predicate; }

		// Override Triple
		void setObjectIRI(std::string_view object) override {
			object_ = StringType(object);
			xsdType_ = std::nullopt;
		}

		// Override Triple
		void setSubjectBlank(std::string_view identifier) override { setSubject(identifier); }

		// Override Triple
		void setObjectBlank(std::string_view identifier) override { setObjectIRI(identifier); }

		// Override Triple
		void setStringValue(std::string_view v) override {
			object_ = StringType(v);
			xsdType_ = XSDType::STRING;
		}

		// Override Triple
		void setDoubleValue(double v) override { set(v, XSDType::DOUBLE); }

		// Override Triple
		void setFloatValue(float v) override { set(v, XSDType::FLOAT); }

		// Override Triple
		void setIntValue(int v) override { set(v, XSDType::INTEGER); }

		// Override Triple
		void setBooleanValue(bool v) override { set(v, XSDType::BOOLEAN); }

		// Override Triple
		void setLongValue(long v) override { set(v, XSDType::LONG); }

		// Override Triple
		void setShortValue(short v) override { set(v, XSDType::SHORT); }

		// Override Triple
		void setUnsignedLongValue(unsigned long v) override { set(v, XSDType::UNSIGNED_LONG); }

		// Override Triple
		void setUnsignedIntValue(unsigned int v) override { set(v, XSDType::UNSIGNED_INT); }

		// Override Triple
		void setUnsignedShortValue(unsigned short v) override { set(v, XSDType::UNSIGNED_SHORT); }

		// Override Triple
		void setGraph(std::string_view graph) override { graph_ = graph; }

		// Override Triple
		void setPerspective(std::string_view perspective) override { perspective_ = perspective; }

		// Override Triple
		std::string_view subject() const override { return subject_; }

		// Override Triple
		std::string_view predicate() const override { return predicate_; }

		// Override Triple
		std::string_view valueAsString() const override { return std::get<StringType>(object_); }

		// Override Triple
		double valueAsDouble() const override { return std::get<double>(object_); }

		// Override Triple
		float valueAsFloat() const override { return std::get<float>(object_); }

		// Override Triple
		long valueAsLong() const override { return std::get<long>(object_); }

		// Override Triple
		int valueAsInt() const override { return std::get<int>(object_); }

		// Override Triple
		bool valueAsBoolean() const override { return std::get<bool>(object_); }

		// Override Triple
		short valueAsShort() const override { return std::get<short>(object_); }

		// Override Triple
		unsigned long valueAsUnsignedLong() const override { return std::get<unsigned long>(object_); }

		// Override Triple
		unsigned int valueAsUnsignedInt() const override { return std::get<unsigned int>(object_); }

		// Override Triple
		unsigned short valueAsUnsignedShort() const override { return std::get<unsigned short>(object_); }

		// Override Triple
		std::optional<std::string_view> graph() const override {
			if (graph_.has_value()) return graph_.value();
			else return std::nullopt;
		}

		// Override Triple
		std::optional<std::string_view> perspective() const override {
			if (perspective_.has_value()) return perspective_.value();
			else return std::nullopt;
		}

		void reset() override {
			graph_ = std::nullopt;
			begin_ = std::nullopt;
			end_ = std::nullopt;
			xsdType_ = std::nullopt;
			isUncertain_ = false;
			isOccasional_ = false;
		}

	protected:
		StringType subject_;
		StringType predicate_;
		TypedObject object_;

		std::optional<StringType> graph_;
		std::optional<StringType> perspective_;

		template<typename ValT>
		void set(const ValT &value, XSDType type) {
			object_ = value;
			xsdType_ = type;
		}
	};

	/**
	 * A Triple that holds a copy of the data.
	 */
	using TripleCopy = TripleTemplate<std::string>;
	/**
	 * A Triple with eternally allocated data.
	 */
	using TripleView = TripleTemplate<std::string_view>;

	/**
	 * A shared pointer to a Triple that can have ownership of the object.
	 * Be careful when using this struct as the triple is deleted by the ptr which has
	 * the ownership flag set to true.
	 * Copies are allowed and have the ownership flag set to false.
	 */
	struct TriplePtr {
		Triple *ptr;
		// The ownership flag.
		// It is marked as mutable to allow changing ownership even if we have a const reference to the object.
		mutable bool owned;

		explicit TriplePtr() : ptr(nullptr), owned(false) {}

		explicit TriplePtr(Triple *ptr) : ptr(ptr), owned(true) {}

		TriplePtr(const TriplePtr &other) : ptr(other.ptr), owned(false) {}

		~TriplePtr() { if (owned) delete ptr; }

		Triple &operator*() const { return *ptr; }

		Triple *operator->() const { return ptr; }

		bool operator==(const TriplePtr &other) const {
			return ptr==other.ptr || (ptr && other.ptr && *ptr == *other.ptr);
		}

		bool operator<(const TriplePtr &other) const {
			return (ptr && other.ptr) ? (*ptr < *other.ptr) : (ptr < other.ptr);
		}

		Triple &get() const { return *ptr; }
	};
}

#endif //KNOWROB_FRAMED_TRIPLE_H