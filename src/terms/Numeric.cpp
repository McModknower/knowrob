/*
 * This file is part of KnowRob, please consult
 * https://github.com/knowrob/knowrob for license details.
 */

#include "knowrob/terms/Numeric.h"
#include "knowrob/integration/python/utils.h"

using namespace knowrob;

bool Numeric::isFloatingNumber() const {
	return xsdType() == XSDType::FLOAT || xsdType() == XSDType::DOUBLE;
}

std::shared_ptr<Numeric> Numeric::trueAtom() {
	static auto trueAtom = std::make_shared<Boolean>(true);
	return trueAtom;
}

std::shared_ptr<Numeric> Numeric::falseAtom() {
	static auto falseAtom = std::make_shared<Boolean>(false);
	return falseAtom;
}

namespace knowrob::py {
	template<>
	void createType<Numeric>() {
		using namespace boost::python;
		class_<Numeric, std::shared_ptr<Numeric>, bases<XSDAtomic>, boost::noncopyable>
				("Numeric", no_init)
				.def("isFloatingNumber", &Numeric::isFloatingNumber)
				.def("isSameNumeric", &Numeric::isSameNumeric);
		class_<Double, std::shared_ptr<Double>, bases<Numeric>>
				("Double", init<double>())
				.def(init<std::string_view>())
				.def("numericForm", &Double::numericForm);
		class_<Float, std::shared_ptr<Float>, bases<Numeric>>
				("Float", init<float>())
				.def(init<std::string_view>())
				.def("numericForm", &Float::numericForm);
		class_<Integer, std::shared_ptr<Integer>, bases<Numeric>>
				("Integer", init<int>())
				.def(init<std::string_view>())
				.def("numericForm", &Integer::numericForm);
		class_<Long, std::shared_ptr<Long>, bases<Numeric>>
				("Long", init<long>())
				.def(init<std::string_view>())
				.def("numericForm", &Long::numericForm);
		class_<Short, std::shared_ptr<Short>, bases<Numeric>>
				("Short", init<short>())
				.def(init<std::string_view>())
				.def("numericForm", &Short::numericForm);
	}
}
