/*
 * This file is part of KnowRob, please consult
 * https://github.com/knowrob/knowrob for license details.
 */

#ifndef KNOWROB_PY_WITH_H
#define KNOWROB_PY_WITH_H

#include <boost/python.hpp>
#include <boost/function.hpp>
#include <boost/function_types/components.hpp>
#include <boost/function_types/function_type.hpp>
#include <boost/function_types/result_type.hpp>

namespace knowrob::py {
	/**
	 * Functor that will invoke a function while holding a guard.
	 * Upon returning from the function, the guard is released.
	 */
	template<typename Signature, typename Guard>
	class guarded_function {
	public:
		typedef typename boost::function_types::result_type<Signature>::type result_type;

		template<typename Fn>
		explicit guarded_function(Fn fn)
				: fn_(fn) {}

		result_type operator()() {
			Guard g;
			return fn_();
		}

		template<typename A1>
		result_type operator()(const A1 &a1) {
			Guard g;
			return fn_(a1);
		}

		template<typename A1, typename A2>
		result_type operator()(const A1 &a1, const A2 &a2) {
			Guard g;
			return fn_(a1, a2);
		}

		template<typename A1, typename A2, typename A3>
		result_type operator()(const A1 &a1, const A2 &a2, const A3 &a3) {
			Guard g;
			return fn_(a1, a2, a3);
		}

	private:
		boost::function<Signature> fn_;
	};

	/** Provides signature type. */
	template<typename Signature>
	struct mpl_signature {
		typedef typename boost::function_types::components<Signature,
				boost::add_pointer<boost::mpl::placeholders::_> >::type type;
	};

	/** Support boost::function. */
	template<typename Signature>
	struct mpl_signature<boost::function<Signature> > {
		typedef typename boost::function_types::components<Signature>::type type;
	};

	/** Create a callable object with guards. */
	template<typename Guard, typename Fn, typename Policy>
	boost::python::object with_aux(Fn fn, const Policy &policy) {
		// Obtain the components of the Fn.  This will decompose non-member
		// and member functions into an mpl sequence.
		//   R (*)(A1)    => R, A1
		//   R (C::*)(A1) => R, C*, A1
		typedef typename mpl_signature<Fn>::type mpl_signature_type;

		// Synthesize the components into a function type.  This process
		// causes member functions to require the instance argument.
		// This is necessary because member functions will be explicitly
		// provided the 'self' argument.
		//   R, A1     => R (*)(A1)
		//   R, C*, A1 => R (*)(C*, A1)
		typedef typename boost::function_types::function_type<
				mpl_signature_type>::type signature_type;

		// Create a callable boost::python::object that delegates to the
		// guarded_function.
		return boost::python::make_function(
				guarded_function<signature_type, Guard>(fn),
				policy, mpl_signature_type());
	}

	/** Create a callable object with guards. */
	template<typename Guard, typename Fn, typename Policy>
	boost::python::object with(const Fn &fn, const Policy &policy) {
		return with_aux<Guard>(fn, policy);
	}

	/** @brief Create a callable object with guards. */
	template<typename Guard, typename Fn>
	boost::python::object with(const Fn &fn) {
		return with<Guard>(fn, boost::python::default_call_policies());
	}
}

#endif //KNOWROB_PY_WITH_H
