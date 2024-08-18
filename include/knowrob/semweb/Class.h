//
// Created by daniel on 07.04.23.
//

#ifndef KNOWROB_SEMWEB_CLASS_H
#define KNOWROB_SEMWEB_CLASS_H

#include <memory>
#include <list>
#include <functional>
#include "Resource.h"

namespace knowrob::semweb {

	// forward declaration
	class Class;

	// called for each parent in the property hierarchy
	using ClassVisitor = std::function<void(Class &)>;
	// called for each parent in the property hierarchy
	using ClassTupleVisitor = std::function<void(Class &, Class &)>;

	/**
	 * A RDF class.
	 */
	class Class : public Resource, public std::enable_shared_from_this<Class> {
	public:
		/**
		 * @param iri A class IRI.
		 */
		explicit Class(std::string_view iri);

		/**
		 * @param iri A class IRI.
		 */
		explicit Class(const IRIAtomPtr &iri);

		/**
		 * @param directParent a direct super class.
		 * @param graph the graph where the relation is defined.
		 */
		void addDirectParent(const std::shared_ptr<Class> &directParent, std::optional<std::string_view> graph);

		/**
		 * @param directParent a direct super class.
		 * @param graph the graph where the relation is defined.
		 */
		void removeDirectParent(const std::shared_ptr<Class> &directParent, std::optional<std::string_view> graph);

		/**
		 * @return all direct super classes of this class.
		 */
		const auto &directParents() const { return directParents_; }

		/**
		 * @param directParent a direct super class.
		 * @return true if directParent is a direct super class of this class.
		 */
		bool isDirectSubClassOf(const std::shared_ptr<Class> &directParent);

		/**
		 * @param parent a super class.
		 * @param includeSelf if true, the method returns true if this class is the same
		 * @return true if this class is a sub class of parent.
		 */
		bool isSubClassOf(const std::shared_ptr<Class> &parent, bool includeSelf = true);

		/**
		 * @param visitor a function that is called for each parent in the class hierarchy.
		 * @param includeSelf if true, the method calls the visitor for this class.
		 * @param skipDuplicates if true, the method calls the visitor only once for each class.
		 */
		void forallParents(const ClassVisitor &visitor, bool includeSelf = true, bool skipDuplicates = true);

		/**
		 * @param visitor a function that is called for each child in the class hierarchy.
		 * @param skipDuplicates if true, the method calls the visitor only once for each class.
		 */
		void forallChildren(const ClassTupleVisitor &visitor, bool skipDuplicates = true);

	protected:
		struct ClassComparator {
			bool operator()(const std::shared_ptr<Class> &lhs, const std::shared_ptr<Class> &rhs) const;
		};

		std::map<std::shared_ptr<Class>,
				std::set<AtomPtr, AtomComparator>,
				ClassComparator> directParents_;
		std::set<std::shared_ptr<Class>, ClassComparator> directChildren_;
	};

	using ClassPtr = std::shared_ptr<Class>;

} // knowrob::semweb

#endif //KNOWROB_SEMWEB_CLASS_H
