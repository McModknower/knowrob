#include "meshImporter.h"

#include <assimp/Importer.hpp>      // C++ importer interface
#include <assimp/scene.h>           // Output data structure
#include <assimp/postprocess.h>     // Post processing flags

#include <bullet/BulletCollision/CollisionShapes/btCollisionShape.h>
#include <bullet/BulletCollision/CollisionShapes/btConvexHullShape.h>
#include <bullet/BulletCollision/CollisionShapes/btCompoundShape.h>
#include <bullet/LinearMath/btTransform.h>

// For the PL exceptions
#define PL_SAFE_ARG_MACROS
#include <SWI-cpp.h>

#include <vector>
#include "window/userData.h"

#include <ros/console.h>

// to use package-relative paths
#include <ros/package.h>
#include <string>

btTransform ai2bullet(const aiMatrix4x4& transform) {
  btTransform nodeTransform;
  nodeTransform.getBasis()
	  .setValue(transform.a1, transform.a2, transform.a3,
				transform.b1, transform.b2, transform.b3,
				transform.c1, transform.c2, transform.c3);
	nodeTransform.getOrigin()
	  .setValue(transform.a4, transform.b4, transform.c4);
	return nodeTransform;
}

btVector3 ai2bullet(const aiVector3D& aivec) {
  return btVector3(aivec.x,
				   aivec.y,
				   aivec.z);
}

// The filename is always a parameter so the error/log messages can include the filename.

btConvexHullShape* buildMesh(const aiMesh* mesh, const std::string& filename) {
  if (mesh->mPrimitiveTypes != aiPrimitiveType_TRIANGLE) {
    ROS_WARN("Mesh file '%s' contains non-triangle primitive(s), ignoring them", filename.c_str());
  }
  btConvexHullShape* shape = new btConvexHullShape();
  for (unsigned int i = 0; i < mesh->mNumVertices; i++) {
	shape->addPoint(ai2bullet(mesh->mVertices[i]));
  }
  std::vector<Face> faces;
  for (unsigned int i = 0; i < mesh->mNumFaces; i++) {
	aiFace& face = mesh->mFaces[i];
	if (face.mNumIndices != 3) {
	  continue;
	}
	unsigned int* idx = face.mIndices;
	faces.push_back(Face({ai2bullet(mesh->mVertices[idx[0]]),
						  ai2bullet(mesh->mVertices[idx[1]]),
						  ai2bullet(mesh->mVertices[idx[2]])},
						 {ai2bullet(mesh->mNormals[idx[0]]),
						  ai2bullet(mesh->mNormals[idx[1]]),
						  ai2bullet(mesh->mNormals[idx[2]])}));
  }
  MeshRenderData* data = new MeshRenderData(std::move(faces), false, true);
  shape->setUserPointer(data);
  return shape;
}

btCollisionShape* buildNode(const aiScene* scene, const aiNode* aiNode, const btTransform& parentTransform, bool hasParent, const std::string& filename) {
  btTransform totalTransform;
  if (hasParent) {
	totalTransform.setIdentity();
  } else {
	totalTransform = parentTransform * ai2bullet(aiNode->mTransformation);
  }
  // TODO check if these checks are actually useful
  if (aiNode->mNumMeshes == 1 && aiNode->mNumChildren == 0) {
	return buildMesh(scene->mMeshes[aiNode->mMeshes[0]], filename);
  } else if (aiNode->mNumMeshes == 0 && aiNode->mNumChildren == 1) {
	return buildNode(scene, aiNode->mChildren[0], totalTransform, true, filename);
  }
  btCompoundShape* shape = new btCompoundShape();
  btTransform ident;
  ident.setIdentity();
  for (int i = 0; i < aiNode->mNumMeshes; i++) {
	shape->addChildShape(ident, buildMesh(scene->mMeshes[aiNode->mMeshes[i]], filename));
  }
  for (int i = 0; i < aiNode->mNumChildren; i++) {
	shape->addChildShape(ai2bullet(aiNode->mChildren[i]->mTransformation),
						 buildNode(scene, aiNode->mChildren[i], totalTransform, true, filename));
  }
  return shape;
}

btCollisionShape* buildModelMesh(const aiScene* scene, const std::string& filename, bool fixNormals = true) {
  return buildNode(scene, scene->mRootNode, btTransform(), false, filename);
}

std::string resolveFilename(const char* filename) {
  const std::string prefix="package://";
  const std::string::size_type prefixlen=prefix.length();

  std::string name(filename);
  if(name.find(prefix) != 0) {
    // it doesn't start with package://
    return name;
  }
  //This might not work on windows, but idk anyone with ros on windows and don't have windows, so i won't try it.
  auto end = name.find('/', prefixlen);
  if(end == std::string::npos) {
    //if it is only the package path, it is an error, since a model can't be a folder as far as i know.
    PlException e(PlCompound("mesh_file_error", PlTermv(PlTerm("only_package_name"), PlTerm(filename))));
    e.cppThrow();
  }
  std::string package(name,prefixlen,end-prefixlen);
  std::string fullPath = ros::package::getPath(package);
  if(fullPath.empty()) {
    //give back the whole filename to make it easier to find which one is the erroring one
    PlException e(PlCompound("mesh_file_error", PlTermv(PlTerm("package_not_found"), PlTerm(filename))));
    e.cppThrow();
  }
  //`rospack find <package>` doesn't have a / at the end, so i can use ``end'' as the start for the rest
  fullPath.append(name, end, std::string::npos);
  return fullPath;
}

btCollisionShape* loadMesh(const char* filename,
						  bool flipWindingOrder,
						  bool removeIdenticalVertices,
						  bool fixNormals) {

  std::string name = resolveFilename(filename);

  // basic structure from the official assimp docs at https://assimp-docs.readthedocs.io/en/latest/usage/use_the_lib.html

  // Create an instance of the Importer class
  Assimp::Importer importer;

  // And have it read the given file with some example postprocessing
  // Usually - if speed is not the most important aspect for you - you'll
  // probably to request more postprocessing than we do in this example.
  unsigned int extraFlags = 0;
  if (flipWindingOrder) {
	extraFlags |= aiProcess_FlipWindingOrder;
  }
  const aiScene* scene = importer.ReadFile( name.c_str(), // ReadFile expects a null-termiated c string
											aiProcess_Triangulate            |
											aiProcess_JoinIdenticalVertices  |
											aiProcess_GenSmoothNormals       |
											aiProcess_FixInfacingNormals     |
											extraFlags);

  // If the import failed, report it
  if (scene == nullptr) {
    PlException e(PlCompound("mesh_file_error", PlTermv(PlTerm(importer.GetErrorString()), PlTerm(name.c_str()))));
	e.cppThrow();
  }

  if (scene->mNumMeshes <= 0) {
	PlException e(PlCompound("mesh_file_error", PlTermv(PlTerm("no_meshes"), PlTerm(name.c_str()))));
	e.cppThrow();
  }

  // this CollisionShape is either a btCompoundShape or a btConvexHullShape
  btCollisionShape* meshes = buildModelMesh(scene, filename, fixNormals);
  

  // We're done. Everything will be cleaned up by the importer destructor
  return meshes;
}
