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

btConvexHullShape* buildMesh(const aiMesh* mesh, const char* filename) {
  if (mesh->mPrimitiveTypes != aiPrimitiveType_TRIANGLE) {
	ROS_WARN("Mesh file '%s' contains non-triangle primitive(s), ignoring them", filename);
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

btCollisionShape* buildNode(const aiScene* scene, const aiNode* aiNode, const btTransform& parentTransform, bool hasParent, const char* filename) {
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

btCollisionShape* buildModelMesh(const aiScene* scene, const char* filename, bool fixNormals = true) {
  return buildNode(scene, scene->mRootNode, btTransform(), false, filename);
}

btCollisionShape* loadMesh(const char* filename,
						  bool flipWindingOrder,
						  bool removeIdenticalVertices,
						  bool fixNormals) {
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
  const aiScene* scene = importer.ReadFile( filename, // ReadFile expects a null-termiated c string
											aiProcess_Triangulate            |
											aiProcess_JoinIdenticalVertices  |
											aiProcess_GenSmoothNormals       |
											aiProcess_FixInfacingNormals     |
											extraFlags);

  // If the import failed, report it
  if (scene == nullptr) {
	PlException e(PlCompound("mesh_file_error", PlTermv(PlTerm(importer.GetErrorString()), PlTerm(filename))));
	e.cppThrow();
  }

  if (scene->mNumMeshes <= 0) {
	PlException e(PlCompound("mesh_file_error", PlTermv(PlTerm("no_meshes"), PlTerm(filename))));
	e.cppThrow();
  }

  // this CollisionShape is either a btCompoundShape or a btConvexHullShape
  btCollisionShape* meshes = buildModelMesh(scene, filename, fixNormals);
  

  // We're done. Everything will be cleaned up by the importer destructor
  return meshes;
}
