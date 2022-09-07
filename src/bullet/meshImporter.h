#ifndef MESH_IMPORTER_H__
#define MESH_IMPORTER_H__

#include <bullet/BulletCollision/CollisionShapes/btCollisionShape.h>

/**
 * Import a mesh from a file. the three optional values currently don't do anything, but might be implemented in the future.
 */
btCollisionShape* loadMesh(const char* filename,
						   bool flipWindingOrder = false,
						   bool removeIdenticalVertices = true,
						   bool fixNormals = true);

#endif
