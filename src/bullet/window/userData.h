#ifndef USERDATA_H__
#define USERDATA_H__

#include <vector>
#include <bullet/LinearMath/btVector3.h>

class Face {
 public:
  // Copy of the points so the points get deleted together with this objects
  const std::vector<btVector3> points;
  const std::vector<btVector3> normals;
 Face(std::vector<btVector3>&& points,
	  std::vector<btVector3>&& normals)
   : points(std::move(points)),
	normals(std::move(normals))
	{}
  Face(std::initializer_list<btVector3> points,
	  std::initializer_list<btVector3>  normals)
   : points(points),
	normals(normals)
	{}
};

class MeshRenderData {
 public:
  const std::vector<Face> faces;
  const bool smoothShading;
  const bool disableFaceCulling;
 MeshRenderData(const std::vector<Face>&& faces, bool smoothShading = false, bool disableFaceCulling = false) :
	faces(faces),
	smoothShading(smoothShading),
	disableFaceCulling(disableFaceCulling)
	{}
};

#endif
