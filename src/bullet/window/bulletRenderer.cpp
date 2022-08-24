#include "bulletRenderer.h"

#include <cmath>

#include <bullet/BulletCollision/BroadphaseCollision/btBroadphaseProxy.h>  //for the shape types

//getCollisionObjectArray()

// all the individual shapes
#include <bullet/BulletCollision/CollisionShapes/btBoxShape.h>
#include <bullet/BulletCollision/CollisionShapes/btSphereShape.h>
#include <bullet/BulletCollision/CollisionShapes/btStaticPlaneShape.h>

#include "glut.h"

//for debugging
#include <iostream>

void render(const btCollisionObjectArray &objects) {
  //objects is actually a btAlignedObjectArray<class btCollisionObject*>
  for(int i = 0; i<objects.size(); i++) {
    render(objects[i]);
  }
}

void render(const btCollisionObject *object) {
  btScalar glMatrix[16];
  object->getWorldTransform().getOpenGLMatrix(glMatrix);
  glPushMatrix();
  glMultMatrixf(glMatrix);
  render(object->getCollisionShape());
  glPopMatrix();
}

void renderBox(const btBoxShape &shape) {
  const btVector3 &dimensions = shape.getHalfExtentsWithoutMargin();
  glPushMatrix();
  glScalef(dimensions.getX(), dimensions.getY(), dimensions.getZ());
  glutSolidCube(2);
  glPopMatrix();
}

void renderSphere(const btSphereShape &shape) {
  btScalar radius = shape.getRadius();
  // use 50 divisions, just like cram
  glutSolidSphere(radius, 50, 50);
}

void renderPlane(const btStaticPlaneShape &shape) {
  const btVector3 normal = shape.getPlaneNormal();
  const btScalar constant = shape.getPlaneConstant();
  glPushMatrix();
  glTranslatef(normal.getX() * constant,
	       normal.getY() * constant,
	       normal.getZ() * constant);
  btVector3 z_axis = btVector3(0, 0, 1);
  btVector3 rotation_axis = z_axis.cross(normal);
  glRotatef(
	    /*(* 180 (/ (acos (/ (cl-transforms:dot-product normal z-axis)
	     *                   (cl-transforms:v-norm normal)))
	     *          pi))*/
	    180 * (acos(normal.dot(z_axis) / normal.norm() ) / M_PI),
	    rotation_axis.getX(),
	    rotation_axis.getY(),
	    rotation_axis.getZ());
  glPushAttrib(GL_ENABLE_BIT);
  glDisable(GL_CULL_FACE);
  glDisable(GL_LIGHTING);
  glBegin(GL_QUADS);
  glNormal3f(0, 0, 1);
  glTexCoord2f(0, 0);
  glVertex2f(-100, -100);
  glTexCoord2f(200, 0);
  glVertex2f(100, -100);
  glTexCoord2f(200, 200);
  glVertex2f(100, 100);
  glTexCoord2f(0, 200);
  glVertex2f(-100, 100);
  glEnd();
  glPopAttrib();
  glPopMatrix();
}

void render(const btCollisionShape *shape) {
  switch(shape->getShapeType()) {
  case BOX_SHAPE_PROXYTYPE:
    renderBox(dynamic_cast<const btBoxShape&>(*shape));
    break;
  case SPHERE_SHAPE_PROXYTYPE:
    renderSphere(dynamic_cast<const btSphereShape&>(*shape));
    break;
  case STATIC_PLANE_PROXYTYPE:
    renderPlane(dynamic_cast<const btStaticPlaneShape&>(*shape));
    break;
  default:
    break;
  }
}

