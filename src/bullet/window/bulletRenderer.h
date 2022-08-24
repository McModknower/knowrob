#ifndef BULLET_RENDERER_H__
#define BULLET_RENDERER_H__

#include "settings.h"
#include <bullet/BulletCollision/CollisionDispatch/btCollisionObject.h>
#include <bullet/BulletCollision/CollisionShapes/btCollisionShape.h>

void render(const btCollisionObjectArray &objects);

void render(const btCollisionObject *object);

void render(const btCollisionShape *shape);

#endif
