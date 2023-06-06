#define PL_SAFE_ARG_MACROS
#include <bullet/btBulletDynamicsCommon.h>
#include <SWI-cpp.h>

// This file uses the same structure as utility/algebra.cpp to transform Prolog lists to the types of a library, here the library is bullet.

void pl2bullet(const PlTerm &arg, btQuaternion &q) {
  PlTail list(arg); PlTerm value;
  list.next(value); q.setX((double)value);
  list.next(value); q.setY((double)value);
  list.next(value); q.setZ((double)value);
  list.next(value); q.setW((double)value);
}

void pl2bullet(const PlTerm &arg, btVector3 &v) {
  PlTail list(arg); PlTerm value;
  list.next(value); v.setX((double)value);
  list.next(value); v.setY((double)value);
  list.next(value); v.setZ((double)value);
}

void pl2bullet(const PlTerm &arg, btTransform &t) {
  PlTail list(arg); PlTerm value;
  list.next(value); btVector3 v; pl2bullet(value, v);
  list.next(value); btQuaternion q; pl2bullet(value, q);
  t.setOrigin(v);
  t.setRotation(q);
}

void bullet2pl(const btQuaternion &q, const PlTerm &out) {
  PlTail l(out);
  l.append(q.getX());
  l.append(q.getY());
  l.append(q.getZ());
  l.append(q.getW());
  l.close();
}

void bullet2pl(const btVector3 &v, const PlTerm &out) {
  PlTail l(out);
  l.append(v.getX());
  l.append(v.getY());
  l.append(v.getZ());
  l.close();
}

void bullet2pl(const btTransform &t, const PlTerm &out) {
  PlTail l(out);
  PlTerm origin;
  bullet2pl(t.getOrigin(), origin);
  l.append(origin);
  PlTerm rotation;
  bullet2pl(t.getRotation(), rotation);
  l.append(rotation);
  l.close();
}
