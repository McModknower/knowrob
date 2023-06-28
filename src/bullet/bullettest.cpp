#define PL_SAFE_ARG_MACROS
#include <SWI-cpp.h>

# include <iostream>
# include <GL/glut.h>

#include <btBulletDynamicsCommon.h>

#include <thread>
#include <mutex>
#include <map>

#include <optional>

#include "knowrobBulletWindow.h"
#include "bullet2pl.h"

#include "window/userData.h"
#include "meshImporter.h"

std::mutex glut_main_loop_mutex;

void threaded(Window *window) {

  // only allow one initialized glut at a time.
  // might be changed to allow multiple windows in one glut main loop later
  const std::lock_guard<std::mutex> lock(glut_main_loop_mutex);
  int i = 0;
  glutInit(&i, nullptr);

  glutInitWindowPosition(100,100);
  glutInitWindowSize(320,320);

  //window.setBulletSpeedMultiplier(0.1);
  window->display();
  // enter GLUT event processing cycle
  glutSetOption(GLUT_ACTION_ON_WINDOW_CLOSE, GLUT_ACTION_CONTINUE_EXECUTION);

  glutMainLoop();
}

class NamedRigidBody : public btRigidBody
{
public:
  const PlAtom m_name;
  NamedRigidBody(const PlAtom& name,
				 const btRigidBody::btRigidBodyConstructionInfo& constructionInfo) :
	btRigidBody(constructionInfo),
	m_name(name)
  {}
};

PREDICATE(create_world,1) {
  auto *w = new KnowrobBulletWindow();
  w->m_world->setGravity(btVector3(0, 0, -9.81));
  PL_A1 = w->getKnowrobId();
  return true;
}

PREDICATE(show_world,1){
  auto window = KnowrobBulletWindow::findById(PL_A1);
  if(window == nullptr) {
	return false;
  }
  std::thread loopThread(threaded, window);
  loopThread.detach();
  return true;
}

PREDICATE(delete_world,1) {
  auto window = KnowrobBulletWindow::findById(PL_A1);
  if(window == nullptr) {
	return false;
  }
  window->closeOnNextTick();
  return true;
}

// step_world(World, Seconds) is det.
//
PREDICATE(step_world,2) {
	auto window = KnowrobBulletWindow::findById(PL_A1);
	if(window == nullptr) {
		return false;
	}
	window->setTimeLeft((double)PL_A2);
	return TRUE;
}

/**
 * add_object(World, Object, Pose, Data)
 * Object can be one of:
 * box(X,Y,Z)
 * testmesh()
 * mesh(filename)
 * //TODO Add more
 * Pose should be in the format
 * [[X,Y,Z],[X,Y,Z,W]]
 * where the second part is a quaternion for the rotation.
 * Data is a list of attributes. Currently supported:
 * mass(12.34)
 */
PREDICATE(add_object,4) {
  btTransform pose;
  pl2bullet(PL_A3, pose);

  btCollisionShape* colShape;
  const char *type = PL_A2.name();
  if(strcmp(type, "box") == 0) {
    colShape = new btBoxShape(btVector3((double)PL_A2[1],
					(double)PL_A2[2],
					(double)PL_A2[3]));
  } else if(strcmp(type, "testmesh") == 0) {
	btConvexHullShape* mesh = new btConvexHullShape();
	mesh->addPoint(btVector3(-1, 0, 0));
	mesh->addPoint(btVector3(1, 0, 0));
	mesh->addPoint(btVector3(0, -1, 0));
	mesh->addPoint(btVector3(0, 1, 0));
	mesh->addPoint(btVector3(0, 0, 1));
	std::vector<Face> faces;
	faces.push_back(Face({btVector3(0, 0, 1),btVector3(-1, 0, 0), btVector3(0, -1, 0)},
						 {btVector3(0, 0, 1),btVector3(-1, 0, 0), btVector3(0, -1, 0)}));
	faces.push_back(Face({btVector3(0, 0, 1),btVector3( 1, 0, 0), btVector3(0, -1, 0)},
						 {btVector3(0, 0, 1),btVector3( 1, 0, 0), btVector3(0, -1, 0)}));
	faces.push_back(Face({btVector3(0, 0, 1),btVector3( 1, 0, 0), btVector3(0,  1, 0)},
						 {btVector3(0, 0, 1),btVector3( 1, 0, 0), btVector3(0,  1, 0)}));
	faces.push_back(Face({btVector3(0, 0, 1),btVector3(-1, 0, 0), btVector3(0,  1, 0)},
						 {btVector3(0, 0, 1),btVector3(-1, 0, 0), btVector3(0,  1, 0)}));
	MeshRenderData* data = new MeshRenderData(std::move(faces), false, true);
	mesh->setUserPointer(data);
    colShape = mesh;
  } else if(strcmp(type, "mesh") == 0) {
	colShape = loadMesh((const char*)PL_A2[1]);
  } else {
    PlException e(PlCompound("not_valid_bullet_object_type", PlTerm(type)));
    e.cppThrow();
  }

  btDefaultMotionState* myMotionState = new btDefaultMotionState(pose);
  btRigidBody::btRigidBodyConstructionInfo rbInfo(0.0f, myMotionState, colShape);

  PlTail dataList(PL_A4);
  PlTerm data;
  PlAtom objectName{"unnamed-object"};
  while(dataList.next(data)) {
    const char *dataType = data.name();
    if(strcmp(dataType, "mass") == 0) {
      btScalar mass((double)data[1]);

      //rigidbody is dynamic if and only if mass is non zero, otherwise static
      bool isDynamic = (mass != 0.f);

      btVector3 localInertia(0, 0, 0);
      if (isDynamic) {
        colShape->calculateLocalInertia(mass, localInertia);
      }
      rbInfo.m_mass = mass;
      rbInfo.m_localInertia = localInertia;
    } else if(strcmp(dataType, "m_friction") == 0) {
      btScalar friction((double)data[1]);
      rbInfo.m_friction = friction;
    } else if(strcmp(dataType, "name") == 0) {
		objectName = PlAtom(data[1]);
	}
  }

  btRigidBody* body = new NamedRigidBody(objectName, rbInfo);

  auto window = KnowrobBulletWindow::findById(PL_A1);
  if(window == nullptr) {
	  return false;
  }

  window->m_world->addRigidBody(body);
  return true;
}

PREDICATE(query_object_pose, 3) {
	auto window = KnowrobBulletWindow::findById(PL_A1);
	if(window == nullptr) {
		return false;
	}
  const btCollisionObjectArray &objects = window->m_world->getCollisionObjectArray();
  PlAtom name(PL_A2);
  for(int i = 0; i<objects.size(); i++) {
	NamedRigidBody* body = dynamic_cast<NamedRigidBody*>(objects[i]);
	if(body) {
	  if(body->m_name == name) {
		bullet2pl(body->getWorldTransform(), PL_A3);
		return TRUE;
	  }
	}
  }
  return FALSE;
}

PREDICATE(wait_until_finished_simulating, 1) {
	auto window = KnowrobBulletWindow::findById(PL_A1);
	if(window == nullptr) {
		return false;
	}
	window->waitUntilStopped();
	return true;
}
