#define PL_SAFE_ARG_MACROS
#include <SWI-cpp.h>

# include <iostream>
# include <GL/glut.h>

#include <btBulletDynamicsCommon.h>

#include <thread>

#include <map>

#include "window/tickingBulletWindow.h"
#include "bullet2pl.h"

#include "window/userData.h"
#include "meshImporter.h"

void threaded(TickingBulletWindow *window) {

  int i = 0;
  glutInit(&i, nullptr);

  glutInitWindowPosition(100,100);
  glutInitWindowSize(320,320);

  //window.setBulletSpeedMultiplier(0.1);
  window->display();
  // enter GLUT event processing cycle
  glutSetOption(GLUT_ACTION_ON_WINDOW_CLOSE, GLUT_ACTION_CONTINUE_EXECUTION);

  glutMainLoop();
  // // after glutMainLoop, glut needs to be reinitialized
  // i = 0;
  // glutInit(&i, nullptr);

  // glutInitWindowPosition(100,100);
  // glutInitWindowSize(320,320);

  // window.setBulletSpeedMultiplier(10);

  // window.display();
  // // enter GLUT event processing cycle
  // glutSetOption(GLUT_ACTION_ON_WINDOW_CLOSE, GLUT_ACTION_CONTINUE_EXECUTION);

  // glutMainLoop();

}

class DynamicsWorldHandle
{
public:
  btBroadphaseInterface *broadphase;
  btCollisionConfiguration *collisionConfiguration;
  btCollisionDispatcher *dispatcher;
  btConstraintSolver *solver;
  btDynamicsWorld *dynamicsWorld;
  TickingBulletWindow *window;
  const uint16_t id;
  DynamicsWorldHandle(const uint16_t id_param) :
    id(id_param),
    broadphase(new btDbvtBroadphase()),
    collisionConfiguration(new btDefaultCollisionConfiguration()),
    dispatcher(new btCollisionDispatcher(this->collisionConfiguration)),
    solver(new btSequentialImpulseConstraintSolver()),
    dynamicsWorld(new btDiscreteDynamicsWorld(this->dispatcher,
					      this->broadphase,
					      this->solver,
					      this->collisionConfiguration)),
    window(new TickingBulletWindow("Bullet Visualization",this->dynamicsWorld))
  {}
  ~DynamicsWorldHandle() {
    this->window->close();
    delete this->window;
    delete this->dynamicsWorld;
    delete this->solver;
    delete this->dispatcher;
    delete this->collisionConfiguration;
    delete this->broadphase;
  }
};

std::map<int, DynamicsWorldHandle*> allBulletWindows{};
int bulletWindowCounter = 0;

// Returning the pointer, because it is the easiest way.
// This might be changed later
PREDICATE(create_world,1) {
  DynamicsWorldHandle *w = new DynamicsWorldHandle(bulletWindowCounter++);
  w->dynamicsWorld->setGravity(btVector3(0, 0, -9.81));
  allBulletWindows[w->id] = w;
  PL_A1 = w->id;
  return true;
}

PREDICATE(show_world,1){
  if(allBulletWindows.find(PL_A1) == allBulletWindows.end()) {
    return false;
  }
  std::thread loopThread(threaded, allBulletWindows[PL_A1]->window);
  loopThread.detach();
  return true;
}

PREDICATE(delete_world,1) {
  if(allBulletWindows.find(PL_A1) == allBulletWindows.end()) {
    return false;
  }
  DynamicsWorldHandle* ptr = allBulletWindows[PL_A1];
  allBulletWindows.erase(PL_A1);
  delete ptr;
  return true;
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

  if(allBulletWindows.find(PL_A1) == allBulletWindows.end()) {
    return false;
  }
  DynamicsWorldHandle* world = allBulletWindows[PL_A1];


  btDefaultMotionState* myMotionState = new btDefaultMotionState(pose);
  btRigidBody::btRigidBodyConstructionInfo rbInfo(0.0f, myMotionState, colShape);

  PlTail dataList(PL_A4);
  PlTerm data;
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
    }
  }

  btRigidBody* body = new btRigidBody(rbInfo);

  world->dynamicsWorld->addRigidBody(body);
  return true;
  //TODO delete the body, motionState, and colShape when the world is deleted
}

/// This is a Hello World program for running a basic Bullet physics simulation

// PREDICATE(start_world,0) {

//   {
//     int i = 0;
//     glutInit(&i, nullptr);
//   }
//     //copied code from bullet example hello world
//     int i;
//     ///-----initialization_start-----

//     ///collision configuration contains default setup for memory, collision setup. Advanced users can create their own configuration.
//     btDefaultCollisionConfiguration* collisionConfiguration = new btDefaultCollisionConfiguration();

//     ///use the default collision dispatcher. For parallel processing you can use a diffent dispatcher (see Extras/BulletMultiThreaded)
//     btCollisionDispatcher* dispatcher = new btCollisionDispatcher(collisionConfiguration);

//     ///btDbvtBroadphase is a good general purpose broadphase. You can also try out btAxis3Sweep.
//     btBroadphaseInterface* overlappingPairCache = new btDbvtBroadphase();

//     ///the default constraint solver. For parallel processing you can use a different solver (see Extras/BulletMultiThreaded)
//     btSequentialImpulseConstraintSolver* solver = new btSequentialImpulseConstraintSolver;

//     btDiscreteDynamicsWorld* dynamicsWorld = new btDiscreteDynamicsWorld(dispatcher, overlappingPairCache, solver, collisionConfiguration);

//     dynamicsWorld->setGravity(btVector3(0, -10, 0));

//     ///-----initialization_end-----

//     //keep track of the shapes, we release memory at exit.
//     //make sure to re-use collision shapes among rigid bodies whenever possible!
//     btAlignedObjectArray<btCollisionShape*> collisionShapes;

//     ///create a few basic rigid bodies

//     //the ground is a cube of side 100 at position y = -56.
//     //the sphere will hit it at y = -6, with center at -5
//     {
//       btCollisionShape* groundShape = new btBoxShape(btVector3(btScalar(50.), btScalar(50.), btScalar(50.)));

//       collisionShapes.push_back(groundShape);

//       btTransform groundTransform;
//       groundTransform.setIdentity();
//       groundTransform.setOrigin(btVector3(0, -56, 0));

//       btScalar mass(0.);

//       //rigidbody is dynamic if and only if mass is non zero, otherwise static
//       bool isDynamic = (mass != 0.f);

//       btVector3 localInertia(0, 0, 0);
//       if (isDynamic)
// 	groundShape->calculateLocalInertia(mass, localInertia);

//       //using motionstate is optional, it provides interpolation capabilities, and only synchronizes 'active' objects
//       btDefaultMotionState* myMotionState = new btDefaultMotionState(groundTransform);
//       btRigidBody::btRigidBodyConstructionInfo rbInfo(mass, myMotionState, groundShape, localInertia);
//       btRigidBody* body = new btRigidBody(rbInfo);

//       //add the body to the dynamics world
//       dynamicsWorld->addRigidBody(body);
//     }

//     {
//       //create a dynamic rigidbody

//       //btCollisionShape* colShape = new btBoxShape(btVector3(1,1,1));
//       btCollisionShape* colShape = new btSphereShape(btScalar(1.));
//       collisionShapes.push_back(colShape);

//       /// Create Dynamic Objects
//       btTransform startTransform;
//       startTransform.setIdentity();

//       btScalar mass(1.f);

//       //rigidbody is dynamic if and only if mass is non zero, otherwise static
//       bool isDynamic = (mass != 0.f);

//       btVector3 localInertia(0, 0, 0);
//       if (isDynamic)
// 	colShape->calculateLocalInertia(mass, localInertia);

//       startTransform.setOrigin(btVector3(2, 10, 0));

//       //using motionstate is recommended, it provides interpolation capabilities, and only synchronizes 'active' objects
//       btDefaultMotionState* myMotionState = new btDefaultMotionState(startTransform);
//       btRigidBody::btRigidBodyConstructionInfo rbInfo(mass, myMotionState, colShape, localInertia);
//       btRigidBody* body = new btRigidBody(rbInfo);

//       dynamicsWorld->addRigidBody(body);
//     }
//     {
//       //create a ground plane
//       btCollisionShape* colShape = new btStaticPlaneShape(btVector3(0, 0, 1), 0);
//       btTransform startTransform;
//       startTransform.setIdentity();
//       btDefaultMotionState* myMotionState = new btDefaultMotionState(startTransform);
//       btRigidBody::btRigidBodyConstructionInfo rbInfo(0, myMotionState, colShape);
//       btRigidBody* body = new btRigidBody(rbInfo);
//       dynamicsWorld->addRigidBody(body);
//     }



//   std::thread loopThread(threaded, dynamicsWorld);

//   // keeping this code so it will be easier for me to test ticking the bullet world.
//   //   ///-----stepsimulation_start-----
//   //   for (i = 0; i < 150; i++)
//   //     {
//   //       dynamicsWorld->stepSimulation(1.f / 60.f, 10);

//   //       //print positions of all objects
//   //       for (int j = dynamicsWorld->getNumCollisionObjects() - 1; j >= 0; j--)
//   // 	{
//   // 	  btCollisionObject* obj = dynamicsWorld->getCollisionObjectArray()[j];
//   // 	  btRigidBody* body = btRigidBody::upcast(obj);
//   // 	  btTransform trans;
//   // 	  if (body && body->getMotionState())
//   // 	    {
//   // 	      body->getMotionState()->getWorldTransform(trans);
//   // 	    }
//   // 	  else
//   // 	    {
//   // 	      trans = obj->getWorldTransform();
//   // 	    }
//   // 	  printf("world pos object %d = %f,%f,%f\n", j, float(trans.getOrigin().getX()), float(trans.getOrigin().getY()), float(trans.getOrigin().getZ()));
//   // 	}
//   // 	window.postRedisplay();
//   //     }

//   //   ///-----stepsimulation_end-----

//   // window.close();
//   loopThread.join();

//     //cleanup in the reverse order of creation/initialization

//     ///-----cleanup_start-----

//     //remove the rigidbodies from the dynamics world and delete them
//     for (i = dynamicsWorld->getNumCollisionObjects() - 1; i >= 0; i--)
//       {
//         btCollisionObject* obj = dynamicsWorld->getCollisionObjectArray()[i];
//         btRigidBody* body = btRigidBody::upcast(obj);
//         if (body && body->getMotionState())
//   	{
//   	  delete body->getMotionState();
//   	}
//         dynamicsWorld->removeCollisionObject(obj);
//         delete obj;
//       }

//     //delete collision shapes
//     for (int j = 0; j < collisionShapes.size(); j++)
//       {
//         btCollisionShape* shape = collisionShapes[j];
//         collisionShapes[j] = 0;
//         delete shape;
//       }

//     //delete dynamics world
//     delete dynamicsWorld;

//     //delete solver
//     delete solver;

//     //delete broadphase
//     delete overlappingPairCache;

//     //delete dispatcher
//     delete dispatcher;

//     delete collisionConfiguration;

//     //next line is optional: it will be cleared by the destructor when the array goes out of scope
//     collisionShapes.clear();

//   std::cout << "Goodbye" << std::endl;
//   return TRUE;
// }
