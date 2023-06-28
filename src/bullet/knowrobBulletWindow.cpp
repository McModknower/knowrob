#include "knowrobBulletWindow.h"
#include <btBulletDynamicsCommon.h>
#include <map>

btDynamicsWorld* create_default_world() {
    auto config = new btDefaultCollisionConfiguration();
	return
		new btDiscreteDynamicsWorld(
				new btCollisionDispatcher(config),
				new btDbvtBroadphase(),
				new btSequentialImpulseConstraintSolver(),
				config);
}

// TODO make sure the bullet windows are thread-safe
std::map<int, KnowrobBulletWindow*> allBulletWindows{};
int bulletWindowCounter = 0;

int next_id() {
	return bulletWindowCounter++;
}

KnowrobBulletWindow* KnowrobBulletWindow::findById(int id) {
	auto it = allBulletWindows.find(id);
	if(it == allBulletWindows.end()) {
		return nullptr;
	}
	return it->second;
}

KnowrobBulletWindow::KnowrobBulletWindow() :
	TickingBulletWindow("Bullet Visualization", create_default_world()),
	m_knowrob_id(next_id())
{
	allBulletWindows[m_knowrob_id] = this;
}

void deleteFullBulletWorld(btDynamicsWorld *world) {
	if(world == nullptr) {
		return;
	}
	delete world->getBroadphase();
	delete ((btCollisionDispatcher*)world->getDispatcher())->getCollisionConfiguration();
	delete world->getDispatcher();
	delete world->getConstraintSolver();
	delete world;
}

KnowrobBulletWindow::~KnowrobBulletWindow() {
	allBulletWindows.erase(m_knowrob_id);
	deleteFullBulletWorld(m_world);
}
