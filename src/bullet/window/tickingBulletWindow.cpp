#include "tickingBulletWindow.h"

TickingBulletWindow::TickingBulletWindow(const char* title, btDynamicsWorld *world) :
  BulletWindow(title, world),
  m_paused(true),
  m_tick_delay(1000 / 60),
  m_bullet_speed_multiplier(1)
{
}

void TickingBulletWindow::display() {
  BulletWindow::display();
  if(!m_paused) {
    start();
  }
}

void TickingBulletWindow::keyboardCallback(unsigned char key, int x, int y) {
  switch(key) {
  case 'p':
    if(m_paused) {
      start();
    } else {
      stop();
    }
    break;
  default:
    BulletWindow::keyboardCallback(key, x, y);
  }
}

void TickingBulletWindow::tick() {
  // make sure that there is no more motion even in the last tick after ticking has been paused
  if(!m_paused) {
    m_world->stepSimulation((m_tick_delay / 1000.) * m_bullet_speed_multiplier, 60);
    postRedisplay();
  }
}

void TickingBulletWindow::start() {
  int delay = m_tick_delay;
  m_paused = delay < 0;
  enableTick(delay);
}

void TickingBulletWindow::stop() {
  m_paused = true;
  disableTick();
}

void TickingBulletWindow::setTickDelay(int millis) {
  m_tick_delay = millis;
  start();
}

int TickingBulletWindow::getTickDelay() {
  return m_tick_delay;
}

void TickingBulletWindow::setBulletSpeedMultiplier(btScalar multiplier) {
  m_bullet_speed_multiplier = multiplier;
}

btScalar TickingBulletWindow::getBulletSpeedMultiplier() {
  return m_bullet_speed_multiplier;
}
