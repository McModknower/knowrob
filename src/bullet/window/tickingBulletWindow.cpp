#include "tickingBulletWindow.h"

#include <iostream>

TickingBulletWindow::TickingBulletWindow(const char* title, btDynamicsWorld *world) :
  BulletWindow(title, world),
  m_time_left(0),
  m_tick_delay(1000 / 60),
  m_bullet_speed_multiplier(1)
{
}

void TickingBulletWindow::display() {
  BulletWindow::display();
  // Always call the tick function, otherwise the window won't get updated if stuff if done off-thread
  enableTick(m_tick_delay);
}

void TickingBulletWindow::tick() {
	// only tick the world when there is still time left
	if(m_time_left > 0) {
		m_world->stepSimulation((m_tick_delay / 1000.) * m_bullet_speed_multiplier, 60);
		m_time_left -= m_bullet_speed_multiplier;
		postRedisplay();
	}
}

void TickingBulletWindow::setTickDelay(int millis) {
  m_tick_delay = millis;
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

void TickingBulletWindow::setTimeLeft(btScalar seconds) {
	m_time_left = seconds;
}

btScalar TickingBulletWindow::getTimeLeft() {
	return m_time_left;
}
