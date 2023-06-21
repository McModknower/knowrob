#ifndef TICKING_BULLET_WINDOW_H__
#define TICKING_BULLET_WINDOW_H__
/**
 * A TickingBulletWindow is a BulletWindow that additionally allows the program to tick the simulation over some set time.
 */

#include "bulletWindow.h"

class TickingBulletWindow : public BulletWindow
{
 private:
  btScalar m_time_left;
  int m_tick_delay;
  btScalar m_bullet_speed_multiplier;
 public:
  TickingBulletWindow(const char* title, btDynamicsWorld *world);

  void setTickDelay(int millis);
  int getTickDelay();

  /**
   * A Multiplier for how much faster than real time the bullet world should tick.
   * The maximum value depends on the tick delay as follows:
   * (tickDelay / 1000 ) * multiplier <= 1
   * If you want to have more that 1 second per frame, change the second argument from stepSimulation to a higher number.
   * WARNING: Setting this to very high values may result in no visual updates when one tick takes more milliseconds that the frameDelay. This is just an untested assumtion.
   * Also setting this to negative values results in undefined behaviour.
   */
  void setBulletSpeedMultiplier(btScalar multiplier);
  btScalar getBulletSpeedMultiplier();

  /**
   * How much simulation time the simulation should run for.
   * every tick, the time left is reduced by the time simulated until the time left is <= 0.
   * As soon as this is set, the simulation starts on the next window tick. (aka when setting this, the simulation starts.
   */
  void setTimeLeft(btScalar seconds);
  btScalar getTimeLeft();

  void display() override;

  void tick() override;
};

#endif