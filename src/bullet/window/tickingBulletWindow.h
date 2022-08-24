#ifndef TICKING_BULLET_WINDOW_H__
#define TICKING_BULLET_WINDOW_H__
/**
 * A TickingBulletWindow is a BulletWindow that additionally allows the user to start/stop ticking of the world via the 'P' key.
 * In the future this might be updated to include controls for setting the speed.
 */

#include "bulletWindow.h"

class TickingBulletWindow : public BulletWindow
{
 private:
  bool m_paused;
  // Stores the tick delay while playback is paused. 
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

  void display() override;

  void keyboardCallback(unsigned char key, int x, int y) override;
  void tick() override;

  void start();
  void stop();
};

#endif
