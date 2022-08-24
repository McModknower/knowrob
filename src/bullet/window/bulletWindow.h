#ifndef BULLET_WINDOW_H__
#define BULLET_WINDOW_H__

#include "settings.h"
#include "glut.h"

#include "camera.h"

#include <bullet/BulletDynamics/Dynamics/btDynamicsWorld.h>

class BulletWindow : public Window
{
 private:
  GLfloat m_aspectRatio;

  btVector3 m_light_position;

  GLfloat m_red = 1;
  GLfloat m_green = 1;
  GLfloat m_blue = 1;

 public:
  btDynamicsWorld *m_world;
  CameraMouseData m_cam;

  BulletWindow(const char* title, btDynamicsWorld *world);

  void display() override;
  void displayCallback() override;
  void reshapeCallback(int width, int height) override;
  void keyboardCallback(unsigned char key, int x, int y) override;
  void specialKeyCallback(int key, int x, int y) override;
  void mouseCallback(int button, int state, int x, int y) override;
  void motionCallback(int x, int y) override;
};

#endif
