#ifndef CAMERA_H
#define CAMERA_H

#include "settings.h"
#include "window.h"

#include <bullet/LinearMath/btTransform.h>

#define MOTION_MODE_NOTHING 0
#define MOTION_MODE_ROTATE 1
#define MOTION_MODE_TRANSLATE 2

struct CameraMouseData{
public:
  Window &m_window;
  int m_motion_mode;
  int m_lx;
  int m_ly;

  btTransform m_camera_transform;

  btScalar m_camera_center_distance;

  CameraMouseData(btScalar distance, Window &window);
  void startMouseMove(int button, int state, int x, int y);
  void updateMouseMove(int x, int y);
};

#endif
