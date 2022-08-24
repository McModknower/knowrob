#include "camera.h"

#include <cmath>

#include "glut.h"

CameraMouseData::CameraMouseData(btScalar distance, Window &window) :
  m_motion_mode(MOTION_MODE_NOTHING),
  m_camera_center_distance(distance),
  m_camera_transform(btQuaternion(0, 0, 0, 1), btVector3(-3, 0, 0)),
  m_window(window)
{
  
}

void CameraMouseData::startMouseMove(int button, int state, int x, int y) {
  if(state == GLUT_UP) {
    m_motion_mode = MOTION_MODE_NOTHING;
  } else {

    btTransform other(btQuaternion(0, 0, 0, 1));
    switch(button) {
    case 3: // WHEEL_UP
      other.setOrigin(btVector3(0.10*m_camera_center_distance, 0.0, 0.0));
      m_camera_transform *= other;
      m_camera_center_distance *= 0.90;
      break;
    case 4: // WHEEL_DOWN
      other.setOrigin(btVector3((-0.10)*m_camera_center_distance, 0.0, 0.0));
      m_camera_transform *= other;
      m_camera_center_distance *= 1.10;
      break;
    }
    if(m_motion_mode == MOTION_MODE_NOTHING) {
      m_lx = x;
      m_ly = y;
      switch(button) {
      case GLUT_LEFT_BUTTON:
	m_motion_mode = MOTION_MODE_ROTATE; break;
      case GLUT_RIGHT_BUTTON:
	m_motion_mode = MOTION_MODE_TRANSLATE; break;
      }
    }
  }
  m_window.postRedisplay();
}

void CameraMouseData::updateMouseMove(int x, int y)
{
  if(m_motion_mode == MOTION_MODE_NOTHING) {
    return;
  }
  // TODO use width and height instead of 100
  btScalar dx = (x - m_lx) / (btScalar) m_window.m_width;
  btScalar dy = (y - m_ly) / (btScalar) m_window.m_height;
  m_lx = x;
  m_ly = y;

  if(m_motion_mode == MOTION_MODE_ROTATE) {
    btVector3 y_axis(0, 1, 0);
    btQuaternion cameraRotation = m_camera_transform.getRotation();
    y_axis = quatRotate(cameraRotation, y_axis);
    
    btVector3 camera_center(m_camera_center_distance, 0, 0);
    camera_center = m_camera_transform(camera_center);

    btTransform a(btQuaternion(0, 0, 0, 1), camera_center);

    m_camera_transform = a *
      btTransform(btQuaternion(btVector3(0, 0, 1), dx*M_PI)) *
      btTransform(btQuaternion(y_axis, dy*M_PI)) *
      a.inverse() *
      m_camera_transform;
    
  } else if(m_motion_mode == MOTION_MODE_TRANSLATE) {
    
    btTransform other(btQuaternion(0, 0, 0, 1), btVector3(0, dx * 10, dy * 10));
    
    btTransform result(btQuaternion(0, 0, 0, 1));
    result.mult(m_camera_transform, other);
    m_camera_transform = result;
  }
  m_window.postRedisplay();
}
