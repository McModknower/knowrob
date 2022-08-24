#include "settings.h"

#include "glut.h"

#include "bulletWindow.h"
#include "bulletRenderer.h"

// for debugging
#include <iostream>

BulletWindow::BulletWindow(const char* title, btDynamicsWorld *world) :
  Window(title),
  m_cam(3.0,*this),
  m_light_position(0, 0, 5),
  m_world(world),
  m_aspectRatio(1)
{
}

void BulletWindow::display() {
  glutInitDisplayMode(GLUT_DEPTH | GLUT_DOUBLE | GLUT_RGBA);
  Window::display();
  m_aspectRatio = m_width / (GLfloat) m_height;
  // (gl:enable :light0 :lighting :color-material :blend)
  glEnable(GL_DEPTH_TEST);
  glEnable(GL_CULL_FACE);
  glEnable(GL_RESCALE_NORMAL);
  glEnable(GL_LIGHT0);
  glEnable(GL_LIGHTING);
  glEnable(GL_COLOR_MATERIAL);
  glEnable(GL_BLEND);
  // (gl:cull-face :back)
  glCullFace(GL_BACK);
  // (gl:depth-func :lequal)
  glDepthFunc(GL_LEQUAL);
  // (gl:shade-model :smooth)
  glShadeModel(GL_SMOOTH);
  // (gl:blend-func :src-alpha :one-minus-src-alpha)
  glBlendFunc(GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA);
  // (gl:hint :perspective-correction-hint :nicest)))
  glHint(GL_PERSPECTIVE_CORRECTION_HINT, GL_NICEST);
}

void BulletWindow::reshapeCallback(int w, int h) {
  // Prevent a divide by zero, when window is too short
  // (you cant make a window of zero width).
  if(h == 0)
    h = 1;
  m_aspectRatio = 1.0* w / h;
  glViewport(0,0,w,h);
}

void BulletWindow::mouseCallback(int button, int state, int x, int y) {
  m_cam.startMouseMove(button, state, x, y);
}

void BulletWindow::motionCallback(int x, int y) {
  m_cam.updateMouseMove(x, y);
}

void BulletWindow::keyboardCallback(unsigned char key, int x, int y) {
  // 0x1B is escape
  switch(key) {
  case 0x1B:
    close(); break;
  }
}

void BulletWindow::specialKeyCallback(int key, int x, int y) {

  switch(key) {
  case GLUT_KEY_F1 :
    m_red = m_red < 0.5 ? 1.0 : 0.0;
    break;
  case GLUT_KEY_F2 :
    m_green = m_green < 0.5 ? 1.0 : 0.0;
    break;
  case GLUT_KEY_F3 :
    m_blue = m_blue < 0.5 ? 1.0 : 0.0;
    break;
  }
  postRedisplay();
}


void BulletWindow::displayCallback() {
  glMatrixMode(GL_PROJECTION);
  // Reset transformations
  glLoadIdentity();
  gluPerspective(50, m_aspectRatio, 0.1, 1000);
  glMatrixMode(GL_MODELVIEW);
  glLoadIdentity();
  // Rotate to use ROS standard Frame of Reference of Z UP
  glRotated(90, 1, 0, 0);
  glRotated(-90, 0, 0, 1);
  glRotated(180, 1, 0, 0);

  // Clear Color and Depth Buffers
  glClear(GL_COLOR_BUFFER_BIT | GL_DEPTH_BUFFER_BIT);

  { // Rotate to the perspective defined by the camera
    btScalar camMatrix[16];
    m_cam.m_camera_transform.inverse().getOpenGLMatrix(camMatrix);
    glMultMatrixf(camMatrix);
  }

  // Setup the light
  {
    // (gl:light :light0 :position (vector
    //                                (cl-transforms:x (light-position window))
    //                                (cl-transforms:y (light-position window))
    //                                (cl-transforms:z (light-position window))
    //                                0))
    GLfloat pos[4];
    pos[0] = m_light_position.getX();
    pos[1] = m_light_position.getY();
    pos[2] = m_light_position.getZ();
    pos[3] = 0;
    glLightfv(GL_LIGHT0, GL_POSITION, pos);
  }{
    //   (gl:light-model :light-model-ambient #(0.5 0.5 0.5 1.0))
    GLfloat data[] = {0.5, 0.5, 0.5, 1};
    glLightfv(GL_LIGHT0, GL_AMBIENT, data);
  }{
    //   (gl:light :light0 :diffuse #(0.8 0.8 0.8 1))
    GLfloat data[] = {0.8, 0.8, 0.8, 1};
    glLightfv(GL_LIGHT0, GL_DIFFUSE, data);
    //   (gl:light :light0 :specular #(0.8 0.8 0.8 1))
    glLightfv(GL_LIGHT0, GL_SPECULAR, data);
  }


  // some objects for testing
  glColor3f(m_red,m_green,m_blue);
  glBegin(GL_TRIANGLES);
  glVertex3f(-2.0f,-2.0f, 0.0f);
  glVertex3f( 2.0f, 0.0f, 0.0f);
  glVertex3f( 0.0f, 2.0f, 0.0f);
  glEnd();

  glBegin(GL_LINE_LOOP);
  glVertex3f(-1.0f,-1.0f,-1.0f);
  glVertex3f(-1.0f,-1.0f, 1.0f);
  glVertex3f(-1.0f, 1.0f, 1.0f);
  glVertex3f(-1.0f, 1.0f,-1.0f);
  glEnd();

  
  glBegin(GL_LINE_LOOP);
  glVertex3f(1.0f,-1.0f,-1.0f);
  glVertex3f(1.0f,-1.0f, 1.0f);
  glVertex3f(1.0f, 1.0f, 1.0f);
  glVertex3f(1.0f, 1.0f,-1.0f);
  glEnd();

  glColor3f(1-m_red,1-m_green,m_blue);

  glBegin(GL_POINTS);
  glVertex3f(0.0f,0.1f,0.0f);
  glEnd();

  // // Draw ground
  // glColor3f(0.9f, 0.9f, 0.9f);
  // glBegin(GL_QUADS);
  // glVertex3f(-100.0f, -100.0f, 0.0f);
  // glVertex3f(-100.0f,  100.0f, 0.0f);
  // glVertex3f( 100.0f,  100.0f, 0.0f);
  // glVertex3f( 100.0f, -100.0f, 0.0f);
  // glEnd();

  render(m_world->getCollisionObjectArray());
  
  //glutSolidTeapot(1);

  if(m_cam.m_motion_mode != MOTION_MODE_NOTHING) {
    // When we are moving around, draw a little yellow disk similar to
    // that one RVIZ draws.
    btVector3 disk_pos(m_cam.m_camera_center_distance, 0, 0);
    disk_pos = m_cam.m_camera_transform(disk_pos);
    glPushMatrix();
    {
      glTranslated(disk_pos.getX() ,disk_pos.getY() , disk_pos.getZ());
      glColor3f(0.8f, 0.8f, 0.0f);
      glScalef(1.0f, 1.0f, 0.1f);
      glutSolidSphere(0.1, 50, 50);
    }
    glPopMatrix();
  }
  
  glFlush();
  
  glutSwapBuffers();
}
