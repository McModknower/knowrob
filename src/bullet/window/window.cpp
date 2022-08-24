#include "window.h"

#include "glut.h"

#include <map>
#include <chrono>

std::map<int,Window*> activeWindows;

inline unsigned int currentMillis() {
  return std::chrono::duration_cast<std::chrono::milliseconds>
    // use the steady_clock, since we don't want time jumps and don't care about the actual time, just the duration between ticks
    (std::chrono::steady_clock::now().time_since_epoch())
    .count();
}

Window::Window(const char* title)
  : m_window_id(0),
    m_tick_delay(-1)
{}

Window::~Window(){
  if(m_window_id != 0) {
    activeWindows.erase(m_window_id);
  }
}

void Window::display(void)
{
  m_window_id = glutCreateWindow("Lighthouse3D- GLUT Tutorial");
  m_height = glutGet(GLUT_WINDOW_HEIGHT);
  m_width = glutGet(GLUT_WINDOW_WIDTH);
  activeWindows[m_window_id] = this;

  
  glutDisplayFunc(&Window::staticDisplayCallback);
  glutReshapeFunc(&Window::staticReshapeCallback);
  
  glutKeyboardFunc(&Window::staticKeyboardCallback);
  glutSpecialFunc(&Window::staticSpecialKeyCallback);

  glutMouseFunc(&Window::staticMouseCallback);
  glutMotionFunc(&Window::staticMotionCallback);
  glutCloseFunc(&Window::staticCloseCallback);
}

void Window::postRedisplay() {
  glutPostWindowRedisplay(m_window_id);
}

void Window::enableTick(unsigned int tick_ms) {
  auto old = m_tick_delay;
  m_tick_delay = tick_ms;
  if(old < 0) {
    m_next_tick_at = currentMillis() + tick_ms;
    glutTimerFunc(tick_ms, Window::staticTick, m_window_id);
  }
}
void Window::disableTick() {
  m_tick_delay = -1;
}

void Window::close() {
  glutDestroyWindow(m_window_id);
}

bool Window::isDisplayed() {
  return m_window_id != 0;
}

// static callbacks, since c++ doesn't like to use instance methods as callbacks for c functions.
void Window::staticDisplayCallback() {
  activeWindows[glutGetWindow()]->displayCallback();
}

void Window::staticReshapeCallback(int width, int height) {
  Window *w = activeWindows[glutGetWindow()];
  w->m_width = width;
  w->m_height = height;
  activeWindows[glutGetWindow()]->reshapeCallback(width, height);
}

void Window::staticKeyboardCallback(unsigned char key, int x, int y) {
  activeWindows[glutGetWindow()]->keyboardCallback(key, x, y);
}

void Window::staticSpecialKeyCallback(int key, int x, int y) {
  activeWindows[glutGetWindow()]->specialKeyCallback(key, x, y);
}

void Window::staticMouseCallback(int button, int state, int x, int y) {
  activeWindows[glutGetWindow()]->mouseCallback(button, state, x, y);
}

void Window::staticMotionCallback(int x, int y) {
  activeWindows[glutGetWindow()]->motionCallback(x, y);
}

void Window::staticCloseCallback() {
  int winID = glutGetWindow();
  Window *w = activeWindows[winID];
  w->closeCallback();
  activeWindows.erase(winID);
  w->m_window_id = 0;
  w->m_tick_delay = -1;
}


void Window::staticTick(int windowID) {
  Window *w = activeWindows[windowID];
  w->tick();

  // When ticking is disabled, don't reschedule
  if(w->m_tick_delay < 0)
    return;
  // rescedule in `(last + diff) - now` millis
  w->m_next_tick_at += w->m_tick_delay;
  unsigned int now = currentMillis();
  unsigned int when = w->m_next_tick_at;
  unsigned int scheduleTime;
  if(when <= now) {
    scheduleTime = 0;
  } else {
    scheduleTime = when - now;
  }
  glutTimerFunc(scheduleTime, Window::staticTick, windowID);
}
