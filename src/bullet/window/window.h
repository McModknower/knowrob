#ifndef WINDOW_H__
#define WINDOW_H__

class Window
{
private:
  static void staticDisplayCallback();
  static void staticReshapeCallback(int width, int height);
  static void staticKeyboardCallback(unsigned char key, int x, int y);
  static void staticSpecialKeyCallback(int key, int x, int y);
  static void staticMouseCallback(int button, int state, int x, int y);
  static void staticMotionCallback(int x, int y);
  static void staticTick(int windowID);
  static void staticCloseCallback();
  // at what millis the next tick should be done.
  unsigned int m_next_tick_at;
  int m_tick_delay;
  const char *m_title;
public:
  int m_window_id;
  int m_height;
  int m_width;

  Window(const char* title);

  /**
   * Create the glut window, init m_window_id, m_height, m_width, and register callbacks.
   * Subclasses that overwrite this method need to call this for the window to be actually displayed
   */
  virtual void display();

  virtual void displayCallback() {}
  virtual void reshapeCallback(int width, int height) {}
  virtual void keyboardCallback(unsigned char key, int x, int y) {}
  virtual void specialKeyCallback(int key, int x, int y) {}
  virtual void mouseCallback(int button, int state, int x, int y) {}
  virtual void motionCallback(int x, int y) {}
  virtual void closeCallback() {}
  virtual void tick() {}

  void postRedisplay();
  /**
   * Enables ticking and starts the timer.
   * If enableTick is called while ticking is enabled, the new tick delay will be used starting after the next tick after the method gets called.
   */
  void enableTick(unsigned int tick_ms);
  /**
   * Disables ticking. Internally sets the tick delay to -1.
   */
  void disableTick();
  void close();
private:
  void internalClose();

public:
  bool isDisplayed();

  virtual ~Window();
};

#endif
