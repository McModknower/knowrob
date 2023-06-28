#ifndef KNOWROB_BULLET_WINDOW_H__
#define KNOWROB_BULLET_WINDOW_H__
/**
 * A KnowrobBulletWindow is a TickingBulletWindow that additionally stores extra
 * data and makes sure stuff gets deleted on close.
 */

#include "window/tickingBulletWindow.h"

class KnowrobBulletWindow : public TickingBulletWindow
{
 private:
	const int m_knowrob_id;
 public:
	KnowrobBulletWindow();
	~KnowrobBulletWindow() override;
	static KnowrobBulletWindow* findById(int id);
	int getKnowrobId() {
		return m_knowrob_id;
	}
};

#endif
