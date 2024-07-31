#include <gtest/gtest.h>
#include <knowrob/knowrob.h>

int main(int argc, char **argv)
{
	knowrob::InitKnowRob(argc, argv);
	testing::InitGoogleTest(&argc, argv);
	auto status = RUN_ALL_TESTS();
	knowrob::ShutdownKnowRob();
	return status;
}
