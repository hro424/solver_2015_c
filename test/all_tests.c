#include "unity_fixture.h"

static void
RunAllTests(void)
{
	RUN_TEST_GROUP(Init);
	RUN_TEST_GROUP(Game);
}

int
main(int argc, const char* argv[])
{
	return UnityMain(argc, argv, RunAllTests);
}
