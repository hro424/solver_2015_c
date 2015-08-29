#include "../solver.c"
#include "unity.h"
#include "unity_fixture.h"

TEST_GROUP_RUNNER(Init)
{
	RUN_TEST_CASE(Init, Random);
	RUN_TEST_CASE(Init, InitializationWithZero);
	RUN_TEST_CASE(Init, InitializationWithNonZero);
	RUN_TEST_CASE(Init, CalcScore);
}

TEST_GROUP(Init);

TEST_SETUP(Init)
{
}

TEST_TEAR_DOWN(Init)
{
}

TEST(Init, Random)
{
	_rand = 10;
	TEST_ASSERT_EQUAL_INT(4543, _random());
}

TEST(Init, InitializationWithZero)
{
	field_t f;

	initialize(&f, 0);

	TEST_ASSERT_EQUAL_INT8(20, f.height);
	TEST_ASSERT_EQUAL_INT8(20, f.width);
	TEST_ASSERT_NOT_NULL(f.grid);

	for (int x = 0; x < f.width; ++x) {
		for (int y = 0; y < f.height; ++y) {
			TEST_ASSERT_EQUAL_INT8(0xF, f.grid[x][y]);
		}
	}

	finalize(&f);
}

TEST(Init, InitializationWithNonZero)
{
	field_t f;

	initialize(&f, 10);

	TEST_ASSERT_NOT_NULL(f.grid);

	for (int x = 0; x < f.width; ++x) {
		for (int y = 0; y < f.height; ++y) {
			TEST_ASSERT_EQUAL_INT8(0xF, f.grid[x][y]);
		}
	}

	finalize(&f);
}

TEST(Init, CalcScore)
{
	TEST_ASSERT_EQUAL_INT(7, calc_score(21));
	TEST_ASSERT_EQUAL_INT(234, calc_score(3622589));
	TEST_ASSERT_EQUAL_INT(455, calc_score(92500143622589));
}

