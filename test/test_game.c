#include "../solver.c"
#include "unity.h"
#include "unity_fixture.h"

TEST_GROUP_RUNNER(Game)
{
	RUN_TEST_CASE(Game, MoveDown);
	RUN_TEST_CASE(Game, MoveLeft);
	RUN_TEST_CASE(Game, MoveRight);
	RUN_TEST_CASE(Game, Rotate);
}

TEST_GROUP(Game);

static field_t test_field;

TEST_SETUP(Game)
{
	initialize(&test_field, 10);
}

TEST_TEAR_DOWN(Game)
{
	finalize(&test_field);
}

TEST(Game, MoveDown)
{
	field_t f;
	block_t b;

	TEST_FAIL();
	move_down(&f, &b);
}

TEST(Game, MoveLeft)
{
	field_t f;
	block_t b;

	TEST_FAIL();
	move_left(&f, &b);
}

TEST(Game, MoveRight)
{
	field_t f;
	block_t b;

	TEST_FAIL();
	move_right(&f, &b);
}

TEST(Game, Rotate)
{
	field_t f;
	block_t b;

	TEST_FAIL();
	rotate(&f, &b);
}

