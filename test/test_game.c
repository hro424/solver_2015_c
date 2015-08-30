#define TEST
#include "../solver.c"
#include "unity.h"
#include "unity_fixture.h"

TEST_GROUP_RUNNER(Game)
{
	RUN_TEST_CASE(Game, DetectCollisionWall);
	RUN_TEST_CASE(Game, DetectCollisionGrid);
	RUN_TEST_CASE(Game, MoveDownHorizontal);
	RUN_TEST_CASE(Game, MoveDownVertical);
	RUN_TEST_CASE(Game, MoveLeftHorizontal);
	RUN_TEST_CASE(Game, MoveLeftVertical);
	RUN_TEST_CASE(Game, MoveRightHorizontal);
	RUN_TEST_CASE(Game, MoveRightVertical);
	RUN_TEST_CASE(Game, Rotate);
}

TEST_GROUP(Game);

static field_t test_field;

TEST_SETUP(Game)
{
	initialize(&test_field, 0);
}

TEST_TEAR_DOWN(Game)
{
	finalize(&test_field);
}

TEST(Game, DetectCollisionWall)
{
	int result;
	int dir;

	dir = HORIZONTAL;
	result = detect_collision(&test_field, 0, 0, dir);
	TEST_ASSERT_EQUAL(0, result);

	result = detect_collision(&test_field, test_field.width, 0, dir);
	TEST_ASSERT_EQUAL(1, result);

	result = detect_collision(&test_field, test_field.width - 1, 0, dir);
	TEST_ASSERT_EQUAL(1, result);

	result = detect_collision(&test_field, test_field.width - 2, 0, dir);
	TEST_ASSERT_EQUAL(1, result);

	result = detect_collision(&test_field, test_field.width - 3, 0, dir);
	TEST_ASSERT_EQUAL(0, result);

	result = detect_collision(&test_field, 0, test_field.height, dir);
	TEST_ASSERT_EQUAL(1, result);

	result = detect_collision(&test_field, 0, test_field.height - 1, dir);
	TEST_ASSERT_EQUAL(0, result);

	result = detect_collision(&test_field, -1, 0, dir);
	TEST_ASSERT_EQUAL(1, result);


	dir = VERTICAL;
	result = detect_collision(&test_field, 0, 0, dir);
	TEST_ASSERT_EQUAL(1, result);

	result = detect_collision(&test_field, test_field.width, 0, dir);
	TEST_ASSERT_EQUAL(1, result);

	result = detect_collision(&test_field, test_field.width - 1, 0, dir);
	TEST_ASSERT_EQUAL(1, result);

	result = detect_collision(&test_field, test_field.width - 2, 0, dir);
	TEST_ASSERT_EQUAL(1, result);

	result = detect_collision(&test_field, test_field.width - 3, 0, dir);
	TEST_ASSERT_EQUAL(1, result);

	result = detect_collision(&test_field, test_field.width - 3, 1, dir);
	TEST_ASSERT_EQUAL(1, result);

	result = detect_collision(&test_field, test_field.width - 3, 2, dir);
	TEST_ASSERT_EQUAL(0, result);

	result = detect_collision(&test_field, 0, test_field.height, dir);
	TEST_ASSERT_EQUAL(1, result);

	result = detect_collision(&test_field, 0, test_field.height - 1, dir);
	TEST_ASSERT_EQUAL(0, result);

	result = detect_collision(&test_field, -1, 2, dir);
	TEST_ASSERT_EQUAL(1, result);
}

TEST(Game, DetectCollisionGrid)
{
}

TEST(Game, MoveDownHorizontal)
{
	block_t b;
	int xx = test_field.width / 2 - 1;
	int yy = 0;

	b.x = xx;
	b.y = yy;
	b.direction = HORIZONTAL;

	move_down(&test_field, &b);

	TEST_ASSERT_EQUAL(xx, b.x);
	TEST_ASSERT_EQUAL(yy + 1, b.y);
}

TEST(Game, MoveDownVertical)
{
	TEST_FAIL();
}

TEST(Game, MoveLeftHorizontal)
{
	field_t f;
	block_t b;

	TEST_FAIL();
	move_left(&f, &b);
}

TEST(Game, MoveLeftVertical)
{
	TEST_FAIL();
}

TEST(Game, MoveRightHorizontal)
{
	field_t f;
	block_t b;

	TEST_FAIL();
	move_right(&f, &b);
}

TEST(Game, MoveRightVertical)
{
	TEST_FAIL();
}

TEST(Game, Rotate)
{
	field_t f;
	block_t b;

	b.x = 0;
	b.y = 0;
	b.direction = LEFT2RIGHT;

	TEST_FAIL();
	rotate(&f, &b);

}

