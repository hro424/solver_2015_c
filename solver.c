#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

typedef unsigned long ULONG;
typedef unsigned int UINT;
typedef uint64_t U64;
typedef uint32_t U32;
typedef uint16_t U16;
typedef uint8_t U8;

typedef struct block {
	UINT x;
	UINT y;
	UINT direction;
} block_t;

static const UINT HORIZONTAL = 0;
static const UINT VERTICAL = 1;
static ULONG _rand = 0;
static UINT _runtime = 0;
static UINT _timeout = 5;
static UINT _numbers = 100;
static UINT _height = 20;
static UINT _width = 20;
static UINT _score;
static UINT _x = 10;
static UINT _y = 10;

static UINT _field_len;
static U8 *_field;

static unsigned int
_random(void)
{
	const ULONG m = 1UL << 32;
	const ULONG a = 1103515245;
	const ULONG c = 12345;
	_rand = (a * _rand + c) % m;
	return (_rand >> 16) & 0x7FFF;
}

static void
initialize(int seed)
{
	_runtime = 0;

	if (seed) {
		_rand = seed;
		_timeout = _random() % 196 + 5;
		_numbers = _random() % 1901 + 100;
		_height = _random() % 81 + 20;
		_width = _random() % 181 + 20;
	}
	else {
		_timeout = 5;
		_numbers = 100;
		_height = 20;
		_width = 20;
	}
	_score = 0;
	_y = 10;
	_x = 10;
	_field_len = _height * _width;

	_field = malloc(_field_len);
	memset(_field, 0xFF, _field_len);
}

#define NG	0
#define OK	1

#define PROLOGUE(bp)					\
	block_t tmp;					\
	memcpy(&tmp, bp, sizeof(block_t));

#define EPILOGUE(bp)					\
	if (detect_collision(&tmp)) {			\
		memcpy(bp, &tmp, sizeof(block_t));	\
	}



static void
move(UI dir)
{
}

static void
move_down(block_t *b)
{
	PROLOGUE(b);
	b->y++;
	EPILOGUE(b);
}

static void
move_left(block_t *b)
{
	PROLOGUE(b);
	b->x--;
	EPILOGUE(b);
}

static void
move_right(block_t *b)
{
	b->x++;
}

static int
rotate(block_t *b)
{
	block_t tmp;
	memcpy(&tmp, b, sizeof(block_t));

	tmp.x++;
	tmp.y--;
	tmp.direction ^= VERTICAL;

	if (detect_collision(&tmp)) {
		memcpy(b, &tmp, sizeof(block_t));
		return OK;
	}
	else {
		return NG;
	}
}

static UINT
calc_score(ULONG n)
{
	UINT score = 0;

	while (n > 1UL) {
		n = n & 1UL ? (n << 1) + n + 1UL : n >> 1;
		score++;
	}

	return score;
}

int
main(int argc, char *argv[])
{
	_rand = 10;
	printf("%u\n", _random());
	initialize(10);
	printf("score %u\n", calc_score(21));
	printf("score %u\n", calc_score(3622589));
	printf("score %u\n", calc_score(92500143622589));

	return 0;
}
