#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

typedef unsigned long	ULONG;
typedef unsigned int	UINT;
typedef uint64_t	U64;
typedef uint32_t	U32;
typedef uint16_t	U16;
typedef uint8_t		U8;
typedef int64_t		S64;
typedef int32_t		S32;
typedef int16_t		S16;
typedef int8_t		S8;

typedef struct block {
	S16 x;
	S16 y;
	U8 direction;
	U16 number;
} block_t;

typedef struct field {
	U8 timeout;
	U16 nblocks;
	U8 height;
	U8 width;
	U8 **grid;
} field_t;

static const U8 HORIZONTAL = 0;
static const U8 VERTICAL = 1;
static const U8 MASK_DIR = VERTICAL;
static const U8 LEFT2RIGHT = HORIZONTAL | 0x0;
static const U8 RIGHT2LEFT = HORIZONTAL | 0x2;
static const U8 TOP2BOTTOM = VERTICAL | 0x0;
static const U8 BOTTOM2TOP = VERTICAL | 0x2;

static ULONG _rand = 0;
static UINT _runtime = 0;
static UINT _score;

static U16
_random(void)
{
	const ULONG m = 1UL << 32;
	const ULONG a = 1103515245UL;
	const ULONG c = 12345UL;
	_rand = (a * _rand + c) % m;
	return (_rand >> 16) & 0x7FFFUL;
}

static void
initialize(field_t *fp, int seed)
{
	U8* ptr;

	_runtime = 0;
	_rand = seed;
	if (_rand) {
		fp->timeout = _random() % 196 + 5;
		fp->nblocks = _random() % 1901 + 100;
		fp->height = _random() % 81 + 20;
		fp->width = _random() % 181 + 20;
	}
	else {
		fp->timeout = 5;
		fp->nblocks = 100;
		fp->height = 20;
		fp->width = 20;
	}

	_score = 0;

	fp->grid = malloc(sizeof(U8*) * fp->width);
	ptr = malloc(sizeof(U8) * fp->width * fp->height);
	for (int i = 0; i < fp->width; ++i) {
		fp->grid[i] = &ptr[i];
		for (int j = 0; j < fp->height; ++j) {
			fp->grid[i][j] = 0xF;
		}
	}
}

static void
finalize(field_t *fp)
{
	free(fp->grid[0]);
	free(fp->grid);
}

static int
detect_collision(field_t *fp, int x, int y, int c)
{
	if (c & MASK_DIR) {	// vertical
		if (x >= 0 &&
		    x < fp->width &&
		    y - 2 >= 0 &&
		    y < fp->height &&
		    fp->grid[x][y] > 9 &&
		    fp->grid[x][y - 1] > 9 &&
		    fp->grid[x][y - 2] > 9) {
			return 0;
		}
	}
	else {
		if (x >= 0 &&
		    x + 2 < fp->width &&
		    y >= 0 &&
		    y < fp->height &&
		    fp->grid[x][y] > 9 &&
		    fp->grid[x + 1][y] > 9 &&
		    fp->grid[x + 2][y] > 9) {
			return 0;
		}
	}
	return 1;
}

static int
move_down(field_t *fp, block_t *bp)
{
	int yy = bp->y + 1;
	if (!detect_collision(fp, bp->x, yy, bp->direction)) {
		bp->y = yy;
		return 1;
	}
	return 0;
}

static int
move_left(field_t *fp, block_t *bp)
{
	int xx = bp->x - 1;
	if (!detect_collision(fp, xx, bp->y, bp->direction)) {
		bp->x = xx;
		return 1;
	}
	return 0;
}

static int
move_right(field_t *fp, block_t *bp)
{
	int xx = bp->x + 1;
	if (!detect_collision(fp, xx, bp->y, bp->direction)) {
		bp->x = xx;
		return 1;
	}
	return 0;
}

static int
rotate(field_t *fp, block_t *bp)
{
	int xx, yy, cc;

	if (bp->direction & MASK_DIR) { // vertical to horizontal
		xx = bp->x - 1;
		yy = bp->y - 1;
	}
	else {
		xx = bp->x + 1;
		yy = bp->y + 1;
	}
	cc = (bp->direction + 1) & 0x3;

	if (!detect_collision(fp, xx, yy, cc)) {
		bp->x = xx;
		bp->y = yy;
		bp->direction = cc;
		return 1;
	}
	return 0;
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

#ifndef TEST
int
main(int argc, char *argv[])
{
	_rand = 10;
	printf("%u\n", _random());
	//initialize(10);
	printf("score %u\n", calc_score(21));
	printf("score %u\n", calc_score(3622589));
	printf("score %u\n", calc_score(92500143622589));

	for (ULONG i = 0; i < 0xFFFFFFFFFFFFFFFF; i++) {
		printf("%u: %lu\n", i, calc_score(i));
	}

	return 0;
}
#endif
