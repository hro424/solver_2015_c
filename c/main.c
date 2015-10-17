#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>
#include <string.h>
#include <assert.h>

#if defined(DEBUG)
#define ENTER()		printf("--> %s\n", __func__)
#define EXIT()		printf("<-- %s\n", __func__)
#else
#define ENTER()
#define EXIT()
#endif

typedef struct game {
	FILE *fp;
	uint16_t timeout;
	size_t numblocks;
	size_t height;
	size_t width;
	size_t center;
} game_t;

typedef struct block_t {
	uint16_t number;
	uint16_t count;
	char *ops;
	size_t ops_len;
} block_t;

static uint32_t Seed = 0;
static block_t *BlockBuf;
static size_t BlockBufLen;
static block_t **TableBuf;
static size_t TableBufLen;

static uint16_t
prand(void)
{
	uint64_t s = Seed;
	Seed = (s * 1103515245 + 12345) & 0xFFFFFFFF;
	return (Seed >> 16) & 0x7FFF;
}

#define move_down(bp, count)	move(bp, 'D', count)
#define move_right(bp, count)	move(bp, 'R', count)
#define move_left(bp, count)	move(bp, 'L', count)
#define rotate(bp, count)	move(bp, 'C', count)

static void
move(block_t *b, char direction, size_t count)
{
	assert(b->count + count < b->ops_len);
	memset(&b->ops[b->count], direction, count);
	b->count += count;
}

static int
init_game(const char *seed, const char *out, game_t *g)
{
	uint32_t s;

	Seed = s = atoi(seed);

	g->fp = fopen(out, "w");
	if (g->fp == NULL) {
		return 0;
	}

	g->timeout = prand() % 196 + 5;
	g->numblocks = prand() % 1901 + 100;
	g->height = prand() % 81 + 20;
	g->width = prand() % 181 + 20;

	if (s == 0) {
		g->timeout = 5;
		g->numblocks = 100;
		g->height = 20;
		g->width = 20;
	}

	g->center = g->width / 2;

	return 1;
}

static void
exit_game(game_t *g)
{
	fclose(g->fp);
}

static block_t *
create_blocks(size_t count, const game_t *g)
{
	ENTER();

	block_t *bs;
	bs = malloc(sizeof(block_t) * count);
	if (bs == NULL) {
		fprintf(stderr, "out of memory @ %s\n", __func__);
		exit(1);
	}

	for (size_t i = 0; i < count; ++i) {
		bs[i].ops_len = g->width + g->height + 4;
		bs[i].ops = malloc(bs[i].ops_len);
		if (bs[i].ops == NULL) {
			fprintf(stderr, "out of memmory @ %s\n", __func__);
			exit(1);
		}
	}

	EXIT();
	return bs;
}

static void
destroy_blocks(block_t *bs, size_t count)
{
	ENTER();
	assert(bs != NULL);

	for (size_t i = 0; i < count; ++i) {
		free(bs[i].ops);
	}

	free(bs);
	EXIT();
}

static block_t *
get_block_buffer(size_t count, const game_t *g)
{
	ENTER();

	block_t *bb;

	if (BlockBuf != NULL && BlockBufLen < count) {
		destroy_blocks(BlockBuf, BlockBufLen);
		BlockBuf = NULL;
	}

	if (BlockBuf == NULL) {
		BlockBuf = create_blocks(count, g);
		BlockBufLen = count;
	}

	bb = BlockBuf;

	for (size_t i = 0; i < count; ++i) {
		bb[i].number = prand() % 1000;
		bb[i].count = 0;
		memset(bb[i].ops, 0, bb[i].ops_len);
	}

	EXIT();
	return bb;
}

static block_t **
create_table(size_t count)
{
	ENTER();

	block_t **table;

	table = malloc(sizeof(block_t*) * count);
	if (table == NULL) {
		fprintf(stderr, "out of memory @ %s\n", __func__);
		exit(1);
	}

	EXIT();
	return table;
}

static void
destroy_table(block_t **t)
{
	ENTER();
	assert(t != NULL);
	free(t);
	EXIT();
}

static block_t **
get_table_buffer(block_t *bs, size_t count)
{
	ENTER();

	if (TableBuf != NULL && TableBufLen < count) {
		destroy_table(TableBuf);
		TableBuf = NULL;
	}

	if (TableBuf == NULL) {
		TableBuf = create_table(count);
		TableBufLen = count;
	}

	for (size_t i = 0; i < count; ++i) {
		TableBuf[i] = &bs[i];
	}

	EXIT();
	return TableBuf;
}

static int
compare(const void *lhs, const void *rhs)
{
	const block_t **l = (const block_t **)lhs;
	const block_t **r = (const block_t **)rhs;

	// reverse order
	return (*r)->number - (*l)->number;
}

static void
sort(block_t *bs[], size_t count)
{
	for (size_t i = 0; i < count; ++i) {
		uint16_t n = bs[i]->number;
		if (n / 100 < n % 10) {
			rotate(bs[i], 2);
			bs[i]->number = n / 100 +
				n % 100 / 10 * 10 +
				n % 10 * 100;
		}
	}
	qsort(bs, count, sizeof(block_t*), compare);
}

static void
play_v(const game_t *g)
{
	ENTER();

	block_t *bs;
	block_t **table;

	bs = get_block_buffer(g->width, g);
	table = get_table_buffer(bs, g->width);

	for (size_t i = 0; i < g->width; ++i) {
		move_down(table[i], 1);
	}

	sort(table, g->width);

	for (size_t i = 0; i < g->width; ++i) {
		block_t *bp = table[i];
		rotate(bp, 1);
		if (i < g->center) {
			move_left(bp, g->center - i);
		}
		else {
			move_right(bp, i - g->center);
		}

		// move down the height - 2 because it has already been
		// moved down before sort().
		move_down(bp, g->height - 2);
	}

	for (size_t i = 0; i < g->width; ++i) {
		fputs(bs[i].ops, g->fp);
	}

	EXIT();
}

static void
play_h(const game_t *g)
{
	ENTER();

	block_t *bs;
	block_t **table;
	size_t count = g->width / 3 + g->width % 3;

	bs = get_block_buffer(count, g);
	table = get_table_buffer(bs, count);

	for (size_t i = 0; i < count; ++i) {
		move_down(table[i], 1);
	}

	sort(table, count);

	for (size_t i = 0; i < g->width / 3; ++i) {
		block_t *bp = table[i];
		size_t dest = i * 3 + 1;

		if (dest < g->center) {
			move_left(bp, g->center - dest);
		}
		else {
			move_right(bp, dest - g->center);
		}

		// move down the height because it has already been
		// moved down before sort().
		move_down(bp, g->height - 1);
	}

	for (size_t i = g->width / 3; i < g->width % 3; ++i) {
		block_t *bp = table[i];
		rotate(bp, 1);
		if (i < g->center) {
			move_left(bp, g->center - i);
		}
		else {
			move_right(bp, i - g->center);
		}

		// move down the height because it has already been
		// moved down before sort().
		move_down(bp, g->height - 2);
	}

	for (size_t i = 0; i < count; ++i) {
		fputs(bs[i].ops, g->fp);
	}

	EXIT();
}

static void
play_horizontal(const game_t *g)
{
	play_h(g);
}

static void
play_vertical(const game_t *g)
{
	size_t n;
	for (n = 0; n < g->numblocks; n += g->width) {
		play_v(g);
	}

	if (n == g->numblocks) {
		play_v(g);
	}
	else {
		play_h(g);
	}
}

static void
play(const game_t *g)
{
	if (g->numblocks < g->width) {
		play_horizontal(g);
	}
	else {
		play_vertical(g);
	}
}

int
main(int argc, char *argv[])
{
	game_t game;

	if (argc != 3) {
		return EXIT_FAILURE;
	}

	BlockBuf = NULL;
	BlockBufLen = 0;
	TableBuf = NULL;
	TableBufLen = 0;

	if (!init_game(argv[1], argv[2], &game)) {
		return EXIT_FAILURE;
	}

	play(&game);

	destroy_blocks(BlockBuf, BlockBufLen);
	destroy_table(TableBuf);

	exit_game(&game);

	return EXIT_SUCCESS;
}

