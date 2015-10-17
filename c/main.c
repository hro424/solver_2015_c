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

static void
move_down(block_t *b)
{
	assert(b->count < b->ops_len);
	b->ops[b->count++] = 'D';
}

static void
move_left(block_t *b)
{
	assert(b->count < b->ops_len);
	b->ops[b->count++] = 'L';
}

static void
move_right(block_t *b)
{
	assert(b->count < b->ops_len);
	b->ops[b->count++] = 'R';
}

static void
rotate(block_t *b)
{
	assert(b->count < b->ops_len);
	b->ops[b->count++] = 'C';
}

static int
init_game(const char *seed, const char *out, game_t *g)
{
	uint32_t s;

	if (g == NULL) {
		return 0;
	}

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
	if (g != NULL) {
		fclose(g->fp);
	}
}

static void
play(const game_t *g)
{
	size_t init;
	size_t hoffset;
	size_t voffset;

	if (g == NULL) {
		return;
	}

	if (g->width & 1) {
		init = 0;
	}
	else {
		init = 1;
	}

	hoffset = init;
	voffset = 0;

	for (size_t i = 0; i < g->numblocks; ++i) {
		if (hoffset > g->width) {
			hoffset = init;
			continue;
		}

		// place it in vertical
		fputc('D', g->fp);
		fputc('C', g->fp);

		if (hoffset > g->center) {
			for (size_t j = 0; j < hoffset - g->center; ++j) {
				fputc('L', g->fp);
			}
		}
		else {
			for (size_t j = 0; j < g->center - hoffset; ++j) {
				fputc('R', g->fp);
			}
		}

		++hoffset;

		for (size_t j = 0; j < g->height - 2; ++j) {
			fputc('D', g->fp);
		}
	}
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

	if (bs == NULL) {
		return;
	}

	for (size_t i = 0; i < count; ++i) {
		if (bs[i].ops != NULL) {
			free(bs[i].ops);
			bs[i].ops = NULL;
		}
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

	if (t != NULL) {
		free(t);
	}

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

	return (*l)->number - (*r)->number;
}

static void
sort(block_t *bs[], size_t count)
{
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

	sort(table, g->width);

	for (size_t i = 0; i < g->width; ++i) {
		block_t *bp = table[i];
		move_down(bp);
		rotate(bp);
		if (i < g->center) {
			for (size_t j = i; j < g->center; ++j) {
				move_left(bp);
			}
		}
		else {
			for (size_t j = g->center; j < i; ++j) {
				move_right(bp);
			}
		}

		for (size_t j = 0; j < g->height - 2; ++j) {
			move_down(bp);
		}
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

	sort(table, count);

	for (size_t i = 0; i < g->width / 3; ++i) {
		block_t *bp = table[i];
		size_t dest = i * 3 + 1;

		if (dest < g->center) {
			for (size_t j = dest; j < g->center; ++j) {
				move_left(bp);
			}
		}
		else {
			for (size_t j = g->center; j < dest; ++j) {
				move_right(bp);
			}
		}

		for (size_t j = 0; j < g->height; ++j) {
			move_down(bp);
		}
	}

	for (size_t i = g->width / 3; i < g->width % 3; ++i) {
		block_t *bp = table[i];
		move_down(bp);
		rotate(bp);
		if (i < g->center) {
			for (size_t j = i; j < g->center; ++j) {
				move_left(bp);
			}
		}
		else {
			for (size_t j = g->center; j < i; ++j) {
				move_right(bp);
			}
		}

		for (size_t j = 0; j < g->height - 2; ++j) {
			move_down(bp);
		}
	}

	for (size_t i = 0; i < g->width / 3; ++i) {
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
play2(const game_t *g)
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

	play2(&game);

	destroy_blocks(BlockBuf, BlockBufLen);
	destroy_table(TableBuf);

	exit_game(&game);

	return EXIT_SUCCESS;
}

