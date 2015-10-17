// usage: solver <seed> <file>
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

typedef struct block {
	uint16_t number;
	size_t count;
	char *ops;
	size_t ops_len;
} block_t;

#if defined(CONFIG_OPT)
typedef struct bignum {
	uint32_t *num;
	size_t count;
} bignum_t;

static const uint32_t BIGNUM_OOE = 1000000;
static const size_t BIGNUM_ORDER = 6;
#endif

static const size_t OPS_MARGIN = 8;

static uint32_t Seed;
static block_t *BlockBuf;
static size_t BlockBufLen;
static block_t **TableBuf;
static block_t **TmpTable;
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
	if (count == 0) {
		return;
	}

	assert(b->count + count < b->ops_len);
	memset(&b->ops[b->count], direction, count);
	b->count += count;
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
		bs[i].ops_len = g->width + g->height + OPS_MARGIN;
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
		TmpTable = create_table(count);
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

#if defined(CONFIG_OPT)

static void
bignum_dump(bignum_t *b)
{
	assert(b != NULL);
	printf("===\n");
	for (ssize_t i = b->count - 1; i >= 0; --i) {
		printf("%.6u ", b->num[i]);
	}
	printf("\n");
}

static bignum_t *
bignum_create(size_t order)
{
	bignum_t *ptr;

	ptr = malloc(sizeof(bignum_t));
	ptr->count = order / BIGNUM_ORDER + 1;
	ptr->num = malloc(sizeof(uint32_t) * ptr->count);
	memset(ptr->num, 0, sizeof(uint32_t) * ptr->count);

	return ptr;
}

static void
bignum_destroy(bignum_t *num)
{
	assert(num != NULL);
	assert(num->num != NULL);
	free(num->num);
	free(num);
}

static void
bignum_mul(bignum_t *lhs, uint16_t rhs)
{
	assert(lhs != NULL);

	uint32_t carry = 0;

	for (size_t i = 0; i < lhs->count; ++i) {
		uint32_t tmp = lhs->num[i] * rhs + carry;
		lhs->num[i] = tmp % BIGNUM_OOE;
		carry = tmp / BIGNUM_OOE;
	}
}

static void
bignum_add(bignum_t *lhs, uint16_t rhs)
{
	assert(lhs != NULL);

	uint32_t carry = 0;
	uint32_t tmp = lhs->num[0] + rhs;
	lhs->num[0] = tmp % BIGNUM_OOE;
	carry = tmp / BIGNUM_OOE;
	if (carry > 0) {
		for (size_t i = 1; i < lhs->count; ++i) {
			uint32_t tmp = lhs->num[i] + carry;
			lhs->num[i] = tmp % BIGNUM_OOE;
			carry = tmp / BIGNUM_OOE;
			if (carry == 0) {
				break;
			}
		}
	}
}

static void
bignum_div2(bignum_t *dst)
{
	assert(dst != NULL);

	uint32_t borrow = 0;

	for (ssize_t i = dst->count - 1; i >= 0; --i) {
		uint32_t tmp = (dst->num[i] + borrow) / 2;
		if (dst->num[i] & 1) {
			borrow = BIGNUM_OOE;
		}
		else {
			borrow = 0;
		}
		dst->num[i] = tmp;
	}
}

static int
bignum_isOne(bignum_t *num)
{
	assert(num != NULL);

	for (size_t i = 1; i < num->count; ++i) {
		if (num->num[i] != 0) {
			return 0;
		}
	}

	return num->num[0] == 1;
}

static uint32_t
bignum_calc_score(bignum_t *num)
{
	assert(num != NULL);

	uint32_t score = 0;
	while (!bignum_isOne(num)) {
		if (num->num[0] & 1) {
			bignum_mul(num, 3);
			bignum_add(num, 1);
		}
		else {
			bignum_div2(num);
		}
		++score;
	}
	return score;
}

static bignum_t *
convert_bignum(block_t * const bs[], size_t count)
{
	bignum_t *dst = bignum_create(count * 3);
	for (size_t i = 0; i < count; ++i) {
		bignum_mul(dst, 1000);
		bignum_add(dst, bs[i]->number);
	}

	return dst;
}

static int
isMultiplesOf3(block_t * const bs[], size_t count)
{
	uint32_t total = 0;

	for (size_t i = 0; i < count; ++i) {
		uint16_t n = bs[i]->number;
		uint16_t nn = n / 100 + n % 100 / 10 + n % 10;
		total += nn;
	}

	return total % 3 != 0;
}

static void
reverse(block_t *bs)
{
	uint16_t n = bs->number;
	rotate(bs, 2);
	bs->number = n / 100 + n % 100 / 10 * 10 + n % 10 * 100;
}

static void
exchange(block_t *bs[], size_t lhs, size_t rhs)
{
	block_t *tmp = bs[lhs];
	bs[lhs] = bs[rhs];
	bs[rhs] = tmp;
}

static void
save(block_t *bs[], size_t count)
{
	for (size_t i = 0; i < count; ++i) {
		TmpTable[i] = bs[i];
	}
}

static void
load(block_t *bs[], size_t count)
{
	for (size_t i = 0; i < count; ++i) {
		bs[i] = TmpTable[i];
	}
}

#endif // CONFIG_OPT

static void
sort_v(block_t *bs[], size_t count)
{
#if defined(CONFIG_OPT)
	uint32_t score1, score2;
	bignum_t *bnum;

	qsort(bs, count, sizeof(block_t *), compare);
	save(bs, count);

	bnum = convert_bignum(bs, count);
	score1 = bignum_calc_score(bnum);
	bignum_destroy(bnum);

	for (size_t i = 0; i < count; ++i) {
		uint16_t n = bs[i]->number;
		if (n / 100 < n % 10) {
			reverse(bs[i]);
		}
	}
#endif // CONFIG_OPT

	qsort(bs, count, sizeof(block_t*), compare);

#if defined(CONFIG_OPT)
	if (isMultiplesOf3(bs, count)) {
		uint16_t n;
		size_t offset = 2;
		while (((n = bs[count - 1]->number) & 1) == 0) {
			if ((n / 100) & 1) {
				rotate(bs[count - 1], 2);
				break;
			}
			else {
				exchange(bs, count - 1, count - offset);
				++offset;
			}
		}
	}

	bnum = convert_bignum(bs, count);
	score2 = bignum_calc_score(bnum);
	bignum_destroy(bnum);

	if (score1 > score2) {
		// reset
		load(bs, count);
		for (size_t i = 0; i < count; ++i) {
			bs[i]->count = 1;
		}
	}
#endif // CONFIG_OPT
}

static void
sort_h(block_t *bs[], size_t count)
{
#if defined (CONFIG_OPT)
	for (size_t i = 0; i < count; ++i) {
		uint16_t n = bs[i]->number;
		if (n / 100 < n % 10) {
			reverse(bs[i]);
		}
	}
#endif // CONFIG_OPT

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

	// for rotation in the sort
	for (size_t i = 0; i < g->width; ++i) {
		move_down(table[i], 1);
	}

	sort_v(table, g->width);

	for (size_t i = 0; i < g->width; ++i) {
		block_t *bp = table[i];
		rotate(bp, 1);
		if (i < g->center) {
			move_left(bp, g->center - i);
		}
		else if (i > g->center) {
			move_right(bp, i - g->center);
		}

		// final steps are calculated as follows:
		//	height	- 3 (length of a block)
		//		+ 1 (to finalize)
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

	// for rotation in the sort
	for (size_t i = 0; i < count; ++i) {
		move_down(table[i], 1);
	}

	sort_h(table, count);

	for (size_t i = 0; i < g->width / 3; ++i) {
		block_t *bp = table[i];
		size_t dest = i * 3 + 1;

		if (dest < g->center) {
			move_left(bp, g->center - dest);
		}
		else if (dest > g->center) {
			move_right(bp, dest - g->center);
		}

		// final steps are calculated as follows:
		//	height	- 1 (moved down before sort())
		//		- 1 (vertial length of a block)
		//		+ 1 (to finalize)
		move_down(bp, g->height - 1);
	}

	for (size_t i = 0; i < g->width % 3; ++i) {
		block_t *bp = table[g->width / 3 + i];
		size_t dest = g->width - g->width % 3 + i;

		rotate(bp, 1);
		move_right(bp, dest - g->center);

		// final steps are calculated as follows:
		//	height	- 3 (length of a block)
		//		+ 1 (to finalize)
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
	for (size_t n = 0;
	     n <= g->numblocks;
	     n += (g->width / 3 + g->width % 3)) {
		play_h(g);
	}
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

int
main(int argc, char *argv[])
{
	game_t game;

	if (argc != 3) {
		fprintf(stderr, "usage: solver <seed> <file>\n");
		return EXIT_FAILURE;
	}

	BlockBuf = NULL;
	TableBuf = NULL;

	if (!init_game(argv[1], argv[2], &game)) {
		return EXIT_FAILURE;
	}

	play(&game);

	destroy_blocks(BlockBuf, BlockBufLen);
	destroy_table(TableBuf);

	exit_game(&game);

	return EXIT_SUCCESS;
}

