#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>

typedef struct game {
	FILE *fp;
	uint16_t timeout;
	size_t numblocks;
	size_t height;
	size_t width;
} game_t;

static uint32_t Seed = 0;

static uint16_t
prand(void)
{
	uint64_t s = Seed;
	Seed = (uint32_t)((s * 1103515245ULL + 12345ULL) & 0xFFFFFFFFULL;
	return (Seed >> 16) & 0x7FFFU;
}

static int
init_game(const char *seed, const char *out, game_t *g)
{
	if (g == NULL) {
		return 0;
	}

	Seed = atoi(argv[1]);

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
}

static void
play(game_t *g, FILE *fp)
{
	uint32_t init;
	uint32_t hoffset;
	uint32_t voffset;

	if (g == NULL) {
		return;
	}

	if (g->width & 1) {
		init = 0;
	}
	else {
		init = 1;
	}

	hoffset = 1;
	voffset = 0;

	for (int i = 0; i < g->numblocks; ++i) {
		if (hoffset > g->width) {
			hoffset = init;
			continue;
		}

		// place it in vertical
		fputc('D', fp);
		fputc('C', fp);

		if (hoffset > g->width / 2) {
			for (int j = 0; j < hoffset - g->width / 2; ++j) {
				fputc('L', fp);
			}
		}
		else {
			for (int j = 0; j < g->width / 2 - hoffset; ++j) {
				fputc('R', fp);
			}
		}

		++hoffset;

		for (int j = 0; j < g->height - 2; ++j) {
			fputc('D', fp);
		}
	}
}

int
main(int argc, char *argv[])
{
	uint32_t s;
	game_t game;

	if (argc != 3) {
		return EXIT_FAILURE;
	}


	if (!init_game(argv[1], argv[2], &game)) {
		return EXIT_FAILURE;
	}

	play(&game);

	exit_game(&game);

	return EXIT_SUCCESS;
}
