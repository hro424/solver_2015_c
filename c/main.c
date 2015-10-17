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
	Seed = (s * 1103515245 + 12345) & 0xFFFFFFFF;
	return (Seed >> 16) & 0x7FFF;
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
play(game_t *g)
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

		if (hoffset > g->width / 2) {
			for (size_t j = 0; j < hoffset - g->width / 2; ++j) {
				fputc('L', g->fp);
			}
		}
		else {
			for (size_t j = 0; j < g->width / 2 - hoffset; ++j) {
				fputc('R', g->fp);
			}
		}

		++hoffset;

		for (size_t j = 0; j < g->height - 2; ++j) {
			fputc('D', g->fp);
		}
	}
}

int
main(int argc, char *argv[])
{
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

