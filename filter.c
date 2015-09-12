#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>
#include <fcntl.h>
#include <unistd.h>

static int
pop(unsigned int x)
{
	x = (x & 0x55555555) + ((x >> 1) & 0x55555555);
	x = (x & 0x33333333) + ((x >> 2) & 0x33333333);
	x = (x & 0x0F0F0F0F) + ((x >> 4) & 0x0F0F0F0F);
	x = (x & 0x00FF00FF) + ((x >> 8) & 0x00FF00FF);
	x = (x & 0x0000FFFF) + ((x >> 16) & 0x0000FFFF);

	return x;
}

static int
mpop(unsigned short x[], size_t l)
{
	int m = 0;

	if (l & 1) {
		m = pop(x[0]);
		--l;
	}

	for (int i = 0; i < l; i += 2) {
		m += pop((x[i] << 16) | x[i + 1]);
	}

	return m;
}

static int
mdiv3(unsigned short in[], size_t l)
{
	return 0;
}

static int
mmul3(unsigned short inout[], size_t l)
{
	return 0;
}

static int
mscanf(FILE *fp, struct pair *p)
{
	return 0;
}

int
main(int argc, char* argv[])
{
	int ret;
	unsigned int step;
	unsigned int number;
	FILE *fp;

	fp = fopen(argv[1], "r");
	if (fp == NULL) {
		perror("fopen");
		return EXIT_FAILURE;
	}

	while ((ret = fscanf(fp, "%d%d", &step, &number)) != EOF) {
		if (ret == 2) {
			if (pop(number) == 1) {
				continue;
			}
			else if (number % 3 == 0 && (number & 1) == 0) {
				continue;
			}
			printf("%u %u\n", step, number);
		}
	}

	fclose(fp);

	return EXIT_SUCCESS;
}
