/*
 * 00000011
 * 00000110
 * 00001001
 * 00001010
 * 00001111
 * 00010010
 */
#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>
#include <string.h>
#include <fcntl.h>
#include <unistd.h>

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
gennum(U32 out[], size_t count)
{
	for (size_t i = 0; i < count; ++i) {
		out[i] = _random();
		out[i] = (out[i] << 16) | _random();
		out[i] %= 100000000U;
	}
}

static int
pop(U32 x)
{
	x = (x & 0x55555555) + ((x >> 1) & 0x55555555);
	x = (x & 0x33333333) + ((x >> 2) & 0x33333333);
	x = (x & 0x0F0F0F0F) + ((x >> 4) & 0x0F0F0F0F);
	x = (x & 0x00FF00FF) + ((x >> 8) & 0x00FF00FF);
	x = (x & 0x0000FFFF) + ((x >> 16) & 0x0000FFFF);

	return x;
}

static int
mpop(U32 x[], size_t l)
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
misdividable3(U32 in[], size_t l)
{
	U32 tmp;

	for (size_t i = 0; i < l; ++i) {
		tmp += in[i];
	}

	return tmp % 3;
}

static int
mmul3(unsigned short inout[], size_t l)
{
	return 0;
}

static int
mscanf(FILE *fp, struct pair *p)
{
	for (;;) {
		c = fgetc(fp);
		if (c == ' ') {
			is_next = 1;
		}
		else if (c == '\n') {
			is_next = 0;
		}
		else {
		}
	}

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
