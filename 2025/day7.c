/*
 * Advent of Code 2025, Day 7 (afterwards)
 * Sunday, December 07, 2025
 *
 * cc -Wall -O2 -o day7-after day7.c
 */

#include <stdio.h>

// lines are 142 chars long; the first and last characters are never '^'
#define ROW_SIZE 200

int main (int argc, char **argv) {
  char line[ROW_SIZE];
  int splits = 0;
  unsigned long beams[ROW_SIZE];
  for (int i=0; i<ROW_SIZE; i++)
    beams[i] = 0;

  while (fgets(line, ROW_SIZE, stdin) != NULL) {
    for (int i=0; line[i] != '\0'; i++) {
      if (line[i] == 'S') {
	beams[i] = 1;
      }
      else if (line[i] == '^') {
	if (beams[i]) {
	  splits ++;
	  beams[i-1] += beams[i];
	  beams[i+1] += beams[i];
	  beams[i] = 0;
	}
      }
    }
  }
  printf("part 1: %d\n", splits);
  unsigned long sum = 0;
  for (int i=0; i<ROW_SIZE; i++)
    sum += beams[i];
  printf("part 2: %lu\n", sum);

  return 0;
}
