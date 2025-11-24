/*
 * November 24, 2025
 * AoC 2016, Day 15
 */

#include <stdio.h>
#include <math.h>
#include <time.h>

typedef struct {
  int a;
  int b;
  int x;
} Series15;

int curval (Series15 *ss) {
  return ss->a * ss->x + ss->b;
}

void incr (Series15 *ss, int y) {
  float f = 1.0 * (y - ss->b) / ss->a;
  ss->x = trunc(ceil(f));
}

int main (int argc, char **argv) {
  Series15 ss[] = {
    {11,4,0},
    {5,2,0},
    {13,4,0},
    {17,4,0},
    {3,0,0},
    {19,5,0},
    {7,1,0} };
  int max = 1;
  clock_t t = clock();
  for (;;) {
    int updates = 0;
    for (int i=0; i<7; i++) {
      int x = ss[i].x;
      incr(&ss[i], max);
      if (x != ss[i].x)
		updates ++;
      max = max  > curval(&ss[i]) ? max : curval(&ss[i]);
    }
    if (! updates)
      break;
  }
  printf("found %d in %fs\n", max, (float)(clock() - t)/CLOCKS_PER_SEC);
  return 1;
}
