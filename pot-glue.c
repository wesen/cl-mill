#include <potracelib.h>

#define SET_BIT(port, bit)   ((port) |= (1 << (bit)))
#define CLEAR_BIT(port, bit) ((port) &= ~(1 << (bit)))
#define IS_BIT_SET(port, bit) (((port) & (1 << (bit))) ? 1 : 0)
#define IS_BIT_CLEAR(port, bit) (((port) & (1 << (bit))) == 0 ? 1 : 0)
#define MIN(x, y) ((x) > (y) ? (y) : (x))
#define MAX(x, y) ((x) > (y) ? (x) : (y))

#define bitcount(data) (sizeof(data)*8)
#define N (bitcount(potrace_word))

#define POTRACE_MAP_PIXEL(bitmap, i, j) \
  IS_BIT_SET((bitmap->map + j * bitmap->dy)[i / N], \
	     (N - 1 - i % N))

#define SET_POTRACE_MAP_PIXEL(bitmap, i, j)	    \
  SET_BIT((bitmap->map + j * bitmap->dy)[i / N], \
	  (N - 1 - i % N))

#define CLEAR_POTRACE_MAP_PIXEL(bitmap, i, j)	    \
  CLEAR_BIT((bitmap->map + j * bitmap->dy)[i / N],    \
	  (N - 1 - i % N))

void potrace_set_pixel(potrace_bitmap_t *bitmap,
		       unsigned short i, unsigned short j) {
  SET_POTRACE_MAP_PIXEL(bitmap, i, j);
}

void potrace_clear_pixel(potrace_bitmap_t *bitmap,
		       unsigned short i, unsigned short j) {
  CLEAR_POTRACE_MAP_PIXEL(bitmap, i, j);
}

unsigned char potrace_get_pixel(potrace_bitmap_t *bitmap,
				unsigned short i, unsigned short j) {
  return POTRACE_MAP_PIXEL(bitmap, i, j) ? 1 : 0;
}
