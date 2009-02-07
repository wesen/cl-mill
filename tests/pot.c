#include <inttypes.h>
#include <stdio.h>

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

void pixels_to_map(uint32_t **pixels, potrace_bitmap_t *bitmap) {
  int i, j;
  for (j = 0; j < bitmap->h; j++) {
    for (i = 0; i < bitmap->w; i++) {
      if (pixels[i][j] != 0) {
	SET_POTRACE_MAP_PIXEL(bitmap, i, j);
      } else {
	CLEAR_POTRACE_MAP_PIXEL(bitmap, i, j);
      }
    }
  }
}

void map_print(potrace_bitmap_t *bitmap) {
  int i, j;
  for (j = bitmap->h - 1; j >= 0; j--) {
    for (i = 0; i < bitmap->w; i++) {
      if (POTRACE_MAP_PIXEL(bitmap, i, j)) {
	printf("X");
      } else {
	printf(".");
      }
    }
    printf("\n");
  }
  printf("\n");
  
}

void print_potrace_curve(potrace_curve_t *curve) {
  int i;
  for (i = 0; i < curve->n; i++) {
    printf("%d: ", i);
    switch (curve->tag[i]) {
    case POTRACE_CURVETO:
      printf("CURVE u: %f,%f w: %f,%f b: %f,%f\n",
	     curve->c[i][0].x, curve->c[i][0].y,
	     curve->c[i][1].x, curve->c[i][1].y,
	     curve->c[i][2].x, curve->c[i][2].y
	     );
      break;

    case POTRACE_CORNER:
      printf("CORNER v: %f,%f b: %f,%f\n",
	     curve->c[i][1].x, curve->c[i][1].y,
	     curve->c[i][2].x, curve->c[i][2].y
	     );
      break;
    }
  }
}

void print_potrace_path(potrace_path_t *path) {
  printf("area: %d, sign: %c\n", path->area, path->sign);
  print_potrace_curve(&path->curve);
  if (path->next != NULL)
    print_potrace_path(path->next);
}

potrace_bitmap_t bitmap;
potrace_word map[24];

int main(int argc, char *argv[]) {
  bitmap.w = 36;
  bitmap.h = 12;
  bitmap.dy = 2;
  bitmap.map = map;

  map[22] = 0xfff0fc02; map[23] = 0x00000000; 
  map[20] = 0x7ff1fe02; map[21] = 0x00000000; 
  map[18] = 0x3ff3ff07; map[19] = 0x00000000; 
  map[16] = 0x1ff7ff87; map[17] = 0x00000000; 
  map[14] = 0x0ff7cf8f; map[15] = 0x80000000; 
  map[12] = 0x07f7878f; map[13] = 0x80000000; 
  map[10] = 0x03f7879f; map[11] = 0xc0000000; 
  map[8] = 0x01f7cf9f; map[9] = 0xc0000000; 
  map[6] = 0x00f7ffbf; map[7] = 0xe0000000; 
  map[4] = 0x0073ff3f; map[5] = 0xe0000000; 
  map[2] = 0x0031fe7f; map[3] = 0xf0000000; 
  map[0] = 0x0010fc7f; map[1] = 0xf0000000;

  map_print(&bitmap);

  potrace_param_t *params = potrace_param_default();
  potrace_state_t *state = potrace_trace(params, &bitmap);
  if (state->status == POTRACE_STATUS_OK) {
    printf("trace succesfull\n");
    print_potrace_path(state->plist);
  } else {
    printf("trace incomplete\n");
  }
  potrace_state_free(state);
  state = NULL;
  potrace_param_free(params);
  params = NULL;
  return 0;
}
