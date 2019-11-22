#include <stdio.h>
#include <stdlib.h>

extern int64_t our_code_starts_here() asm("our_code_starts_here");
extern void print_err_exit() asm("print_err_exit");
extern void gc() asm("gc");
extern int64_t* HEAP_END asm("HEAP_END");

typedef struct {
  int64_t gc_word;
  int64_t first;
  int64_t second;
} Pair;

void print_err_exit() {
  printf("Out of memory!");
  exit(1);
}

void print(int64_t val) {
  if((val & 1)) {
    printf("%lld", (val - 1) / 2);
  } else if(val == 6) {
    printf("true");
  } else if(val == 2) {
    printf("false");
  } else if(val == 0) {
    printf("null");
  } else if((val & 7) == 0) {
    int64_t* as_ref = (int64_t*)val;
    printf("(pair ");
    print(as_ref[1]);
    printf(" ");
    print(as_ref[2]);
    printf(")");
  } else {
    printf("Got unrepresentable value: %lld", val);
  }
}

void print_heap(int64_t* where, uint64_t how_many) {
  uint64_t i = 0;
  while(i < how_many) {
    printf("%p:\t%#010llx\t%lld\n", where, *where, *where);
    i += 1;
    where += 1;
  }
}

// Spin up a gc thread? Not today...

// return new r15
// 
void gc() {
  printf("Called gc!\n");
}

int64_t* HEAP_END;

int main(int argc, char** argv) {
  int heap_size = 10;
  int64_t* HEAP = calloc(sizeof(int64_t), heap_size);
  HEAP_END = HEAP + heap_size;
  int64_t result = our_code_starts_here(HEAP);
  //printf("%p =? %p", HEAP_END, result);
  print(result);
  printf("\n");
  print_heap(HEAP, 10);
  return 0;
}
