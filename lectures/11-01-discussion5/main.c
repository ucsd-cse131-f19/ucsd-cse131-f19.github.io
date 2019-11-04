#include <stdio.h>
#include <stdlib.h>

extern int64_t our_code_starts_here() asm("our_code_starts_here");
extern void print_err_exit() asm("print_err_exit");

// Extensions for more specific error messages
void print_op_error() {}
void print_if_error() {}

void print_err_exit(int64_t what_value) {
  printf("Error!\n");
  exit(1);
}

int main(int argc, char** argv) {
  int64_t result = our_code_starts_here();
  if((result & 1)) {
    printf("%ld\n", (result - 1) / 2);
  }
  else if(result == 2) {
    printf("true\n");
  }
  else if(result == 0) {
    printf("false\n");
  }
  else {
    printf("Got unrerpresentable value: %ld", result);
  }
  return 0;
}
