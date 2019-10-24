#include <stdio.h>
#include <stdlib.h>

extern int64_t our_code_starts_here()
  asm("our_code_starts_here");
extern void print_error_and_exit(int64_t errcode) asm("print_error_and_exit");

void print_error_and_exit(int64_t errcode) {
  printf("Error: %lld\n", errcode);
  exit(1);
}

int main(int argc, char** argv) {
  int64_t result = our_code_starts_here();
  if((result & 1) == 1) {
    printf("%lld\n", result >> 1);
  }
  else if(result == 0x7FFFFFFFFFFFFFFE) {
    printf("false\n");
  }
  else if(result == 0xFFFFFFFFFFFFFFFE) {
    printf("true\n");
  }
  else {
    printf("Unknown value: %lld\n", result);
  }
  printf("\nThe rep was: %lld\n", result);
  return 0;
}