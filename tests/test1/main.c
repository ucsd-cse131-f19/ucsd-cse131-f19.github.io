#include <stdio.h>
#include <stdlib.h>

extern int64_t our_code_starts_here() asm("our_code_starts_here");
extern void print_err_exit() asm("print_err_exit");

void print_err_exit() {
  printf("Error\n");
  exit(1);
}

int main(int argc, char** argv) {
  int64_t result = our_code_starts_here();
  if((result & 1)) {
    printf("%lld\n", (result - 1) / 2);
  }
  else if(result < 512) {
    // %c is a formatting character for printing single chars
    printf("`%c\n", (char)(result / 2));
  }
  else {
    printf("Unrepresentable\n");
  }
  return 0;
}