
typedef struct {
  int a;
  int *p;
} T;

void memset_zero(char *ptr, unsigned int n) {
  int i;
  for (i = 0; i < n; i++) {
    *ptr = 0;
    ptr++;
  }
}

int main() {
  T x;
  char witness[10];
  memset_zero((char*)&x, sizeof(T));
  *(x.p) = 1;
  // unreachable code, should not print any warnings
  witness[100] = 1;
  
  return 0;
}
