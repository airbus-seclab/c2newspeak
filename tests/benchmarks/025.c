char t1[10];
char t2[20];

int main() {
  int rnd;
  int error;
  char *p;
  int n;
  int i;
  error = 0;
  switch (rnd) {
  case 0:
    p = &t1[0];
    n = 10;
    break;
  case 1:
    p = &t2[0];
    n = 20;
    break;
  default:
    error = 1;
    break;
  }
  if (error == 0) {
    for (i = 0; i < n; i++) {
      // should not signal any array out of bounds here:
      p[i] = 1;
    }
  }
  return 0;
}
