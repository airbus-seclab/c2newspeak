typedef void (*fptr)();

typedef struct {
  char *name;
  fptr func;
} S;

void f() {
  char t[10];
  t[100] = 1;
}

void g() {
  char u[10];
  u[100] = 2;
}

S table[2] = {
  {"f", &f},
  {"g", &g}
};

int my_strcmp(char *s1, char *s2) {
  while (*s1 == *s2) {
    if (*s1 == '\0') return 0;
    s1++;
    s2++;
  }
  return (*(unsigned char *)s1) - (*(unsigned char *)s2);
}

fptr find(char *s) {
  int i;
  for (i = 0; i < 2; i++) {
    if (my_strcmp(table[i].name, s) == 0) {
      return table[i].func;
    }
  }
  return 0;
}

int main() {
  char name[10];
  fptr f;
  f = find("h");
  if (f) {
    (*f)();
  }
  return 0;
}
