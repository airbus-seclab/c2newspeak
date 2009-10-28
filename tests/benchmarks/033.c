typedef struct {
  void (*f1)();
  void (*f2)();
} S;

void set_f1(S* s, void (*f1)()) {
  s->f1 = f1;
}

void set_f2(S* s, void (*f2)()) {
  s->f1 = f2;
}

void g() {
  char t[10];
  t[199] = 1;
}

void h() {
}

int main() {
  S x;
  set_f1(&x, &g);
  set_f2(&x, &h);
  
  (*x.f1)();

  return 1;
}
