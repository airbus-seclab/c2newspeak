void f(void);
void g(void);

void f(void){
  g();
}

void g(void){
  f();
}
void main(void) {
  f();
}
