Warning: 679.c:26#5: incomplete prototype for function f
Warning: 679.c:30#2: dirty cast from integer to pointer accepted
Newspeak output
---------------
679.c
void f(ptr) {
}

void main(void) {
  (679.c:29#6)^int32 x;
  (679.c:30#2)^ptr ptr;
  (679.c:30#2)^0- =(ptr) (ptr) 1-_int32;
  (679.c:30#2)^f();
}


