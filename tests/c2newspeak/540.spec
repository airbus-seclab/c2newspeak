Warning: 540.c:26#5: incomplete prototype for function f
Newspeak output
---------------
540.c
void f(ptr ptr) {
}

void main(void) {
  (540.c:29#7)^ptr x;
  (540.c:30#2)^f(x_ptr: ptr);
}


