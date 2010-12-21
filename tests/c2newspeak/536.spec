Warning: 536.c:26#5: incomplete prototype for function h
Newspeak output
---------------
void h(int32 x) {
}

void main(void) {
  (536.c:29#9)^fptr ptr;
  (536.c:31#2)^ptr =(fptr) &_{(int32) -> void}(h);
}


