Warning: 536.c:26#5: incomplete prototype for function h
Newspeak output
---------------
536.c
void h(int32) {
}

void main(void) {
  (536.c:29#9)^fptr ptr;
  (536.c:31#2)^0- =(fptr) &_{int32 -> void}(h);
}


