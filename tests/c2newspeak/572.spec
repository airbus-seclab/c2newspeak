Warning: 572.c:29#1095: extern function definition accepted
Newspeak output
---------------
572.c
void f(void) {
  (572.c:27#6)^int32 x;
  (572.c:28#2)^0- =(int32) 2;
}

void main(void) {
  (572.c:32#9)^fptr fptr;
  (572.c:33#2)^0- =(fptr) &_{void -> void}(f);
  (572.c:34#2)^f();
}


