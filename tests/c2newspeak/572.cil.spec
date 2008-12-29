Newspeak output
---------------
572.c
void f__extinline(void) {
  (572.c:27#1087)^int32 x;
  (572.c:28#1092)^0- =(int32) 2;
}

void main(void) {
  (572.c:32#1123)^fptr fptr;
  (572.c:33#1140)^0- =(fptr) &_{void -> void}(f__extinline);
  (572.c:34#1152)^f__extinline();
}


