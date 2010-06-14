Newspeak output
---------------
573.c
void f__extinline(void) {
  (573.c:27#1087)^int32 x;
  (573.c:28#1092)^x =(int32) 2;
}

void main(void) {
  (573.c:32#1123)^fptr fptr;
  (573.c:33#1140)^fptr =(fptr) &_{void -> void}(f__extinline);
  (573.c:34#1152)^f__extinline();
}


