Newspeak output
---------------
062.c
int32 f(int32 a) {
  (062.c:27#1069)^!return =(int32) a_int32;
}

void main(void) {
  (062.c:31#1102)^int32 x;
  int32 tmp;
  (062.c:32#1107)^tmp <- f(1);
  (062.c:32#1107)^x <- f(tmp_int32);
}


