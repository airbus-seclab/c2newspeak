Newspeak output
---------------
063.c
int32 f(int32 a) {
  (063.c:27#1069)^!return =(int32) a_int32;
}

void main(void) {
  (063.c:31#1102)^int32 x;
  int32 tmp;
  (063.c:34#1132)^int32 y;
  (063.c:32#1107)^tmp <- f(1);
  (063.c:32#1107)^x <- f(tmp_int32);
  (063.c:35#1139)^x =(int32) y_int32;
}


