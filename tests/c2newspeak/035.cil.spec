Newspeak output
---------------
035.c
int32 f(void) {
  int32 tmp;
  (035.c:34#1149)^tmp <- g();
  (035.c:34#1149)^!return =(int32) tmp_int32;
}

int32 g(void) {
  (035.c:30#1124)^!return =(int32) 1;
}


