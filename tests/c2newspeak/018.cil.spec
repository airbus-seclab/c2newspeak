Newspeak output
---------------
018.c
int32 f1(int32 n1) {
  int32 tmp;
  (018.c:32#1146)^tmp <- f2(n1_int32);
  (018.c:32#1146)^!return =(int32) tmp_int32;
}

int32 f2(int32 n2) {
  int32 tmp;
  (018.c:36#1184)^tmp <- f1(n2_int32);
  (018.c:36#1184)^!return =(int32) tmp_int32;
}


