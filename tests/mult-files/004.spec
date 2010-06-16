Newspeak output
---------------
004-a.c
004-b.c
int32 !004-b.c.f(int32 x) {
  (004-b.c:27#2)^!return =(int32) coerce[-2147483648,2147483647] (x_int32 + 1);
}

int32 f(int32 x) {
  (004-a.c:27#2)^!return =(int32) x_int32;
}


