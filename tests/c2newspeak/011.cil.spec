Newspeak output
---------------
011.c
int32 f(int32, int32) {
  (011.c:33#1269)^2- =(int32) coerce[-2147483648,2147483647] (1-_int32 + 0-_int32);
}

void main(void) {
  (011.c:37#1306)^int32 x;
  (011.c:37#1309)^int32 y;
  (011.c:37#1312)^int32 z;
  (011.c:38#1317)^int32 a;
  (011.c:38#1317)^0- =(int32) 3-_int32;
  (011.c:38#1317)^{
    int32 b;
    (011.c:38#1317)^0- =(int32) 3-_int32;
    (011.c:38#1317)^f();
  }
}


