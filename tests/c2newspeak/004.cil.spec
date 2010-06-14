Newspeak output
---------------
004.c
void main(void) {
  (004.c:33#1256)^int32 x;
  (004.c:33#1259)^int32 y;
  (004.c:33#1262)^int32 z;
  (004.c:35#1270)^x =(int32) coerce[-2147483648,2147483647] (y_int32 + z_int32);
  (004.c:36#1283)^x =(int32) coerce[-2147483648,2147483647] (y_int32 * z_int32);
}


