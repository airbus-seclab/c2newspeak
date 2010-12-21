Newspeak output
---------------
void main(void) {
  (004.c:33#6)^int32 x;
  (004.c:33#9)^int32 y;
  (004.c:33#12)^int32 z;
  (004.c:35#2)^x =(int32) coerce[-2147483648,2147483647] (y_int32 + z_int32);
  (004.c:36#2)^x =(int32) coerce[-2147483648,2147483647] (y_int32 * z_int32);
}


