Newspeak output
---------------
int32 t081.a(void) {
  (t081.adb:8#12)^!return =(int32) 5;
}

void t081.main(void) {
  (t081.adb:14#9)^int32 x;
  (t081.adb:15#9)^int32 y;
  (t081.adb:14#9)^{
    int32 tmp_cir!0;
    (t081.adb:14#9)^tmp_cir!0: int32 <- t081.a();
    (t081.adb:14#9)^x =(int32) tmp_cir!0_int32;
  }
  (t081.adb:19#10)^y =(int32) belongs[-2147483648,2147483647] (belongs[-2147483648,2147483647] (belongs[-2147483648,2147483647] (x_int32 + 15) + t081.c_int32) + 15);
}

int32 t081.c;

