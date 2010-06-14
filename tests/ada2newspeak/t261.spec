Newspeak output
---------------
t261.adb
int32 t261.!op_+(int32 left, int32 right) {
  (t261.adb:9#10)^!return =(int32) 0;
}

void t261.main(void) {
  (t261.adb:13#7)^int32 x;
  (t261.adb:15#8)^int32 !tmp0;
  (t261.adb:15#8)^!tmp0 <- t261.!op_+(2, 3);
  (t261.adb:15#8)^x =(int32) !tmp0_int32;
}


