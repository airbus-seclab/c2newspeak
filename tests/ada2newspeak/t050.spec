Newspeak output
---------------
t050.adb
t095.adb
void t050.main(void) {
  (t050.adb:11#10)^t050.x: uint1 <- t095.a();
}

uint1 t095.a(void) {
  (t095.adb:5#12)^!return =(uint1) 1;
}

int32 t050.a;
uint1 t050.x;
(t050.adb:5#6)^t050.a =(int32) 0;

