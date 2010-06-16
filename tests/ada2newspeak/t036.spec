Newspeak output
---------------
t036.adb
void t036.appelprocedure(void) {
  (t036.adb:17#9)^t036.inc();
  (t036.adb:18#10)^t036.set(10);
}

void t036.inc(void) {
  (t036.adb:7#10)^t036.x =(int32) belongs[-2147483648,2147483647] (t036.x_int32 + 1);
}

void t036.set(int32 y) {
  (t036.adb:12#10)^t036.x =(int32) y_int32;
}

int32 t036.x;
(t036.adb:3#6)^t036.x =(int32) 0;

