Newspeak output
---------------
t036.adb
void t036.appelprocedure(void) {
  (t036.adb:17#212)^t036.inc();
  (t036.adb:18#223)^{
    int32 y;
    (t036.adb:18#223)^0- =(int32) 10;
    (t036.adb:18#223)^t036.set();
  }
}

void t036.inc(void) {
  (t036.adb:7#74)^t036.x =(int32) belongs[-2147483648,2147483647] (t036.x_int32 + 1);
}

void t036.set(int32) {
  (t036.adb:12#145)^t036.x =(int32) 0-_int32;
}

int32 t036.x = {0: int32 0};

