Newspeak Object output
----------------------
t104.adb
Global used

Global variables

Function definitions
t104.main() {
  do {
    int32 z;
    int32 !tmp-1073741821;
    t104.a();
    1- =(int32) belongs[0,13-1] 0-_int32;
  } with lbl0: {
  }
}


t104.a() {
  do {
    0- =(int32) belongs[-2147483648,2147483648-1] 12;
    goto lbl0;
  } with lbl0: {
  }
}



Newspeak output
---------------
t104.adb
int32 t104.a(void) {
  (t104.adb:5#63)^0- =(int32) 12;
}

void t104.main(void) {
  (t104.adb:10#113)^int32 z;
  (t104.adb:12#153)^int32 !tmp-1073741821;
  (t104.adb:12#153)^t104.a();
  (t104.adb:12#153)^1- =(int32) belongs[0,12] 0-_int32;
}


