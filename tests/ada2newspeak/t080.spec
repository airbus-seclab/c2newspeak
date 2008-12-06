Newspeak Object output
----------------------
t080.adb
Global used

Global variables
int32 t080a.a;

Function definitions
t080.main() {
  do {
    int32 y;
    0- =(int32) belongs[-2147483648,2147483648-1] 0;
  } with lbl0: {
  }
}


t080.a() {
  do {
    0- =(int32) belongs[-2147483648,2147483648-1] 5;
    goto lbl0;
  } with lbl0: {
  }
}



Newspeak output
---------------
t080.adb
int32 t080.a(void) {
  (t080.adb:7#129)^0- =(int32) 5;
}

void t080.main(void) {
  (t080.adb:13#227)^int32 y;
  (t080.adb:15#255)^0- =(int32) 0;
}


