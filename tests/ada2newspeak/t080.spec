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
t080.a() {
  (t080.adb:7#129)^0- =(int32) 5;
}

t080.main() {
  (t080.adb:13#227)^int32 y;
  (t080.adb:15#255)^0- =(int32) 0;
}


