Newspeak Object output
----------------------
t215.adb
Global used
t215.a

Global variables
int32 t215.a;

Function definitions
t215.donull() {
  do {
    Global(t215.a) =(int32) belongs[0,65536-1] 65535;
    Global(t215.a) =(int32) belongs[0,65536-1] 0;
    Global(t215.a) =(int32) belongs[0,65536-1] coerce[-2147483648,2147483647] (0 - 1);
    Global(t215.a) =(int32) belongs[0,65536-1] coerce[-2147483648,2147483647] (0 - 65535);
    Global(t215.a) =(int32) belongs[0,65536-1] coerce[-2147483648,2147483647] (0 - 65535);
    Global(t215.a) =(int32) belongs[0,65536-1] 65536;
  } with lbl0: {
  }
}



Newspeak output
---------------
t215.adb
t215.donull() {
  (t215.adb:13#190)^t215.a =(int32) 65535;
  (t215.adb:14#214)^t215.a =(int32) 0;
  (t215.adb:15#239)^t215.a =(int32) belongs[0,65535] -1;
  (t215.adb:17#269)^t215.a =(int32) belongs[0,65535] -65535;
  (t215.adb:18#308)^t215.a =(int32) belongs[0,65535] -65535;
  (t215.adb:19#346)^t215.a =(int32) belongs[0,65535] 65536;
}

int32 t215.a = 0;

