Newspeak Object output
----------------------
t216.adb
Global used
t216.r
t216.a

Global variables
int32 t216.r;
int32[7] t216.a;

Function definitions
t216.donull() {
  do {
    Global(t216.a) + ((belongs[0,7-1] 0 - 0) * 32) =(int32) belongs[-2147483648,2147483648-1] 1;
    Global(t216.a) + ((belongs[0,7-1] 1 - 0) * 32) =(int32) belongs[-2147483648,2147483648-1] 2;
    Global(t216.a) + ((belongs[0,7-1] 6 - 0) * 32) =(int32) belongs[-2147483648,2147483648-1] 7;
    Global(t216.r) =(int32) belongs[-2147483648,2147483648-1] 7;
    Global(t216.r) =(int32) belongs[-2147483648,2147483648-1] 0;
    Global(t216.r) =(int32) belongs[-2147483648,2147483648-1] 7;
  } with lbl0: {
  }
}



Newspeak output
---------------
t216.adb
void t216.donull(void) {
  (t216.adb:20#374)^t216.a =(int32) 1;
  (t216.adb:21#389)^t216.a + 32 =(int32) 2;
  (t216.adb:22#405)^t216.a + 192 =(int32) 7;
  (t216.adb:23#420)^t216.r =(int32) 7;
  (t216.adb:24#445)^t216.r =(int32) 0;
  (t216.adb:25#469)^t216.r =(int32) 7;
}

int32[7] t216.a = 0;
int32 t216.r = 0;

