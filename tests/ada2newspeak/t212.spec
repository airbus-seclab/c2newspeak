Newspeak Object output
----------------------
t212.adb
Global used
t212.my_ar

Global variables
int32[3] t212.my_ar;

Function definitions
t212.blabla() {
  do {
    Global(t212.my_ar) + ((belongs[1,4-1] 2 - 1) * 32) =(int32) belongs[-2147483648,2147483648-1] 4;
    Global(t212.my_ar) + ((belongs[1,4-1] 4 - 1) * 32) =(int32) belongs[-2147483648,2147483648-1] 3;
  } with lbl0: {
  }
}



Newspeak output
---------------
t212.adb
t212.blabla() {
  (t212.adb:13#169)^t212.my_ar + 32 =(int32) 4;
  (t212.adb:14#190)^t212.my_ar + ((belongs[1,3] 4 - 1) * 32) =(int32) 3;
}

int32[3] t212.my_ar = 0;

