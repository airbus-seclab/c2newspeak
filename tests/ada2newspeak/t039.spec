Newspeak Object output
----------------------
t039.adb
Global used

Global variables

Function definitions
t039.proc_out() {
  do {
    [0-_uint1]1 =(uint1) 1;
  } with lbl0: {
  }
}


t039.appelfonction() {
  do {
    uint1 y;
    ptr t039.proc_out.arg1;
    0- =(ptr) &_1(1-);
    t039.proc_out();
  } with lbl0: {
  }
}



Newspeak output
---------------
t039.adb
void t039.appelfonction(void) {
  (t039.adb:11#138)^uint1 y;
  (t039.adb:13#166)^ptr t039.proc_out.arg1;
  (t039.adb:13#166)^0- =(ptr) &_1(1-);
  (t039.adb:13#166)^t039.proc_out();
}

void t039.proc_out(ptr) {
  (t039.adb:6#72)^[0-_uint1]1 =(uint1) 1;
}


