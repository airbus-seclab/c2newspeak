Warning: t008.adb:6#127: Code after return statement can't be reached
Newspeak output
---------------
t008.adb
void t008(void) {
  (t008.adb:3#96)^int32 x;
  (t008.adb:2#78)^do {
    (t008.adb:5#117)^goto lbl0;
    (t008.adb:6#127)^0- =(int32) 1;
  } with lbl0: {
  }
}


