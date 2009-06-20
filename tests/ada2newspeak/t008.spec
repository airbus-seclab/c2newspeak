Warning: t008.adb:6#6: Code after return statement can't be reached
Newspeak output
---------------
t008.adb
void t008(void) {
  (t008.adb:3#5)^int32 x;
  (t008.adb:2#9)^do {
    (t008.adb:5#8)^goto lbl0;
    (t008.adb:6#6)^0- =(int32) 1;
  } with lbl0: {
  }
}


