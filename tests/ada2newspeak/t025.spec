Newspeak Object output
----------------------
t025.adb
Global used

Global variables

Function definitions
t025() {
  do {
    do {
      while (1) {
                choose {
         -->
          guard(! 1);
          goto lbl2;
         -->
          guard(1);
        }
      }
    } with lbl2: {
    }
  } with lbl0: {
  }
}



Newspeak output
---------------
t025.adb
void t025(void) {
  (t025.adb:2#27)^do {
    (t025.adb:4#51)^while (1) {
      (t025.adb:4#51)^choose {
       -->
        (t025.adb:4#51)^guard(! 1);
        (t025.adb:4#51)^goto lbl0;
       -->
      }
    }
  } with lbl0: {
  }
}


