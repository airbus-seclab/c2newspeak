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
        --> assert(! 1);
            goto lbl2;
        --> assert(1);
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
t025() {
  (t025.adb:2#27)^do {
    (t025.adb:4#51)^while (1) {
      (t025.adb:4#51)^choose {
        | ! 1 -->
          (t025.adb:4#51)^goto lbl2;
        | 1 -->
      }
    }
  } with lbl2: {
  }
}


