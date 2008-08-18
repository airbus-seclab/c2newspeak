Newspeak Object output
----------------------
t018.adb
Global used

Global variables

Function definitions
t018() {
  do {
    uint3 x1;
    uint3 x2;
    uint3 x3;
    2- =(uint3) 1;
    1- =(uint3) 6;
    0- =(uint3) 3;
    choose {
    --> assert((0 ==_uint3 2-_uint3));
    --> assert((2-_uint3 ==_uint3 0));
        choose {
        --> assert((2 ==_uint3 1-_uint3));
        --> assert((1-_uint3 ==_uint3 2));
            choose {
            --> assert((4 ==_uint3 0-_uint3));
            --> assert((0-_uint3 ==_uint3 4));
            }
        }
    }
  } with lbl0: {
  }
}



Newspeak output
---------------
t018.adb
t018() {
  (t018.adb:6#163)^uint3 x1;
  (t018.adb:7#184)^uint3 x2;
  (t018.adb:8#210)^uint3 x3;
  (t018.adb:6#163)^2- =(uint3) 1;
  (t018.adb:7#184)^1- =(uint3) 6;
  (t018.adb:8#210)^0- =(uint3) 3;
  (t018.adb:10#238)^choose {
    | (0 ==_uint3 2-_uint3) -->
    | ! (0 ==_uint3 2-_uint3) -->
      (t018.adb:12#267)^choose {
        | (2 ==_uint3 1-_uint3) -->
        | ! (2 ==_uint3 1-_uint3) -->
          (t018.adb:14#301)^choose {
            | (4 ==_uint3 0-_uint3) -->
            | ! (4 ==_uint3 0-_uint3) -->
          }
      }
  }
}


