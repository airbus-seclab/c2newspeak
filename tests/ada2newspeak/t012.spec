Newspeak Object output
----------------------
t012.adb
Global used

Global variables

Function definitions
t012() {
  int32 x;
  do {
    0- =(int32) 3;
    choose {
    --> assert((0-_int32 > 2));
        0- =(int32) (0-_int32 + 1);
        0- =(int32) (0-_int32 + 4);
        choose {
        --> assert((10 > 0-_int32));
            0- =(int32) (0-_int32 * 2);
        --> assert((0-_int32 > 10));
            choose {
            --> assert((0-_int32 ==_int32 42));
                goto lbl0;
            --> assert((42 ==_int32 0-_int32));
                choose {
                --> assert((26 > 0-_int32));
                    0- =(int32) 42;
                --> assert((0-_int32 > 26));
                    0- =(int32) (0-_int32 - 1);
                }
            }
        }
    --> assert((2 > 0-_int32));
    }
  } with lbl0: {
  }
}



Newspeak output
---------------
t012.adb
t012() {
  (t012.adb:3#28)^int32 x;
  (t012.adb:3#28)^0- =(int32) 3;
  (t012.adb:2#10)^do {
    (t012.adb:5#55)^choose {
      | (0-_int32 > 2) -->
        (t012.adb:7#74)^0- =(int32) (0-_int32 + 1);
        (t012.adb:8#90)^0- =(int32) (0-_int32 + 4);
        (t012.adb:9#106)^choose {
          | (10 > 0-_int32) -->
            (t012.adb:11#134)^0- =(int32) (0-_int32 * 2);
          | ! (10 > 0-_int32) -->
            (t012.adb:12#153)^choose {
              | (0-_int32 ==_int32 42) -->
                (t012.adb:14#184)^goto lbl0;
              | ! (0-_int32 ==_int32 42) -->
                (t012.adb:15#201)^choose {
                  | (26 > 0-_int32) -->
                    (t012.adb:17#229)^0- =(int32) 42;
                  | ! (26 > 0-_int32) -->
                    (t012.adb:19#258)^0- =(int32) (0-_int32 - 1);
                }
            }
        }
      | ! (0-_int32 > 2) -->
    }
  } with lbl0: {
  }
}


