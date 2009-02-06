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
    2- =(uint3) belongs[0,8-1] 1;
    1- =(uint3) belongs[0,8-1] 6;
    0- =(uint3) belongs[0,8-1] 3;
        choose {
     -->
      guard((0 ==_uint3 2-_uint3));
     -->
      guard((2-_uint3 ==_uint3 0));
            choose {
       -->
        guard((2 ==_uint3 1-_uint3));
       -->
        guard((1-_uint3 ==_uint3 2));
                choose {
         -->
          guard((4 ==_uint3 0-_uint3));
         -->
          guard((0-_uint3 ==_uint3 4));
        }
      }
    }
  } with lbl0: {
  }
}



Newspeak output
---------------
t018.adb
void t018(void) {
  (t018.adb:6#163)^uint3 x1;
  (t018.adb:7#184)^uint3 x2;
  (t018.adb:8#210)^uint3 x3;
  (t018.adb:6#163)^2- =(uint3) 1;
  (t018.adb:7#184)^1- =(uint3) 6;
  (t018.adb:8#210)^0- =(uint3) 3;
  (t018.adb:10#238)^choose {
   -->
    (t018.adb:10#238)^guard((0 ==_uint3 2-_uint3));
   -->
    (t018.adb:10#238)^guard(! (0 ==_uint3 2-_uint3));
    (t018.adb:12#267)^choose {
     -->
      (t018.adb:12#267)^guard((2 ==_uint3 1-_uint3));
     -->
      (t018.adb:12#267)^guard(! (2 ==_uint3 1-_uint3));
      (t018.adb:14#301)^choose {
       -->
        (t018.adb:14#301)^guard((4 ==_uint3 0-_uint3));
       -->
        (t018.adb:14#301)^guard(! (4 ==_uint3 0-_uint3));
      }
    }
  }
}


