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


