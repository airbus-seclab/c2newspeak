Newspeak output
---------------
t231.adb
void t231(void) {
  (t231.adb:7#124)^int32 x;
  (t231.adb:8#146)^int32 y;
  (t231.adb:7#124)^1- =(int32) 3;
  (t231.adb:10#169)^0- =(int32) 1;
  (t231.adb:15#289)^choose {
   -->
    (t231.adb:15#289)^guard((1-_int32 ==_int32 0));
    (t231.adb:12#195)^0- =(int32) 0;
   -->
    (t231.adb:15#289)^choose {
     -->
      (t231.adb:15#289)^guard((1-_int32 ==_int32 2));
      (t231.adb:13#226)^0- =(int32) 5;
     -->
      (t231.adb:15#289)^guard(! (1-_int32 ==_int32 2));
      (t231.adb:15#289)^guard(! (1-_int32 ==_int32 0));
      (t231.adb:14#257)^0- =(int32) -2;
    }
  }
  (t231.adb:16#303)^0- =(int32) 8;
}


