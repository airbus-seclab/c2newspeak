Newspeak output
---------------
t035.adb
int32 t035.chose(int32, uint2) {
  (t035.adb:9#11)^do {
    (t035.adb:12#8)^choose {
     -->
      (t035.adb:12#8)^guard((0-_uint2 ==_uint2 3));
      (t035.adb:13#13)^t035.z =(int32) 3;
      (t035.adb:14#15)^2- =(int32) 0;
      (t035.adb:14#15)^goto lbl0;
     -->
      (t035.adb:12#8)^guard(! (0-_uint2 ==_uint2 3));
      (t035.adb:16#13)^t035.z =(int32) 2;
      (t035.adb:17#15)^2- =(int32) 1;
      (t035.adb:17#15)^goto lbl0;
    }
  } with lbl0: {
  }
}

void t035.proc(void) {
  (t035.adb:6#10)^t035.x =(int32) 2;
}

int32 t035.x = {0: int32 0};
int32 t035.z = {0: int32 0};

