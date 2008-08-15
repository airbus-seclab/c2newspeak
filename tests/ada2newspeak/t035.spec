Newspeak Object output
----------------------
t035.adb
Global used
t035.x
t035.z

Global variables
int32 t035.t = {0: int32 1}
;
int32 t035.x = {0: int32 0}
;
int32 t035.y = {0: int32 0}
;
int32 t035.z = {0: int32 0}
;

Function definitions
t035.chose() {
  do {
    choose {
    --> assert(!(0-_uint2 ==_uint2 3));
        Global(t035.z) =(int32) 3;
        2- =(int32) 0;
        goto lbl0;
    --> assert((3 ==_uint2 0-_uint2));
        Global(t035.z) =(int32) 2;
        2- =(int32) 1;
        goto lbl0;
    }
  } with lbl0: {
  }
}


t035.proc() {
  do {
    Global(t035.x) =(int32) 2;
  } with lbl0: {
  }
}



Newspeak output
---------------
t035.adb
t035.chose() {
  (t035.adb:9#132)^do {
    (t035.adb:12#227)^choose {
      | (0-_uint2 ==_uint2 3) -->
        (t035.adb:13#249)^t035.z =(int32) 3;
        (t035.adb:14#266)^2- =(int32) 0;
        (t035.adb:14#266)^goto lbl0;
      | ! (0-_uint2 ==_uint2 3) -->
        (t035.adb:16#296)^t035.z =(int32) 2;
        (t035.adb:17#313)^2- =(int32) 1;
        (t035.adb:17#313)^goto lbl0;
    }
  } with lbl0: {
  }
}

t035.proc() {
  (t035.adb:6#104)^t035.x =(int32) 2;
}

int32 t035.x = {0: int32 0};
int32 t035.z = {0: int32 0};

