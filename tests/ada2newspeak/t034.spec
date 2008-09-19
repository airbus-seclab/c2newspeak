Newspeak Object output
----------------------
t034.adb
Global used
t034.x

Global variables
int32 t034.x = {0: int32 1}
;

Function definitions
t034.appelfonction() {
  do {
    int32 y;
    int32 !tmp-1073741821;
    {
      int32 value_of_t034.f;
      t034.f();
      1- =(int32) 0-_int32;
    }
    1- =(int32) belongs[-2147483648,2147483648-1] 0-_int32;
  } with lbl0: {
  }
}


t034.f() {
  do {
    Global(t034.x) =(int32) belongs[-2147483648,2147483648-1] coerce[-2147483648,2147483647] (Global(t034.x)_int32 + 1);
    0- =(int32) belongs[-2147483648,2147483648-1] Global(t034.x)_int32;
    goto lbl0;
  } with lbl0: {
  }
}



Newspeak output
---------------
t034.adb
t034.appelfonction() {
  (t034.adb:11#158)^int32 y;
  (t034.adb:13#186)^int32 !tmp-1073741821;
  (t034.adb:13#186)^{
    int32 value_of_t034.f;
    (t034.adb:13#186)^t034.f();
    (t034.adb:13#186)^1- =(int32) 0-_int32;
  }
  (t034.adb:13#186)^1- =(int32) 0-_int32;
}

t034.f() {
  (t034.adb:6#85)^t034.x =(int32) belongs[-2147483648,2147483647] coerce[-2147483648,2147483647] (t034.x_int32 + 1);
  (t034.adb:7#101)^0- =(int32) t034.x_int32;
}

int32 t034.x = {0: int32 1};

