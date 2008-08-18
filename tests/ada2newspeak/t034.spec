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
    int32 value_of_t034.f;
    t034.f();
    1- =(int32) 0-_int32;
  } with lbl0: {
  }
}


t034.f() {
  do {
    Global(t034.x) =(int32) (Global(t034.x)_int32 + 1);
    0- =(int32) Global(t034.x)_int32;
    goto lbl0;
  } with lbl0: {
  }
}



Newspeak output
---------------
t034.adb
t034.appelfonction() {
  (t034.adb:11#158)^int32 y;
  (t034.adb:13#186)^int32 value_of_t034.f;
  (t034.adb:13#186)^t034.f();
  (t034.adb:13#186)^1- =(int32) 0-_int32;
}

t034.f() {
  (t034.adb:6#85)^t034.x =(int32) (t034.x_int32 + 1);
  (t034.adb:7#101)^0- =(int32) t034.x_int32;
}

int32 t034.x = {0: int32 1};

