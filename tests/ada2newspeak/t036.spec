Newspeak Object output
----------------------
t036.adb
Global used
t036.x

Global variables
int32 t036.x = {0: int32 0}
;

Function definitions
t036.set() {
  do {
    Global(t036.x) =(int32) belongs[-2147483648,2147483648-1] 0-_int32;
  } with lbl0: {
  }
}


t036.inc() {
  do {
    Global(t036.x) =(int32) belongs[-2147483648,2147483648-1] coerce[-2147483648,2147483647] (Global(t036.x)_int32 + 1);
  } with lbl0: {
  }
}


t036.appelprocedure() {
  do {
    t036.inc();
    {
      int32 t036.set.arg1;
      0- =(int32) 10;
      t036.set();
    }
  } with lbl0: {
  }
}



Newspeak output
---------------
t036.adb
t036.appelprocedure() {
  (t036.adb:17#212)^t036.inc();
  (t036.adb:18#223)^{
    int32 t036.set.arg1;
    (t036.adb:18#223)^0- =(int32) 10;
    (t036.adb:18#223)^t036.set();
  }
}

t036.inc() {
  (t036.adb:7#74)^t036.x =(int32) belongs[-2147483648,2147483647] coerce[-2147483648,2147483647] (t036.x_int32 + 1);
}

t036.set() {
  (t036.adb:12#145)^t036.x =(int32) 0-_int32;
}

int32 t036.x = {0: int32 0};

