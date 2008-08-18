Newspeak Object output
----------------------
t081.adb
Global used
t081.c

Global variables
int32 t081.c;
extern int32 t081a.a;

Function definitions
t081.a() {
  do {
    0- =(int32) 5;
    goto lbl0;
  } with lbl0: {
  }
}


t081.main() {
  do {
    int32 x;
    int32 y;
    {
      int32 value_of_t081.a;
      t081.a();
      2- =(int32) 0-_int32;
    }
    0- =(int32) (((1-_int32 + 15) + Global(t081.c)_int32) + 15);
  } with lbl0: {
  }
}



Newspeak output
---------------
t081.adb
t081.a() {
  (t081.adb:8#178)^0- =(int32) 5;
}

t081.main() {
  (t081.adb:14#243)^int32 x;
  (t081.adb:15#276)^int32 y;
  (t081.adb:14#243)^{
    int32 value_of_t081.a;
    (t081.adb:14#243)^t081.a();
    (t081.adb:14#243)^2- =(int32) 0-_int32;
  }
  (t081.adb:19#336)^0- =(int32) (((1-_int32 + 15) + t081.c_int32) + 15);
}

int32 t081.c = 0;

