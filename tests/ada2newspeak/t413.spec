Warning: t413a.ads:3#6: multiple definitions of global variable t413a.u, in file t413a.ads variable t413a.u should probably be extern accepted
Newspeak output
---------------
t413.adb
t413a.adb
void t413(void) {
  (t413.adb:5#6)^int32 x;
  (t413.adb:7#7)^int32 tmp_cir!0;
  (t413.adb:7#7)^tmp_cir!0: int32 <- t413a.troc();
  (t413.adb:7#7)^x =(int32) tmp_cir!0_int32;
}

int32 t413a.troc(void) {
  (t413a.adb:3#11)^do {
    (t413a.adb:5#8)^choose {
     -->
      (t413a.adb:5#8)^guard(t413a.u_uint1);
      (t413a.adb:6#15)^!return =(int32) 8;
      (t413a.adb:6#15)^goto lbl0;
     -->
      (t413a.adb:5#8)^guard(! t413a.u_uint1);
      (t413a.adb:8#15)^!return =(int32) 4;
      (t413a.adb:8#15)^goto lbl0;
    }
  } with lbl0:
}

uint1 t413a.u;

