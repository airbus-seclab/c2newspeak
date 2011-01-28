Warning: t434b.ads:4#8: multiple definitions of global variable t434b.c_s, in file t434b.ads variable t434b.c_s should probably be extern accepted
Warning: t434b.ads:3#8: multiple definitions of global variable t434b.c_x, in file t434b.ads variable t434b.c_x should probably be extern accepted
Newspeak output
---------------
void t434.mm(void) {
  (t434.adb:6#6)^uint1 z;
  (t434.adb:7#7)^{ uint1 1; uint1 0; }2[3] tg;
  (t434.adb:7#7)^tg =3 t434c.c_c;
  (t434.adb:9#7)^choose {
   -->
    (t434.adb:9#7)^guard((tg_uint1 ==_uint1 t434b.c_x_uint1));
   -->
    (t434.adb:9#7)^choose {
     -->
      (t434.adb:9#7)^guard((tg_uint1 ==_uint1 t434b.c_s_uint1));
     -->
      (t434.adb:9#7)^guard(! (tg_uint1 ==_uint1 t434b.c_s_uint1));
      (t434.adb:9#7)^guard(! (tg_uint1 ==_uint1 t434b.c_x_uint1));
    }
  }
}

uint1 t434b.c_s;
uint1 t434b.c_x;
{ uint1 1; uint1 0; }2[3] t434c.c_c;

