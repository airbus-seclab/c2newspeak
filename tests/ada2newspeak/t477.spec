Newspeak output
---------------
void t477({ uint2 2; uint2 0; }4 v) {
  (t477.adb:7#5)^uint1 tmp_ada_firstpass!0;
  (t477.adb:7#5)^choose {
   -->
    (t477.adb:7#5)^guard(! (t477a.mm_uint2 ==_uint2 1));
    (t477.adb:7#5)^tmp_ada_firstpass!0 =(uint1) 1;
   -->
    (t477.adb:7#5)^guard((t477a.mm_uint2 ==_uint2 1));
    (t477.adb:7#5)^tmp_ada_firstpass!0 =(uint1) ! (v_uint2 ==_uint2 3);
  }
  (t477.adb:7#5)^choose {
   -->
    (t477.adb:7#5)^guard(tmp_ada_firstpass!0_uint1);
   -->
    (t477.adb:7#5)^guard(! tmp_ada_firstpass!0_uint1);
  }
}

uint2 t477a.mm;

