Newspeak Object output
----------------------
t005.adb
Global used

Global variables

Function definitions
t005() {
  do {
    [1-_int32]32 =(int32) (0 - ((1 * 2) * [1-_int32]32_ptr));
    [1-_int32]32 =(int32) (((0 - 1) + 2) + (0 - 3));
    [1-_int32]32 =(int32) (((0 - (1 * 2)) + (0 - 3)) - (4 / 2));
    [1-_int32]32 =(int32) (((0 - ((1 / 2) * 3)) + (5 % 2)) - ((0 - 3) * (5 % 4)));
    [0-_uint1]1 =(uint1) (5 > [1-_int32]32_ptr);
    [0-_uint1]1 =(uint1) ([1-_int32]32_ptr ==_int32 5);
    [0-_uint1]1 =(uint1) (2 > [1-_int32]32_ptr);
    [0-_uint1]1 =(uint1) (3 ==_int32 2);
    [0-_uint1]1 =(uint1) (6 > [1-_int32]32_ptr);
    [0-_uint1]1 =(uint1) ((2 * 3) > [1-_int32]32_ptr);
  } with lbl0: {
  }
}



Newspeak output
---------------
t005.adb
t005() {
  (t005.adb:4#94)^[1-_int32]32 =(int32) (0 - (2 * [1-_int32]32_ptr));
  (t005.adb:5#109)^[1-_int32]32 =(int32) -2;
  (t005.adb:7#128)^[1-_int32]32 =(int32) -7;
  (t005.adb:9#151)^[1-_int32]32 =(int32) ((0 + (5 % 2)) - (-3 * (5 % 4)));
  (t005.adb:10#195)^[0-_uint1]1 =(uint1) (5 > [1-_int32]32_ptr);
  (t005.adb:11#209)^[0-_uint1]1 =(uint1) ([1-_int32]32_ptr ==_int32 5);
  (t005.adb:12#223)^[0-_uint1]1 =(uint1) ! ([1-_int32]32_ptr > 2);
  (t005.adb:13#238)^[0-_uint1]1 =(uint1) ! (2 ==_int32 3);
  (t005.adb:14#253)^[0-_uint1]1 =(uint1) (6 > [1-_int32]32_ptr);
  (t005.adb:15#267)^[0-_uint1]1 =(uint1) ! ([1-_int32]32_ptr > 6);
}

