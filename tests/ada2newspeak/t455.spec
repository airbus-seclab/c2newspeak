
L = {H=33f64f6c, U=6, trait = Float 6, range = <unlimited>}
R = {H=2e06a9f9, U=8, trait = Signed I[-128;127], range = <unlimited>}
Warning: t455.adb:5#8: UNSOUND static evaluation of cast

L = {H=33f64f6c, U=6, trait = Float 6, range = <unlimited>}
R = {H=2e06a9f9, U=8, trait = Signed I[-128;127], range = <unlimited>}

L = {H=33f64f6c, U=6, trait = Float 6, range = <unlimited>}
R = {H=2e06a9f9, U=8, trait = Signed I[-128;127], range = <unlimited>}
Warning: t455.adb:6#8: UNSOUND static evaluation of cast

L = {H=33f64f6c, U=6, trait = Float 6, range = <unlimited>}
R = {H=2e06a9f9, U=8, trait = Signed I[-128;127], range = <unlimited>}
Newspeak output
---------------
void t455(void) {
  (t455.adb:3#8)^float32 v;
  (t455.adb:4#8)^float32 c3;
  (t455.adb:5#8)^float32 c2;
  (t455.adb:6#8)^float32 c1;
  (t455.adb:4#8)^c3 =(float32) 3.40282e+38;
  (t455.adb:5#8)^c2 =(float32) (float32 <= int8) 1016;
  (t455.adb:6#8)^c1 =(float32) (3.40282e+38 /. (float32 <= int8) 1016);
  (t455.adb:8#12)^v =(float32) c3_float32;
  (t455.adb:9#12)^v =(float32) c1_float32;
  (t455.adb:10#12)^v =(float32) c2_float32;
}


