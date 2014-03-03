Warning: t457.adb:6#7: Ignoring representation clause for 'tf'

L = {H=374f57f2, U=9, trait = Signed I[-1024;1023], range = <unlimited>}
R = {H=2e06a9f9, U=8, trait = Signed I[-128;127], range = <unlimited>}

L = {H=374f57f2, U=9, trait = Signed I[-1024;1023], range = <unlimited>}
R = {H=2e06a9f9, U=8, trait = Signed I[-128;127], range = <unlimited>}

L = {H=3b2176e4, U=10, trait = Signed I[-4;3], range = <unlimited>}
R = {H=2e06a9f9, U=8, trait = Signed I[-128;127], range = <unlimited>}

L = {H=3b2176e4, U=10, trait = Signed I[-4;3], range = <unlimited>}
R = {H=2e06a9f9, U=8, trait = Signed I[-128;127], range = <unlimited>}

L = {H=28984fbb, U=11, trait = Float 6, range = <unlimited>}
R = {H=2e06a9f9, U=8, trait = Signed I[-128;127], range = <unlimited>}
Warning: t457.adb:11#7: UNSOUND static evaluation of cast

L = {H=28984fbb, U=11, trait = Float 6, range = <unlimited>}
R = {H=2e06a9f9, U=8, trait = Signed I[-128;127], range = <unlimited>}

L = {H=28984fbb, U=11, trait = Float 6, range = <unlimited>}
R = {H=2e06a9f9, U=8, trait = Signed I[-128;127], range = <unlimited>}
Warning: t457.adb:12#7: UNSOUND static evaluation of cast

L = {H=28984fbb, U=11, trait = Float 6, range = <unlimited>}
R = {H=2e06a9f9, U=8, trait = Signed I[-128;127], range = <unlimited>}
Newspeak output
---------------
void t457(void) {
  (t457.adb:7#8)^float32 v;
  (t457.adb:8#7)^int11 e;
  (t457.adb:9#8)^int3 e2;
  (t457.adb:10#7)^float32 c;
  (t457.adb:11#7)^float32 d;
  (t457.adb:12#7)^float32 c1;
  (t457.adb:8#7)^e =(int11) 1016;
  (t457.adb:9#8)^e2 =(int3) belongs[-4,3] 1016;
  (t457.adb:10#7)^c =(float32) 3.40282e+38;
  (t457.adb:11#7)^d =(float32) (float32 <= int8) 1016;
  (t457.adb:12#7)^c1 =(float32) (3.40282e+38 /. (float32 <= int8) 1016);
  (t457.adb:14#12)^v =(float32) c1_float32;
}


