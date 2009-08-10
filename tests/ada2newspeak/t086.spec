Warning: t086.adb:17#8: An interval check should be inserted here [12.5;25.2]
Warning: t086.adb:16#7: An interval check should be inserted here [10.;50.2]
Warning: t086.adb:10#6: An interval check should be inserted here [10.;50.2]
Warning: t086.adb:8#7: An interval check should be inserted here [12.5;25.2]
Warning: t086.adb:7#6: An interval check should be inserted here [10.;50.2]
Newspeak output
---------------
t086.adb
void t086(void) {
  (t086.adb:7#6)^float32 x;
  (t086.adb:8#7)^float32 x2;
  (t086.adb:10#6)^float32 a;
  (t086.adb:7#6)^2- =(float32) 12.5;
  (t086.adb:8#7)^1- =(float32) 20.;
  (t086.adb:10#6)^0- =(float32) 32.3;
  (t086.adb:16#7)^2- =(float32) 15.4;
  (t086.adb:17#8)^1- =(float32) 20.225;
}


