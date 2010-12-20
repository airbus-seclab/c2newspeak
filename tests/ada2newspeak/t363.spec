Warning: t363.adb:15#6: An interval check should be inserted here [2.;5.]
Warning: t363.adb:14#6: An interval check should be inserted here [2.;5.]
Warning: t363.adb:13#6: An interval check should be inserted here [2.;5.]
Warning: t363.adb:12#6: An interval check should be inserted here [2.;5.]
Newspeak output
---------------
void t363(void) {
  (t363.adb:10#5)^float32 f;
  (t363.adb:12#6)^f =(float32) 2.;
  (t363.adb:13#6)^f =(float32) 2.;
  (t363.adb:14#6)^f =(float32) 5.;
  (t363.adb:15#6)^f =(float32) 5.;
}


