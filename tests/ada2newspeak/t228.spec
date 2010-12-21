Warning: t228.adb:13#0: pragma 'another_pragma_with_named_args' is ignored
Warning: t228.adb:13#0: pragma 'another_pragma_with_args' is ignored
Warning: t228.adb:13#0: pragma 'another_pragma_with_arg' is ignored
Warning: t228.adb:13#0: pragma 'a_first_pragma' is ignored
Newspeak output
---------------
void t228(void) {
  (t228.adb:8#7)^int32 x;
  (t228.adb:8#7)^x =(int32) 2;
}


