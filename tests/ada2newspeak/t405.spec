Warning: t405a.ads:2#6: multiple definitions of global variable t405a.y, in file t405a.ads variable t405a.y should probably be extern accepted
Newspeak output
---------------
t405.adb
t405a.adb
void t405(void) {
  (t405.adb:6#5)^{ float32 32; int32 0; }64 x;
  (t405.adb:8#14)^t405a.troc(x_{ float32 32; int32 0; }64: { float32 32; int32 0; }64);
}

void t405a.troc({ float32 32; int32 0; }64 p) {
}


