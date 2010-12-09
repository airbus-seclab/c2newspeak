Warning: t406a.ads:2#6: multiple definitions of global variable t406a.y, in file t406a.ads variable t406a.y should probably be extern accepted
Newspeak output
---------------
t406.adb
t406a.adb
void t406(void) {
  (t406.adb:7#5)^{ float32 32; int32 0; }64 x;
  (t406.adb:10#11)^t406a.t(x_{ float32 32; int32 0; }64: { float32 32; int32 0; }64);
}

void t406a.t({ float32 32; int32 0; }64 p) {
}


