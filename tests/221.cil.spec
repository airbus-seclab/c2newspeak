Newspeak output
---------------
221.c
f() {
  (221.c:29#1138)^221.c.x =(int32) coerce[-2147483648,2147483647] (221.c.x_int32 + 1);
}

main() {
  (221.c:33#1168)^f();
  (221.c:34#1175)^f();
}

int32 221.c.x = {0: int32 3};

