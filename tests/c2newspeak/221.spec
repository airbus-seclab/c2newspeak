Newspeak output
---------------
221.c
void f(void) {
  (221.c:29#2)^!221.c.f.0.x =(int32) coerce[-2147483648,2147483647] (!221.c.f.0.x_int32 + 1);
}

void main(void) {
  (221.c:33#2)^f();
  (221.c:34#2)^f();
}

int32 !221.c.f.0.x = {0: int32 3};

