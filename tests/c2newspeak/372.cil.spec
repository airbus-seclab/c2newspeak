Newspeak output
---------------
372.c
void f(void) {
}

void main(void) {
  (372.c:31#1124)^int32 a;
  (372.c:31#1127)^int32 b;
  (372.c:32#1132)^choose {
   -->
    (372.c:32#1132)^guard(! (a_int32 ==_int32 0));
    (372.c:33#1150)^f();
   -->
    (372.c:32#1132)^guard((a_int32 ==_int32 0));
    (372.c:32#1132)^choose {
     -->
      (372.c:32#1132)^guard(! (b_int32 ==_int32 0));
      (372.c:33#1150)^f();
     -->
      (372.c:32#1132)^guard((b_int32 ==_int32 0));
    }
  }
}


