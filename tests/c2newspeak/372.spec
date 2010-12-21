Newspeak output
---------------
void f(void) {
}

void main(void) {
  (372.c:31#6)^int32 a;
  (372.c:31#9)^int32 b;
  (372.c:32#2)^choose {
   -->
    (372.c:32#2)^choose {
     -->
      (372.c:32#2)^guard(! (a_int32 ==_int32 0));
     -->
      (372.c:32#2)^guard((a_int32 ==_int32 0));
      (372.c:32#2)^guard(! (b_int32 ==_int32 0));
    }
    (372.c:33#4)^f();
   -->
    (372.c:32#2)^guard((a_int32 ==_int32 0));
    (372.c:32#2)^guard((b_int32 ==_int32 0));
  }
}


