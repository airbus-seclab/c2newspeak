Newspeak output
---------------
void (495.c:31#5)^f(void) {
  (495.c:32#2)^choose {
   -->
    (495.c:32#2)^guard(! (a_int32 ==_int32 0));
    (495.c:32#2)^guard(! (b_int32 ==_int32 0));
    (495.c:32#2)^a =(int32) 1;
   -->
    (495.c:32#2)^choose {
     -->
      (495.c:32#2)^guard(! (a_int32 ==_int32 0));
      (495.c:32#2)^guard((b_int32 ==_int32 0));
     -->
      (495.c:32#2)^guard((a_int32 ==_int32 0));
    }
    (495.c:32#2)^a =(int32) ! (c_int32 ==_int32 0);
  }
}

void (495.c:38#5)^g(void) {
  (495.c:39#2)^a =(int32) ! (c_int32 ==_int32 0);
}

int32 a;
int32 b;
int32 c;

