Newspeak output
---------------
495.c
void f(void) {
  int32 tmp;
  (495.c:32#1139)^choose {
   -->
    (495.c:32#1139)^guard(! (a_int32 ==_int32 0));
    (495.c:32#1139)^choose {
     -->
      (495.c:32#1139)^guard(! (b_int32 ==_int32 0));
      (495.c:32#1139)^tmp =(int32) 1;
     -->
      (495.c:32#1139)^guard((b_int32 ==_int32 0));
      (495.c:32#1139)^choose {
       -->
        (495.c:32#1139)^guard(! (c_int32 ==_int32 0));
        (495.c:32#1139)^tmp =(int32) 1;
       -->
        (495.c:32#1139)^guard((c_int32 ==_int32 0));
        (495.c:32#1139)^tmp =(int32) 0;
      }
    }
   -->
    (495.c:32#1139)^guard((a_int32 ==_int32 0));
    (495.c:32#1139)^choose {
     -->
      (495.c:32#1139)^guard(! (c_int32 ==_int32 0));
      (495.c:32#1139)^tmp =(int32) 1;
     -->
      (495.c:32#1139)^guard((c_int32 ==_int32 0));
      (495.c:32#1139)^tmp =(int32) 0;
    }
  }
  (495.c:32#1139)^a =(int32) tmp_int32;
}

void g(void) {
  (495.c:39#1295)^a =(int32) ! (c_int32 ==_int32 0);
}

int32 a;
int32 b;
int32 c;

