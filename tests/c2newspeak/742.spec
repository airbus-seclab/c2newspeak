Warning: 742.c:29#0: comma in expression accepted
Warning: 742.c:30#0: comma in expression accepted
Newspeak output
---------------
void (742.c:28#5)^main(void) {
  (742.c:29#2)^choose {
   -->
    (742.c:29#2)^guard(! (r_int32 ==_int32 0));
    (742.c:29#2)^s =(int32) y_int32;
   -->
    (742.c:29#2)^guard((r_int32 ==_int32 0));
    (742.c:29#2)^s =(int32) z_int32;
  }
  (742.c:30#2)^choose {
   -->
    (742.c:30#2)^guard(! (r_int32 ==_int32 0));
    (742.c:30#2)^s =(int32) y_int32;
   -->
    (742.c:30#2)^guard((r_int32 ==_int32 0));
    (742.c:30#2)^s =(int32) z_int32;
  }
}

int32 r;
int32 s;
int32 y;
int32 z;

