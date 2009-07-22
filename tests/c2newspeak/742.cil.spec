Newspeak output
---------------
742.c
void main(void) {
  (742.c:29#1065)^choose {
   -->
    (742.c:29#1065)^guard(! (r_int32 ==_int32 0));
    (742.c:29#1065)^s =(int32) y_int32;
   -->
    (742.c:29#1065)^guard((r_int32 ==_int32 0));
    (742.c:29#1065)^s =(int32) z_int32;
  }
  (742.c:30#1086)^choose {
   -->
    (742.c:30#1086)^guard(! (r_int32 ==_int32 0));
    (742.c:30#1086)^s =(int32) y_int32;
   -->
    (742.c:30#1086)^guard((r_int32 ==_int32 0));
    (742.c:30#1086)^s =(int32) z_int32;
  }
}

int32 r;
int32 s;
int32 y;
int32 z;

