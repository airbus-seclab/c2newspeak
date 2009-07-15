Newspeak output
---------------
374.c
void main(void) {
  (374.c:29#1)^choose {
   -->
    (374.c:29#1)^guard(! (y_int32 ==_int32 0));
    (374.c:29#1)^guard(! (z_int32 ==_int32 0));
    (374.c:29#1)^x =(int32) 1;
   -->
    (374.c:29#1)^choose {
     -->
      (374.c:29#1)^guard(! (y_int32 ==_int32 0));
      (374.c:29#1)^guard((z_int32 ==_int32 0));
     -->
      (374.c:29#1)^guard((y_int32 ==_int32 0));
    }
    (374.c:29#1)^choose {
     -->
      (374.c:29#1)^guard(! (t_int32 ==_int32 0));
      (374.c:29#1)^x =(int32) ! (u_int32 ==_int32 0);
     -->
      (374.c:29#1)^guard((t_int32 ==_int32 0));
      (374.c:29#1)^x =(int32) 0;
    }
  }
}

int32 t;
int32 u;
int32 x;
int32 y;
int32 z;

