Newspeak output
---------------
374.c
void main(void) {
  int32 tmp;
  (374.c:29#1091)^choose {
   -->
    (374.c:29#1091)^guard(! (y_int32 ==_int32 0));
    (374.c:29#1091)^choose {
     -->
      (374.c:29#1091)^guard(! (z_int32 ==_int32 0));
      (374.c:29#1091)^tmp =(int32) 1;
     -->
      (374.c:29#1091)^guard((z_int32 ==_int32 0));
      (374.c:29#1091)^choose {
       -->
        (374.c:29#1091)^guard(! (t_int32 ==_int32 0));
        (374.c:29#1091)^choose {
         -->
          (374.c:29#1091)^guard(! (u_int32 ==_int32 0));
          (374.c:29#1091)^tmp =(int32) 1;
         -->
          (374.c:29#1091)^guard((u_int32 ==_int32 0));
          (374.c:29#1091)^tmp =(int32) 0;
        }
       -->
        (374.c:29#1091)^guard((t_int32 ==_int32 0));
        (374.c:29#1091)^tmp =(int32) 0;
      }
    }
   -->
    (374.c:29#1091)^guard((y_int32 ==_int32 0));
    (374.c:29#1091)^choose {
     -->
      (374.c:29#1091)^guard(! (t_int32 ==_int32 0));
      (374.c:29#1091)^choose {
       -->
        (374.c:29#1091)^guard(! (u_int32 ==_int32 0));
        (374.c:29#1091)^tmp =(int32) 1;
       -->
        (374.c:29#1091)^guard((u_int32 ==_int32 0));
        (374.c:29#1091)^tmp =(int32) 0;
      }
     -->
      (374.c:29#1091)^guard((t_int32 ==_int32 0));
      (374.c:29#1091)^tmp =(int32) 0;
    }
  }
  (374.c:29#1091)^x =(int32) tmp_int32;
}

int32 t;
int32 u;
int32 x;
int32 y;
int32 z;

