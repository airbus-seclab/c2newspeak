Newspeak output
---------------
void main(void) {
  (504.c:30#2)^choose {
   -->
    (504.c:30#2)^choose {
     -->
      (504.c:30#2)^guard(! (a_int32 ==_int32 0));
      (504.c:30#2)^choose {
       -->
        (504.c:30#2)^guard(! (b_int32 ==_int32 0));
       -->
        (504.c:30#2)^guard((b_int32 ==_int32 0));
        (504.c:30#2)^guard(! (c_int32 ==_int32 0));
      }
     -->
      (504.c:30#2)^guard((a_int32 ==_int32 0));
      (504.c:30#2)^guard(! (c_int32 ==_int32 0));
    }
    (504.c:31#4)^x =(int32) 0;
    (504.c:32#4)^x =(int32) 1;
    (504.c:33#4)^x =(int32) 2;
    (504.c:34#4)^x =(int32) 3;
    (504.c:35#4)^x =(int32) 4;
   -->
    (504.c:30#2)^choose {
     -->
      (504.c:30#2)^guard(! (a_int32 ==_int32 0));
      (504.c:30#2)^guard((b_int32 ==_int32 0));
      (504.c:30#2)^guard((c_int32 ==_int32 0));
     -->
      (504.c:30#2)^guard((a_int32 ==_int32 0));
      (504.c:30#2)^guard((c_int32 ==_int32 0));
    }
    (504.c:37#4)^x =(int32) 0;
    (504.c:38#4)^x =(int32) 1;
    (504.c:39#4)^x =(int32) 2;
    (504.c:40#4)^x =(int32) 3;
    (504.c:41#4)^x =(int32) 4;
    (504.c:42#4)^x =(int32) 5;
  }
}

int32 a;
int32 b;
int32 c;
int32 x;

