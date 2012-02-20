f : () -> ()
void (013.c:12#5)^f(void) {
  (013.c:14#8)^Int x;
  (013.c:14#8)^x =(int32) (0 : Int);
  (013.c:15#8)^{
    Int y;
    (013.c:16#4)^{
      Ptr (Int) tmp_cir!0;
      (013.c:16#4)^tmp_cir!0: Ptr (Int) <- memcpy((focus32 (&(y) : Ptr (Int)) : Ptr (Int)): Ptr (Int), (focus32 (&(x) : Ptr (Int)) : Ptr (Int)): Ptr (Int), (4 : Int): Int);
    }
    (013.c:17#8)^{
      Int z;
      (013.c:17#8)^z =(int32) (coerce[-2147483648,2147483647] (((y_Int : Int) + (2 : Int)) : Int) : Int);
    }
  }
}

memcpy : (Ptr (Int) * Ptr (Int) * Int) -> (Ptr (Int))
Ptr (Int) (013.c:1#6)^memcpy(Ptr (Int) dst, Ptr (Int) src, Int n) {
  (013.c:5#18)^Int i;
  (013.c:4#10)^Ptr (Int) s;
  (013.c:3#10)^Ptr (Int) d;
  (013.c:3#10)^d =(ptr) (dst_Ptr (Int) : Ptr (Int));
  (013.c:4#10)^s =(ptr) (src_Ptr (Int) : Ptr (Int));
  (013.c:5#18)^i =(uint32) (0 : Int);
  (013.c:6#9)^i =(uint32) (0 : Int);
  (013.c:6#4)^do {
    (013.c:6#4)^while (1) {
      (013.c:6#4)^choose {
       -->
        (013.c:6#4)^guard((((n_Int : Int) > (i_Int : Int)) : Int));
       -->
        (013.c:6#4)^guard((! (((n_Int : Int) > (i_Int : Int)) : Int) : Int));
        (013.c:6#4)^goto lbl1;
      }
      (013.c:7#8)^[(((d_Ptr (Int) : Ptr (Int)) + (((i_Int : Int) * (8 : Int)) : Int)) : Ptr (Int))]8 =(int8) ([(((s_Ptr (Int) : Ptr (Int)) + (((i_Int : Int) * (8 : Int)) : Int)) : Ptr (Int))]8_Int : Int);
      (013.c:6#17)^i =(uint32) (coerce[0,4294967295] (((i_Int : Int) + (1 : Int)) : Int) : Int);
    }
  } with lbl1:
  (013.c:9#4)^!return =(ptr) (dst_Ptr (Int) : Ptr (Int));
}

