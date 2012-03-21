memcpy : (Ptr (_a12) * Ptr (_a12) * Int) -> (Ptr (_a12))
Ptr (_a12) (012.c:1#6)^memcpy(Ptr (_a12) dst, Ptr (_a12) src, Int n) {
  (012.c:5#18)^Int i;
  (012.c:4#10)^Ptr (_a12) s;
  (012.c:3#10)^Ptr (_a12) d;
  (012.c:3#10)^d =(ptr) (dst_Ptr (_a12) : Ptr (_a12));
  (012.c:4#10)^s =(ptr) (src_Ptr (_a12) : Ptr (_a12));
  (012.c:5#18)^i =(uint32) (0 : Int);
  (012.c:6#9)^i =(uint32) (0 : Int);
  (012.c:6#4)^do {
    (012.c:6#4)^while (1) {
      (012.c:6#4)^choose {
       -->
        (012.c:6#4)^guard((((n_Int : Int) > (i_Int : Int)) : Int));
       -->
        (012.c:6#4)^guard((! (((n_Int : Int) > (i_Int : Int)) : Int) : Int));
        (012.c:6#4)^goto lbl1;
      }
      (012.c:7#8)^[(((d_Ptr (_a12) : Ptr (_a12)) + (((i_Int : Int) * (8 : Int)) : Int)) : Ptr (_a12))]8 =(int8) ([(((s_Ptr (_a12) : Ptr (_a12)) + (((i_Int : Int) * (8 : Int)) : Int)) : Ptr (_a12))]8__a12 : _a12);
      (012.c:6#17)^i =(uint32) (coerce[0,4294967295] (((i_Int : Int) + (1 : Int)) : Int) : Int);
    }
  } with lbl1:
  (012.c:9#4)^!return =(ptr) (dst_Ptr (_a12) : Ptr (_a12));
}

