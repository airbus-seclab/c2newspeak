package T477b is
    type MC is ( P, H, S );
    for MC use ( P => 0, H => 1, S => 2);
    type RD is ( D, P, I, H );
    for  RD use (D=> 0 ,P => 1 ,I => 2, H => 3) ;
    type T is
         record
            R : RD;
            M : MC;
         end record;
end T477b;
