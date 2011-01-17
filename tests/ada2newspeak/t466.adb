procedure T466 is
   type Toto is ( A, B, C);
   type Titi is ( A, B, C);
   R : Titi;
   TMP : Integer;
begin
   case (R) is
      when A => TMP := 1;
      when B => TMP := 2;
      when C => TMP := 3;
   end case;

end T466;
