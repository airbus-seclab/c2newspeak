Package body T497a is
   Procedure Inc_in (X : in Integer; Y : out Integer) is
   begin
      Y := X + 1;
   end Inc_in;
    Procedure Inc_in2 (X : in Integer; Y : in out Integer; Z : in out Integer) is
   begin
      Y := X + 1;
      Z := X + 1;
   end Inc_in2;

   Procedure Inc_in3 (X : in Integer; Y : out Integer) is
   begin
      Y := X + 1;
   end Inc_in3;

   Procedure Incr (X : in out Integer) is
   begin
      X := X ;
   end Incr;

end T497a;
