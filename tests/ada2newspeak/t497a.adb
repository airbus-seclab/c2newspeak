Package body T497a is
   Procedure Inc_in (X : in Integer; Y : out Integer) is
   begin
      Y := X + 1;
   end Inc_in;
   Procedure Incr (X : in out Integer) is
   begin
      X := X ;
   end Incr;
end T497a;
