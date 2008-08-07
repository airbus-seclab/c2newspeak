package body T036 is

   X : Integer := 0;

   procedure Inc  is
   begin
      X := X+1;
   end Inc;

   procedure Set(Y : Integer) is
   begin
      X := Y;
   end Set;

   procedure AppelProcedure is
   begin
      Inc;
      Set(10);
   end AppelProcedure;

end T036;
