package body T034 is
   X : Integer := 1;

   function F  return Integer is
   begin
      X := X+1;
      return X;
   end F;

   procedure AppelFonction is
      Y : Integer;
   begin
      Y := F;
   end AppelFonction;

end T034;
