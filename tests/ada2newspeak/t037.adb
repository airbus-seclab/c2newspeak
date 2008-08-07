package body T037 is

   function F(X: Integer) return Enum is
   begin
      return A;
   end F;

   procedure AppelFonction is
      X : Enum;
   begin

      X := F(10);
      X := F;
   end AppelFonction;

end T037;
