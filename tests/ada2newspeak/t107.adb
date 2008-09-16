package body T107 is
   type Enum is (X);
   procedure Main is
      X : constant := 2;
      Val : constant Enum := X;
   begin
      null;
   end Main;

end T107;
