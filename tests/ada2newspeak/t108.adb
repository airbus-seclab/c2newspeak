package body T108 is
   type Enum is (X);
   procedure Main is
      X : Integer;
      Val : constant Enum := X;
   begin
      null;
   end Main;

end T108;
