package body T106 is
   type Enum is (X);
   procedure Main is
      type E is (A,X);
      Val : constant Enum := X;
   begin
      null;
   end Main;

end T106;
