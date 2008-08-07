package body T035 is
   type Enum is (A,B,C,D);
   X, Y, Z : Entier := 0;
   procedure Proc is
   begin
      X := 2;
   end Proc;

   function Chose (A:Integer; B: Enum) return Entier is
      type Enum is (E,F,G,H);
   begin
      if (B = D) then
         Z := 3;
         return 0;
      else
         Z := 2;
         return 1;
      end if;
   end Chose;

end T035;
