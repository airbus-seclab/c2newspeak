package body T038 is

   function A return Enum2 is
   begin
      return C;
   end A;

   procedure Appel_Fonction is
      X: Enum1;
      Y: Enum2;
   begin
      X := A;
      Y := A;
   end Appel_Fonction;
end T038;
