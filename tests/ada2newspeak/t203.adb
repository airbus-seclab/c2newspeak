procedure T203 is

   subtype Roo is Integer range 1..5;

 --  type R is range -2**7..2**7-1;

   type T is array ( Roo ) of Integer;

   TT : T;
begin
   TT(2) := 4;
   null;
end T203;
