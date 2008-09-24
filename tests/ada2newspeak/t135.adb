procedure T135 is

   subtype Iter is Integer range 0..100;
   type T is range -2**7..2**7-1;

   type TabT is array (Iter) of T;
   X : TabT;
begin
   null;
end T135;
