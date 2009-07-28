procedure T202 is

   subtype Iter is Integer range 2..100;
   type T is range -2**7..2**7-1;

   type TabT is array (Iter) of T;
   X : TabT;
   Y : T;
begin
   Y := X(4);
end T202;
