procedure T319 is

   subtype Iter is Integer range 2..100;

   type TabT is array (Iter) of Integer;
   X : TabT;
   Y : Integer;
begin
   Y := X(4);
end T319;
