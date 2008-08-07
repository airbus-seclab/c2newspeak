procedure T070 is
   -- test type range
   type Entier is new Integer;
   A, B: Integer := 0;
   I, J: Entier := 0;
   C : constant ENTIER := i;
   subtype Pos100 is Entier range C..100;
begin
   null;
end T070;
