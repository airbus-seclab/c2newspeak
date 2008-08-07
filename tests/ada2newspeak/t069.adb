procedure T069 is
   -- test type range
   type Entier is new Integer;
   A, B: Integer := 0;
   I, J: Entier := 0;
   subtype Pos100 is Entier range I..100;
begin
   null;
end T069;
