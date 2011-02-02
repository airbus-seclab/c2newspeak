procedure T478 is
   subtype Ran is Integer range 1..3;
   type Arr is array (Ran) of Integer;
   type Rec1 is record
      B : Integer;
       A : Arr;
   end record;
   type Rec2 is record
      B2 : Integer;
      A2 : Rec1;
   end record;
   C : Rec2;
   I : Integer;
  begin
   I:= C.A2.A(2);
end T478;
