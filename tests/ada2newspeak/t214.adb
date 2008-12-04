-- with a Variable as array Index

procedure T214 is

   subtype Roo is Integer range 1..3;

   type T is array ( Roo ) of Integer;

   TT : T;
   I : Integer;

begin
   TT(I) := 4;
   I := TT(I + 1);
   null;

end T214;
