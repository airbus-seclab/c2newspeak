--    XXX: array(1..2) of Integer; valid mais pas dans le code

procedure T201 is

   subtype Roo is Integer range 1..5;

   type TRR is array ( Roo
                     , Roo
                       ) of Integer;

   T : TRR;

begin
 T(5)(6) :=  4;
   --T(5) :=  4;
null;
end T201;
