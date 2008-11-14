--    XXX: array(1..2) of Integer; valid mais pas dans le code

procedure T205 is
   
   subtype Roo is Integer range 1..3;
   
   type TRR is array ( Roo, Roo, Roo ) of Integer;
   
   T : TRR;

begin
   
    T(1)(2)(3) :=  4;
   
end T205;
