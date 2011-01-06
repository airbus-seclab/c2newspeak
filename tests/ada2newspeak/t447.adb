with UNCHECKED_CONVERSION;
with SYSTEM;

procedure t447 is

   function ADDRESS_TO_I is new UNCHECKED_CONVERSION
     (
      SYSTEM.ADDRESS,
      Integer
     );
   ADD :SYSTEM.ADDRESS;
   I : integer;

begin
   I :=  ADDRESS_TO_I( ADD );

end t447;



