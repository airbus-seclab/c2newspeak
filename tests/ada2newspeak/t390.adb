with UNCHECKED_CONVERSION;
with SYSTEM;

procedure t390 is

   function ADDRESS_TO_I is new UNCHECKED_CONVERSION
     (
      SOURCE => SYSTEM.ADDRESS,
      TARGET => Integer
     );
   ADD :SYSTEM.ADDRESS;
   I : integer;

begin
   I :=  ADDRESS_TO_I( ADD );

end t390;



