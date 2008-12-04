-- procedure null

package body T215 is

   type UINT16  is range 0 .. 65535;

   subtype INDEX is UINT16 range UINT16'FIRST .. UINT16'LAST;

   A : UINT16;

   procedure donull is
   begin
      A := UINT16'Last;
      A := UINT16'FIRST;
      A := UINT16'First - 1;

      A := UINT16'First - UINT16'LAST;
      A := INDEX'First  - INDEX'Last;
      A :=  UINT16'LENGTH;

      null;

   end donull;

end T215;
