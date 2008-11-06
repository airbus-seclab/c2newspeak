-- procedure null

package body T000 is

   type UINT16  is range 0 .. 65535;
   subtype INDEX is UINT16 range 1 ..  UINT16'LAST;
   procedure donull is
   begin
      null;
   end donull;

end T000;
