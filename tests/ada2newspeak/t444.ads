--TODO complicate case with 'B : B' lequel doit etre choisi?
package T444 is
   type UINT16  is range 1 .. 35;
   subtype INDEX is UINT16 range UINT16'FIRST .. UINT16'LAST;
   type AA is array (INDEX) of UINT16;
   procedure Donull;
end T444;
