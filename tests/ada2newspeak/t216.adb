-- to do later R  := (A'FIRST);

package body T216 is

--   type UINT16  is range 0 .. 65535;
-- subtype INDEX is UINT16 range UINT16'FIRST .. UINT16'LAST;
--A : UINT16;

   type Jour is (L, Ma, Me, J , V, S ,D);

   type JAR is  array (Jour) of Integer;

   --   A : array (1 .. 3) of Integer; pas dans le code
   A : JAR;
   R : Integer;

   procedure donull is
   begin

      A(L):=1;
      A(Ma):=2;
      A(D):=7;
      R := (JAR'LENGTH);
      R := (JAR'FIRST);
      R := (JAR'LENGTH);
      --R  := (A'FIRST);
      null;

   end donull;

end T216;
