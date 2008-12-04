--package t212 is
--   procedure Blabla;
--end t212;

package body  t212 is

   type Tab is array (1 .. 3) of Integer;

   My_Ar : Tab;

   procedure blabla is
   begin
      My_Ar(2) := 4;
      My_Ar(4) := 3;
   end blabla;

end t212;
