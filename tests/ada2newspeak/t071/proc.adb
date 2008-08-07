with P1, P2;
use P1, P2;
procedure Proc is
   X : constant P1.Enum := F;
   Y : constant P2.Enum := F;
   Z : constant P2.Enum := Y;
begin
   null;
end Proc;
