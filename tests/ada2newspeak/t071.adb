with T071a, T103;
use T071a, T103;
procedure Proc is
   X : constant T071a.Enum := F;
   Y : constant T103.Enum := F;
   Z : constant T103.Enum := Y;
begin
   null;
end Proc;
