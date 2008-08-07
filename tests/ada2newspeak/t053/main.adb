with P1;
use P1;
with P2;
procedure Main (Z: P1.Enum) is
   use P2, p1;
begin
   null;

   P1.Proc(12);
end Main;
