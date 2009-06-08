-- Binary "or" with no shortcut.
--
-- Author : Etienne Millon
-- Date   : Mon Jun  8 2009
--
procedure t275 is
  TT : Boolean := True  or True ;
  FT : Boolean := False or True ;
  TF : Boolean := True  or False;
  FF : Boolean := False or False;
begin
  null;
end t275;
