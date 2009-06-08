-- Binary "and" with no shortcut.
--
-- Author : Etienne Millon
-- Date   : Mon Jun  8 2009
--
procedure t275 is
  TT : Boolean := True  and True ;
  FT : Boolean := False and True ;
  TF : Boolean := True  and False;
  FF : Boolean := False and False;
begin
  null;
end t275;
