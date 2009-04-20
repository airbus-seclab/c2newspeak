-- Declarative items in a declarative_part shall get out of scope after the
-- "end" keyword. Therefore, "T" is not visible when used.
--
-- Author : Etienne Millon
-- Date   : Mon Apr 20 2009
--
with t248a;
procedure t248 is
begin
  declare
    use t248a;
  begin
    null;
  end;
  declare
    X : T;
  begin
    null;
  end;
end t248;
