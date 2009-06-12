-- Chained renaming_declarations.
--
--    A --> B --> C
--
-- Author : Etienne Millon
-- Date   : Tue Jun  9 2009
--
package body t278 is
  procedure a is
  begin
    null;
  end;

  procedure main is
  begin
    a;
    b;
    c;
  end;
end t278;
