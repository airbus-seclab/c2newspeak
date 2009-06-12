-- Operator overloading.
--
-- Author : Etienne Millon
-- Date   : Tue May 19 2009
--
package body t263 is
  function  "+"  (Left, Right : Integer) return Integer is
  begin
    return 0;
  end;
  function  "-"  (Left, Right : Integer) return Integer is
  begin
    return 1;
  end;
  function  "*"  (Left, Right : Integer) return Integer is
  begin
    return 2;
  end;
  function  "/"  (Left, Right : Integer) return Integer is
  begin
    return 3;
  end;
  function "mod" (Left, Right : Integer) return Integer is
  begin
    return 4;
  end;
  function "rem" (Left, Right : Integer) return Integer is
  begin
    return 5;
  end;
  function "xor" (Left, Right : Integer) return Integer is
  begin
    return 6;
  end;
  function  "<"  (Left, Right : Integer) return Integer is
  begin
    return 7;
  end;
  function  "<=" (Left, Right : Integer) return Integer is
  begin
    return 8;
  end;
  function  ">"  (Left, Right : Integer) return Integer is
  begin
    return 9;
  end;
  function  ">=" (Left, Right : Integer) return Integer is
  begin
    return 10;
  end;
  function  "=" (Left, Right : Integer) return Integer is
  begin
    return 11;
  end;
  function  "/=" (Left, Right : Integer) return Integer is
  begin
    return 11;
  end;
end t263;
