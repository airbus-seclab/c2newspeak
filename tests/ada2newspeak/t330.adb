-- 'size attribute for types.
--
-- Author : Etienne Millon
-- Date   : Wed Jul 29 2009
--
procedure t330 is
  type R is record
    F1 : INTEGER;
    F2 : BOOLEAN;
  end record;
  type SBYTE is range -128..127;
  subtype INDEX is INTEGER range 1 .. 10;
  type A is array (INDEX) of INTEGER;
  X : INTEGER;
begin
    X := INTEGER'SIZE;  -- 32
    X := FLOAT'SIZE;    -- 32
    X := BOOLEAN'SIZE;  -- 1
    X := R'SIZE;        -- 33
    X := SBYTE'SIZE;    -- 8
    X := A'SIZE;        -- 320
end t330 ;
