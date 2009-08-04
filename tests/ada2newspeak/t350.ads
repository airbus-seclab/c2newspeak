-- Package specification for t350.adb.
--
-- Author : Etienne Millon
-- Date   : Tue Aug  4 2009
--
package t350 is
  type R is record
    X : Integer;
    Y : Integer;
  end record;

  function F (P : Integer) return R;

  procedure main;
end t350;
