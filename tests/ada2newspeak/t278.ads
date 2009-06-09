-- Package specification for t278.adb.
--
-- Author : Etienne Millon
-- Date   : Tue Jun  9 2009
--
package t278 is
  procedure a;
  procedure b renames a;
  procedure c renames b;
  procedure main;
end t278;
