-- An (non-null) instruction after a return statement should report a warning
procedure t008 is
  X : Integer;
begin
  return;
  X := 1;
end t008;
