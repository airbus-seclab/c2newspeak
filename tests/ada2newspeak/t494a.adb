package body  T494a is
  procedure F ( X  : in out Integer; XX  : in Integer) is
   begin
      X := X + XX;
   end F;
   procedure G ( X  : in Under) is
   begin
      null;
   end G;
end t494a;
