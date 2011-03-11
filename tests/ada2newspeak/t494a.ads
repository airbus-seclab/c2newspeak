package T494a is
   subtype Under is Integer range 2..3;
   procedure F ( X  : in out  Integer; XX  : in Integer);
   procedure G ( X  : in Under);
end t494a;
