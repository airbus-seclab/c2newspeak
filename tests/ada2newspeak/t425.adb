with   t425a;
use   t425a;
with   t425b;

Procedure T425 is

   DATA :  T425b.toto ;
   L : Integer := 6;
begin
   DATA := ( T425a.GET_DATA(2))*L;
end t425;
