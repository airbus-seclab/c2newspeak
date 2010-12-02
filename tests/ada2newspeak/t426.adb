Procedure T426 is
   subtype R is Integer range 1 .. 3;
   type toto is array (R) of Integer;

   DATA :  toto ;
   DATA2 :  toto ;

begin

   DATA :=  DATA2;

end t426;
