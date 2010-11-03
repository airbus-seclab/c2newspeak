package T400 is
     subtype INDEX  is Integer range 1 .. 4;
     type    ARR is array ( INDEX ) of  Integer;
     function S_MUL (
                    LEFT  : in  ARR;
                    RIGHT : in  ARR)
                return  Integer;
 end T400;

