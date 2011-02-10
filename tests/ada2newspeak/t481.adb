with T481a;


procedure T481 (DATA: in T481a.P)  is
   X : Integer;
   DATA2 :  T481a.toto2;
begin
    for I in T481a.R loop
       DATA2.NA(I) := DATA.NAV1.NA(I);
    end loop;
end t481;
