procedure T483 is
   Mi : constant float := -0.5 ;
   Ma : constant float :=  0.5 ;
   subtype T is FLOAT range Mi .. MA;
   C_M         : constant FLOAT   := 2.0**16 ;
   C :constant FLOAT := Float( T'Last - T'FIRST) / C_M;
begin
   null;
end T483;
