--test de la taille des enums
procedure t014 is
   type Enum1 is (a1,a2,a3);
   type Enum1b is (a1,a2,A3,a4);
   type Enum2b is (a1,a2,A3,A4,A5,A6);
   type Enum2 is (a1,a2,A3,A4,A5,A6,A7,a8);
   type Enum3 is (a1,a2,A3,A4,A5,A6,A7,A8,a9);
   type Enum3b is (a1,a2,A3,A4,A5,A6,A7,A8,A9,
                  ba1,ba2,bA3,bA4,bA5,bA6,bA7,bA8,bA9);
begin
   null;
end t014;
