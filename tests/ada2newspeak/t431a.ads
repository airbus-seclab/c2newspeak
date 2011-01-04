package t431a  is

   type X is (A, B , C);
   for X use ( A => 0,B => 1, C => 2);

   type Y is (AA, BB , C);
   for Y use ( AA => 2,BB => 4, C => 6);


end t431a;
