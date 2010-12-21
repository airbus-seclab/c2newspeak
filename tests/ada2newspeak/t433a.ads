package  t433a is

   function ORIG (Y : Integer) return Integer;

   function REN  (X : Integer) return integer renames  ORIG ;

end T433a;



