package T480a is
   subtype Ran is Integer range 1..3;
   type T is array (Ran) of Integer;
   function G return T;
end T480a;
