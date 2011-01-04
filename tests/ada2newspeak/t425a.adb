with T425b;
package body T425a is
   function GET_DATA(X : in Integer) return  T425b.toto is
      Y : T425b.Toto;
   begin
      Y(1):= 1;
      Y(2):= X;
      Y(3):= 3;
      return(Y);
   end GET_DATA ;
end T425a;
