-- résultat identtique à équivalent c2newspeak
package body T033 is

   function F(Y:Entier) return entier is
   begin
      return (Y+1);
   end F;

   procedure AppelFonction is

      X : entier;
   begin

      X := F(2);
   end AppelFonction;

end T033;
