package body t089 is

   type Jour is (Lundi, Mardi, Mercredi, Jeudi, Vendredi, Samedi, Dimanche);
   subtype JourChome is Jour range vendredi..Dimanche;

   type Weekend is new JourChome range samedi..dimanche;

   subtype T1 is Integer range 0..5;
   subtype T2 is Integer range 10..15;

   type Dt is new T2;

   function Jourdusoleil return Jourchome is
   begin
      return samedi;
   end Jourdusoleil;

   function B return T1 is
   begin
      return 2;
   end B;

   function A return Dt is
   begin
      return 12;
   end A;


   procedure Main is
      X : Integer;
      Y : Weekend;
      Z : Dt;
      W : Jour;
   begin
      X := B;
      Y := Samedi;
      Z := A;
      W := JourDuSoleil;
   end Main;

end t089;
