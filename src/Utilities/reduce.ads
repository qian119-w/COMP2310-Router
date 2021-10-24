--
--  Uwe R. Zimmer, Australia, September 2020
--

generic

   type Element is private;
   type Index is (<>);
   type Data is array (Index) of Element;

   Neutral, Final : Element;
   with function Combine (Left, Right : Element) return Element;

function Reduce (Elements : Data; I : Index := Index'First; Carry : Element := Neutral) return Element;
