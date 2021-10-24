--
--  Uwe R. Zimmer, Australia, September 2020
--

function Reduce (Elements : Data; I : Index := Index'First; Carry : Element := Neutral) return Element is

begin
   return (if Carry = Final
           then Carry
           elsif I = Index'Last
           then Combine (Carry, Elements (I))
           else Reduce (Elements, Index'Succ (I), Combine (Carry, Elements (I))));
end Reduce;
