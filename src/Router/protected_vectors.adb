package body Protected_Vectors is

   protected body Protected_V is

      function Is_Empty return Boolean is
        (Queue.Is_Empty);

      function Size return Natural is
        (Natural (Queue.Length));

      procedure Enqueue (Item : Element) is
      begin
         Queue.Append (New_Item => Item);
      end Enqueue;

      entry Dequeue (Item : out Element) when not Queue.Is_Empty is
      begin
         Item := Queue.First_Element;
         Queue.Delete_First;
      end Dequeue;
   end Protected_V;

end Protected_Vectors;
