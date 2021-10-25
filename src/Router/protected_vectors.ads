with Ada.Containers.Vectors;

generic
   type Element is private;
package Protected_Vectors is

   package Concrete_Vector is new Ada.Containers.Vectors (Index_Type   => Positive,
                                                          Element_Type => Element);
   use Concrete_Vector;
   protected type Protected_V is
      procedure Enqueue (Item : Element);
      entry Dequeue (Item : out Element);
      function Is_Empty return Boolean;
      function Size return Natural;
   private
      Queue : Concrete_Vector.Vector;
   end Protected_V;

end Protected_Vectors;
