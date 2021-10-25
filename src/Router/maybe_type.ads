generic 
   type Element is private;
package Maybe_Type is
   type Maybe (Valid : Boolean := False) is 
          record
             case Valid is
                when True => Value : Element;
                when False => null;
             end case;
          end record;
   
   Invalid_Value : constant Maybe := (Valid => False);
   function Valid_Value (E : Element) return Maybe is 
      (Valid => True, Value => E);
end Maybe_Type;
