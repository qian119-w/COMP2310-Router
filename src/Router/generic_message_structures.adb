package body Generic_Message_Structures is

   protected body Flag is
      procedure Change_Flag (B : Boolean) is
      begin
         F := B;
      end Change_Flag;
      entry Wait when F is
      begin
         null;
      end;
   end Flag;

end Generic_Message_Structures;
