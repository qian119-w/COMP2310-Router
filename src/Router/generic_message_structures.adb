package body Generic_Message_Structures is

   protected body Linkages is
      procedure Update (Msg : Inter_Msg; Multicast : out Boolean; Compute : out Boolean) is
         B : Boolean := False;
         C : Boolean := True;
      begin
         if Msg.Msg_Seq_No > L (Msg.Sender).Local_Seq_No then
            L (Msg.Sender).Local_Seq_No := Msg.Msg_Seq_No;
            L (Msg.Sender).Links := Msg.Neighbours;
            B := True;
         end if;
         for Idx in Router_Range'Range loop
            if L (Idx).Local_Seq_No = 0 then
               C := False;
            end if;
         end loop;
         Multicast := B;
         Compute := C;
      end Update;
   end Linkages;

   protected body Flag is
      procedure Change_Flag (B : Boolean) is
      begin
         F := B;
      end Change_Flag;
      function Read_Flag return Boolean is (F);
   end Flag;

end Generic_Message_Structures;
