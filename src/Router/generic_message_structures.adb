package body Generic_Message_Structures is

   protected body Linkages is
      procedure Update (Msg : Inter_Msg; Multicast : out Boolean) is
         B : Boolean := False;
      begin
         if Msg.Msg_Seq_No > Local_Seq_No then
            Local_Seq_No := Msg.Msg_Seq_No;
            Links := Msg.Neighbours;
            B := True;
         end if;
         Multicast := B;
      end Update;

      function Read_Seq_No return Natural is (Local_Seq_No);
      function Read_Neighbours return Vector_Pkg.Vector is (Links);
   end Linkages;

   protected body Flag is
      procedure Change_Flag (B : Boolean) is
      begin
         F := B;
      end Change_Flag;
      function Read_Flag return Boolean is (F);
   end Flag;

end Generic_Message_Structures;
