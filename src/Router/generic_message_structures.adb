package body Generic_Message_Structures is

   protected body Linkage is
      procedure Update (Msg : Inter_Msg) is
      begin
         if Msg.Msg_Seq_No > Local_Seq_No then
            Local_Seq_No := Msg.Msg_Seq_No;
            Links := Msg.Neighbours;
         end if;
      end Update;

      function Read_Seq_No return Natural is (Local_Seq_No);
      function Read_Neighbours return Vector_Pkg.Vector is (Links);
   end Linkage;

end Generic_Message_Structures;
