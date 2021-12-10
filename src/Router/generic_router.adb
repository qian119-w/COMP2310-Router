--
--  Framework: Uwe R. Zimmer, Australia, 2019
--

with Exceptions; use Exceptions;
with Protected_Vectors;
with Ada.Text_IO; use Ada.Text_IO;
package body Generic_Router is

   task body Router_Task is

      Connected_Routers : Ids_To_Links;
      package Protected_Vector_Pkg is new Protected_Vectors (Element => Maybe_Inter_Msg);
      Comm_Queue : Protected_Vector_Pkg.Protected_V;
      package Protected_Vector_Pkg_2 is new Protected_Vectors (Element => Maybe_Client_Msg);
      Client_Queue : Protected_Vector_Pkg_2.Protected_V;
      package Protected_Vector_Pkg_3 is new Protected_Vectors (Element => Client_Msg);
      Storage : Protected_Vector_Pkg_3.Protected_V;

   begin
      accept Configure (Links : Ids_To_Links) do
         Connected_Routers := Links;
      end Configure;

      declare
         Port_List : constant Connected_Router_Ports := To_Router_Ports (Task_Id, Connected_Routers);
         -- store the neighbour information of all routers
         Local_Linkages : Linkage_Array;
         Visited : Vector_Pkg.Vector;
         -- connectivity table --
         Par_Table : Par_Array;
         Start_Forward : Flag;
         task type Worker is
            entry Worker_Comm (Msg : in Inter_Msg);
            entry Worker_Forward (Msg : in Client_Msg);
         end Worker;
         Limit : constant Positive := 3;
         Workers : array (1 .. Limit) of Worker;

         procedure Send_Own_Message is
         begin
            declare
               M : Inter_Msg;
            begin
               M.Sender := Task_Id;
               Comm_Queue.Enqueue (Item => Inter_Msg_Maybe.Valid_Value (E => M));
            end;
         end Send_Own_Message;

         procedure Send (Msg : Inter_Msg) is
         begin
            Outer : loop
               for W of Workers loop
                  select
                     W.Worker_Comm (Msg);
                     exit Outer;
                  or
                     delay 0.01;
                  end select;
               end loop;
            end loop Outer;
         end Send;

         -- remove only the affected nodes on the shortest path tree after link-state changes have been detected --
         -- enable partial recomputation --
         procedure Change_Affected_Nodes (Idx : Router_Range) is
         begin
            for R in Router_Range'Range loop
               declare
                  Del : Boolean := False;
                  I : Router_Range := R;
               begin
                  while Par_Table (I) /= Task_Id loop
                     if Par_Table (I) = Idx then
                        Del := True;
                        exit;
                     else
                        I := Par_Table (I);
                     end if;
                  end loop;
                  if Del then
                     declare
                        Ind : constant Vector_Pkg.Extended_Index := Visited.Find_Index (Item  => R);
                     begin
                        if Ind /= Vector_Pkg.No_Index then
                           Visited.Delete (Index => Ind);
                        end if;
                     end;
                  end if;
               end;
            end loop;
         end Change_Affected_Nodes;

         procedure Compute_Graph (Update : Boolean) is
            All_Received : Boolean := True;
         begin
            if Natural (Local_Linkages (Task_Id).Links.Length) = Natural (Router_Range'Last) - 1 then
               if Update then
                  Put_Line (Router_Range'Image (Task_Id));
                  for Idx in Router_Range'Range loop
                     Par_Table (Idx) := Task_Id;
                  end loop;
               end if;
               Start_Forward.Change_Flag (B => True);
            else
               for Idx in Router_Range'Range loop
                  -- a variation from the original link-state protocol --
                  -- I only used sequence numbers to check if all link-state messages have been collected --
                  if Local_Linkages (Idx).Local_Seq_No < 1 then
                     All_Received := False;
                  end if;
               end loop;
               if All_Received then
                  if Update then
                     Put_Line (Router_Range'Image (Task_Id) & " received");
                     -- build the shortest path tree; record as a parent table Par_Table --
                     declare
                        Searched : array (Router_Range) of Boolean := (others => False);
                     begin
                        if not Visited.Contains (Item => Task_Id) then
                           Visited.Append (New_Item => Task_Id);
                           Par_Table (Task_Id) := Task_Id;
                        end if;
                        loop
                           exit when Natural (Visited.Length) = Natural (Router_Range'Last);
                           declare
                              Current_Last_Idx : constant Positive := Visited.Last_Index;
                           begin
                              for Idx in 1 .. Current_Last_Idx loop
                                 if not Searched (Visited.Element (Index => Idx)) then
                                    for N of Local_Linkages (Visited.Element (Index => Idx)).Links loop
                                       if not Visited.Contains (Item => N) then
                                          Visited.Append (New_Item => N);
                                          Par_Table (N) := Visited.Element (Index => Idx);
                                       end if;
                                    end loop;
                                    Searched (Visited.Element (Index => Idx)) := True;
                                 end if;
                              end loop;
                           end;
                        end loop;
                     end;
                  end if;
                  Start_Forward.Change_Flag (B => True);
               end if;
            end if;
         end Compute_Graph;

         task Fetcher;
         task body Fetcher is
         begin
            Fetching : loop
               declare
                  M : Maybe_Inter_Msg;
               begin
                  Comm_Queue.Dequeue (Item => M);
                  if not M.Valid then
                     exit Fetching;
                  end if;
                  declare
                     Msg : constant Inter_Msg := M.Value;
                  begin
                     -- msg comes from myself -> check own current link-state
                     if Msg.Sender = Task_Id then
                        declare
                           N : Vector_Pkg.Vector; -- neighbours
                           Dropped : Vector_Pkg.Vector;
                           Recompute : Boolean := False;
                        begin
                           for Port of Port_List loop
                              N.Append (New_Item => Port.Id);
                              begin
                                 Port.Link.all.Responsive;
                              exception
                                 when Tasking_Error =>
                                    Dropped.Append (New_Item => Port.Id);
                              end;
                           end loop;
                           if Natural (Local_Linkages (Task_Id).Links.Length) /= Natural (N.Length) then
                              Recompute := True;
                              Local_Linkages (Task_Id).Links := N;
                              Local_Linkages (Task_Id).Local_Seq_No := Local_Linkages (Task_Id).Local_Seq_No + 1;
                              declare
                                 Update_M : Inter_Msg;
                              begin
                                 Update_M.Sender := Task_Id;
                                 for Nei of N loop
                                    Update_M.Length := Update_M.Length + 1;
                                    Update_M.Neighbours (Update_M.Length) := Nei;
                                 end loop;
                                 Send (Msg => Update_M);
                              end;
                           end if;
                           for Idx in Dropped.First_Index .. Dropped.Last_Index loop
                              declare
                                 N_D : Vector_Pkg.Vector; -- neighbours of a dropped router is nil
                              begin
                                 if Natural (Local_Linkages (Dropped.Element (Index => Idx)).Links.Length) /= Natural (N_D.Length) then
                                    Recompute := True;
                                    Change_Affected_Nodes (Idx => Dropped.Element (Index => Idx));
                                    Local_Linkages (Dropped.Element (Index => Idx)).Links := N_D;
                                    Local_Linkages (Dropped.Element (Index => Idx)).Local_Seq_No
                                      := Local_Linkages (Dropped.Element (Index => Idx)).Local_Seq_No + 1;
                                    declare
                                       Update_M : Inter_Msg;
                                    begin
                                       Update_M.Sender := Dropped.Element (Index => Idx);
                                       Send (Msg => Update_M);
                                    end;
                                 end if;
                              end;
                           end loop;
                           if Recompute then
                              Start_Forward.Change_Flag (B => False);
                           end if;
                           Compute_Graph (Update => Recompute);
                        end;
                        -- msg comes from others -> check updates
                     else
                        -- if I am connected to all other routers --
                        -- -> no need to store link-state messages (waste of storage!)
                        -- -> an optimization for Star/Fully_Connected topology
                        -- pass to others who need it --
                        if Natural (Local_Linkages (Task_Id).Links.Length) = Natural (Router_Range'Last) - 1 then
                           Start_Forward.Change_Flag (B => True);
                           if Msg.Length /= Natural (Router_Range'Last) - 1
                             and then Local_Linkages (Msg.Sender).Local_Seq_No = 0
                           then
                              Send (Msg => Msg);
                           end if;
                           Local_Linkages (Msg.Sender).Local_Seq_No :=
                                   Local_Linkages (Msg.Sender).Local_Seq_No + 1;
                        else
                           declare
                              Recompute : Boolean := False;
                           begin
                              -- check updates of link-state --
                              if Natural (Local_Linkages (Msg.Sender).Links.Length) /= Msg.Length then
                                 if Msg.Length = 0 then
                                    Change_Affected_Nodes (Idx => Msg.Sender);
                                 end if;
                                 Recompute := True;
                                 Local_Linkages (Msg.Sender).Links.Clear;
                                 for Idx in 1 .. Msg.Length loop
                                    Local_Linkages (Msg.Sender).Links.Append (New_Item => Msg.Neighbours (Idx));
                                 end loop;
                                 Local_Linkages (Msg.Sender).Local_Seq_No :=
                                   Local_Linkages (Msg.Sender).Local_Seq_No + 1;
                                 Send (Msg => Msg);
                              end if;
                              if Recompute then
                                 Start_Forward.Change_Flag (B => False);
                              end if;
                              Compute_Graph (Update => Recompute);
                           end;
                        end if;
                     end if;
                  end;
               end;
            end loop Fetching;
         end Fetcher;

         task Client_Fetcher;
         task body Client_Fetcher is
         begin
            F : loop
               declare
                  M : Maybe_Client_Msg;
               begin
                  Client_Queue.Dequeue (Item => M);
                  if not M.Valid then
                     exit F;
                  end if;
                  Start_Forward.Wait;
                  Outer : loop
                     for W of Workers loop
                        select
                           W.Worker_Forward (M.Value);
                           exit Outer;
                        or
                           delay 0.01;
                        end select;
                     end loop;
                  end loop Outer;
               end;
            end loop F;
         end Client_Fetcher;

         task body Worker is
            M : Inter_Msg;

            Client_M : Client_Msg;
         begin
            loop
               select
                  accept Worker_Comm (Msg : in Inter_Msg) do
                     M := Msg;
                  end Worker_Comm;

                  for Port of Port_List loop
                     begin
                        Port.Link.all.Comm (M);
                     exception
                        when Tasking_Error => null;
                     end;
                  end loop;
               or
                  accept Worker_Forward (Msg : in Client_Msg) do
                     Client_M := Msg;
                  end Worker_Forward;
                  if not Start_Forward.Read_Flag then
                     Client_Queue.Enqueue (Client_Msg_Maybe.Valid_Value (E => Client_M));
                  else
                     declare
                        Idx : Router_Range := Client_M.Destination;
                     begin
                        while Par_Table (Idx) /= Task_Id loop
                           Idx := Par_Table (Idx);
                        end loop;
                        for Port of Port_List loop
                           if Port.Id = Idx then
                              begin
                                 Client_M.Hop_Counter := Client_M.Hop_Counter + 1;
                                 Port.Link.all.Forward (Client_M);
                              exception
                                 when Tasking_Error =>
                                    Client_M.Hop_Counter := Client_M.Hop_Counter - 1;
                                    Client_Queue.Enqueue (Client_Msg_Maybe.Valid_Value (E => Client_M));
                                    Send_Own_Message; -- re-check own link-state
                              end;
                              exit;
                           end if;
                        end loop;
                     end;
                  end if;
               or
                  terminate;
               end select;
            end loop;
         end Worker;

      begin
         -- initial link-state message --
         Send_Own_Message;
         loop
            select
               accept Responsive;
            or
               accept Comm (Msg : in Inter_Msg) do
                  Comm_Queue.Enqueue (Item => Inter_Msg_Maybe.Valid_Value (E => Msg));
               end Comm;
            or
               accept Forward (Msg : in Client_Msg) do
                  if Msg.Destination = Task_Id then
                     Storage.Enqueue (Item => Msg);
                  else
                     Client_Queue.Enqueue (Item => Client_Msg_Maybe.Valid_Value (E => Msg));
                  end if;
               end Forward;
            or
               accept Send_Message (Msg : in Messages_Client) do
                  declare
                     M_Processed : Client_Msg;
                  begin
                     M_Processed.Sender := Task_Id;
                     M_Processed.Destination := Msg.Destination;
                     M_Processed.The_Message := Msg.The_Message;
                     Client_Queue.Enqueue (Item => Client_Msg_Maybe.Valid_Value (E => M_Processed));
                  end;
               end Send_Message;
            or
               when not Storage.Is_Empty =>
                  accept Receive_Message (Msg : out Messages_Mailbox) do
                     declare
                        M : Client_Msg;
                        Out_M : Messages_Mailbox;
                     begin
                        Storage.Dequeue (Item => M);
                        Out_M.Sender := M.Sender;
                        Out_M.The_Message := M.The_Message;
                        Out_M.Hop_Counter := M.Hop_Counter;
                        Msg := Out_M;
                     end;
                  end Receive_Message;
            or
               accept Shutdown;
               Comm_Queue.Enqueue (Item => Inter_Msg_Maybe.Invalid_Value);
               Client_Queue.Enqueue (Item => Client_Msg_Maybe.Invalid_Value);
               exit;
            end select;
         end loop;
      end;

   exception
      when Exception_Id : others => Show_Exception (Exception_Id);
   end Router_Task;

end Generic_Router;
