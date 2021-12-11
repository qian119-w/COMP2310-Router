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
      -- Inter-Router Msgs --
      Comm_Queue : Protected_Vector_Pkg.Protected_V;
      package Protected_Vector_Pkg_2 is new Protected_Vectors (Element => Maybe_Client_Msg);
      -- Msgs that reached intermediate routers --
      Client_Queue : Protected_Vector_Pkg_2.Protected_V;
      package Protected_Vector_Pkg_3 is new Protected_Vectors (Element => Client_Msg);
      -- Msgs that reached destination --
      Storage : Protected_Vector_Pkg_3.Protected_V;

   begin
      accept Configure (Links : Ids_To_Links) do
         Connected_Routers := Links;
      end Configure;

      declare
         Port_List : constant Connected_Router_Ports := To_Router_Ports (Task_Id, Connected_Routers);
         -- local copy of link-states --
         Local_Linkages : Linkage_Array;
         Visited : Vector_Pkg.Vector;
         -- Shortest Paths table --
         Par_Table : Par_Array;
         Start_Forward : Flag;
         task type Worker;
         Limit : constant Positive := 5;
         Workers : array (1 .. Limit) of Worker;
         pragma Unreferenced (Workers);

         procedure Send_Self_Message is
         begin
            declare
               M : Inter_Msg;
            begin
               M.Sender := Task_Id;
               Comm_Queue.Enqueue (Item => Inter_Msg_Maybe.Valid_Value (E => M));
            end;
         end Send_Self_Message;

         -- send inter-router message to neighbours --
         procedure Send (Msg : Inter_Msg) is
         begin
            for Port of Port_List loop
               begin
                  Port.Link.all.Comm (Msg);
               exception
                  when Tasking_Error => null;
               end;
            end loop;
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

         -- compute shortest path tree --
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
                     -- self message --
                     -- -> check self current link-state --
                     if Msg.Sender = Task_Id then
                        declare
                           -- neighbours --
                           N : Vector_Pkg.Vector;
                           -- unresponsive neighbours --
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
                                 -- dropped routers have no linkages --
                                 N_D : Vector_Pkg.Vector;
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
                        -- msg comes from others -> check updates --
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

         task body Worker is
            M : Maybe_Client_Msg;
         begin
            Client_Queue_Loop : loop
               Start_Forward.Wait;
               Client_Queue.Dequeue (Item => M);
               if not M.Valid then
                  exit Client_Queue_Loop;
               end if;
               declare
                  Client_M : Client_Msg := M.Value;
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
                              Send_Self_Message;
                        end;
                        exit;
                     end if;
                  end loop;
               end;
            end loop Client_Queue_Loop;
         end Worker;

      begin
         -- initial link-state message --
         Send_Self_Message;
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
                     begin
                        Storage.Dequeue (Item => M);
                        Msg.Sender := M.Sender;
                        Msg.The_Message := M.The_Message;
                        Msg.Hop_Counter := M.Hop_Counter;
                     end;
                  end Receive_Message;
            or
               accept Shutdown;
               Comm_Queue.Enqueue (Item => Inter_Msg_Maybe.Invalid_Value);
               for I in 1 .. Limit loop
                  Client_Queue.Enqueue (Item => Client_Msg_Maybe.Invalid_Value);
               end loop;
               exit;
            end select;
         end loop;
      end;

   exception
      when Exception_Id : others => Show_Exception (Exception_Id);
   end Router_Task;

end Generic_Router;
