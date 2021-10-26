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
      Inter_Storage : Protected_Vector_Pkg.Protected_V;
      package Protected_Vector_Pkg_2 is new Protected_Vectors (Element => Maybe_Client_Msg);
      Repository : Protected_Vector_Pkg_2.Protected_V;
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

         procedure Send (Msg : Inter_Msg) is
         begin
            Outer : loop
               for W of Workers loop
                  select
                     W.Worker_Comm (Msg);
                     exit Outer;
                  else
                     null;
                  end select;
               end loop;
            end loop Outer;
         end Send;

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
            for Idx in Router_Range'Range loop
               if Local_Linkages (Idx).Local_Seq_No < 1 then
                  All_Received := False;
               end if;
            end loop;
            if All_Received then
               if Update then
                  Put_Line (Router_Range'Image (Task_Id));
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
         end Compute_Graph;

         task Local_Link is
            entry Start;
         end Local_Link;

         task body Local_Link is
         begin
            loop
               select
                  accept Start;
                  declare
                     M : Inter_Msg;
                  begin
                     M.Sender := Task_Id;
                     Inter_Storage.Enqueue (Item => Inter_Msg_Maybe.Valid_Value (E => M));
                  end;
               or
                  terminate;
               end select;
            end loop;
         end Local_Link;

         task Fetcher;
         task body Fetcher is
         begin
            Fetching : loop
               declare
                  M : Maybe_Inter_Msg;
               begin
                  Inter_Storage.Dequeue (Item => M);
                  if not M.Valid then
                     exit Fetching;
                  end if;
                  Start_Forward.Change_Flag (B => False);
                  declare
                     Msg : constant Inter_Msg := M.Value;
                  begin
                     -- msg comes from myself -- check current linkage changes
                     if Msg.Sender = Task_Id then
                        declare
                           N : Vector_Pkg.Vector; -- neighbours
                           Current : constant Linkage := Local_Linkages (Task_Id);
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
                           if Natural (Current.Links.Length) /= Natural (N.Length) then
                              Recompute := True;
                              Local_Linkages (Task_Id).Links := N;
                              Local_Linkages (Task_Id).Local_Seq_No := Local_Linkages (Task_Id).Local_Seq_No + 1;
                              declare
                                 Update_M : Inter_Msg;
                              begin
                                 Update_M.Sender := Task_Id;
                                 Update_M.Neighbours := N;
                                 Send (Msg => Update_M);
                              end;
                           end if;
                           for Idx in Dropped.First_Index .. Dropped.Last_Index loop
                              declare
                                 N_D : Vector_Pkg.Vector; -- neighbours of a dropped router is nil
                                 Current_D : constant Linkage := Local_Linkages (Dropped.Element (Index => Idx));
                              begin
                                 if Natural (Current_D.Links.Length) /= Natural (N_D.Length) then
                                    Recompute := True;
                                    Change_Affected_Nodes (Idx => Dropped.Element (Index => Idx));
                                    Local_Linkages (Dropped.Element (Index => Idx)).Links := N_D;
                                    Local_Linkages (Dropped.Element (Index => Idx)).Local_Seq_No
                                      := Local_Linkages (Dropped.Element (Index => Idx)).Local_Seq_No + 1;
                                    declare
                                       Update_M : Inter_Msg;
                                    begin
                                       Update_M.Sender := Dropped.Element (Index => Idx);
                                       Update_M.Neighbours := N_D;
                                       Send (Msg => Update_M);
                                    end;
                                 end if;
                              end;
                           end loop;
                           Compute_Graph (Update => Recompute);
                        end;
                     else -- msg comes from others -- check updates
                        declare
                           Current : constant Linkage := Local_Linkages (Msg.Sender);
                           Recompute : Boolean := False;
                        begin
                           if Natural (Current.Links.Length) /= Natural (Msg.Neighbours.Length) then
                              if Natural (Msg.Neighbours.Length) = 0 then
                                 Change_Affected_Nodes (Idx => Msg.Sender);
                              end if;
                              Recompute := True;
                              Local_Linkages (Msg.Sender).Links := Msg.Neighbours;
                              Local_Linkages (Msg.Sender).Local_Seq_No :=
                                Local_Linkages (Msg.Sender).Local_Seq_No + 1;
                              Send (Msg => Msg); -- pass this message to the next nodes
                           end if;
                           Compute_Graph (Update => Recompute);
                        end;
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
                  Repository.Dequeue (Item => M);
                  if not M.Valid then
                     exit F;
                  end if;
                  Start_Forward.Wait;
                  Outer : loop
                     for W of Workers loop
                        select
                           W.Worker_Forward (M.Value);
                           exit Outer;
                        else
                           null;
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
                     Repository.Enqueue (Client_Msg_Maybe.Valid_Value (E => Client_M));
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
                                    Repository.Enqueue (Client_Msg_Maybe.Valid_Value (E => Client_M));
                                    Local_Link.Start;
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
         Local_Link.Start;
         loop
            select
               accept Responsive;
            or
               accept Comm (Msg : in Inter_Msg) do
                  Inter_Storage.Enqueue (Item => Inter_Msg_Maybe.Valid_Value (E => Msg));
               end Comm;
            or
               accept Forward (Msg : in Client_Msg) do
                  if Msg.Destination = Task_Id then
                     Storage.Enqueue (Item => Msg);
                  else
                     Repository.Enqueue (Item => Client_Msg_Maybe.Valid_Value (E => Msg));
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
                     Repository.Enqueue (Item => Client_Msg_Maybe.Valid_Value (E => M_Processed));
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
               Inter_Storage.Enqueue (Item => Inter_Msg_Maybe.Invalid_Value);
               Repository.Enqueue (Item => Client_Msg_Maybe.Invalid_Value);
               exit;
            end select;
         end loop;
      end;

   exception
      when Exception_Id : others => Show_Exception (Exception_Id);
   end Router_Task;

end Generic_Router;
