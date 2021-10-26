--
--  Framework: Uwe R. Zimmer, Australia, 2019
--

with Exceptions; use Exceptions;
with Protected_Vectors;
-- with Ada.Text_IO; use Ada.Text_IO;
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
         --     Local_Linkages : Linkages;
         Local_Linkages : array (Router_Range) of Linkage;
         -- connectivity table --
         Par_Table : Par_Array;
         Start_Forward : Flag;
         task type Worker is
            entry Worker_Comm (Msg : in Inter_Msg);
            entry Worker_Forward (Msg : in Client_Msg);
         end Worker;
         Limit : constant Positive := 5;
         Workers : array (1 .. Limit) of Worker;

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
                     Msg : Inter_Msg := M.Value;
                  begin
                     -- msg comes from myself -- check current linkage changes
                     if Msg.Sender = Task_Id then
                        declare
                           N : Vector_Pkg.Vector;
                           Current : Linkage := Local_Linkages.Read_Link (Idx => Task_Id);
                           Dropped : Vector_Pkg.Vector;
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
                           if Current.Links.Length /= N.Length then
                              declare
                                 Update_M : Inter_Msg;
                                 Has_Update : Boolean;
                                 C : Boolean;
                              begin
                                 Update_M.Sender := Task_Id;
                                 Update_M.Neighbours := N;
                                 Update_M.Msg_Seq_No := Current.Local_Seq_No + 1;

                                 Local_Linkages.Update (Msg       => Update_M,
                                                        Multicast => Has_Update,
                                                        Compute   => C);
                                 if Has_Update then
                                    Outer : loop
                                       for W of Workers loop
                                          select
                                             W.Worker_Comm (Update_M);
                                             exit Outer;
                                          else
                                             null;
                                          end select;
                                       end loop;
                                    end loop Outer;
                                 end if;
                              end;
                           end if;
                        end;
                     else -- msg comes from others -- check sequence number
                        declare
                           Has_Update : Boolean;
                           Recompute : Boolean;
                        begin
                           Local_Linkages.Update (Msg       => M,
                                                  Multicast => Has_Update,
                                                  Compute   => Recompute);
                        end;

                        Outer : loop
                           for W of Workers loop
                              select
                                 W.Worker_Comm (M.Value);
                                 exit Outer;
                              else
                                 null;
                              end select;
                           end loop;
                        end loop Outer;
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
                  end if;
               or
                  terminate;
               end select;
            end loop;
         end Worker;

      begin

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
