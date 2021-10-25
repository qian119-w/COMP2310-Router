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
         Local_Linkages : Linkages;
         Start_Forward : Flag;
         task type Worker is
            entry Worker_Comm (Msg : in Inter_Msg);
            entry Forward (Msg : in Client_Msg);
         end Worker;
         Limit : constant Positive := 5;
         Workers : array (1 .. Limit) of Worker;

         task Compute_Graph is
            entry Start;
         end Compute_Graph;
         task body Compute_Graph is
         begin
            loop
               select
                  accept Start;

               or
                  terminate;
               end select;
            end loop;
         end Compute_Graph;

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
                           W.Forward (M.Value);
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
                  declare
                     Has_Update : Boolean;
                     Recompute : Boolean;
                  begin
                     Local_Linkages.Update (Msg       => M,
                                            Multicast => Has_Update,
                                            Compute   => Recompute);
                     if Has_Update then
                        Start_Forward.Change_Flag (B => False);
                        for Port of Port_List loop
                           Port.Link.all.Comm (M);
                        end loop;
                     end if;
                     if Recompute then
                        Compute_Graph.Start;
                     end if;
                  end;
               or
                  accept Forward (Msg : in Client_Msg) do
                     Client_M := Msg;
                  end Forward;
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
               accept Comm (Msg : in Inter_Msg) do
                  declare
                     Successful : Boolean := False;
                  begin
                     for W of Workers loop
                        select
                           W.Worker_Comm (Msg);
                           Successful := True;
                           exit;
                        else
                           null;
                        end select;
                     end loop;
                     if not Successful then
                        Inter_Storage.Enqueue (Item => Inter_Msg_Maybe.Valid_Value (E => Msg));
                     end if;
                  end;
               end Comm;
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
               accept Receive_Message (Message : out Messages_Mailbox) do
                  declare
                     Made_Up_Mailbox_Message : constant Messages_Mailbox :=
                       (Sender      => Task_Id,
                        The_Message => Message_Strings.To_Bounded_String ("I just see things"),
                        Hop_Counter => 0);
                  begin
                     Message := Made_Up_Mailbox_Message;
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
