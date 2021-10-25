--
--  Framework: Uwe R. Zimmer, Australia, 2019
--

with Exceptions; use Exceptions;
-- with Ada.Text_IO; use Ada.Text_IO;
package body Generic_Router is

   task body Router_Task is

      Connected_Routers : Ids_To_Links;

   begin
      accept Configure (Links : Ids_To_Links) do
         Connected_Routers := Links;
      end Configure;

      declare
         Port_List : constant Connected_Router_Ports := To_Router_Ports (Task_Id, Connected_Routers);
         pragma Unreferenced (Port_List);
         -- store the neighbour information of all routers
         Local_Linkages_Storage : array (Router_Range) of Linkage;
         task type Worker is
            entry Worker_Comm (Msg : in Inter_Msg);
         end Worker;
         Limit : constant Positive := 5;
         Workers : array (1 .. Limit) of Worker;

         protected Count is
            procedure Inc;
            procedure Dec;
            entry Hold (Msg : Inter_Msg);
         private
            Num : Natural := 0;
         end Count;

         protected body Count is
            procedure Inc is
            begin
               Num := Num + 1;
            end Inc;

            procedure Dec is
            begin
               Num := Num - 1;
            end Dec;

            entry Hold (Msg : Inter_Msg) when Num < Limit is
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
            end Hold;
         end Count;

         task body Worker is
         begin
            loop
               select
                  accept Worker_Comm (Msg : in Inter_Msg) do
                     null;
                  end Worker_Comm;
               or
                  terminate;
               end select;
            end loop;
         end Worker;

      begin

         declare
         begin
            loop
               select

                  accept Comm (Msg : in Inter_Msg) do
                     null;
                  end Comm;
               or
                  accept Send_Message (Message : in Messages_Client) do
                     declare
                        Swallow_Message : Messages_Client := Message; pragma Unreferenced (Swallow_Message);
                     begin
                        null;
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
                  exit;
               end select;
            end loop;
         end;
      end;

   exception
      when Exception_Id : others => Show_Exception (Exception_Id);
   end Router_Task;

end Generic_Router;
