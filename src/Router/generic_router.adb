--
--  Framework: Uwe R. Zimmer, Australia, 2019
--

with Exceptions; use Exceptions;
with Ada.Text_IO; use Ada.Text_IO;
package body Generic_Router is

   task body Router_Task is

      Connected_Routers : Ids_To_Links;
      protected Shutdown_Flag is
         procedure Sd;
         function Read return Boolean;
         entry Wait;
         entry Release;
      private
         Flag : Boolean := False;
         Value : Boolean := False;
      end Shutdown_Flag;

      protected body Shutdown_Flag is
         procedure Sd is
         begin
            Flag := True;
         end Sd;
         function Read return Boolean is (Flag);
         entry Wait when Value is
         begin
            Value := False;
         end Wait;
         entry Release when not Value is
         begin
            Value := True;
         end Release;
      end Shutdown_Flag;

   begin
      accept Configure (Links : Ids_To_Links) do
         Connected_Routers := Links;
      end Configure;

      declare
         Port_List : constant Connected_Router_Ports := To_Router_Ports (Task_Id, Connected_Routers);
         pragma Unreferenced (Port_List);

         task T;
         task body T is
         begin
            loop
               Shutdown_Flag.Release;
               Shutdown_Flag.Wait;
               exit when Shutdown_Flag.Read;
            end loop;
         end T;

      begin
         loop
            select

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
               accept Shutdown  do
                  Shutdown_Flag.Sd;
                  Put_Line ("here");
               end Shutdown;
               exit;
            end select;
         end loop;
      end;

   exception
      when Exception_Id : others => Show_Exception (Exception_Id);
   end Router_Task;

end Generic_Router;
