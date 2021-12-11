--
--  Framework: Uwe R. Zimmer, Australia, 2015
--

with Ada.Strings.Bounded;           use Ada.Strings.Bounded;
with Generic_Routers_Configuration;
with Ada.Containers.Vectors;
with Maybe_Type;

generic
   with package Routers_Configuration is new Generic_Routers_Configuration (<>);

package Generic_Message_Structures is

   use Routers_Configuration;

   package Message_Strings is new Generic_Bounded_Length (Max => 80);
   use Message_Strings;

   subtype The_Core_Message is Bounded_String;

   type Messages_Client is record
      Destination : Router_Range;
      The_Message : The_Core_Message;
   end record;

   type Messages_Mailbox is record
      Sender      : Router_Range     := Router_Range'Invalid_Value;
      The_Message : The_Core_Message := Message_Strings.To_Bounded_String ("");
      Hop_Counter : Natural          := 0;
   end record;

   package Vector_Pkg is new Ada.Containers.Vectors (Index_Type   => Positive,
                                                     Element_Type => Router_Range);

   type Neighbour is array (1 .. Positive (Router_Range'Last)) of Router_Range;

   type Inter_Msg is record
      Sender : Router_Range;
      Neighbours : Neighbour;
      Length : Natural := 0;
   end record;

   type Client_Msg is record
      Sender      : Router_Range     := Router_Range'Invalid_Value;
      Destination : Router_Range;
      The_Message : The_Core_Message;
      Hop_Counter : Natural          := 0;
   end record;

   package Inter_Msg_Maybe is new Maybe_Type (Element => Inter_Msg);
   subtype Maybe_Inter_Msg is Inter_Msg_Maybe.Maybe;

   package Client_Msg_Maybe is new Maybe_Type (Element => Client_Msg);
   subtype Maybe_Client_Msg is Client_Msg_Maybe.Maybe;

   type Linkage is record
      Links : Vector_Pkg.Vector;
      Local_Seq_No : Natural := 0;
   end record;

   type Linkage_Array is array (Router_Range) of Linkage;
   type Par_Array is array (Router_Range) of Router_Range;

   protected type Flag is
      procedure Change_Flag (B : Boolean);
      entry Wait;
   private
      F : Boolean := False;
   end Flag;

end Generic_Message_Structures;
