with GNAT.Sockets;
with Ada.Strings.Unbounded;

package Basic_Example.Parameters is
   use GNAT;
   use Ada.Strings.Unbounded;

   type User_Parameters is
      record
         Host : Sockets.Inet_Addr_Type;
         Port : Sockets.Port_Type;
         Username : Unbounded_String;
         Password : Unbounded_String;
      end record;
   --  Record that holds the paramters that can be specified by the user.
   --  The following four procedures can be used as callbacks in an
   --  instantiation of Generic_Line_Parser.

   procedure Set_Host
      (Name   : in     Unbounded_String;
       Value  : in     Unbounded_String;
       Result : in out User_Parameters);

   procedure Set_Port
      (Name   : in     Unbounded_String;
       Value  : in     Unbounded_String;
       Result : in out User_Parameters);

   procedure Set_Username
      (Name   : in     Unbounded_String;
       Value  : in     Unbounded_String;
       Result : in out User_Parameters);

   procedure Set_Password
      (Name   : in     Unbounded_String;
       Value  : in     Unbounded_String;
       Result : in out User_Parameters);
end Basic_Example.Parameters;
