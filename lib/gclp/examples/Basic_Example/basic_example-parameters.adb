
package body Basic_Example.Parameters is
   use GNAT;
   use Ada.Strings;

   --------------
   -- Set_Host --
   --------------

   procedure Set_Host
     (Name   : in     Unbounded_String;
      Value  : in     Unbounded_String;
      Result : in out User_Parameters)
   is
   begin
      Result.Host := Sockets.Inet_Addr (Unbounded.To_String (Value));
   end Set_Host;

   --------------
   -- Set_Port --
   --------------

   procedure Set_Port
     (Name   : in     Unbounded_String;
      Value  : in     Unbounded_String;
      Result : in out User_Parameters)
   is
   begin
      Result.Port := Sockets.Port_Type'Value (Unbounded.To_String (Value));
   end Set_Port;

   ------------------
   -- Set_Username --
   ------------------

   procedure Set_Username
     (Name   : in     Unbounded_String;
      Value  : in     Unbounded_String;
      Result : in out User_Parameters)
   is
   begin
      Result.Username := Value;
   end Set_Username;

   ------------------
   -- Set_Password --
   ------------------

   procedure Set_Password
     (Name   : in     Unbounded_String;
      Value  : in     Unbounded_String;
      Result : in out User_Parameters)
   is
   begin
      Result.Password := Value;
   end Set_Password;

end Basic_Example.Parameters;
