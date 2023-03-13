with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

with Tokenize;

package body Parameters is

   --
   --  Admins.
   --

   procedure Set_Admins
     (Name   : in     Unbounded_String; Value : in Unbounded_String;
      Result : in out User_Parameters)
   is
      pragma Unreferenced (Name);
   begin
      declare
         Splitted_Admins : constant Tokenize.Token_Array :=
           Tokenize.Split (To_String (Value), ',');
      begin
         for i in Splitted_Admins'Range loop
            Result.Admins.Append (Splitted_Admins (i));
         end loop;
      end;
   end Set_Admins;

   --
   --  Devel.
   --

   procedure Set_Devel
     (Name   : in     Unbounded_String; Value : in Unbounded_String;
      Result : in out User_Parameters)
   is
      pragma Unreferenced (Name);
   begin
      Result.Devel := Value;
   end Set_Devel;

   --
   --  Hostname.
   --

   procedure Set_Hostname
     (Name   : in     Unbounded_String; Value : in Unbounded_String;
      Result : in out User_Parameters)
   is
      pragma Unreferenced (Name);
   begin
      Result.Hostname := Value;
   end Set_Hostname;

   --
   --  Scheme.
   --

   procedure Set_Scheme
     (Name   : in     Unbounded_String; Value : in Unbounded_String;
      Result : in out User_Parameters)
   is
      pragma Unreferenced (Name);
   begin
      Result.Scheme := Value;
   end Set_Scheme;

   --
   --  Docker path prefix.
   --

   procedure Set_DockerPathPrefix
     (Name   : in     Unbounded_String; Value : in Unbounded_String;
      Result : in out User_Parameters)
   is
      pragma Unreferenced (Name);
   begin
      Result.DockerPathPrefix := Value;
   end Set_DockerPathPrefix;

   --
   --  Docker network name.
   --

   procedure Set_DockerNetworkName
     (Name   : in     Unbounded_String; Value : in Unbounded_String;
      Result : in out User_Parameters)
   is
      pragma Unreferenced (Name);
   begin
      Result.DockerNetworkName := Value;
   end Set_DockerNetworkName;

   --
   --  Max containers.
   --

   procedure Set_MaxAllowedContainers
     (Name   : in     Unbounded_String; Value : in Unbounded_String;
      Result : in out User_Parameters)
   is
      pragma Unreferenced (Name);
   begin
      Result.MaxAllowedContainers := Natural'Value (To_String (value));
   end Set_MaxAllowedContainers;

end Parameters;
