with Ada.Strings.Unbounded;
with Ada.Containers.Vectors;
with Types;

package Parameters is

   use Ada.Strings.Unbounded;
   use Types;

   package Admins_Vector is new Ada.Containers.Vectors
     (Index_Type => Natural, Element_Type => Unbounded_String);

   type User_Parameters is record
      Devel                : Unbounded_String;
      Hostname             : Unbounded_String;
      Admins               : Admins_Vector.Vector;
      Scheme               : Unbounded_String;
      DockerPathPrefix     : Unbounded_String;
      DockerNetworkName    : Unbounded_String;
      MaxAllowedContainers : MaxAllowedContainers_Type;
   end record;

   procedure Set_Admins
     (Name   : in     Unbounded_String; Value : in Unbounded_String;
      Result : in out User_Parameters);

   procedure Set_Devel
     (Name   : in     Unbounded_String; Value : in Unbounded_String;
      Result : in out User_Parameters);

   procedure Set_Hostname
     (Name   : in     Unbounded_String; Value : in Unbounded_String;
      Result : in out User_Parameters);

   procedure Set_Scheme
     (Name   : in     Unbounded_String; Value : in Unbounded_String;
      Result : in out User_Parameters);

   procedure Set_DockerPathPrefix
     (Name   : in     Unbounded_String; Value : in Unbounded_String;
      Result : in out User_Parameters);

   procedure Set_DockerNetworkName
     (Name   : in     Unbounded_String; Value : in Unbounded_String;
      Result : in out User_Parameters);

   procedure Set_MaxAllowedContainers
     (Name   : in     Unbounded_String; Value : in Unbounded_String;
      Result : in out User_Parameters);

end Parameters;
