with GNAT.IO;           use GNAT.IO;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

with AWS.Config.Set;
with AWS.Server;

with Dockerproxy.Callbacks;

with Generic_Line_Parser;

with Parameters;

with Globals;

procedure Dockerproxy.Main is
   use AWS;

   Web_Server : Server.HTTP;
   Web_Config : Config.Object;

   package Line_Parser is new Generic_Line_Parser (Parameters.User_Parameters);

   use type Line_Parser.Missing_Action;

   Descriptors : constant Line_Parser.Parameter_Descriptor_Array :=
     [(Name       => To_Unbounded_String ("--hostname"),
       Default    => To_Unbounded_String ("localhost"), Only_Once => True,
       If_Missing => Line_Parser.Use_Default,
       Callback   => Parameters.Set_Hostname'Access),

      (Name       => To_Unbounded_String ("--scheme"),
       Default    => To_Unbounded_String ("http"), Only_Once => True,
       If_Missing => Line_Parser.Use_Default,
       Callback   => Parameters.Set_Scheme'Access),

      (Name       => To_Unbounded_String ("--dockerpathprefix"),
       Default    => To_Unbounded_String ("containers"), Only_Once => True,
       If_Missing => Line_Parser.Use_Default,
       Callback   => Parameters.Set_DockerPathPrefix'Access),

      (Name       => To_Unbounded_String ("--dockernetworkname"),
       Default    => To_Unbounded_String ("run_net"), Only_Once => True,
       If_Missing => Line_Parser.Use_Default,
       Callback   => Parameters.Set_DockerNetworkName'Access),

      (Name       => To_Unbounded_String ("--maxallowedcontainers"),
       Default    => To_Unbounded_String ("3"), Only_Once => True,
       If_Missing => Line_Parser.Use_Default,
       Callback   => Parameters.Set_MaxAllowedContainers'Access),

      (Name       => To_Unbounded_String ("--devel,-d"),
       Default    => To_Unbounded_String (""), Only_Once => True,
       If_Missing => Line_Parser.Ignore,
       Callback   => Parameters.Set_Devel'Access),

      (Name       => To_Unbounded_String ("--admins"),
       Default    => To_Unbounded_String (""), Only_Once => True,
       If_Missing => Line_Parser.Ignore,
       Callback   => Parameters.Set_Admins'Access)];

   Param : Parameters.User_Parameters;

begin

   --  Command line parameters

   Line_Parser.Parse_Command_Line (Parameters => Descriptors, Result => Param);

   Globals.Param_Devel                := Param.Devel;
   Globals.Param_Admins               := Param.Admins;
   Globals.Param_DockerNetworkName    := Param.DockerNetworkName;
   Globals.Param_DockerPathPrefix     := Param.DockerPathPrefix;
   Globals.Param_Hostname             := Param.Hostname;
   Globals.Param_MaxAllowedContainers := Param.MaxAllowedContainers;
   Globals.Param_Scheme               := Param.Scheme;

   Put_Line ("--devel => " & To_String (Param.Devel));
   Put_Line ("--hostname => " & To_String (Param.Hostname));
   Put_Line ("--scheme => " & To_String (Param.Scheme));
   Put_Line ("--dockerpathprefix => " & To_String (Param.DockerPathPrefix));
   Put_Line ("--dockernetworkname => " & To_String (Param.DockerNetworkName));
   Put_Line ("--maxallowedcontainers => " & Param.MaxAllowedContainers'Img);

   for admin of Param.Admins loop
      Put_Line ("-admin: " & To_String (admin));
   end loop;

   --  Setup

   Config.Set.Server_Host (Web_Config, Host);
   Config.Set.Server_Port (Web_Config, Port);

   --  Start the server

   Server.Start (Web_Server, Callbacks.Default'Access, Web_Config);

   --  Wait for the Q key

   Server.Wait (Server.Forever);
   --  Server.Wait (Server.Q_Key_Pressed);

   --  Stop the server

   Server.Shutdown (Web_Server);
end Dockerproxy.Main;
