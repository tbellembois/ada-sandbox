with Ada.Text_IO;
with Ada.Strings.Unbounded;
with GNAT.Sockets;

with Generic_Line_Parser;
with Basic_Example.Parameters;  -- Defines the record with the user parameters

procedure Basic_Example.Main is
   use Ada.Strings;
   use Ada.Strings.Unbounded;
   use GNAT;

   --  Convenient shorthand for the function that converts strings to
   --  unbounded strings
   function To_U (X : String) return Unbounded.Unbounded_String
                  renames Unbounded.To_Unbounded_String;

   --  Instantiate a specific version of Generic_Line_Parser suited
   --  for the User_Parameters record defined in Basic_Example.Parameters
   package Line_Parser is
      new Generic_Line_Parser (Parameters.User_Parameters);

   --  Declare the array of parameter descriptors.  Note that the
   --  callbacks are set to the procedures declared in
   --  Basic_Example.Parameters
   Descriptors : Line_Parser.Parameter_Descriptor_Array :=
                   ((Name      => To_U ("host"),
                     Default   => <>,
                     Mandatory => True,
                     Only_Once => True,
                     Callback  => Parameters.Set_Host'Access),

                    (Name      => To_U ("port"),
                     Default   => To_U ("4242"),
                     Mandatory => False,
                     Only_Once => True,
                     Callback  => Parameters.Set_Port'Access),

                    --  Both "user" and "username" are acceptable
                    (Name      => To_U ("user,username"),
                     Default   => <>,
                     Mandatory => True,
                     Only_Once => True,
                     Callback  => Parameters.Set_Username'Access),

                    --  Both "pwd" and "password" are acceptable
                    (Name      => To_U ("pwd,password"),
                     Default   => Unbounded.Null_Unbounded_String,
                     Mandatory => False,
                     Only_Once => True,
                     Callback  => Parameters.Set_Password'Access));

   Param : Parameters.User_Parameters;
begin
   Line_Parser.Parse_Command_Line
      (Parameters => Descriptors,
       Result     => Param);

   Ada.Text_IO.Put ("I am going to contact host '"
                    & Sockets.Image (Param.Host)
                    & "' on port "
                    & Sockets.Port_Type'Image (Param.Port)
                    & " with username '"
                    & Unbounded.To_String (Param.Username)
                    & "'");

   if Param.Password = Unbounded.Null_Unbounded_String then
      Ada.Text_IO.Put_Line (" and no password");
   else
      Ada.Text_IO.Put_Line (" and password '"
                            & Unbounded.To_String (Param.Password)
                            & "'");
   end if;
end Basic_Example.Main;
