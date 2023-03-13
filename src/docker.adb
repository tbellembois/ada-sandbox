with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Strings.Fixed;
with Ada.Streams;

with AWS.Response;
with AWS.Status;
with AWS.MIME;
with AWS.Messages;
with AWS.Response.Set;

with GNAT.IO;
with Socket;
with HTTP;
with GNATCOLL.JSON;
with Tokenize;

with Globals;
with Utils;
-- build: ctrl+ shift + b
-- check file: alt + v

package body Docker is

   use AWS;
   use GNAT.IO;

   --
   -- List containers.
   --

   function List_Containers (Request : in Status.Data) return Response.Data is
      Message             : Unbounded_String;
      Socket_Response_Vec : Socket.Socket_Response_Vectors.Vector;
      HTTP_Response       : HTTP.Response;
      X_Forwarded_User    : String := HTTP.Get_X_Forwarded_User (Request);

      Resp : Response.Data;
   begin

      Put_Line ("[" & X_Forwarded_User & "] list containers");

      Build_Request :
      declare
         Is_Admin : Boolean := Utils.Is_Admin (X_Forwarded_User);
      begin
         if Is_Admin then
            Message :=
              To_Unbounded_String
                ("GET /containers/json?size=true&filters={""label"":[""codefirst-usercontainer=true""]} HTTP/1.0" &
                 ASCII.CR & ASCII.LF & ASCII.CR & ASCII.LF);
         else
            Message :=
              To_Unbounded_String
                ("GET /containers/json?size=true&filters={""label"":[""" &
                 X_Forwarded_User & "=admin""]} HTTP/1.0" & ASCII.CR &
                 ASCII.LF & ASCII.CR & ASCII.LF);
         end if;
      end Build_Request;

      --  Put_Line (To_String (Message));

      Socket_Response_Vec := Socket.Send_To_Socket (To_String (Message));
      HTTP_Response       := HTTP.From_Socket_Response (Socket_Response_Vec);

      Resp :=
        Response.Build
          (Content_Type => MIME.Application_JSON,
           Message_Body => To_String (HTTP_Response.Body_Content),
           Status_Code  => HTTP_Response.Status_Code);

      AWS.Response.Set.Add_Header (Resp, "access-control-allow-origin", "* ");

      --  Put_Line (To_String (HTTP_Response.Body_Content));

      return Resp;

   end List_Containers;

   --
   -- Create container.
   --

   function Create_Container
     (Request : in Status.Data; Container_Name : in String)
      return Response.Data
   is
      use GNATCOLL;
      use Ada.Streams;
      use Globals;

      Container          : JSON.JSON_Value;
      New_Container      : JSON.JSON_Value := JSON.Create_Object;
      New_Container_Name : Unbounded_String;

      Message             : Unbounded_String;
      Socket_Response_Vec : Socket.Socket_Response_Vectors.Vector;
      HTTP_Response       : HTTP.Response;
      X_Forwarded_User    : String := HTTP.Get_X_Forwarded_User (Request);

      Resp : Response.Data;
   begin

      Put_Line
        ("[" & X_Forwarded_User & "] create container " & Container_Name);

      declare
         Body_Content         : String := HTTP.Extract_Request_Body (Request);
         New_Body_Content     : Unbounded_String;
         New_Body_Content_Len : Natural;
         Image_Name           : JSON.UTF8_Unbounded_String;
         Admins               : JSON.UTF8_Unbounded_String;
         Private_Container    : Boolean         := False;
         Env_Variables        : JSON.JSON_Array;
         Labels               : JSON.JSON_Value := JSON.Create_Object;
         HostConfig           : JSON.JSON_Value := JSON.Create_Object;
         RestartPolicy        : JSON.JSON_Value := JSON.Create_Object;
         NetworkMode          : JSON.UTF8_Unbounded_String;
      begin

         Container := JSON.Read (Body_Content);

         -- Building new container name;
      --  Append (New_Container_Name, Container_Name & "-" & X_Forwarded_User);
         Append (New_Container_Name, Container_Name);

         -- Getting required image name.
         if not JSON.Has_Field (Container, "Image") then
            return
              Response.Acknowledge
                (Messages.S400, "Image name missing.", MIME.Text_Plain);
         end if;

         Image_Name := JSON.Get (Container, "Image");

         -- Private container ?
         if JSON.Has_Field (Container, "Private") then
            Private_Container := True;
         end if;

-- Admins: comma separated list of admins with the syntax "firstname.lastname".
         if JSON.Has_Field (Container, "Admins") then
            Admins := JSON.Get (Container, "Admins");
         end if;

         declare
            Splitted_Admins : Tokenize.Token_Array :=
              Tokenize.Split (To_String (Admins), ',');
         begin
            Set_Admins:
            for I in Splitted_Admins'Range loop
               JSON.Set_Field
                 (Labels, To_String (Splitted_Admins (I)), "admin");
            end loop Set_Admins;
         end;

         JSON.Set_Field (Labels, X_Forwarded_User, "admin");

         -- Getting optionnal Env values.
         if JSON.Has_Field (Container, "Env") then
            Env_Variables := JSON.Get (Container, "Env");
         end if;

         -- Configuring network.
         JSON.Set_Field (RestartPolicy, "Name", "unless-stopped");
         JSON.Set_Field
           (RestartPolicy, "MaximumRetryCount", Integer'Value ("0"));
         JSON.Set_Field (HostConfig, "NetworkMode", Param_DockerNetworkName);
         JSON.Set_Field (HostConfig, "RestartPolicy", RestartPolicy);
         JSON.Set_Field (New_Container, "HostConfig", HostConfig);

         -- Building labels.
         JSON.Set_Field
           (Labels,
            "traefik.http.routers." & To_String (New_Container_Name) & ".rule",
            "Host(`" & Param_Hostname & "`) && PathPrefix(`/" &
            Param_DockerPathPrefix & "/" & To_String (New_Container_Name) &
            "`)");
         JSON.Set_Field
           (Labels,
            "traefik.http.routers." & To_String (New_Container_Name) &
            ".entrypoints",
            "websecure");
         JSON.Set_Field
           (Labels,
            "traefik.http.routers." & To_String (New_Container_Name) &
            ".tls.certresolver",
            "letsEncrypt");
         JSON.Set_Field
           (Labels,
            "traefik.http.middlewares.strip-" &
            To_String (New_Container_Name) & ".stripprefix.prefixes",
            "/" & Param_DockerPathPrefix & "/" &
            To_String (New_Container_Name));
         JSON.Set_Field
           (Labels,
            "traefik.http.routers." & To_String (New_Container_Name) &
            ".middlewares",
            "strip-" & To_String (New_Container_Name) & "@docker");
         JSON.Set_Field
           (Labels, "codefirst-containername", To_String (New_Container_Name));
         JSON.Set_Field (Labels, "codefirst-user", X_Forwarded_User);
         JSON.Set_Field (Labels, "codefirst-usercontainer", "true");
         JSON.Set_Field
           (Labels, "traefik.docker.network", Param_DockerNetworkName);

         if Private_Container then
            JSON.Set_Field (Labels, "codefirst-private", "true");
            JSON.Set_Field
              (Labels, "codefirst-container-endpoint",
               To_String (New_Container_Name));
         else
            JSON.Set_Field (Labels, "traefik.enable", "true");
            JSON.Set_Field (Labels, "codefirst-private", "false");
            JSON.Set_Field
              (Labels, "codefirst-container-endpoint",
               Param_Scheme & "://" & Param_Hostname & "/" &
               Param_DockerPathPrefix & "/" & To_String (New_Container_Name));
         end if;

         -- Building final new container.
         JSON.Set_Field (New_Container, "Image", Image_Name);
         JSON.Set_Field (New_Container, "Labels", Labels);
         JSON.Set_Field (New_Container, "Env", Env_Variables);

         New_Body_Content := To_Unbounded_String (JSON.Write (New_Container));
         New_Body_Content_Len := Length (New_Body_Content);

         Message :=
           To_Unbounded_String
             ("POST /containers/create?name=" &
              To_String (New_Container_Name) & " HTTP/1.0" & ASCII.CR &
              ASCII.LF & "Content-Type: application/json" & ASCII.CR &
              ASCII.LF & "Host: docker" & ASCII.CR & ASCII.LF &
              "Content-length: " & New_Body_Content_Len'Img & ASCII.CR &
              ASCII.LF & "Connection: close" & ASCII.CR & ASCII.LF & ASCII.CR &
              ASCII.LF & To_String (New_Body_Content) & ASCII.CR & ASCII.LF &
              "EOF" & ASCII.CR & ASCII.LF);

      end;

      Socket_Response_Vec := Socket.Send_To_Socket (To_String (Message));
      HTTP_Response       := HTTP.From_Socket_Response (Socket_Response_Vec);

      Resp :=
        Response.Build
          (Content_Type => MIME.Application_JSON,
           Message_Body => To_String (HTTP_Response.Body_Content),
           Status_Code  => HTTP_Response.Status_Code);

      AWS.Response.Set.Add_Header (Resp, "access-control-allow-origin", "* ");

      return Resp;

   end Create_Container;

   --
   -- Start container.
   --

   function Start_Container
     (Request : in Status.Data; Container_Id : in String) return Response.Data
   is
      Message             : Unbounded_String;
      Socket_Response_Vec : Socket.Socket_Response_Vectors.Vector;
      HTTP_Response       : HTTP.Response;
      X_Forwarded_User    : String := HTTP.Get_X_Forwarded_User (Request);

      Resp : Response.Data;
   begin

      Put_Line ("[" & X_Forwarded_User & "] start container " & Container_Id);

      if (Container_Id'Length = 0) then
         return
           Response.Acknowledge
             (Messages.S400, "Container ID missing.", MIME.Text_Plain);
      end if;

      Message :=
        To_Unbounded_String
          ("POST /containers/" & Container_Id & "/start HTTP/1.0" & ASCII.CR &
           ASCII.LF & ASCII.CR & ASCII.LF);

      Socket_Response_Vec := Socket.Send_To_Socket (To_String (Message));
      HTTP_Response       := HTTP.From_Socket_Response (Socket_Response_Vec);

      Resp :=
        Response.Build
          (Content_Type => MIME.Application_JSON,
           Message_Body => To_String (HTTP_Response.Body_Content),
           Status_Code  => HTTP_Response.Status_Code);

      AWS.Response.Set.Add_Header (Resp, "access-control-allow-origin", "* ");

      return Resp;

   end Start_Container;

   --
   -- Stop container.
   --

   function Stop_Container
     (Request : in Status.Data; Container_Id : in String) return Response.Data
   is
      Message             : Unbounded_String;
      Socket_Response_Vec : Socket.Socket_Response_Vectors.Vector;
      HTTP_Response       : HTTP.Response;
      X_Forwarded_User    : String := HTTP.Get_X_Forwarded_User (Request);

      Resp : Response.Data;
   begin

      Put_Line ("[" & X_Forwarded_User & "] stop container " & Container_Id);

      if (Container_Id'Length = 0) then
         return
           Response.Acknowledge
             (Messages.S400, "Container ID missing.", MIME.Text_Plain);
      end if;

      Message :=
        To_Unbounded_String
          ("POST /containers/" & Container_Id & "/stop?t=10 HTTP/1.0" &
           ASCII.CR & ASCII.LF & ASCII.CR & ASCII.LF);

      Socket_Response_Vec := Socket.Send_To_Socket (To_String (Message));
      HTTP_Response       := HTTP.From_Socket_Response (Socket_Response_Vec);

      Resp :=
        Response.Build
          (Content_Type => MIME.Application_JSON,
           Message_Body => To_String (HTTP_Response.Body_Content),
           Status_Code  => HTTP_Response.Status_Code);

      AWS.Response.Set.Add_Header (Resp, "access-control-allow-origin", "* ");

      return Resp;

   end Stop_Container;

   --
   -- Inspect container.
   --

   function Inspect_Container
     (Request : in Status.Data; Container_Id : in String) return Response.Data
   is
      Message             : Unbounded_String;
      Socket_Response_Vec : Socket.Socket_Response_Vectors.Vector;
      HTTP_Response       : HTTP.Response;
      X_Forwarded_User    : String := HTTP.Get_X_Forwarded_User (Request);

      Resp : Response.Data;
   begin

      Put_Line
        ("[" & X_Forwarded_User & "] inspect container " & Container_Id);

      if (Container_Id'Length = 0) then
         return
           Response.Acknowledge
             (Messages.S400, "Container ID missing.", MIME.Text_Plain);
      end if;

      Message :=
        To_Unbounded_String
          ("GET /containers/" & Container_Id & "/json HTTP/1.0" & ASCII.CR &
           ASCII.LF & ASCII.CR & ASCII.LF);

      Socket_Response_Vec := Socket.Send_To_Socket (To_String (Message));
      HTTP_Response       := HTTP.From_Socket_Response (Socket_Response_Vec);

      Resp :=
        Response.Build
          (Content_Type => MIME.Application_JSON,
           Message_Body => To_String (HTTP_Response.Body_Content),
           Status_Code  => HTTP_Response.Status_Code);

      AWS.Response.Set.Add_Header (Resp, "access-control-allow-origin", "* ");

      return Resp;

   end Inspect_Container;

   --
   -- Get container log.
   --

   function Get_Container_Log
     (Request : in Status.Data; Container_Id : in String) return Response.Data
   is
      Message             : Unbounded_String;
      Socket_Response_Vec : Socket.Socket_Response_Vectors.Vector;
      HTTP_Response       : HTTP.Response;
      X_Forwarded_User    : String := HTTP.Get_X_Forwarded_User (Request);

      Resp : Response.Data;
   begin

      Put_Line
        ("[" & X_Forwarded_User & "] get container log " & Container_Id);

      if (Container_Id'Length = 0) then
         return
           Response.Acknowledge
             (Messages.S400, "Container ID missing.", MIME.Text_Plain);
      end if;

      Message :=
        To_Unbounded_String
          ("GET /containers/" & Container_Id &
           "/logs?stdout=true&stderr=true HTTP/1.0" & ASCII.CR & ASCII.LF &
           ASCII.CR & ASCII.LF);

      Socket_Response_Vec := Socket.Send_To_Socket (To_String (Message));
      HTTP_Response       := HTTP.From_Socket_Response (Socket_Response_Vec);

      Resp :=
        Response.Build
          (Content_Type => MIME.Application_JSON,
           Message_Body => To_String (HTTP_Response.Body_Content),
           Status_Code  => HTTP_Response.Status_Code);

      AWS.Response.Set.Add_Header (Resp, "access-control-allow-origin", "* ");

      return Resp;

   end Get_Container_Log;

   --
   -- Remove container.
   --

   function Remove_Container
     (Request : in Status.Data; Container_Id : in String) return Response.Data
   is
      Message             : Unbounded_String;
      Socket_Response_Vec : Socket.Socket_Response_Vectors.Vector;
      HTTP_Response       : HTTP.Response;
      X_Forwarded_User    : String := HTTP.Get_X_Forwarded_User (Request);

      Resp : Response.Data;
   begin

      Put_Line ("[" & X_Forwarded_User & "] remove container " & Container_Id);

      if (Container_Id'Length = 0) then
         return
           Response.Acknowledge
             (Messages.S400, "Container ID missing.", MIME.Text_Plain);
      end if;

      Message :=
        To_Unbounded_String
          ("DELETE /containers/" & Container_Id &
           "?v=true&force=true HTTP/1.0" & ASCII.CR & ASCII.LF & ASCII.CR &
           ASCII.LF);

      Socket_Response_Vec := Socket.Send_To_Socket (To_String (Message));
      HTTP_Response       := HTTP.From_Socket_Response (Socket_Response_Vec);

      Resp :=
        Response.Build
          (Content_Type => MIME.Application_JSON,
           Message_Body => To_String (HTTP_Response.Body_Content),
           Status_Code  => HTTP_Response.Status_Code);

      AWS.Response.Set.Add_Header (Resp, "access-control-allow-origin", "* ");

      return Resp;

   end Remove_Container;

   --
   -- Create image.
   --

   function Create_Image
     (Request : in Status.Data; Image_Name : in String) return Response.Data
   is
      New_Image_Name : Unbounded_String := To_Unbounded_String (Image_Name);
      Message             : Unbounded_String;
      Socket_Response_Vec : Socket.Socket_Response_Vectors.Vector;
      HTTP_Response       : HTTP.Response;
      X_Forwarded_User    : String := HTTP.Get_X_Forwarded_User (Request);
      Image_Has_Tag       : Boolean          := False;

      Resp : Response.Data;
   begin

      Put_Line ("[" & X_Forwarded_User & "] create image from " & Image_Name);

      for i in Image_Name'Range loop
         if (Image_Name (i) = ':') then
            Image_Has_Tag := True;
         end if;
      end loop;

      if not Image_Has_Tag then
         Append (New_Image_Name, ":latest");
      end if;

      if (Image_Name'Length = 0) then
         return
           Response.Acknowledge
             (Messages.S400, "Image name missing.", MIME.Text_Plain);
      end if;

      Message :=
        To_Unbounded_String
          ("POST /images/create?fromImage=" & To_String (New_Image_Name) &
           " HTTP/1.0" & ASCII.CR & ASCII.LF &
           "Content-Type: application/tar" & ASCII.CR & ASCII.LF & ASCII.CR &
           ASCII.LF & ASCII.CR & ASCII.LF);

      Socket_Response_Vec := Socket.Send_To_Socket (To_String (Message));
      HTTP_Response       := HTTP.From_Socket_Response (Socket_Response_Vec);

      Resp :=
        Response.Build
          (Content_Type => MIME.Application_JSON,
           Message_Body => To_String (HTTP_Response.Body_Content),
           Status_Code  => HTTP_Response.Status_Code);

      AWS.Response.Set.Add_Header (Resp, "access-control-allow-origin", "* ");

      return Resp;

   end Create_Image;

end Docker;
