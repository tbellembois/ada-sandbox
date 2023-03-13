with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Strings.Bounded; use Ada.Strings.Bounded;

with AWS.Messages;
with AWS.MIME;
with AWS.Status;
with AWS.Response.Set;

with GNAT.Regpat;
with GNAT.IO;

with Docker;
with Types;

package body Dockerproxy.Callbacks is

   -- Container names are max 128 characters.

   use Types.Container_Name_Str;

   -------------
   -- Default --
   -------------

   function Default (Request : in Status.Data) return Response.Data is

      use AWS.Status;
      use GNAT.Regpat;
      use GNAT.IO;
      use Ada.Strings.Unbounded;

      URI    : constant String         := Status.URI (Request);
      Method : constant Request_Method := Status.Method (Request);

      Create_Container_Re       : Pattern_Matcher (100);
      Create_Container_Matches  : Match_Array (1 .. 1);
      Remove_Container_Re       : Pattern_Matcher (100);
      Remove_Container_Matches  : Match_Array (1 .. 1);
      List_Containers_Re        : Pattern_Matcher (100);
      List_Containers_Matches   : Match_Array (1 .. 1);
      Get_Container_Log_Re      : Pattern_Matcher (100);
      Get_Container_Log_Matches : Match_Array (1 .. 1);
      Inspect_Container_Re      : Pattern_Matcher (100);
      Inspect_Container_Matches : Match_Array (1 .. 1);
      Start_Container_Re        : Pattern_Matcher (100);
      Start_Container_Matches   : Match_Array (1 .. 1);
      Stop_Container_Re         : Pattern_Matcher (100);
      Stop_Container_Matches    : Match_Array (1 .. 1);
      Create_Image_Re           : Pattern_Matcher (100);
      Create_Image_Matches      : Match_Array (1 .. 1);

      -- Container name or ID, image name...
      Request_Variable_Part : Bounded_String;

   begin

      Compile (Create_Container_Re, "/containers/create/(.+)");
      Compile (Remove_Container_Re, "/containers/(.+)");
      Compile (List_Containers_Re, "(/containers/json)");
      Compile (Get_Container_Log_Re, "/containers/(.+)/logs");
      Compile (Inspect_Container_Re, "/containers/(.+)/json");
      Compile (Start_Container_Re, "/containers/(.+)/start");
      Compile (Stop_Container_Re, "/containers/(.+)/stop");
      Compile (Create_Image_Re, "(/images/create)");

      Match (Create_Container_Re, URI, Create_Container_Matches);
      Match (Remove_Container_Re, URI, Remove_Container_Matches);
      Match (List_Containers_Re, URI, List_Containers_Matches);
      Match (Get_Container_Log_Re, URI, Get_Container_Log_Matches);
      Match (Inspect_Container_Re, URI, Inspect_Container_Matches);
      Match (Start_Container_Re, URI, Start_Container_Matches);
      Match (Stop_Container_Re, URI, Stop_Container_Matches);
      Match (Create_Image_Re, URI, Create_Image_Matches);

      if
        (Create_Container_Matches (1) /= No_Match and
         Method = Request_Method'(POST))
      then

            Set_Bounded_String (Request_Variable_Part, URI (Create_Container_Matches (1).First .. Create_Container_Matches (1).Last));
            return Docker.Create_Container (Request, To_String (Request_Variable_Part));
         
      elsif
        (Remove_Container_Matches (1) /= No_Match and
         Method = Request_Method'(DELETE))
      then

         Set_Bounded_String (Request_Variable_Part, URI (Remove_Container_Matches (1).First .. Remove_Container_Matches (1).Last));
         return Docker.Remove_Container (Request, To_String (Request_Variable_Part));
         
      elsif
        (List_Containers_Matches (1) /= No_Match and
         Method = Request_Method'(GET))
      then

         return Docker.List_Containers (Request);

      elsif
        (Get_Container_Log_Matches (1) /= No_Match and
         Method = Request_Method'(GET))
      then

         Set_Bounded_String (Request_Variable_Part, URI (Get_Container_Log_Matches (1).First .. Get_Container_Log_Matches (1).Last));
         return Docker.Get_Container_Log (Request, To_String (Request_Variable_Part));

      elsif
        (Inspect_Container_Matches (1) /= No_Match and
         Method = Request_Method'(GET))
      then

         Set_Bounded_String (Request_Variable_Part, URI (Inspect_Container_Matches (1).First .. Inspect_Container_Matches (1).Last));
            return Docker.Inspect_Container (Request, To_String (Request_Variable_Part));

      elsif
        (Start_Container_Matches (1) /= No_Match and
         Method = Request_Method'(POST))
      then

         Set_Bounded_String (Request_Variable_Part, URI (Start_Container_Matches (1).First .. Start_Container_Matches (1).Last));
            return Docker.Start_Container (Request, To_String (Request_Variable_Part));

      elsif
        (Stop_Container_Matches (1) /= No_Match and
         Method = Request_Method'(POST))
      then

         Set_Bounded_String (Request_Variable_Part, URI (Stop_Container_Matches (1).First .. Stop_Container_Matches (1).Last));
           return Docker.Stop_Container (Request, To_String (Request_Variable_Part));

      elsif
        (Create_Image_Matches (1) /= No_Match and
         Method = Request_Method'(POST))
      then

         declare
            Image_Name : String := AWS.Status.Parameter (Request, "fromImage");
         begin
            return Docker.Create_Image (Request, Image_Name);
         end;

      elsif (Method = Request_Method'(OPTIONS)) then

         declare
            Resp : Response.Data;
         begin
            Resp :=
              Response.Build
                (Content_Type => MIME.Text_Plain, Message_Body => "No content",
                 Status_Code  => Messages.S204);

            AWS.Response.Set.Add_Header
              (Resp, "Access-Control-Allow-Origin", "* ");
            AWS.Response.Set.Add_Header
              (Resp, "Access-Control-Allow-Methods",
               "GET, OPTIONS, POST, PUT, DELETE");

            return Resp;
         end;

      else

         return Response.Acknowledge (Messages.S404);

      end if;

   end Default;

end Dockerproxy.Callbacks;
