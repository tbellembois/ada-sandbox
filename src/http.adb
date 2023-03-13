
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Strings.Maps;

with AWS.Headers;
with AWS.Status;

with Tokenize;

with GNAT.IO;               use GNAT.IO;
use type Ada.Strings.Maps.Character_Set;
with Ada.Streams; use Ada.Streams;
use type Ada.Streams.Stream_Element_Count;

with HTTP.Status;
with Globals;

package body HTTP is

   function From_Socket_Response
     (Socket_Response_Vec : in Socket.Socket_Response_Vectors.Vector) return Response
   is
      HTTP_Response : Response;
      Body_Reached  : Boolean := False;
   begin

      --  Building the HTTP response from the socket response.
      for I in
        Socket_Response_Vec.First_Index .. Socket_Response_Vec.Last_Index
      loop

         declare
            Current_Line           : Unbounded_String;
            Current_Line_Len       : Natural;
            Header_Separator_Index : Natural;
            Status_Code_String     : String (1 .. 3);
         begin

            Current_Line           := Socket_Response_Vec (I);
            Current_Line_Len       := Length (Current_Line);
            Header_Separator_Index := Index (Current_Line, ":");

            --  Put_Line (To_String (Current_Line));
            --  Put_Line (Current_Line_Len'Img);

            --  First line is the HTTP status like: HTTP/1.0 200 OK
            if I = 0 then

               --  Put_Line ("HTTP status reached");

               declare
                  Splitted_HTTP_Status : Tokenize.Token_Array :=
                    Tokenize.Split (To_String (Current_Line));
               begin
                  Status_Code_String := To_String (Splitted_HTTP_Status (2));
               end;

               HTTP_Response.Status_Code := HTTP.Status.AWS_StatusCode_From_String
                 (Status_Code_String);

            elsif Current_Line_Len = 0 then

               --  Put_Line ("Body reached");
               Body_Reached := True;

            elsif Body_Reached then

               --  Put_Line ("Body");
               Ada.Strings.Unbounded.Append
                 (HTTP_Response.Body_Content, Current_Line);

            else

               --  Put_Line ("Headers reached");

               declare
                  Header_Key   :
                  String (1 .. (Header_Separator_Index - 1));
                  Header_Value :
                  String
                    (Header_Separator_Index .. (Current_Line_Len));
               begin
                  Header_Key   :=
                    To_String (Current_Line)
                    (1 .. (Header_Separator_Index - 1));
                  Header_Value :=
                    To_String (Current_Line)
                    (Header_Separator_Index .. (Current_Line_Len));

                  --  Put_Line ("Header key " & Header_Key);
                  --  Put_Line ("Header value " & Header_Value);
               end;
            end if;
         end;

      end loop;

      return HTTP_Response;

   end From_Socket_Response;

   function Get_X_Forwarded_User (Request : in AWS.Status.Data) return String
   is
      Headers    : AWS.Headers.List;
      Separators : Ada.Strings.Maps.Character_Set :=
        Ada.Strings.Maps.Null_Set;

   begin
      Headers    := AWS.Status.Header (Request);
      Separators := Ada.Strings.Maps.To_Set ("@.");

      if Length (Globals.Param_Devel) /= 0 then
         return To_String (Globals.Param_Devel);
      end if;

      declare
         --  X-Forwarded-User contains the user email coming from codefirst auth proxy or any string coming from drone.
         X_Header       : String               :=
           AWS.Headers.Get_Values (Headers, "X-Forwarded-User");
         Splitted_Email : Tokenize.Token_Array :=
           Tokenize.Split (X_Header, Separators, True);
         Final_X_Header : Unbounded_String;
      begin
         if X_Header'Length = 0 then
            return "";
         end if;

         Splitted_Email := Tokenize.Split (X_Header, Separators, True);

         Append (Final_X_Header, Splitted_Email (1));
         if Splitted_Email'Length > 1 then
            Append (Final_X_Header, Splitted_Email (2));
         end if;

         return To_String (Final_X_Header);
      end;

   end Get_X_Forwarded_User;

   function Extract_Request_Body (Request : in AWS.Status.Data) return String
   is
      Offset       : Stream_Element_Count;
      Data         : Stream_Element_Array (1 .. 256);
      Body_Content : Unbounded_String;
   begin
      loop
         AWS.Status.Read_Body (Request, Data, Offset);
         exit when Offset = 0;

         for I in 1 .. Offset loop
            Append (Body_Content, Character'Val (Data (I)));
         end loop;
      end loop;

      return To_String (Body_Content);
   end Extract_Request_Body;

end HTTP;
