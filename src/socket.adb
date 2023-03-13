with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Containers.Vectors;
with Ada.Streams;
use type Ada.Streams.Stream_Element_Count;

with GNAT.IO;
with GNAT.Sockets;

package body Socket is

   function Send_To_Socket
     (Message : in String) return Socket_Response_Vectors.Vector
   is
      use Socket_Response_Vectors;
      use GNAT.IO;
      use GNAT.Sockets;
      use Ada.Streams;

      Client  : Socket_Type;
      Channel : Stream_Access;
      Offset  : Stream_Element_Count;
      Data    : Stream_Element_Array (1 .. 256);

      Socket_Response_Vec : Socket_Response_Vectors.Vector;
   begin

      --  Connect to socket.
      Initialize;
      Create_Socket (Socket => Client, Family => Family_Unix);
      Connect_Socket
        (Socket => Client,
         Server =>
           (Family => Family_Unix,
             Name  => To_Unbounded_String ("/var/run/docker.sock")));

      Channel := Stream (Client);

      --  Send message.
      String'Write (Stream (Client), Message);

      Build_Socket_Response_Vector :
      loop
         Read (Channel.all, Data, Offset);
         exit when Offset = 0;

         declare
            Current_Line     : Unbounded_String;
            Current_Line_Len : Natural;
         begin
            for I in 1 .. Offset loop

               --  Put_Line (Character'Val (Data (I))'Img);

               Current_Line_Len := Length (Current_Line);

               if Character'Val (Data (I)) = ASCII.CR then
                  Socket_Response_Vec.Append (Current_Line);
                  Delete (Current_Line, 1, Current_Line_Len);
               elsif Character'Val (Data (I)) /= ASCII.LF then
                  Append (Current_Line, Character'Val (Data (I)));
               end if;

            end loop;

            Socket_Response_Vec.Append (Current_Line);

         end;
      end loop Build_Socket_Response_Vector;

      Close_Socket (Socket => Client);

      return Socket_Response_Vec;

   exception
      when Socket_Error =>
         Put_Line ("Socket error");
         Socket_Response_Vec.Append (To_Unbounded_String ("HTTP/1.0 500 Socket_Error"));
         return Socket_Response_Vec;

   end Send_To_Socket;

end Socket;
