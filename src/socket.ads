with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Containers.Vectors;

package Socket is

    -- Socket_Response_Vectors contains the lines of the socket response.
    package Socket_Response_Vectors is new Ada.Containers.Vectors
       (Index_Type => Natural, Element_Type => Unbounded_String);

    use Socket_Response_Vectors;

    function Send_To_Socket
       (Message : in String) return Socket_Response_Vectors.Vector with
       Post => not Send_To_Socket'Result.Is_Empty;

end Socket;
