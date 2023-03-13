with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

with AWS.Messages;
with AWS.Status;

with Socket;

package HTTP is

    type Response is record
        Status_Code  : AWS.Messages.Status_Code;
        Body_Content : Unbounded_String;
    end record;

    function Extract_Request_Body
       (Request : in AWS.Status.Data) return String with
       Pre => AWS.Status.Content_Type (Request) /= "0";

    function From_Socket_Response
       (Socket_Response_Vec : in Socket.Socket_Response_Vectors.Vector)
        return Response;

    function Get_X_Forwarded_User
       (Request : in AWS.Status.Data) return String with
       Post => Get_X_Forwarded_User'Result'Length /= 0;

end HTTP;
