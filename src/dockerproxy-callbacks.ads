
with AWS.Response;
with AWS.Status;

package Dockerproxy.Callbacks is

   use AWS;

   function Default (Request : in Status.Data) return Response.Data;

end Dockerproxy.Callbacks;
