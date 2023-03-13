with AWS.Response;
with AWS.Status;

package Docker is

   use AWS;
   
   function List_Containers (Request : in Status.Data) return Response.Data;

   function Start_Container (Request : in Status.Data; Container_Id : in String) return Response.Data;

   function Stop_Container (Request : in Status.Data; Container_Id : in String) return Response.Data;

   function Create_Container (Request : in Status.Data; Container_Name : in String) return Response.Data;

   function Remove_Container (Request : in Status.Data; Container_Id : in String) return Response.Data;

   function Inspect_Container (Request : in Status.Data; Container_Id : in String) return Response.Data;

   function Get_Container_Log (Request : in Status.Data; Container_Id : in String) return Response.Data;

   function Create_Image (Request : in Status.Data; Image_Name : in String) return Response.Data;

end Docker;
