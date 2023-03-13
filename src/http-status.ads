with AWS.Messages;

package HTTP.Status is

       function AWS_StatusCode_From_String
             (StatusCode_String : in String)
              return AWS.Messages.Status_Code with
             Pre  => StatusCode_String'Length > 0,
             Post =>
              AWS_StatusCode_From_String'Result in AWS.Messages.Status_Code;

end HTTP.Status;
