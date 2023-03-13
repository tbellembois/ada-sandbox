with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

with Parameters;
with Types;

package Globals is

    use Types;

    Param_Devel                : Unbounded_String;
    Param_Admins               : Parameters.Admins_Vector.Vector;
    Param_Hostname             : Unbounded_String;
    Param_Scheme               : Unbounded_String;
    Param_DockerPathPrefix     : Unbounded_String;
    Param_DockerNetworkName    : Unbounded_String;
    Param_MaxAllowedContainers : MaxAllowedContainers_Type;

end Globals;
