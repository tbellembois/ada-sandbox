with Globals;
with Ada.Strings.Unbounded;

package body Utils is

    function Is_Admin (User: string) return Boolean is
        use Globals;
        use Ada.Strings.Unbounded;

        Result : Boolean := false;
    begin
        for e of Param_Admins loop
            if user = To_String (e) then
                Result := true;
                return Result;
            end if;
        end loop;

        return Result;
    end;

end Utils;
