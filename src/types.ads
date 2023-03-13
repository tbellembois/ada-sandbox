with Ada.Strings.Bounded;

package Types is

        subtype MaxAllowedContainers_Type is Natural range 1 .. 20;

        package Container_Name_Str is new Ada.Strings.Bounded
               .Generic_Bounded_Length
               (Max => 128);
               
end Types;
