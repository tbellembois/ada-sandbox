----------------------------------------------------------------------------
--            Generic Command Line Parser (gclp)
--
--               Copyright (C) 2012, Riccardo Bernardini
--
--      This file is part of gclp.
--
--      gclp is free software: you can redistribute it and/or modify
--      it under the terms of the GNU General Public License as published by
--      the Free Software Foundation, either version 2 of the License, or
--      (at your option) any later version.
--
--      gclp is distributed in the hope that it will be useful,
--      but WITHOUT ANY WARRANTY; without even the implied warranty of
--      MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
--      GNU General Public License for more details.
--
--      You should have received a copy of the GNU General Public License
--      along with gclp.  If not, see <http://www.gnu.org/licenses/>.
----------------------------------------------------------------------------
--
--
with Generic_Line_Parser;
with Ada.Strings.Unbounded;

package body Parsing_Test is
   function To_U (X : String) return Unbounded_String
                  renames To_Unbounded_String;

   -- Instantiate the new package
   package Line_Parser is
      new Generic_Line_Parser (Config_Data);

   -- ------------------- --
   -- Parameter Callbacks --
   -- ------------------- --

   -- X Parameter callbak
   procedure X_Parameter (Name   : in     Unbounded_String;
                          Value  : in     Unbounded_String;
                          Result : in out Config_Data)
   is
   begin
      Result.X := Line_Parser.To_Natural (Value);
   end X_Parameter;

   -- Y Parameter callbak
   procedure Y_Parameter (Name   : in     Unbounded_String;
                          Value  : in     Unbounded_String;
                          Result : in out Config_Data)
   is
   begin
      Result.Y := Line_Parser.To_Natural (Value);
   end Y_Parameter;

   procedure Z_Parameter (Name   : in     Unbounded_String;
                          Value  : in     Unbounded_String;
                          Result : in out Config_Data)
   is
   begin
      Result.Z := Line_Parser.To_Float (Value);
   end Z_Parameter;
   -- Y Parameter callbak
   procedure File_Parameter (Name   : in     Unbounded_String;
                             Value  : in     Unbounded_String;
                             Result : in out Config_Data)
   is
   begin
      Result.Filename := Value;
   end File_Parameter;




   Param_Spec : Line_Parser.Parameter_Descriptor_Array :=
                  ((Name      => To_U ("x,X"),
                    Default    => <>,
                    If_Missing => Line_Parser.Die,
                    Only_Once  => True,
                    Callback   => X_Parameter'Access),

                   (Name       => To_U ("y,Y"),
                    Default    => <>,
                    If_Missing => Line_Parser.Die,
                    Only_Once  => True,
                    Callback   => Y_Parameter'Access),

                   (Name       => To_U ("z"),
                    Default    => To_U ("3.1415"),
                    If_Missing => Line_Parser.Use_Default,
                    Only_Once  => True,
                    Callback   => Z_Parameter'Access),

                   (Name       => To_U ("filename,file,input"),
                    Default    => <>,
                    If_Missing => Line_Parser.Die,
                    Only_Once  => True,
                    Callback   => File_Parameter'Access));

   --------------------
   -- Get_Parameters --
   --------------------

   procedure Get_Parameters (Config : out Config_Data) is
   begin
      Line_Parser.Parse_Command_Line (Parameters  => Param_Spec,
                                      Result      => Config);
   end Get_Parameters;

end Parsing_Test;
