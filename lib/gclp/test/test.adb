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
with Parsing_Test;
with Ada.Strings.Unbounded;    use Ada.Strings.Unbounded;
with Ada.Text_IO; use Ada.Text_IO;

procedure Test is
   Config : Parsing_Test.Config_Data;
begin
   Parsing_Test.Get_Parameters (Config);

   Put_Line ("X=" & Integer'Image (Config.X));
   Put_Line ("Y=" & Integer'Image (Config.Y));
   Put_Line ("Z=" & Float'Image (Config.Z));
   Put_Line ("file='" & To_String (Config.Filename) & "'");
end Test;
