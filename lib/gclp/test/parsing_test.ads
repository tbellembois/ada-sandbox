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
with Ada.Strings.Unbounded;  use Ada.Strings.Unbounded;

package Parsing_Test is
   -- This is the minimal interface that you need to export to the main
   -- program: the configuration record type declaration and a procedure
   -- to initialize it with the command line data.

   type Config_Data is
      record
         X : Integer;
         Y : Integer;
         Z : Float;
         Filename : Unbounded_String;
      end record;

   procedure Get_Parameters (Config : out Config_Data);
end Parsing_Test;
