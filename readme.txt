GEOSDesigner
============

A GUI application for visually creating GEOS application skeletons.


Copyright (C) 2016, Daniel England.
All Rights Reserved.  Released under the GPL.

This program is free software: you can redistribute it and/or modify it under
the terms of the GNU General Public License as published by the Free Software
Foundation, either version 3 of the License, or (at your option) any later
version.

This program is distributed in the hope that it will be useful, but WITHOUT
ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more
details.

You should have received a copy of the GNU General Public License along with
this program.  If not, see <http://www.gnu.org/licenses/>.


Introduction
------------

GEOS is an operating system that is run on a C64, C128 or Apple IIe.  It is a
GUI OS which was quite remarkable at the time given the constraints of its
target systems.

This application allows you to construct skeleton applications for GEOS in a
visual environment.  The skeleton is constructed from "elements" where each
element relates to a feature in GEOS.  There are elements for GraphicsString,
PutString, DoMenu, DoIcons and DoDlgBox.  You can edit the properties of the
elements to construct the application's skeleton.

To find out more about the elements, it is recommended that you should read
The Offical GEOS Programmer's Reference Guide.  However, with GEOS Designer
and with a little understanding of GUI applications, you should be able to get a
very good grasp of what to do while using it.

While you are designing your application, you are given a preview of what the
screen will look like with the currently active elements.  When you create a new
project, you select the type of GEOS display.  40 column, 80 column and a
special Mega65 80 column mode are supported.

When you have constructed all of the application's elements, you can generate
the skeleton code.  All of the data is output as well as place-holders for the
required routines which are then ready for you to fill in with the appropriate
code.


Usage
-----

Under development.


Compiling
---------

You need FPC and Lazarus to compile GEOSDesigner.  You can get Lazarus (which
includes FPC) for your platform from the following address:

        http://www.lazarus-ide.org/

At the time of writing, I am using Lazarus version 1.6 but earlier versions
should be supported so long as they have FPC version 2.1 or higher.

Several platforms are supported, including Windows, Linux and MacOSX.  32bit or
64bit compilation should be supported, depending upon your requirements.

To compile, open the "GEOSDesigner.lpi" file in Lazarus and select Run | Build.
You can switch the Build Mode by using the Compiler Project Options under
Project | Project Options | Compiler Options and selecting either the Release or
Debug Build Mode.

Delphi is presently unsupported due to the extensive use of Lazarus features.


Contact
-------

I can be contacted for further information regarding this tool at the following
address:

        mewpokemon {you know what goes here} hotmail {and here} com

Please include the word "GEOSDesigner" in the subject line.

Thanks for using GEOSDesigner!



Daniel England.
