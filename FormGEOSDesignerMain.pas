//------------------------------------------------------------------------------
//FormGEOSDesignerMain
//====================
//Application main form for the GEOS Designer application.
//
//
//Copyright (C) 2016, Daniel England.
//All Rights Reserved.  Released under the GPL.
//
//This program is free software: you can redistribute it and/or modify it under
//the terms of the GNU General Public License as published by the Free Software
//Foundation, either version 3 of the License, or (at your option) any later
//version.
//
//This program is distributed in the hope that it will be useful, but WITHOUT
//ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
//FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more
//details.
//
//You should have received a copy of the GNU General Public License along with
//this program.  If not, see <http://www.gnu.org/licenses/>.
//
//------------------------------------------------------------------------------
unit FormGEOSDesignerMain;

{$mode objfpc}{$H+}

interface

uses
    Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ComCtrls;

type

{ TGEOSDesignerMainForm }

    TGEOSDesignerMainForm = class(TForm)
        ToolBar2: TToolBar;
        ToolButton2: TToolButton;
        ToolButton3: TToolButton;
        procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
        procedure FormShow(Sender: TObject);
    private
        { private declarations }
    public
        { public declarations }
    end;

var
    GEOSDesignerMainForm: TGEOSDesignerMainForm;

implementation

{$R *.lfm}

uses
    DModGEOSDesignerMain;

{ TGEOSDesignerMainForm }

procedure TGEOSDesignerMainForm.FormClose(Sender: TObject;
        var CloseAction: TCloseAction);
    begin
    GEOSDesignerMainDMod.OnMainClose(CloseAction);
    end;

procedure TGEOSDesignerMainForm.FormShow(Sender: TObject);
    begin
    GEOSDesignerMainDMod.OnMainShow;
    end;

end.

