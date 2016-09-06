//------------------------------------------------------------------------------
//FormGEOSDesignerNew
//===================
//A prompt for the user to enter the details required for a new project in the
//GEOS Designer application.
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
unit FormGEOSDesignerNew;

{$mode objfpc}{$H+}

interface

uses
    Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls;

type

{ TGEOSDesignerNewForm }

    TGEOSDesignerNewForm = class(TForm)
        Button1: TButton;
        Button2: TButton;
        ChkBxBlank: TCheckBox;
        CmbMode: TComboBox;
        EdtProjectName: TEdit;
        Label1: TLabel;
        Label2: TLabel;
        procedure EdtProjectNameKeyPress(Sender: TObject; var Key: char);
        procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    private
        { private declarations }
    public
        { public declarations }
    end;

var
    GEOSDesignerNewForm: TGEOSDesignerNewForm;

implementation

{$R *.lfm}

{ TGEOSDesignerNewForm }

const
    SET_CHR_INVALIDKEYS: TSysCharSet = [';', '/', '\', ':', '.', '"', '-', '''',
            '?', '*', '&', '|'];


procedure TGEOSDesignerNewForm.FormClose(Sender: TObject;
        var CloseAction: TCloseAction);
    begin
    if  (ModalResult = mrOk)
    and (Length(EdtProjectName.Text) = 0) then
        begin
        ModalResult:= mrNone;
        CloseAction:= caNone;
        Beep;
        ActiveControl:= EdtProjectName;
        end;
    end;

procedure TGEOSDesignerNewForm.EdtProjectNameKeyPress(Sender: TObject;
        var Key: char);
    begin
    if  CharInSet(Key, SET_CHR_INVALIDKEYS) then
        begin
        Key:= #0;
        Beep;
        end;

    if  Key = #13 then
        Key:= #0;
    end;

end.

