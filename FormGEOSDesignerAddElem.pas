//------------------------------------------------------------------------------
//FormGEOSDesignerAddElem
//=======================
//Prompt for the user to enter new element details.
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
unit FormGEOSDesignerAddElem;

{$mode objfpc}{$H+}

interface

uses
    Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls;

type

    { TGEOSDesignerAddElemForm }

    TGEOSDesignerAddElemForm = class(TForm)
        Button1: TButton;
        Button2: TButton;
        CmbElements: TComboBox;
        EdtIdentifier: TEdit;
        Label1: TLabel;
        Label2: TLabel;
        procedure EdtIdentifierKeyPress(Sender: TObject; var Key: char);
        procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
        procedure FormCreate(Sender: TObject);
    private
        { private declarations }
    public
        { public declarations }
    end;

var
    GEOSDesignerAddElemForm: TGEOSDesignerAddElemForm;

implementation

{$R *.lfm}

uses
    GEOSDesignerCore;


{ TGEOSDesignerAddElemForm }

procedure TGEOSDesignerAddElemForm.EdtIdentifierKeyPress(Sender: TObject;
        var Key: char);
    begin
//dengland Got to allow backspace.
    if  Key = #8 then
        Exit;

    if  Key = #13 then
        begin
        Key:= #0;
        Exit;
        end;

    if  not CharInSet(Key, SET_CHR_GEOSDSGNIDENT) then
        begin
        Key:= #0;
        Beep;
        end;
    end;

procedure TGEOSDesignerAddElemForm.FormClose(Sender: TObject;
        var CloseAction: TCloseAction);
    begin
    if  (ModalResult = mrOk)
    and (Length(EdtIdentifier.Text) = 0) then
        begin
        ModalResult:= mrNone;
        CloseAction:= caNone;
        Beep;
        ActiveControl:= EdtIdentifier;
        end;
    end;

procedure TGEOSDesignerAddElemForm.FormCreate(Sender: TObject);
    var
    i: Integer;
    e: TGEOSDesignerElementClass;

    begin
    CmbElements.Items.BeginUpdate;
    try
        CmbElements.Clear;

        for i:= 0 to GEOSDesignerElements.Count - 1 do
            begin
            e:= TGEOSDesignerElementClass(GEOSDesignerElements[i]);
            CmbElements.Items.Add(e.ElementName);
            end;

        finally
        CmbElements.Items.EndUpdate;
        end;

    CmbElements.ItemIndex:= 0;
    end;


end.

