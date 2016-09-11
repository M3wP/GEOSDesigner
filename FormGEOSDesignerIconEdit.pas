//------------------------------------------------------------------------------
//FormGEOSDesignerIconEdit
//========================
//Icon editor and viewer for the GEOS Designer application.
//
//
//Please note:
//------------
//Presently, only FPC/Lazarus is supported.  Delphi support is incomplete.
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
unit FormGEOSDesignerIconEdit;

{$mode objfpc}{$H+}

interface

uses
    Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ExtCtrls,
    Grids, StdCtrls, ComCtrls, ExtDlgs, GEOSDesignerCore;

type
{ TGEOSDesignerIconEditForm }

    TGEOSDesignerIconEditForm = class(TForm)
        DrwGrdIcon: TDrawGrid;
        LstBxIcons: TListBox;
        OpenPictureDialog1: TOpenPictureDialog;
        Panel1: TPanel;
        Panel2: TPanel;
        TlBtnIconDel: TToolButton;
        ToolBar1: TToolBar;
        TlBtnIconAdd: TToolButton;
        procedure DrwGrdIconDrawCell(Sender: TObject; aCol, aRow: Integer;
            aRect: TRect; aState: TGridDrawState);
        procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
        procedure FormCreate(Sender: TObject);
        procedure FormDestroy(Sender: TObject);
        procedure FormShow(Sender: TObject);
        procedure LstBxIconsSelectionChange(Sender: TObject; User: boolean);
        procedure TlBtnIconAddClick(Sender: TObject);
        procedure TlBtnIconDelClick(Sender: TObject);
    private
        FData: TMemoryStream;
        FSelected: TGEOSDesignerIcon;

        procedure InitialiseIconView(const AIndex: Integer);

    public
        procedure InitialiseDisplay;
    end;

var
    GEOSDesignerIconEditForm: TGEOSDesignerIconEditForm;

implementation

{$R *.lfm}

uses
    LazFileUtils, GEOSTypes, GEOSGraphics, DModGEOSDesignerMain;

{ TGEOSDesignerIconEditForm }

procedure TGEOSDesignerIconEditForm.FormShow(Sender: TObject);
    begin
    InitialiseDisplay;
    end;

procedure TGEOSDesignerIconEditForm.LstBxIconsSelectionChange(Sender: TObject;
        User: boolean);
    var
    i,
    s: Integer;

    begin
    s:= -1;
    for i:= 0 to LstBxIcons.Items.Count - 1 do
        if  LstBxIcons.Selected[i] then
            begin
            s:= i;
            Break;
            end;

    if  s > -1 then
        InitialiseIconView(s)
    else
        TlBtnIconDel.Enabled:= False;
    end;

procedure TGEOSDesignerIconEditForm.TlBtnIconAddClick(Sender: TObject);
    var
    f: string;
    s: string;
    pic: TPicture;

    procedure DoNewIconFromBitmap(const AIdent: string; const ABitmap: TBitmap);
        var
        i,
        j: Integer;
        d: Byte;
        b: Byte;
        ico: TGEOSDesignerIcon;
        di: TMemoryStream;

        begin
        ico:= TGEOSDesignerIcon.Create(AIdent);
        ico.Width:= ABitmap.Width;
        ico.Height:= ABitmap.Height;

        di:= TMemoryStream.Create;
        try
            d:= 128;
            b:= 0;

            for i:= 0 to ABitmap.Height - 1 do
                for j:= 0 to ABitmap.Width - 1 do
                    begin
                    if  ABitmap.Canvas.Pixels[j, i] = clBlack then
                        b:= b or d;

                    if d = 1 then
                        begin
                        di.WriteByte(b);
                        b:= 0;
                        d:= 128;
                        end
                    else
                        d:= d shr 1;
                    end;

            di.Position:= 0;
            GEOSCompactBitmap(di, ico.Data);

            finally
            di.Free;
            end;

        GEOSDesignerMainDMod.AddIcon(ico);
        InitialiseDisplay;
        end;

    begin
    if  OpenPictureDialog1.Execute then
        begin
        pic:= TPicture.Create;
        try
            pic.LoadFromFile(OpenPictureDialog1.FileName);

//          pic.Bitmap.Monochrome:= True;
//          if  not pic.Bitmap.Monochrome then
//              ShowMessage('Imported image must be monochrome!')
            {else} if (pic.Bitmap.Width mod 8) = 0 then
                begin
                f:= ExtractFileNameOnly(OpenPictureDialog1.FileName);

                s:= InputBox('GEOS Designer Icon import',
                        'Icon identifier:', 'Icon' + f);

                if Length(s) > 0 then
                    DoNewIconFromBitmap(s, pic.Bitmap)
                else
                    ShowMessage('Imported image must have an identifier!');
                end
            else
                ShowMessage('Imported image width must be a multiple of 8!');

            finally
            pic.Free;
            end;
        end;
    end;

procedure TGEOSDesignerIconEditForm.TlBtnIconDelClick(Sender: TObject);
    var
    i,
    j: Integer;
    f: Boolean;
    e: TGEOSDoIconsElement;

    begin
    f:= False;
    for i:= 0 to GEOSDesignerMainDMod.ElementsCount - 1 do
        begin
        if  GEOSDesignerMainDMod.Elements[i] is TGEOSDoIconsElement then
            begin
            e:= GEOSDesignerMainDMod.Elements[i] as TGEOSDoIconsElement;
            for j:= 0 to e.Count - 1 do
                if  e.Icons[j] = FSelected then
                    begin
                    f:= True;
                    Break;
                    end;
            end;

        if  f then
            Break;
        end;

    if  f then
        MessageDlg('GEOS Designer',
                'The selected icon is in use and cannot be deleted.',
                mtWarning, [mbOk], -1)
    else
        begin
        GEOSDesignerMainDMod.RemoveIcon(FSelected);
        InitialiseDisplay;
        end;
    end;

procedure TGEOSDesignerIconEditForm.InitialiseIconView(const AIndex: Integer);
    begin
    FSelected:= GEOSDesignerMainDMod.Icons[AIndex];

    FData.Position:= 0;
    FData.Size:= 0;
    FSelected.Data.Position:= 0;

    TlBtnIconDel.Enabled:= not FSelected.System;

    GEOSDecompactBitmap(FSelected.Data, FData);

    DrwGrdIcon.ColCount:= FSelected.Width;
    DrwGrdIcon.RowCount:= FSelected.Height;
    DrwGrdIcon.Invalidate;
    end;

procedure TGEOSDesignerIconEditForm.InitialiseDisplay;
    var
    i: Integer;

    begin
    LstBxIcons.Items.BeginUpdate;
    try
        LstBxIcons.Clear;

        for i:= 0 to GEOSDesignerMainDMod.IconsCount - 1 do
            LstBxIcons.Items.Add(GEOSDesignerMainDMod.Icons[i].Identifier);

        finally
        LstBxIcons.Items.EndUpdate;
        end;

    if  LstBxIcons.Items.Count > 0 then
        begin
        LstBxIcons.Selected[0]:= True;
        end;
    end;

procedure TGEOSDesignerIconEditForm.DrwGrdIconDrawCell(Sender: TObject; aCol, aRow: Integer;
        aRect: TRect; aState: TGridDrawState);
    var
    bw: Byte;
    i: Integer;
    d: Byte;
    pc,
    bc: TColor;

    begin
    bw:= FSelected.Width div 8;

    i:= ARow * bw + (ACol div 8);
    d:= 1 shl (7 - (ACol mod 8));

    if  i < FData.Size then
        begin
        FData.Position:= i;

        if  GEOSDispMode = gdm40Column then
            begin
            pc:= clC64DkGrey;
            bc:= clC64LtGrey;
            end
        else
            begin
              pc:= clBlack;
              bc:= clVDCLtGrey;
            end;

        if  (d and FData.ReadByte) <> 0 then
            DrwGrdIcon.Canvas.Brush.Color:= pc
        else
            DrwGrdIcon.Canvas.Brush.Color:= bc;
        end
    else
        DrwGrdIcon.Canvas.Brush.Color:= clRed;

    DrwGrdIcon.Canvas.Brush.Style:= bsSolid;
    DrwGrdIcon.Canvas.FillRect(ARect);
    end;

procedure TGEOSDesignerIconEditForm.FormClose(Sender: TObject; var CloseAction: TCloseAction);
    begin
    end;

procedure TGEOSDesignerIconEditForm.FormCreate(Sender: TObject);
    //var
    //mi: TMemoryStream;

    begin
    FData:= TMemoryStream.Create;

    //mi:= TMemoryStream.Create;
    //try
    //    mi.WriteBuffer(ARR_VAL_GEOSSYSICODISK[0],
    //            SizeOf(ARR_VAL_GEOSSYSICODISK));
    //    mi.Position:= 0;
    //    ShowMessage('Initial compressed buffer: ' + IntToStr(mi.Size));
    //
    //    GEOSDecompactBitmap(mi, FData);
    //
    //    ShowMessage('Initial decompressed buffer: ' + IntToStr(FData.Size));
    //
    //    FData.Position:= 0;
    //    mi.Position:= 0;
    //    mi.Size:= 0;
    //    GEOSCompactBitmap(FData, mi);
    //
    //    ShowMessage('My compressed buffer: ' + IntToStr(mi.Size));
    //
    //    FData.Position:= 0;
    //    FData.Size:= 0;
    //    mi.Position:= 0;
    //    GEOSDecompactBitmap(mi, FData);
    //
    //    ShowMessage('My decompressed buffer: ' + IntToStr(FData.Size));
    //
    //    finally
    //    mi.Free;
    //    end;
    end;

procedure TGEOSDesignerIconEditForm.FormDestroy(Sender: TObject);
    begin
    FData.Free;
    end;

end.

