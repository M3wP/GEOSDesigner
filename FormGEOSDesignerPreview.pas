//------------------------------------------------------------------------------
//FormGEOSDesignerPreview
//=======================
//Screen preview form for the GEOS Designer application.
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
unit FormGEOSDesignerPreview;

{$mode objfpc}{$H+}

interface

uses
    Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ExtCtrls,
    StdCtrls, Types;

type

{ TGEOSDesignerPreviewForm }

    TGEOSDesignerPreviewForm = class(TForm)
        CmbScale: TComboBox;
        ImgPreview: TImage;
        Label1: TLabel;
        Panel1: TPanel;
        ScrollBox1: TScrollBox;
        procedure CmbScaleChange(Sender: TObject);
        procedure FormCreate(Sender: TObject);
        procedure ScrollBox1MouseWheel(Sender: TObject; Shift: TShiftState;
            WheelDelta: Integer; MousePos: TPoint; var Handled: Boolean);
    private
        { private declarations }
    public
        procedure InitialiseDisplay;
    end;

var
    GEOSDesignerPreviewForm: TGEOSDesignerPreviewForm;

implementation

{$R *.lfm}

uses
    GEOSTypes, GEOSDesignerCore;

{ TGEOSDesignerPreviewForm }

procedure TGEOSDesignerPreviewForm.FormCreate(Sender: TObject);
    begin
    if  not Assigned(ImgPreview.Picture.Bitmap) then
        ImgPreview.Picture.Bitmap:= TBitmap.Create;
    end;

procedure TGEOSDesignerPreviewForm.ScrollBox1MouseWheel(Sender: TObject;
        Shift: TShiftState; WheelDelta: Integer; MousePos: TPoint;
        var Handled: Boolean);
    begin
//dengland Let's be nice while we're here and allow scaling, as well.
    if  ssCtrl in Shift then
        begin
        Handled:= True;
        if  WheelDelta < 0 then
            begin
            if  CmbScale.ItemIndex < 2 then
                CmbScale.ItemIndex:= CmbScale.ItemIndex + 1;
            end
        else
            begin
            if  CmbScale.ItemIndex > 0 then
                CmbScale.ItemIndex:= CmbScale.ItemIndex - 1;
            end;
        CmbScaleChange(Sender);
        end
//dengland I'm having to handle horizontal scrolling myself.
    else if  ssShift in Shift then
        begin
        Handled:= True;
//dengland Trying to scroll by a percentage of the client area isn't working.
        if  WheelDelta < 0 then
            ScrollBox1.HorzScrollBar.Position:=
                    ScrollBox1.HorzScrollBar.Position + 100
                    //Round(ScrollBox1.ClientWidth * 0.6)
        else
            ScrollBox1.HorzScrollBar.Position:=
                    ScrollBox1.HorzScrollBar.Position - 100;
                    //Round(ScrollBox1.ClientWidth * 0.6);
        end;
    end;

procedure TGEOSDesignerPreviewForm.InitialiseDisplay;
    begin
    ImgPreview.Picture.Bitmap.SetSize(
            ARR_REC_GEOSDISPLAYRES[GEOSDispMode].Width,
            ARR_REC_GEOSDISPLAYRES[GEOSDispMode].Height);
    ImgPreview.Picture.Bitmap.Canvas.Brush.Color:= clBlack;
    ImgPreview.Picture.Bitmap.Canvas.Brush.Style:= bsSolid;
    ImgPreview.Picture.Bitmap.Canvas.FillRect(0, 0,
            ARR_REC_GEOSDISPLAYRES[GEOSDispMode].Width,
            ARR_REC_GEOSDISPLAYRES[GEOSDispMode].Height);

    ImgPreview.Width:= ARR_REC_GEOSDISPLAYPRP[GEOSDispMode].Width;
    ImgPreview.Height:= ARR_REC_GEOSDISPLAYPRP[GEOSDispMode].Height;

    Width:= ImgPreview.Width + 16;
    Height:= Panel1.Height + ImgPreview.Height + 16;

    CmbScale.ItemIndex:= 0;
    end;

procedure TGEOSDesignerPreviewForm.CmbScaleChange(Sender: TObject);
    begin
    if  CmbScale.ItemIndex = 2 then
        begin
        ImgPreview.Width:=
                ARR_REC_GEOSDISPLAYPRP[GEOSDispMode].Width * 4;
        ImgPreview.Height:=
                ARR_REC_GEOSDISPLAYPRP[GEOSDispMode].Height * 4;
        end
    else if CmbScale.ItemIndex = 1 then
        begin
        ImgPreview.Width:=
                ARR_REC_GEOSDISPLAYPRP[GEOSDispMode].Width * 2;
        ImgPreview.Height:=
                ARR_REC_GEOSDISPLAYPRP[GEOSDispMode].Height * 2;
        end
    else
        begin
        ImgPreview.Width:=
                ARR_REC_GEOSDISPLAYPRP[GEOSDispMode].Width;
        ImgPreview.Height:=
                ARR_REC_GEOSDISPLAYPRP[GEOSDispMode].Height;
        end;
    end;

end.

