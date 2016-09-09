//------------------------------------------------------------------------------
//DModGEOSDesignerMain
//====================
//Application main Data Module for the GEOS Designer application.  Controls
//most aspects of the application behaviour.
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
unit DModGEOSDesignerMain;

{$mode objfpc}{$H+}

interface

uses
    Contnrs, Graphics, Classes, SysUtils, FileUtil, Menus, ActnList, Forms,
    Controls, GEOSTypes, GEOSDesignerCore, FrameGEOSDesignerMain;

type

{ TGEOSDesignerMainDMod }

    TGEOSDesignerMainDMod = class(TDataModule)
        ActFileExit: TAction;
        ActEditAddElem: TAction;
        ActEditDelElem: TAction;
        ActFileSave: TAction;
        ActionList1: TActionList;
        ImgLstPatterns: TImageList;
        ImgLstItems: TImageList;
        ImgLstGrphsStr: TImageList;
        MainMenu1: TMainMenu;
        MenuItem1: TMenuItem;
        MenuItem2: TMenuItem;
        MenuItem3: TMenuItem;
        MenuItem4: TMenuItem;
        MenuItem5: TMenuItem;
        MenuItem6: TMenuItem;
        MenuItem7: TMenuItem;
        MenuItem8: TMenuItem;
        procedure ActEditAddElemExecute(Sender: TObject);
        procedure ActEditDelElemExecute(Sender: TObject);
        procedure ActFileExitExecute(Sender: TObject);
        procedure ActFileSaveExecute(Sender: TObject);
        procedure ActionList1Update(AAction: TBasicAction;
                var Handled: Boolean);
        procedure DataModuleCreate(Sender: TObject);
        procedure DataModuleDestroy(Sender: TObject);
    private
        FFirstTime: Boolean;
        FDirty: Boolean;
        FProjectName: string;
        FBitmap: TBitmap;
        FElements: TObjectList;
        FIcons: TObjectList;
        FMainFrame: TGEOSDesignerMainFrame;
        FMouse: TGEOSBitmap;

        procedure DoOnChange;
        procedure DoOnInit;

        procedure DoInitSystemIcons;

        procedure DoClearBitmap;
        procedure DoSetAppTitle;

        function  GetElementsCount: Integer;
        function  GetElements(const AIndex: Integer): TGEOSDesignerElement;
        function  GetIconsCount: Integer;
        function  GetIcons(const AIndex: Integer): TGEOSDesignerIcon;

        procedure DoCreateDefaultProject;

    public
        procedure OnMainShow;
        procedure OnMainClose(var ACloseAction: TCloseAction);

        procedure Changed;

        procedure AddIcon(const AIcon: TGEOSDesignerIcon);
        procedure RemoveIcon(const AIcon: TGEOSDesignerIcon);

        property  ProjectName: string read FProjectName;
        property  ElementsCount: Integer read GetElementsCount;
        property  Elements[const AIndex: Integer]: TGEOSDesignerElement
                read GetElements;
        property  IconsCount: Integer read GetIconsCount;
        property  Icons[const AIndex: Integer]: TGEOSDesignerIcon
                read GetIcons;
    end;

var
    GEOSDesignerMainDMod: TGEOSDesignerMainDMod;

implementation

{$R *.lfm}

uses
    Laz2_DOM, Laz2_XMLWrite, GEOSGraphics, Dialogs, FormGEOSDesignerNew,
    FormGEOSDesignerPreview, FormGEOSDesignerIconEdit,
    FormGEOSDesignerAddElem;

resourcestring
    STR_CAP_GEOSMODE40COL = ' (40 Columns)';
    STR_CAP_GEOSMODE80COL = ' (80 Columns)';
    STR_CAP_GEOSMODE80DBL = ' (80 Columns, M65)';
    STR_CAP_GEOSDESIGNER = 'GEOS Designer';
    STR_MSG_GEOSDESIGNCLSD =
            'The current project has not been saved.'#13#10#13#10+
            'Are you sure you wish to exit the application?';

{ TGEOSDesignerMainDMod }

procedure TGEOSDesignerMainDMod.DataModuleCreate(Sender: TObject);
    begin
    FFirstTime:= True;

    FElements:= TObjectList.Create(True);

    FBitmap:= TBitmap.Create;

    FIcons:= TObjectList.Create(True);
    DoInitSystemIcons;
    end;

procedure TGEOSDesignerMainDMod.ActFileExitExecute(Sender: TObject);
    begin
    Application.MainForm.Close;
    end;

procedure TGEOSDesignerMainDMod.ActFileSaveExecute(Sender: TObject);
    var
    doc: TXMLDocument;
    rn,
    pn,
    en: TDOMNode;
    i: Integer;
    e: TGEOSDesignerElement;

    begin
    doc:= TXMLDocument.Create;
    try
        rn:= doc.CreateElement('GEOSDesigner');
        Doc.Appendchild(rn);

        rn:= doc.DocumentElement;
        pn:= doc.CreateElement('elements');
        TDOMElement(pn).SetAttribute('count', IntToStr(FElements.Count));
        pn:= rn.AppendChild(pn);

        for i:= 0 to FElements.Count - 1 do
            begin
            e:= TGEOSDesignerElement(FElements[i]);

            en:= doc.CreateElement('element');
            TDOMElement(en).SetAttribute('index', IntToStr(i));
            TDOMElement(en).SetAttribute('name', e.ElementName);
            TDOMElement(en).SetAttribute('active', IntToStr(Ord(e.Active)));
            TDOMElement(en).SetAttribute('identifier', e.Identifier);

            e.SaveToXML(doc, en);

            pn.AppendChild(en);
            end;

        WriteXMLFile(doc, 'test.gdesign');

        finally
        doc.Free;
        end;
    end;

procedure TGEOSDesignerMainDMod.ActionList1Update(AAction: TBasicAction;
        var Handled: Boolean);
    begin
    ActEditAddElem.Enabled:= Assigned(FMainFrame);
    ActEditDelElem.Enabled:= Assigned(FMainFrame) and
            Assigned(FMainFrame.SelectedElem);
    end;

procedure TGEOSDesignerMainDMod.ActEditAddElemExecute(Sender: TObject);
    var
    e: TGEOSDesignerElementClass;

    begin
    if  GEOSDesignerAddElemForm.ShowModal = mrOk then
        begin
        with GEOSDesignerAddElemForm do
            begin
            e:= TGEOSDesignerElementClass(
                    GEOSDesignerElements[CmbElements.ItemIndex]);

            FElements.Add(e.Create(EdtIdentifier.Text));
            end;

        FMainFrame.InitialiseDisplay;
        DoOnChange;
        end;
    end;

procedure TGEOSDesignerMainDMod.ActEditDelElemExecute(Sender: TObject);
    begin
    FElements.Remove(FMainFrame.SelectedElem);
//dengland Because the ObjectList owns the objects, this isn't necessary.
//  FMainFrame.SelectedElem.Free;

    FMainFrame.InitialiseDisplay;
    DoOnChange;
    end;

procedure TGEOSDesignerMainDMod.DataModuleDestroy(Sender: TObject);
    begin
    GEOSDesignerOnChange:= nil;

    FMouse.Free;
    FIcons.Free;

    FElements.Free;
    FBitmap.Free;
    end;

procedure TGEOSDesignerMainDMod.DoOnChange;
    var
    e: TGEOSDesignerElement;
    i: Integer;
    r: TRect;

    begin
    DoClearBitmap;
    GEOSSystemFont.Style:= [];

//dengland This is going to cause some "double dipping" on the updates but its
//      necessary to catch activiation changes.  It means that DoOnChange has
//      to be called _after_ FMainFrame.InitialiseDisplay when its used, though.
    if  Assigned(FMainFrame) then
        FMainFrame.UpdateElements;

    for i:= 0 to FElements.Count - 1 do
        begin
        e:= FElements[i] as TGEOSDesignerElement;

        if  e.Active then
            e.PreparePreview(FBitmap);
        end;

    if  GEOSShowMouse then
        GEOSBitmapUp(FBitmap.Canvas, GEOSMouseXPos, GEOSMouseYPos, FMouse,
                False, False, False, True, True);

    r:= Rect(0, 0, ARR_REC_GEOSDISPLAYRES[GEOSDispMode].Width,
            ARR_REC_GEOSDISPLAYRES[GEOSDispMode].Height);

    GEOSDesignerPreviewForm.ImgPreview.Picture.Bitmap.Canvas.CopyRect(r,
            FBitmap.Canvas, r);
    end;

procedure TGEOSDesignerMainDMod.DoOnInit;
    begin
    GEOSDesignerPreviewForm.InitialiseDisplay;
    end;

procedure TGEOSDesignerMainDMod.DoInitSystemIcons;
    procedure DoSetupSysIcon(const AIdent: string; const ABuffer: array of Byte);
        var
        i: TGEOSDesignerIcon;

        begin
        i:= TGEOSDesignerIcon.Create(AIdent);
        i.Width:= 48;
        i.Height:= 16;
        i.Data.WriteBuffer(ABuffer[0], SizeOf(ABuffer));
        i.System:= True;
        FIcons.Add(i);
        end;

    begin
    DoSetupSysIcon('IconOK', ARR_VAL_GEOSSYSICOOK);
    DoSetupSysIcon('IconCancel', ARR_VAL_GEOSSYSICOCANCEL);
    DoSetupSysIcon('IconYes', ARR_VAL_GEOSSYSICOYES);
    DoSetupSysIcon('IconNo', ARR_VAL_GEOSSYSICONO);
    DoSetupSysIcon('IconOpen', ARR_VAL_GEOSSYSICOOPEN);
    DoSetupSysIcon('IconDisk', ARR_VAL_GEOSSYSICODISK);

    FMouse:= TGEOSBitmap.Create;
    FMouse.Width:= 8;
    FMouse.Height:= 8;
    FMouse.Data.WriteBuffer(ARR_VAL_GEOSSYSMOUSE[0],
            SizeOf(ARR_VAL_GEOSSYSMOUSE));
    end;

procedure TGEOSDesignerMainDMod.DoClearBitmap;
    begin
    FBitmap.SetSize(ARR_REC_GEOSDISPLAYRES[GEOSDispMode].Width,
            ARR_REC_GEOSDISPLAYRES[GEOSDispMode].Height);

    if  GEOSDispMode = gdm40Column then
        begin
        FBitmap.Canvas.Brush.Color:= clC64LtGrey;
        FBitmap.Canvas.Pen.Color:= clC64DkGrey;
        end
    else
        begin
        FBitmap.Canvas.Brush.Color:= clVDCLtGrey;
        FBitmap.Canvas.Pen.Color:= clBlack;
        end;

    FBitmap.Canvas.Brush.Style:= bsSolid;
    FBitmap.Canvas.Pen.Style:= psSolid;

    FBitmap.Canvas.FillRect(0, 0, ARR_REC_GEOSDISPLAYRES[GEOSDispMode].Width,
            ARR_REC_GEOSDISPLAYRES[GEOSDispMode].Height);

    FBitmap.Canvas.PenPos:= Point(0, 0);
    end;

procedure TGEOSDesignerMainDMod.DoSetAppTitle;
    var
    s: string;

    begin
    if  FDirty then
        s:= '*'
    else
        s:= EmptyStr;

    s:= s + FProjectName;
    if  GEOSDesignerNewForm.CmbMode.ItemIndex = 0 then
        s:= s + STR_CAP_GEOSMODE40COL
    else if GEOSDesignerNewForm.CmbMode.ItemIndex = 1 then
        s:= s + STR_CAP_GEOSMODE80COL
    else
        s:= s + STR_CAP_GEOSMODE80DBL;

    s:= s + ' - ' + STR_CAP_GEOSDESIGNER;
    Application.Title:= s;
    Application.MainForm.Caption:= s;
    end;

function TGEOSDesignerMainDMod.GetElementsCount: Integer;
    begin
    Result:= FElements.Count;
    end;

function TGEOSDesignerMainDMod.GetElements(
        const AIndex: Integer): TGEOSDesignerElement;
    begin
    Result:= TGEOSDesignerElement(FElements[AIndex]);
    end;

function TGEOSDesignerMainDMod.GetIconsCount: Integer;
    begin
    Result:= FIcons.Count;
    end;

function TGEOSDesignerMainDMod.GetIcons(
        const AIndex: Integer): TGEOSDesignerIcon;
    begin
    Result:= TGEOSDesignerIcon(FIcons[AIndex]);
    end;

procedure TGEOSDesignerMainDMod.DoCreateDefaultProject;
    var
    e: TGEOSGraphicsStrElement;
    m: TGEOSDoMenuElement;
    mi,
    sm: TGEOSDoMenuItem;
    x1,
    y1,
    x2,
    y2: Word;
    wl,
    wh: Byte;
    hl,
    hh: Byte;
    d: array of Byte;
    s: string;
    i: Integer;
    ie: TGEOSDoIconsElement;

    begin
    e:= TGEOSGraphicsStrElement.Create('ClearScreen');
    e.AddItem(ggiGraphics, VAL_CMD_GEOSGSTR_MOVETO, [0, 0, 0, 0]);
    e.AddItem(ggiGraphics, VAL_CMD_GEOSGSTR_NEWPTN, [2]);

    wl:= ARR_REC_GEOSDISPLAYRES[GEOSDispMode].Width and $00FF;
    wh:= (ARR_REC_GEOSDISPLAYRES[GEOSDispMode].Width and $FF00) shr 8;
    hl:= ARR_REC_GEOSDISPLAYRES[GEOSDispMode].Height and $00FF;
    hh:= (ARR_REC_GEOSDISPLAYRES[GEOSDispMode].Height and $FF00) shr 8;
    e.AddItem(ggiGraphics, VAL_CMD_GEOSGSTR_RECTTO, [wl, wh, hl, hh]);

    FElements.Add(e);

    e:= TGEOSGraphicsStrElement.Create('AppBanner');

    x1:= (ARR_REC_GEOSDISPLAYRES[GEOSDispMode].Width div 2) - 76;
    y1:= (ARR_REC_GEOSDISPLAYRES[GEOSDispMode].Height div 2) - 21;
    x2:= (ARR_REC_GEOSDISPLAYRES[GEOSDispMode].Width div 2) + 74;
    y2:= (ARR_REC_GEOSDISPLAYRES[GEOSDispMode].Height div 2) + 19;

    wl:= x1 and $00FF;
    wh:= (x1 and $FF00) shr 8;
    hl:= y1 and $00FF;
    hh:= (y1 and $FF00) shr 8;
    e.AddItem(ggiGraphics, VAL_CMD_GEOSGSTR_MOVETO, [wl, wh, hl, hh]);

    e.AddItem(ggiGraphics, VAL_CMD_GEOSGSTR_NEWPTN, [0]);

    wl:= x2 and $00FF;
    wh:= (x2 and $FF00) shr 8;
    hl:= y2 and $00FF;
    hh:= (y2 and $FF00) shr 8;
    e.AddItem(ggiGraphics, VAL_CMD_GEOSGSTR_RECTTO, [wl, wh, hl, hh]);


    wl:= x1 and $00FF;
    wh:= (x1 and $FF00) shr 8;
    hl:= y1 and $00FF;
    hh:= (y1 and $FF00) shr 8;
    e.AddItem(ggiGraphics, VAL_CMD_GEOSGSTR_FMRECT, [wl, wh, hl, hh]);

    x1:= x1 + 2;
    y1:= y1 + 2;
    wl:= x1 and $00FF;
    wh:= (x1 and $FF00) shr 8;
    hl:= y1 and $00FF;
    hh:= (y1 and $FF00) shr 8;
    e.AddItem(ggiGraphics, VAL_CMD_GEOSGSTR_MOVETO, [wl, wh, hl, hh]);
    e.AddItem(ggiGraphics, VAL_CMD_GEOSGSTR_ESCPTS, []);

    e.AddItem(ggiString, VAL_CMD_GEOSPSTR_BOLDON, []);

    s:= FProjectName;
    SetLength(d, Length(s));
    for i:= 1 to Length(s) do
        d[i - 1]:= Byte(AnsiChar(s[i]));
    e.AddItem(ggiString, VAL_CMD_GEOSPSTR_PUTSTR, d);

    e.AddItem(ggiString, VAL_CMD_GEOSPSTR_PLNTXT, []);

    FElements.Add(e);

    m:= TGEOSDoMenuElement.Create('MainMenu');
    mi:= TGEOSDoMenuItem.Create('GEOSMenu', gmtSubMenu, m, gmaHorizontal);
    mi.Visible:= True;
    x1:= GEOSSystemFont.TextExtent('geosfile').x + 17;
    mi.Bounds:= Rect(0, 0, x1, 14);
    mi.Text:= 'geos';

    sm:= TGEOSDoMenuItem.Create('DoGEOSAbout', gmtMenuAction, mi, gmaVertical);
    sm.Visible:= False;
    sm.Bounds:= Rect(0, 15, GEOSSystemFont.TextExtent(' about  ').x, 30);
    sm.Text:= 'about';

    mi:= TGEOSDoMenuItem.Create('FileMenu', gmtSubMenu, m, gmaHorizontal);
    mi.Text:= 'file';

    sm:= TGEOSDoMenuItem.Create('DoFileQuit', gmtMenuAction, mi, gmaVertical);
    sm.Visible:= True;
    x1:= GEOSSystemFont.TextExtent('geos').x + 8;
    sm.Bounds:= Rect(x1, 15, x1 + GEOSSystemFont.TextExtent(' quit  ').x, 30);
//  sm.Bounds:= Rect(x1, 15, x1 + GEOSSystemFont.TextExtent(' quit  ').x, 44);
    sm.Text:= 'quit';

//  sm:= TGEOSDoMenuItem.Create('DoFileClose', gmtSubMenu, mi, gmaVertical);
//  sm.Text:= 'close';
//
//  sm:= TGEOSDoMenuItem.Create('DoFileExtra', gmtMenuAction, sm, gmaVertical);
//  sm.Visible:= True;
//
//  x1:= GEOSSystemFont.TextExtent('geos  close ').x + 4;
//  sm.Bounds:= Rect(x1, 30, x1 + GEOSSystemFont.TextExtent(' test ').x, 44);
//  sm.Text:= 'test';

    m.Active:= True;
    FElements.Add(m);

    ie:= TGEOSDoIconsElement.Create('MainIcons');

    wl:= ARR_REC_GEOSDISPLAYRES[GEOSDispMode].Width div 8 - 7;
    ie.Add(wl, ARR_REC_GEOSDISPLAYRES[GEOSDispMode].Height - 40, 'DoIconOK',
            TGEOSDesignerIcon(FIcons[0]));
    ie.Add(wl, ARR_REC_GEOSDISPLAYRES[GEOSDispMode].Height - 20, 'DoIconCancel',
            TGEOSDesignerIcon(FIcons[1]));
    ie.ShowMouse:= True;

    ie.Active:= True;
    FElements.Add(ie);
    end;

procedure TGEOSDesignerMainDMod.OnMainShow;
    begin
    if  FFirstTime then
        begin
        if  GEOSDesignerNewForm.ShowModal <> mrOk then
            begin
            Application.Terminate;
            Application.MainForm.Visible:= False;
            Exit;
            end;

        FProjectName:= GEOSDesignerNewForm.EdtProjectName.Text;

        if  GEOSDesignerNewForm.CmbMode.ItemIndex = 0 then
            SetGEOSDispMode(gdm40Column)
        else if  GEOSDesignerNewForm.CmbMode.ItemIndex = 1 then
            SetGEOSDispMode(gdm80Column)
        else
            SetGEOSDispMode(gdm80ColM65);

        DoOnInit;

        if  not GEOSDesignerNewForm.ChkBxBlank.Checked then
            begin
            FDirty:= True;
            DoCreateDefaultProject;
            end
        else
            FDirty:= False;

        DoSetAppTitle;

        GEOSDesignerOnInit:= @DoOnInit;
        GEOSDesignerOnChange:= @DoOnChange;

        GEOSDesignerPreviewForm.Show;
        GEOSDesignerIconEditForm.Show;

        if  not Assigned(FMainFrame) then
            FMainFrame:= TGEOSDesignerMainFrame.Create(Self);
        FMainFrame.Parent:= Application.MainForm;
        FMainFrame.Align:= alClient;

        FMainFrame.InitialiseDisplay;
        DoOnChange;

        FFirstTime:= False;
        end;
    end;

procedure TGEOSDesignerMainDMod.OnMainClose(var ACloseAction: TCloseAction);
    begin
    if  FDirty then
        if  MessageDlg(STR_CAP_GEOSDESIGNER, STR_MSG_GEOSDESIGNCLSD,
                mtConfirmation, mbYesNo, -1) = mrNo then
            begin
            ACloseAction:= caNone;
            Exit;
            end;

    if  Assigned(FMainFrame) then
        begin
        FMainFrame.Parent:= nil;
        FreeAndNil(FMainFrame);
        end;
    end;

procedure TGEOSDesignerMainDMod.Changed;
    begin
    DoOnChange;
    end;

procedure TGEOSDesignerMainDMod.AddIcon(const AIcon: TGEOSDesignerIcon);
    begin
    FIcons.Add(AIcon);

    if  Assigned(FMainFrame.SelectedElem) then
        if  FMainFrame.SelectedElem is TGEOSDoIconsElement then
            FMainFrame.RedisplayActiveElement;
    end;

procedure TGEOSDesignerMainDMod.RemoveIcon(const AIcon: TGEOSDesignerIcon);
    begin
    FIcons.Remove(AIcon);

    if  Assigned(FMainFrame.SelectedElem) then
        if  FMainFrame.SelectedElem is TGEOSDoIconsElement then
            FMainFrame.RedisplayActiveElement;
    end;

end.

