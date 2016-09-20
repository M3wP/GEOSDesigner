//------------------------------------------------------------------------------
//GEOSDesignerCore
//================
//Core application classes, types and constants for the GEOS Designer
//application.
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
unit GEOSDesignerCore;

{$mode objfpc}{$H+}

interface

uses
    Graphics, Contnrs, Classes, SysUtils, GEOSTypes, Laz2_DOM;

const
    SET_CHR_GEOSDSGNIDENT: TSysCharSet = ['a'..'z', 'A'..'Z', '0'..'9', '_'];

type
    TGEOSDesignerOnInit = procedure of object;
    TGEOSDesignerOnChange = procedure of object;

{ TGEOSDesignerElement }

    TGEOSDesignerElement = class(TObject)
    protected
        FActive: Boolean;
        FIdentifier: string;
        FDirty: Boolean;
        FLock: Integer;

        procedure DoChangedUpdate;
        procedure DoChanged;

        procedure SetActive(const AValue: Boolean); virtual;

    public
        constructor Create(const AIdent: string); virtual;
        destructor  Destroy; override;

        class function ElementName: string; virtual; abstract;

        procedure PreparePreview(const ABitmap: TBitmap); virtual; abstract;
        procedure PrepareCodeInit(const AStrings: TStrings); virtual; abstract;
        procedure PrepareCode(const AStrings: TStrings); virtual; abstract;
        procedure PrepareData(const AStrings: TStrings); virtual; abstract;

        procedure SaveToXML(const ADoc: TXMLDocument;
                const AParentNode: TDOMNode); virtual; abstract;
        procedure LoadFromXML(const ASourceNode: TDOMNode); virtual; abstract;

        procedure Lock;
        procedure Unlock;

        property  Active: Boolean read FActive write SetActive;
        property  Identifier: string read FIdentifier;
    end;

    TGEOSDesignerElementClass = class of TGEOSDesignerElement;

    { TGEOSDesignerIcon }

    TGEOSDesignerIcon = class(TGEOSBitmap)
    protected
        FIdentifier: string;
        FSystem: Boolean;
        FRefCount: Integer;

    public
        constructor Create(const AIdent: string);
        destructor  Destroy; override;

        procedure IncrementRef;
        procedure DecrementRef;

        property  Identifier: string read FIdentifier;
        property  System: Boolean read FSystem write FSystem;
        property  RefCount: Integer read FRefCount;
    end;

//------------------------------------------------------------------------------
//GraphicsString (and PutString)
//dengland  I'm wondering if having a TGEOSPutStringElement is warranted.
//------------------------------------------------------------------------------
    TGEOSGraphicsInstrType = (ggiGraphics, ggiString);
    PGEOSGraphicsInstr = ^TGEOSGraphicsInstr;
    TGEOSGraphicsInstr = record
        InstrType: TGEOSGraphicsInstrType;
        InstrCmd: Byte;
        InstrData: array of Byte;
        DoubleW: Boolean;
        Add1W: Boolean;
    end;

const
    VAL_CMD_GEOSGSTR_MOVETO = 1;
    VAL_CMD_GEOSGSTR_LINETO = 2;
    VAL_CMD_GEOSGSTR_RECTTO = 3;
    VAL_CMD_GEOSGSTR_NEWPTN = 5;
    VAL_CMD_GEOSGSTR_ESCPTS = 6;
    VAL_CMD_GEOSGSTR_FMRECT = 7;

    VAL_CMD_GEOSPSTR_BAKSPC = 8;
    VAL_CMD_GEOSPSTR_FWDSPC = 9;
    VAL_CMD_GEOSPSTR_LNFEED = 10;
    VAL_CMD_GEOSPSTR_HOMEPS = 11;
    VAL_CMD_GEOSPSTR_UPLINE = 12;
    VAL_CMD_GEOSPSTR_CRRTRN = 13;
    VAL_CMD_GEOSPSTR_UNDLON = 14;
    VAL_CMD_GEOSPSTR_UNDLOF = 15;
    VAL_CMD_GEOSPSTR_ESCGRP = 16;
    VAL_CMD_GEOSPSTR_ESCRLR = 17;
    VAL_CMD_GEOSPSTR_REVSON = 18;
    VAL_CMD_GEOSPSTR_REVSOF = 19;
    VAL_CMD_GEOSPSTR_GOTOXP = 20;
    VAL_CMD_GEOSPSTR_GOTOYP = 21;
    VAL_CMD_GEOSPSTR_GOTOXY = 22;
    VAL_CMD_GEOSPSTR_NEWFNT = 23;
    VAL_CMD_GEOSPSTR_BOLDON = 24;
    VAL_CMD_GEOSPSTR_ITLCON = 25;
    VAL_CMD_GEOSPSTR_OUTLON = 26;
    VAL_CMD_GEOSPSTR_PLNTXT = 27;

    VAL_CMD_GEOSPSTR_USELST = 127;
    VAL_CMD_GEOSPSTR_SHRTCT = 128;

//dengland  This is a dummy command to say "just print some text" (will result
//      in no escape being output in final data).
    VAL_CMD_GEOSPSTR_PUTSTR = $FF;

    ARR_LIT_GEOSGRPHSTRCMDS: array[0..27] of string = (
        '',                                 //    0,
        'MovePenTo',                        //    VAL_CMD_GEOSGSTR_MOVETO = 1;
        'LineTo',                           //    VAL_CMD_GEOSGSTR_LINETO = 2;
        'RectangleTo',                      //    VAL_CMD_GEOSGSTR_RECTTO = 3;
        '',                                 //    4,
        'NewPattern',                       //    VAL_CMD_GEOSGSTR_NEWPTN = 5;
        'Esc_PutString',                    //    VAL_CMD_GEOSGSTR_ESCPTS = 6;
        'Frame_RecTo',                      //    VAL_CMD_GEOSGSTR_FMRECT = 7;

        'BackSpace',                        //    VAL_CMD_GEOSPSTR_BAKSPC = 8;
        'ForwardSpace',                     //    VAL_CMD_GEOSPSTR_FWDSPC = 9;
        'LF',                               //    VAL_CMD_GEOSPSTR_LNFEED = 10;
        'Home',                             //    VAL_CMD_GEOSPSTR_HOMEPS = 11;
        'UpLine',                           //    VAL_CMD_GEOSPSTR_UPLINE = 12;
        'CR',                               //    VAL_CMD_GEOSPSTR_CRRTRN = 13;
        'UnderlineOn',                      //    VAL_CMD_GEOSPSTR_UNDLON = 14;
        'UnderlineOff',                     //    VAL_CMD_GEOSPSTR_UNDLOF = 15;
        'Esc_Graphics',                     //    VAL_CMD_GEOSPSTR_ESCGRP = 16,
        'Esc_Ruler',                        //    VAL_CMD_GEOSPSTR_ESCRLR = 17;
        'ReverseOn',                        //    VAL_CMD_GEOSPSTR_REVSON = 18;
        'ReverseOff',                       //    VAL_CMD_GEOSPSTR_REVSOF = 19;
        'GotoX',                            //    VAL_CMD_GEOSPSTR_GOTOXP = 20;
        'GotoY',                            //    VAL_CMD_GEOSPSTR_GOTOYP = 21;
        'GotoXY',                           //    VAL_CMD_GEOSPSTR_GOTOXY = 22;
        'NewCardset',                       //    VAL_CMD_GEOSPSTR_NEWFNT = 23;
        'BoldOn',                           //    VAL_CMD_GEOSPSTR_BOLDON = 24;
        'ItalicOn',                         //    VAL_CMD_GEOSPSTR_ITLCON = 25;
        'OutlineOn',                        //    VAL_CMD_GEOSPSTR_OUTLON = 26;
        'PlainText');                       //    VAL_CMD_GEOSPSTR_PLNTXT = 27;

    LIT_CMD_GEOSPSTR_USELST = 'UseLast';
    LIT_CMD_GEOSPSTR_SHRTCT = 'ShortCut';
                                            //    VAL_CMD_GEOSPSTR_PUTSTR = $FF;

type

{ TGEOSGraphicsStrElement }

    TGEOSGraphicsStrElement = class(TGEOSDesignerElement)
    private
        FItems: TList;

        function  GetCount: Integer;
        function  GetItems(const AIndex: Integer): PGEOSGraphicsInstr;

    protected
        FMode: TGEOSGraphicsInstrType;
        FDefaultMode: TGEOSGraphicsInstrType;

    public
        constructor Create(const AIdent: string); override;
        destructor  Destroy; override;

        procedure AddItem(const AType: TGEOSGraphicsInstrType;
                const ACmd: Byte; const AData: array of Byte;
                const ADoubleW: Boolean = False; const AAdd1W: Boolean = False);
        procedure DeleteItem(const AIndex: Integer);

        class function ElementName: string; override;

        procedure PreparePreview(const ABitmap: TBitmap); override;
        procedure PrepareCodeInit(const AStrings: TStrings); override;
        procedure PrepareCode(const AStrings: TStrings); override;
        procedure PrepareData(const AStrings: TStrings); override;

        procedure SaveToXML(const ADoc: TXMLDocument;
                const AParent: TDOMNode); override;
        procedure LoadFromXML(const ASource: TDOMNode); override;

        property  Count: Integer read GetCount;
        property  Items[const AIndex: Integer]: PGEOSGraphicsInstr
                read GetItems; default;
        property  Mode: TGEOSGraphicsInstrType read FMode;
    end;

{ TGEOSPutStringElement }

    TGEOSPutStringElement = class(TGEOSGraphicsStrElement)
    private
        FStartX,
        FStartY: Word;
        FDoubleW,
        FAdd1W: Boolean;

        procedure SetStartX(const AValue: Word);
        procedure SetStartY(const AValue: Word);
        procedure SetDoubleW(const AValue: Boolean);
        procedure SetAdd1W(const AValue: Boolean);

    public
        constructor Create(const AIdent: string); override;

        class function ElementName: string; override;

        procedure PreparePreview(const ABitmap: TBitmap); override;
        procedure PrepareCodeInit(const AStrings: TStrings); override;

        procedure SaveToXML(const ADoc: TXMLDocument;
                const AParent: TDOMNode); override;
        procedure LoadFromXML(const ASource: TDOMNode); override;

        property  StartX: Word read FStartX write SetStartX;
        property  StartY: Word read FStartY write SetStartY;
        property  DoubleW: Boolean read FDoubleW write SetDoubleW;
        property  Add1W: Boolean read FAdd1W write SetAdd1W;
    end;


//------------------------------------------------------------------------------
//DoMenu
//------------------------------------------------------------------------------
    TGEOSMenuAlignment = (gmaHorizontal, gmaVertical);
    TGEOSMenuType = (gmtSubMenu, gmtMenuAction, gmtDynamicSubMenu);
    TGEOSDoMenuElement = class;

{ TGEOSDoMenuItem }

    TGEOSDoMenuItem = class(TObject)
    private
        FIdentifier: string;
        FAlignment: TGEOSMenuAlignment;
        FBounds: TRect;
        FConstrained: Boolean;
        FVisible: Boolean;
        FControlItem: TGEOSDoMenuItem;
        FParent: TGEOSDoMenuItem;
        FElement: TGEOSDoMenuElement;
        FText: string;
        FMenuType: TGEOSMenuType;
        FSubItems: TObjectList;

        function  GetAlignment: TGEOSMenuAlignment;
        function  GetBounds: TRect;
        procedure SetBounds(const AValue: TRect);
        function  GetConstrained: Boolean;
        procedure SetConstrained(const AValue: Boolean);
        function  GetVisible: Boolean;
        procedure SetVisible(const AValue: Boolean);

        procedure SetText(const AValue: string);
        procedure SetMenuType(const AValue: TGEOSMenuType);

        function  GetSubItemsCount: Integer;
        function  GetSubItems(const AIndex: Integer): TGEOSDoMenuItem;

    protected
        procedure Add(AMenuItem: TGEOSDoMenuItem);
        procedure PreparePreview(const ABitmap: TBitmap);

    public
        constructor Create(const AIdent: string; const AType: TGEOSMenuType;
                const ASibling: TGEOSDoMenuItem); overload;
        constructor Create(const AIdent: string; const AType: TGEOSMenuType;
                const AParent: TGEOSDoMenuItem;
                const AAlign: TGEOSMenuAlignment); overload;
        constructor Create(const AIdent: string; const AType: TGEOSMenuType;
                const AMenuElement: TGEOSDoMenuElement;
                const AAlign: TGEOSMenuAlignment); overload;
        destructor  Destroy; override;

        function  Remove(AMenuItem: TGEOSDoMenuItem): Integer;

        property  Identifier: string read FIdentifier;

        property  Parent: TGEOSDoMenuItem read FParent;
        property  Element: TGEOSDoMenuElement read FElement;

        property  Alignment: TGEOSMenuAlignment read GetAlignment;
        property  Bounds: TRect read GetBounds write SetBounds;
        property  Constrained: Boolean read GetConstrained write SetConstrained;
        property  Visible: Boolean read GetVisible write SetVisible;

        property  Text: string read FText write SetText;
        property  MenuType: TGEOSMenuType read FMenuType write SetMenuType;

        property  SubItemsCount: Integer read GetSubItemsCount;
        property  SubItems[const AIndex: Integer]: TGEOSDoMenuItem read GetSubItems;
    end;

{ TGEOSDoMenuElement }

    TGEOSDoMenuElement = class(TGEOSDesignerElement)
    private
        FItems: TObjectList;

        function  GetCount: Integer;
        function  GetItems(const AIndex: Integer): TGEOSDoMenuItem;

    protected
        procedure SetActive(const AValue: Boolean); override;

        procedure Add(AMenuItem: TGEOSDoMenuItem);

    public
        constructor Create(const AIdent: string); override;
        destructor  Destroy; override;

        class function ElementName: string; override;

        procedure PreparePreview(const ABitmap: TBitmap); override;
        procedure PrepareCodeInit(const AStrings: TStrings); override;
        procedure PrepareCode(const AStrings: TStrings); override;
        procedure PrepareData(const AStrings: TStrings); override;

        procedure SaveToXML(const ADoc: TXMLDocument;
                const AParent: TDOMNode); override;
        procedure LoadFromXML(const ASourceNode: TDOMNode); override;

        function  Remove(AMenuItem: TGEOSDoMenuItem): Integer;

        property  Count: Integer read GetCount;
        property  Items[const AIndex: Integer]: TGEOSDoMenuItem read GetItems;
    end;


//------------------------------------------------------------------------------
//DoIcons
//------------------------------------------------------------------------------

{ TGEOSDoIconsElement }

    TGEOSDoIconsElement = class(TGEOSDesignerElement)
    private type
        PGEOSIconDetails = ^TGEOSIconDetails;
        TGEOSIconDetails = packed record
            X,
            Y: Word;
            Ident: string;
            Icon: TGEOSDesignerIcon;
            DblBWidth,
            DblBX: Boolean;
        end;

    private
        FXPos,
        FYPos: Word;
        FDoubleW,
        FAdd1W: Boolean;
        FDetails: TList;
        FShowMouse: Boolean;

    protected
        procedure SetActive(const AValue: Boolean); override;
        procedure SetShowMouse(const AValue: Boolean);

        procedure SetXPos(const AValue: Word);
        procedure SetYPos(const AValue: Word);
        procedure SetDoubleW(const AValue: Boolean);
        procedure SetAdd1W(const AValue: Boolean);

        function  GetCount: Integer;
        function  GetIcons(const AIndex: Integer): TGEOSDesignerIcon;
        procedure SetIcons(const AIndex: Integer; const AIcon: TGEOSDesignerIcon);
        function  GetIconsXPos(const AIndex: Integer): Word;
        procedure SetIconsXPos(const AIndex: Integer; const AValue: Word);
        function  GetIconsYPos(const AIndex: Integer): Word;
        procedure SetIconsYPos(const AIndex: Integer; const AValue: Word);
        function  GetIconsIdent(const AIndex: Integer): string;
        function  GetIconsDblBX(const AIndex: Integer): Boolean;
        procedure SetIconsDblBX(const AIndex: Integer; const AValue: Boolean);
        function  GetIconsDblBWidth(const AIndex: Integer): Boolean;
        procedure SetIconsDblBWidth(const AIndex: Integer; const AValue: Boolean);

    public
        constructor Create(const AIdent: string); override;
        destructor  Destroy; override;

        class function ElementName: string; override;

        procedure PreparePreview(const ABitmap: TBitmap); override;
        procedure PrepareCodeInit(const AStrings: TStrings); override;
        procedure PrepareCode(const AStrings: TStrings); override;
        procedure PrepareData(const AStrings: TStrings); override;

        procedure SaveToXML(const ADoc: TXMLDocument;
                const AParent: TDOMNode); override;
        procedure LoadFromXML(const ASourceNode: TDOMNode); override;

        procedure Add(const AX, AY: Word; const AIdent: string;
                const AIcon: TGEOSDesignerIcon;
                const ADblWidth: Boolean = False;
                const ADblX: Boolean = False);
        procedure Delete(const AIndex: Integer);

        property  ShowMouse: Boolean read FShowMouse write SetShowMouse;
        property  XPos: Word read FXPos write SetXPos;
        property  DoubleW: Boolean read FDoubleW write SetDoubleW;
        property  Add1W: Boolean read FAdd1W write SetAdd1W;
        property  YPos: Word read FYPos write SetYPos;
        property  Count: Integer read GetCount;
        property  Icons[const AIndex: Integer]: TGEOSDesignerIcon
                read GetIcons write SetIcons; default;
        property  IconsIdent[const AIndex: Integer]: string read GetIconsIdent;
        property  IconsXPos[const AIndex: Integer]: Word read GetIconsXPos
                write SetIconsXPos;
        property  IconsYPos[const AIndex: Integer]: Word read GetIconsYPos
                write SetIconsYPos;
        property  IconsDblBWidth[const AIndex: Integer]: Boolean
                read  GetIconsDblBWidth write SetIconsDblBWidth;
        property  IconsDblBX[const AIndex: Integer]: Boolean
                read  GetIconsDblBX write SetIconsDblBX;
    end;

//------------------------------------------------------------------------------
//DoDlgBox
//------------------------------------------------------------------------------




function  GEOSDispMode: TGEOSDisplayMode; inline;
procedure SetGEOSDispMode(const AValue: TGEOSDisplayMode);
function  GEOSShowMouse: Boolean; inline;
function  GEOSMouseXPos: Word; inline;
function  GEOSMouseYPos: Word; inline;

var
    GEOSDesignerIdents: TStringList;
    GEOSDesignerOnInit: TGEOSDesignerOnInit;
    GEOSDesignerOnChange: TGEOSDesignerOnChange;
    GEOSDesignerElements: TClassList;

implementation

uses
    GEOSGraphics, GEOSFont, DModGEOSDesignerMain;

const
    LIT_CAP_GEOSELEMGRPHSTR = 'GraphicsString';
    LIT_CAP_GEOSELEMPUTSTRG = 'PutString';
    LIT_CAP_GEOSELEMENTMENU = 'DoMenu';
    LIT_CAP_GEOSELEMENTICON = 'DoIcons';

var
    FDispMode: TGEOSDisplayMode;
    FShowingMouse: TGEOSDoIconsElement;
    FActiveMenu: TGEOSDoMenuElement;
    FActiveIcons: TGEOSDoIconsElement;


procedure RegisterElements;
    begin
    GEOSDesignerElements.Add(TGEOSGraphicsStrElement);
    GEOSDesignerElements.Add(TGEOSPutStringElement);
    GEOSDesignerElements.Add(TGEOSDoMenuElement);
    GEOSDesignerElements.Add(TGEOSDoIconsElement);
    end;

procedure InitialiseFont;
    begin
    GEOSSystemFont.Lock;
    try
        if  FDispMode = gdm40Column then
            begin
            GEOSSystemFont.PenColor:= clC64DkGrey;
            GEOSSystemFont.BrushColor:= clC64LtGrey;
            end
        else
            begin
            GEOSSystemFont.PenColor:= clBlack;
            GEOSSystemFont.BrushColor:= clVDCLtGrey;
            end;

        GEOSSystemFont.LoadSystemFont(FDispMode);

        finally
        GEOSSystemFont.Unlock;
        end;
    end;

function GEOSDispMode: TGEOSDisplayMode; inline;
    begin
    Result:= FDispMode;
    end;

procedure SetGEOSDispMode(const AValue: TGEOSDisplayMode);
    begin
    if  AValue <> FDispMode then
        begin
        FDispMode:= AValue;

        InitialiseFont;

        if  Assigned(GEOSDesignerOnInit) then
            GEOSDesignerOnInit;

        if  Assigned(GEOSDesignerOnChange) then
            GEOSDesignerOnChange;
        end;
    end;

function GEOSShowMouse: Boolean;
    begin
    Result:= Assigned(FShowingMouse) and FShowingMouse.Active;
    end;

function GEOSMouseXPos: Word;
    begin
    if  Assigned(FShowingMouse) then
        Result:= GEOSNormalizeX(FShowingMouse.FXPos, FShowingMouse.FDoubleW,
                FShowingMouse.FAdd1W)
    else
        Result:= 0;
    end;

function GEOSMouseYPos: Word;
    begin
    if  Assigned(FShowingMouse) then
        Result:= FShowingMouse.FYPos
    else
        Result:= 0;
    end;

procedure SetGEOSShowMouse(const AValue: TGEOSDoIconsElement);
    begin
    if  AValue <> FShowingMouse then
        begin
        if  Assigned(FShowingMouse) then
            FShowingMouse.FShowMouse:= False;

        FShowingMouse:= AValue;

//      if  Assigned(GEOSDesignerOnChange) then
//          GEOSDesignerOnChange;
        end;
    end;

procedure SetGEOSActiveMenu(const AValue: TGEOSDoMenuElement);
    begin
    if  AValue <> FActiveMenu then
        begin
        if  Assigned(FActiveMenu) then
            FActiveMenu.FActive:= False;

        FActiveMenu:= AValue;

//      if  Assigned(GEOSDesignerOnChange) then
//          GEOSDesignerOnChange;
        end;
    end;

procedure SetGEOSActiveIcons(const AValue: TGEOSDoIconsElement);
    begin
    if  AValue <> FActiveIcons then
        begin
        if  Assigned(FActiveIcons) then
            FActiveIcons.FActive:= False;

        FActiveIcons:= AValue;

//      if  Assigned(GEOSDesignerOnChange) then
//          GEOSDesignerOnChange;
        end;
    end;

{ TGEOSPutStringElement }

procedure TGEOSPutStringElement.SetStartX(const AValue: Word);
    begin
    if  FStartX <> AValue then
        begin
        FStartX:= AValue;
        DoChanged;
        end;
    end;

procedure TGEOSPutStringElement.SetStartY(const AValue: Word);
    begin
    if  FStartY <> AValue then
        begin
        FStartY:= AValue;
        DoChanged;
        end;
    end;

procedure TGEOSPutStringElement.SetDoubleW(const AValue: Boolean);
    begin
    if  FDoubleW <> AValue then
        begin
        FDoubleW:= AValue;
        DoChanged;
        end;
    end;

procedure TGEOSPutStringElement.SetAdd1W(const AValue: Boolean);
    begin
    if  FAdd1W <> AValue then
        begin
        FAdd1W:= AValue;
        DoChanged;
        end;
    end;

constructor TGEOSPutStringElement.Create(const AIdent: string);
    begin
    inherited Create(AIdent);

    FDefaultMode:= ggiString;
    FMode:= ggiString;
    end;

class function TGEOSPutStringElement.ElementName: string;
    begin
    Result:= LIT_CAP_GEOSELEMPUTSTRG;
    end;

procedure TGEOSPutStringElement.PreparePreview(const ABitmap: TBitmap);
    var
    x: Integer;

    begin
    x:= GEOSNormalizeX(FStartX, FDoubleW, FAdd1W);
    ABitmap.Canvas.PenPos:= Point(x, FStartY);

    inherited PreparePreview(ABitmap);
    end;

procedure TGEOSPutStringElement.PrepareCodeInit(const AStrings: TStrings);
    var
    s: string;

    begin
    AStrings.Add(#9#9'LoadW'#9'r0, ' + FIdentifier);

    s:= #9#9'LoadW'#9'r11, ' + IntToHex(FStartX, 4);
    if  FDoubleW then
        s:= s + ' | DOUBLE_W';
    if  FAdd1W then
        s:= s + ' | ADD1_W';

    AStrings.Add(s);

    AStrings.Add(#9#9'LoadB'#9'r1H, ' + IntToHex(FStartY, 2));

    AStrings.Add(#9#9'jsr'#9'PutString');
    AStrings.Add(EmptyStr);
    end;

procedure TGEOSPutStringElement.SaveToXML(const ADoc: TXMLDocument;
        const AParent: TDOMNode);
    var
    en: TDOMElement;

    begin
    en:= ADoc.CreateElement('start');

    en.SetAttribute('x', IntToStr(FStartX));
    en.SetAttribute('y', IntToStr(FStartY));
    en.SetAttribute('doubleW', IntToStr(Ord(FDoubleW)));
    en.SetAttribute('add1W', IntToStr(Ord(FAdd1W)));

    AParent.AppendChild(en);

    inherited SaveToXML(ADoc, AParent);
    end;

procedure TGEOSPutStringElement.LoadFromXML(const ASource: TDOMNode);
    var
    en: TDOMElement;

    begin
    en:= ASource.FindNode('start') as TDOMElement;

    FStartX:= StrToInt(en.AttribStrings['x']);
    FStartY:= StrToInt(en.AttribStrings['y']);
    FDoubleW:= Boolean(StrToInt(en.AttribStrings['doubleW']));
    FAdd1W:= Boolean(StrToInt(en.AttribStrings['add1W']));

    inherited LoadFromXML(ASource);
    end;


{ TGEOSDoIconsElement }

procedure TGEOSDoIconsElement.SetActive(const AValue: Boolean);
    begin
    if  AValue <> FActive then
        begin
        if  FActive then
            SetGEOSActiveIcons(nil)
        else
            SetGEOSActiveIcons(Self);

        FActive:= AValue;
        DoChanged;
        end;
    end;

procedure TGEOSDoIconsElement.SetShowMouse(const AValue: Boolean);
    begin
    if  AValue <> FShowMouse then
        begin
        if  FShowMouse then
            SetGEOSShowMouse(nil)
        else
            SetGEOSShowMouse(Self);

        FShowMouse:= AValue;
        DoChanged;
        end;
    end;

procedure TGEOSDoIconsElement.SetXPos(const AValue: Word);
    begin
    if  AValue <> FXPos then
        begin
        FXPos:= AValue;
        DoChanged;
        end;
    end;

procedure TGEOSDoIconsElement.SetYPos(const AValue: Word);
    begin
    if  AValue <> FYPos then
        begin
        FYPos:= AValue;
        DoChanged;
        end;
    end;

procedure TGEOSDoIconsElement.SetDoubleW(const AValue: Boolean);
    begin
    if  AValue <> FDoubleW then
        begin
        FDoubleW:= AValue;
        DoChanged;
        end;
    end;

procedure TGEOSDoIconsElement.SetAdd1W(const AValue: Boolean);
    begin
    if  AValue <> FAdd1W then
        begin
        FAdd1W:= AValue;
        DoChanged;
        end;
    end;

function TGEOSDoIconsElement.GetCount: Integer;
    begin
    Result:= FDetails.Count;
    end;

function TGEOSDoIconsElement.GetIcons(
        const AIndex: Integer): TGEOSDesignerIcon;
    begin
    Result:= PGEOSIconDetails(FDetails[AIndex])^.Icon;
    end;

procedure TGEOSDoIconsElement.SetIcons(const AIndex: Integer;
        const AIcon: TGEOSDesignerIcon);
    begin
    if  PGEOSIconDetails(FDetails[AIndex])^.Icon <> AIcon then
        begin
        PGEOSIconDetails(FDetails[AIndex])^.Icon:= AIcon;
        DoChanged;
        end;
    end;

function TGEOSDoIconsElement.GetIconsXPos(const AIndex: Integer): Word;
    begin
    Result:= PGEOSIconDetails(FDetails[AIndex])^.X;
    end;

procedure TGEOSDoIconsElement.SetIconsXPos(const AIndex: Integer;
        const AValue: Word);
    begin
    if  PGEOSIconDetails(FDetails[AIndex])^.X <> AValue then
        begin
        PGEOSIconDetails(FDetails[AIndex])^.X:= AValue;
        DoChanged;
        end;
    end;

function TGEOSDoIconsElement.GetIconsYPos(const AIndex: Integer): Word;
    begin
    Result:= PGEOSIconDetails(FDetails[AIndex])^.Y;
    end;

procedure TGEOSDoIconsElement.SetIconsYPos(const AIndex: Integer;
        const AValue: Word);
    begin
    if  PGEOSIconDetails(FDetails[AIndex])^.Y <> AValue then
        begin
        PGEOSIconDetails(FDetails[AIndex])^.Y:= AValue;
        DoChanged;
        end;
    end;

function TGEOSDoIconsElement.GetIconsIdent(const AIndex: Integer): string;
    begin
    Result:= PGEOSIconDetails(FDetails[AIndex])^.Ident;
    end;

function TGEOSDoIconsElement.GetIconsDblBX(const AIndex: Integer): Boolean;
    begin
    Result:= PGEOSIconDetails(FDetails[AIndex])^.DblBX;
    end;

procedure TGEOSDoIconsElement.SetIconsDblBX(const AIndex: Integer;
        const AValue: Boolean);
    begin
    if  PGEOSIconDetails(FDetails[AIndex])^.DblBX <> AValue then
        begin
        PGEOSIconDetails(FDetails[AIndex])^.DblBX:= AValue;
        DoChanged;
        end;
    end;

function TGEOSDoIconsElement.GetIconsDblBWidth(const AIndex: Integer): Boolean;
    begin
    Result:= PGEOSIconDetails(FDetails[AIndex])^.DblBWidth;
    end;

procedure TGEOSDoIconsElement.SetIconsDblBWidth(const AIndex: Integer;
        const AValue: Boolean);
    begin
    if  PGEOSIconDetails(FDetails[AIndex])^.DblBWidth <> AValue then
        begin
        PGEOSIconDetails(FDetails[AIndex])^.DblBWidth:= AValue;
        DoChanged;
        end;
    end;

constructor TGEOSDoIconsElement.Create(const AIdent: string);
    begin
    inherited Create(AIdent);

    FDetails:= TList.Create;
    FActive:= False;
    end;

destructor TGEOSDoIconsElement.Destroy;
    var
    i: Integer;
    d: PGEOSIconDetails;

    begin
    for i:= FDetails.Count - 1 downto 0 do
        begin
        d:= PGEOSIconDetails(FDetails[i]);

        d^.Icon.DecrementRef;
        GEOSDesignerIdents.Delete(GEOSDesignerIdents.IndexOf(d^.Ident));

        Dispose(d);
        end;

    FDetails.Free;

    inherited Destroy;
    end;

class function TGEOSDoIconsElement.ElementName: string;
    begin
    Result:= LIT_CAP_GEOSELEMENTICON;
    end;

procedure TGEOSDoIconsElement.PreparePreview(const ABitmap: TBitmap);
    var
    i: Integer;
    d: PGEOSIconDetails;

    begin
    for i:= 0 to FDetails.Count - 1 do
        begin
        d:= PGEOSIconDetails(FDetails[i]);
        GEOSBitmapUp(ABitmap.Canvas, d^.X, d^.Y, d^.Icon, d^.DblBWidth,
                False, d^.DblBX);
        end;
    end;

procedure TGEOSDoIconsElement.PrepareCodeInit(const AStrings: TStrings);
    begin
    AStrings.Add(#9#9'LoadW'#9'r0, ' + FIdentifier);
    AStrings.Add(#9#9'jsr'#9 + 'DoIcons');
    AStrings.Add(EmptyStr);
    end;

procedure TGEOSDoIconsElement.PrepareCode(const AStrings: TStrings);
    var
    i: Integer;
    d: PGEOSIconDetails;

    begin
    for i:= 0 to FDetails.Count - 1 do
        begin
        d:= PGEOSIconDetails(FDetails[i]);
        AStrings.Add(d^.Ident + ':');
        AStrings.Add(#9#9'rts');
        AStrings.Add(EmptyStr);
        end;
    end;

procedure TGEOSDoIconsElement.PrepareData(const AStrings: TStrings);
    var
    i: Integer;
    d: PGEOSIconDetails;
    s: string;

    begin
    AStrings.Add(FIdentifier + ':');
    AStrings.Add(#9#9'.byte'#9'$' + IntToHex(FDetails.Count, 2));

    s:= #9#9'.word'#9'$' + IntToHex(FXPos, 4);
    if  FDoubleW then
        s:= s + ' | DOUBLE_W';
    if  FAdd1W then
        s:= s + ' | ADD1_W';

    AStrings.Add(s);

    AStrings.Add(#9#9'.byte'#9'$' + IntToHex(FYPos, 2));

    for i:= 0 to FDetails.Count - 1 do
        begin
        d:= PGEOSIconDetails(FDetails[i]);

        AStrings.Add(#9#9'.word'#9 + d^.Icon.Identifier);

        s:= #9#9'.byte'#9'$' + IntToHex(d^.X, 2);
        if  d^.DblBX then
            s:= s + ' | DOUBLE_B';
        AStrings.Add(s);

        AStrings.Add(#9#9'.byte'#9'$' + IntToHex(d^.Y, 2));

        s:= #9#9'.byte'#9'$' + IntToHex(d^.Icon.Width div 8, 2);
        if  d^.DblBWidth then
            s:= s + ' | DOUBLE_B';
        AStrings.Add(s);

        AStrings.Add(#9#9'.byte'#9'$' + IntToHex(d^.Icon.Height, 2));

        AStrings.Add(#9#9'.word'#9 + d^.Ident);
        end;

    AStrings.Add(EmptyStr);
    end;

procedure TGEOSDoIconsElement.SaveToXML(const ADoc: TXMLDocument;
        const AParent: TDOMNode);
    var
    it: TDOMElement;
    cn: TDOMElement;
    d: PGEOSIconDetails;
    i: Integer;

    begin
    cn:= ADoc.CreateElement('details');
    cn.SetAttribute('showMouse', IntToStr(Ord(FShowMouse)));
    cn.SetAttribute('xPos', IntToStr(FXPos));
    cn.SetAttribute('doubleW', IntToStr(Ord(FDoubleW)));
    cn.SetAttribute('add1W', IntToStr(Ord(FAdd1W)));
    cn.SetAttribute('yPos', IntToStr(FYPos));

    AParent.AppendChild(cn);

    cn:= ADoc.CreateElement('items');
    cn.SetAttribute('count', IntToStr(FDetails.Count));

    for i:= 0 to FDetails.Count - 1 do
        begin
        it:= ADoc.CreateElement('item');
        it.SetAttribute('index', IntToSTr(i));

        d:= PGEOSIconDetails(FDetails[i]);

        it.SetAttribute('x', IntToStr(d^.X));
        it.SetAttribute('y', IntToStr(d^.Y));
        it.SetAttribute('ident', d^.Ident);
        it.SetAttribute('icon', d^.Icon.Identifier);
        it.SetAttribute('dblBWidth', IntToStr(Ord(d^.DblBWidth)));
        it.SetAttribute('dblBX', IntToStr(Ord(d^.DblBX)));

        cn.AppendChild(it);
        end;

    AParent.AppendChild(cn);
    end;

procedure TGEOSDoIconsElement.LoadFromXML(const ASourceNode: TDOMNode);
    var
    it: TDOMElement;
    cn: TDOMElement;
    d: PGEOSIconDetails;
    i,
    j: Integer;
    ico: TGEOSDesignerIcon;
    s: string;

    begin
    cn:= ASourceNode.FindNode('details') as TDOMElement;

    j:= StrToInt(cn.AttribStrings['showMouse']);
    FShowMouse:= Boolean(j);
    FXPos:= StrToInt(cn.AttribStrings['xPos']);
    j:= StrToInt(cn.AttribStrings['doubleW']);
    FDoubleW:= Boolean(j);
    j:= StrToInt(cn.AttribStrings['add1W']);
    FAdd1W:= Boolean(j);
    FYPos:= StrToInt(cn.AttribStrings['yPos']);

    cn:= ASourceNode.FindNode('items') as TDOMElement;
    it:= cn.FirstChild as TDOMElement;
    while Assigned(it) do
        begin
        New(d);

        d^.X:= StrToInt(it.AttribStrings['x']);
        d^.Y:= StrToInt(it.AttribStrings['y']);
        d^.Ident:= it.AttribStrings['ident'];

        GEOSDesignerIdents.Add(d^.Ident);
        FDetails.Add(d);

        s:= it.AttribStrings['icon'];
        ico:= nil;
        for i:= 0 to GEOSDesignerMainDMod.IconsCount - 1 do
            if  CompareText(s, GEOSDesignerMainDMod.Icons[i].Identifier) = 0 then
                begin
                ico:= GEOSDesignerMainDMod.Icons[i];
                Break;
                end;

        if  not Assigned(ico) then
            raise Exception.Create('Unknown icon reference found in save file!');

        d^.Icon:= ico;
        d^.Icon.IncrementRef;

        d^.DblBWidth:= Boolean(StrToInt(it.AttribStrings['dblBWidth']));
        d^.DblBX:= Boolean(StrToInt(it.AttribStrings['dblBX']));

        it:= it.NextSibling as TDOMElement;
        end;
    end;

procedure TGEOSDoIconsElement.Add(const AX, AY: Word; const AIdent: string;
    const AIcon: TGEOSDesignerIcon; const ADblWidth: Boolean;
    const ADblX: Boolean);
    var
    d: PGEOSIconDetails;

    begin
    GEOSDesignerIdents.Add(AIdent);

    New(d);
    d^.X:= AX;
    d^.Y:= AY;
    d^.Ident:= AIdent;
    d^.Icon:= AIcon;
    d^.DblBWidth:= ADblWidth;
    d^.DblBX:= ADblX;

    FDetails.Add(d);

    AIcon.IncrementRef;

    DoChanged;
    end;

procedure TGEOSDoIconsElement.Delete(const AIndex: Integer);
    var
    d: PGEOSIconDetails;

    begin
    d:= PGEOSIconDetails(FDetails[AIndex]);
    FDetails.Delete(AIndex);

    d^.Icon.DecrementRef;
    GEOSDesignerIdents.Delete(GEOSDesignerIdents.IndexOf(d^.Ident));

    Dispose(d);
    DoChanged;
    end;

{ TGEOSDesignerIcon }

constructor TGEOSDesignerIcon.Create(const AIdent: string);
    begin
    inherited Create;

    FIdentifier:= AIdent;
    GEOSDesignerIdents.Add(AIdent);
    end;

destructor TGEOSDesignerIcon.Destroy;
    begin
    GEOSDesignerIdents.Delete(GEOSDesignerIdents.IndexOf(FIdentifier));

    inherited Destroy;
    end;

procedure TGEOSDesignerIcon.IncrementRef;
    begin
    Inc(FRefCount);
    end;

procedure TGEOSDesignerIcon.DecrementRef;
    begin
    Dec(FRefCount);
    if  FRefCount < 0 then
        FRefCount:= 0;
    end;

{ TGEOSDoMenuElement }

function TGEOSDoMenuElement.GetCount: Integer;
    begin
    Result:= FItems.Count;
    end;

function TGEOSDoMenuElement.GetItems(const AIndex: Integer): TGEOSDoMenuItem;
    begin
    Result:= TGEOSDoMenuItem(FItems[AIndex]);
    end;

procedure TGEOSDoMenuElement.SetActive(const AValue: Boolean);
    begin
    if  AValue <> FActive then
        begin
        if  FActive then
            SetGEOSActiveMenu(nil)
        else
            SetGEOSActiveMenu(Self);

        FActive:= AValue;
        DoChanged;
        end;
    end;

procedure TGEOSDoMenuElement.Add(AMenuItem: TGEOSDoMenuItem);
    begin
    FItems.Add(AMenuItem);
    DoChanged;
    end;

constructor TGEOSDoMenuElement.Create(const AIdent: string);
    begin
    FItems:= TObjectList.Create(True);

    inherited Create(AIdent);

    FActive:= False;
    end;

destructor TGEOSDoMenuElement.Destroy;
    begin
    FItems.Free;

    inherited Destroy;
    end;

class function TGEOSDoMenuElement.ElementName: string;
    begin
    Result:= LIT_CAP_GEOSELEMENTMENU;
    end;

procedure TGEOSDoMenuElement.PreparePreview(const ABitmap: TBitmap);
    procedure DoRecursePreparePreview(const AList: TObjectList);
        var
        i: Integer;

        begin
        if  AList.Count > 0 then
            begin
            TGEOSDoMenuItem(AList[0]).PreparePreview(ABitmap);

            for i:= 0 to AList.Count - 1 do
                DoRecursePreparePreview(TGEOSDoMenuItem(AList[i]).FSubItems);
            end;
        end;

    var
    ss: TGEOSFontStyles;

    begin
    ss:= GEOSSystemFont.SetStyle([]);

    DoRecursePreparePreview(FItems);

    GEOSSystemFont.SetStyle(ss);
    end;

procedure TGEOSDoMenuElement.PrepareCodeInit(const AStrings: TStrings);
    begin
    AStrings.Add(#9#9'LoadW'#9'r0, ' + FIdentifier);
    AStrings.Add(#9#9'jsr'#9 + 'DoMenu');
    AStrings.Add(EmptyStr);
    end;

procedure TGEOSDoMenuElement.PrepareCode(const AStrings: TStrings);
    procedure DoRecurseItems(const AItems: TObjectList);
        var
        i: Integer;
        itm: TGEOSDoMenuItem;

        begin
        for i:= 0 to AItems.Count - 1 do
            begin
            itm:= TGEOSDoMenuItem(AItems[i]);

            if  itm.MenuType in [gmtMenuAction, gmtDynamicSubMenu] then
                begin
                AStrings.Add(itm.FIdentifier + ':');
                AStrings.Add(#9#9'jsr'#9'GotoFirstMenu');
                AStrings.Add(#9#9'rts');
                AStrings.Add(EmptyStr);
                end;

            if  itm.FSubItems.Count > 0 then
                DoRecurseItems(itm.FSubItems);
            end;
        end;

    begin
    DoRecurseItems(FItems);
    end;

procedure TGEOSDoMenuElement.PrepareData(const AStrings: TStrings);
    var
    mt: Integer;

    procedure DoRecurseItems(const AItems: TObjectList; const ALabel: string);
        var
        i: Integer;
        itm: TGEOSDoMenuItem;
        s: string;

        begin
        for i:= 0 to AItems.Count - 1 do
            begin
            itm:= TGEOSDoMenuItem(AItems[i]);

            if  i = 0 then
                begin
                AStrings.Add(ALabel + ':');
                AStrings.Add(#9#9'.byte'#9'$' + IntToHex(itm.FBounds.Top, 2));
                AStrings.Add(#9#9'.byte'#9'$' + IntToHex(itm.FBounds.Bottom, 2));
                AStrings.Add(#9#9'.word'#9'$' + IntToHex(itm.FBounds.Left, 4));
                AStrings.Add(#9#9'.word'#9'$' + IntToHex(itm.FBounds.Right, 4));
                if  itm.FAlignment = gmaHorizontal then
                    s:= 'HORIZONTAL'
                else
                    s:= 'VERTICAL';
                if  itm.FConstrained then
                    s:= s + ' | CONSTRAINED';

                AStrings.Add(#9#9'.byte'#9 + IntToHex(AItems.Count, 2)+' | '+s);
                end;

            s:= FIdentifier + 'Text' + IntToStr(mt);
            Inc(mt);
            AStrings.Add(#9#9'.word'#9 + s);

            if  itm.FMenuType = gmtSubMenu then
                s:= 'SUB_MENU'
            else if itm.FMenuType = gmtDynamicSubMenu then
                s:= 'DYN_SUB_MENU'
            else
                s:= 'MENU_ACTION';
            AStrings.Add(#9#9'.byte'#9 + s);

            AStrings.Add(#9#9'.word'#9 + itm.FIdentifier);
            end;

        for i:= 0 to AItems.Count - 1 do
            begin
            itm:= TGEOSDoMenuItem(AItems[i]);

            if  itm.MenuType = gmtSubMenu then
                DoRecurseItems(itm.FSubItems, itm.FIdentifier);
            end;
        end;

    procedure DoRecurseText(const AItems: TObjectList);
        var
        i: Integer;
        itm: TGEOSDoMenuItem;

        begin
        for i:= 0 to AItems.Count - 1 do
            begin
            itm:= TGEOSDoMenuItem(AItems[i]);
            AStrings.Add(FIdentifier + 'Text' + IntToStr(mt) + ':');
            Inc(mt);
            AStrings.Add(#9#9'.byte'#9'"' + itm.FText + '", $00');
            end;

        for i:= 0 to AItems.Count - 1 do
            begin
            itm:= TGEOSDoMenuItem(AItems[i]);
            if  itm.FSubItems.Count > 0 then
                DoRecurseText(itm.FSubItems);
            end;
        end;

    begin
    mt:= 0;
    DoRecurseItems(FItems, FIdentifier);
    mt:= 0;
    DoRecurseText(FItems);
    AStrings.Add(EmptyStr);
    end;

procedure TGEOSDoMenuElement.SaveToXML(const ADoc: TXMLDocument;
        const AParent: TDOMNode);
    var
    it: TDOMNode;
    cn: TDOMElement;

    procedure DoRecurseItemList(const AList: TObjectList; const ANode: TDOMNode);
        var
        cn,
        sn,
        bn: TDOMElement;
        dn: TDOMText;
        i: Integer;
        itm: TGEOSDoMenuItem;

        begin
        for  i:= 0 to AList.Count - 1 do
            begin
            itm:= TGEOSDoMenuItem(AList[i]);

            cn:= ADoc.CreateElement('item');
            cn.SetAttribute('index', IntToStr(i));
            cn.SetAttribute('identifier', itm.FIdentifier);
            cn.SetAttribute('menutype', IntToStr(Ord(itm.FMenuType)));

            dn:= ADoc.CreateTextNode(itm.FText);
            cn.AppendChild(dn);

            if  i = 0 then
                begin
                sn:= ADoc.CreateElement('control');
                sn.SetAttribute('alignment', IntToStr(Ord(itm.FAlignment)));
                sn.SetAttribute('constrained', IntToStr(Ord(itm.FConstrained)));
                sn.SetAttribute('visible', IntToStr(Ord(itm.FVisible)));

                bn:= ADoc.CreateElement('bounds');
                bn.SetAttribute('top', IntToStr(itm.FBounds.Top));
                bn.SetAttribute('left', IntToStr(itm.FBounds.Left));
                bn.SetAttribute('bottom', IntToStr(itm.FBounds.Bottom));
                bn.SetAttribute('right', IntToStr(itm.FBounds.Right));

                sn.AppendChild(bn);
                cn.AppendChild(sn);
                end;

            if  itm.FSubItems.Count > 0 then
                begin
                sn:= ADoc.CreateElement('subitems');
                sn.SetAttribute('count', IntToStr(itm.FSubItems.Count));

                DoRecurseItemList(itm.FSubItems, sn);
                cn.AppendChild(sn);
                end;

            ANode.AppendChild(cn);
            end;
        end;

    begin
    cn:= ADoc.CreateElement('items');
    cn.SetAttribute('count', IntToStr(FItems.Count));
    it:= AParent.AppendChild(cn);

    DoRecurseItemList(FItems, it);
    end;

procedure TGEOSDoMenuElement.LoadFromXML(const ASourceNode: TDOMNode);
    var
    cn: TDOMElement;

    procedure DoRecurseItems(AItemNode: TDOMElement; AParent: TGEOSDoMenuItem);
        var
        i,
        j: Integer;
        it,
        dn: TDOMElement;
        itm0,
        itm: TGEOSDoMenuItem;
        s: string;
        mt: TGEOSMenuType;
        a: TGEOSMenuAlignment;
        r: TRect;

        begin
        i:= 0;
        itm0:= nil;
        it:= AItemNode.FirstChild as TDOMElement;
        while Assigned(it) do
            begin
            s:= it.AttribStrings['identifier'];
            j:= StrToInt(it.AttribStrings['menutype']);
            mt:= TGEOSMenuType(j);

            if  i = 0 then
                begin
                dn:= it.FindNode('control') as TDOMElement;
                j:= StrToInt(dn.AttribStrings['alignment']);
                a:= TGEOSMenuAlignment(j);

                if  Assigned(AParent) then
                    itm0:= TGEOSDoMenuItem.Create(s, mt, AParent, a)
                else
                    itm0:= TGEOSDoMenuItem.Create(s, mt, Self, a);

                j:= StrToInt(dn.AttribStrings['constrained']);
                itm0.Constrained:= Boolean(j);

                j:= StrToInt(dn.AttribStrings['visible']);
                itm0.Visible:= Boolean(j);

                dn:= dn.FindNode('bounds') as TDOMElement;
                r.Top:= StrToInt(dn.AttribStrings['top']);
                r.Left:= StrToInt(dn.AttribStrings['left']);
                r.Bottom:= StrToInt(dn.AttribStrings['bottom']);
                r.Right:= StrToInt(dn.AttribStrings['right']);

                itm0.Bounds:= r;
                itm:= itm0;
                end
            else
                itm:= TGEOSDoMenuItem.Create(s, mt, itm0);

            itm.Text:= it.FirstChild.NodeValue;

            dn:= it.FindNode('subitems') as TDOMElement;
            if  Assigned(dn) then
                DoRecurseItems(dn, itm);

            Inc(i);
            it:= it.NextSibling as TDOMElement;
            end;
        end;

    begin
    cn:= ASourceNode.FindNode('items') as TDOMElement;

    DoRecurseItems(cn, nil);
    end;

function TGEOSDoMenuElement.Remove(AMenuItem: TGEOSDoMenuItem): Integer;
    var
    i: Integer;
    m,
    c: TGEOSDoMenuItem;

    begin
    Result:= FItems.IndexOf(AMenuItem);

    if  (Result = 0)
    and (FItems.Count > 1) then
        begin
        c:= TGEOSDoMenuItem(FItems[1]);

        for i:= 1 to FItems.Count - 1 do
            begin
            m:= TGEOSDoMenuItem(FItems[i]);
            m.FControlItem:= c;
            m.FAlignment:= c.Alignment;
            m.FBounds:= c.Bounds;
            m.FVisible:= c.Visible;
            end;
        end;

    FItems.Remove(AMenuItem);
    DoChanged;
    end;

{ TGEOSDoMenuItem }

function TGEOSDoMenuItem.GetAlignment: TGEOSMenuAlignment;
    begin
    Result:= FControlItem.FAlignment;
    end;

function TGEOSDoMenuItem.GetBounds: TRect;
    begin
    Result:= FControlItem.FBounds;
    end;

procedure TGEOSDoMenuItem.SetBounds(const AValue: TRect);
    begin
    FControlItem.FBounds:= AValue;
    FElement.DoChanged;
    end;

function TGEOSDoMenuItem.GetConstrained: Boolean;
    begin
    Result:= FControlItem.FConstrained;
    end;

procedure TGEOSDoMenuItem.SetConstrained(const AValue: Boolean);
    begin
    if  AValue <> FControlItem.FConstrained then
        begin
        FControlItem.FConstrained:= AValue;
        FElement.DoChanged;
        end;
    end;

function TGEOSDoMenuItem.GetVisible: Boolean;
    begin
    Result:= FControlItem.FVisible;
    end;

procedure TGEOSDoMenuItem.SetVisible(const AValue: Boolean);
    begin
    if  AValue <> FControlItem.FVisible then
        begin
        FControlItem.FVisible:= AValue;
        FElement.DoChanged;
        end;
    end;

procedure TGEOSDoMenuItem.SetText(const AValue: string);
    begin
    FText:= AValue;
    UniqueString(FText);
    FElement.DoChanged;
    end;

procedure TGEOSDoMenuItem.SetMenuType(const AValue: TGEOSMenuType);
    begin
    if  AValue <> FMenuType then
        begin
        if  (AValue in [gmtDynamicSubMenu, gmtMenuAction])
        and (FSubItems.Count > 0) then
            raise Exception.Create('Unable to change menu type while sub items exist.');

        FMenuType:= AValue;
        FElement.DoChanged;
        end;
    end;

function TGEOSDoMenuItem.GetSubItemsCount: Integer;
    begin
    Result:= FSubItems.Count;
    end;

function TGEOSDoMenuItem.GetSubItems(const AIndex: Integer): TGEOSDoMenuItem;
    begin
    Result:= TGEOSDoMenuItem(FSubItems[AIndex]);
    end;

procedure TGEOSDoMenuItem.Add(AMenuItem: TGEOSDoMenuItem);
    begin
    if  FMenuType = gmtSubMenu then
        begin
        FSubItems.Add(AMenuItem);
        FElement.DoChanged;
        end
    else
        raise Exception.Create('Invalid menu type for adding sub-items');
    end;

procedure TGEOSDoMenuItem.PreparePreview(const ABitmap: TBitmap);
    var
    sp: TPoint;
    cp: Byte;
    mr: TRect;
    mp: TPoint;
    i: Integer;
    lst: TObjectList;
    mi: TGEOSDoMenuItem;
    v: Boolean;

    begin
    if  FControlItem <> Self then
        Exit;

    if  FVisible then
        begin
        sp:= ABitmap.Canvas.PenPos;
        cp:= GEOSSetPattern(0);

        ABitmap.Canvas.PenPos:= Point(FBounds.Left, FBounds.Top);
        GEOSRectangleTo(ABitmap.Canvas, FBounds.Right, FBounds.Bottom);
        GEOSFrameRectTo(ABitmap.Canvas, FBounds.Left, FBounds.Top);

        mp:= Point(FBounds.Left + 4, FBounds.Top + 3);

        if  Assigned(FParent) then
            lst:= FParent.FSubItems
        else
            lst:= FElement.FItems;

        for i:= 0 to lst.Count - 1 do
            begin
            mi:= TGEOSDoMenuItem(lst[i]);

            v:= (mi.FSubItems.Count > 0) and
                    TGEOSDoMenuItem(mi.FSubItems[0]).FVisible;

            GEOSSystemFont.TextOut(ABitmap.Canvas, mp.x, mp.y +
                    GEOSSystemFont.Baseline + 1, mi.FText);

            if  FAlignment = gmaHorizontal then
                begin
                mr:= Rect(mp.x - 4, FBounds.Top + 1,
                        mp.x + GEOSSystemFont.TextExtent(mi.FText).x + 4,
                        FBounds.Bottom - 1);

                mp.x:= mr.Right;

                if  i < (lst.Count - 1) then
                    begin
                    ABitmap.Canvas.PenPos:= Point(mp.x, FBounds.Top);
                    GEOSVertLineTo(ABitmap.Canvas, $55, FBounds.Bottom - 1);
                    end;

                if  v then
                    GEOSInvertRectangle(ABitmap.Canvas, mr);

                Inc(mp.x, 4);
                end
            else
                begin
                mr:= Rect(FBounds.Left + 1, mp.y - 2, FBounds.Right - 1,
                        mp.y + GEOSSystemFont.Size + 1);

                Inc(mp.y, GEOSSystemFont.Size + 2);

                if  i < (lst.Count - 1) then
                    begin
                    ABitmap.Canvas.PenPos:= Point(FBounds.Left, mp.y);
                    GEOSHorzLineTo(ABitmap.Canvas, $AA, FBounds.Right - 1);
                    end;

                if  v then
                    GEOSInvertRectangle(ABitmap.Canvas, mr);

                Inc(mp.y, 2);
                end;
            end;

        GEOSSetPattern(cp);
        ABitmap.Canvas.PenPos:= sp;
        end;
    end;

constructor TGEOSDoMenuItem.Create(const AIdent: string;
        const AType: TGEOSMenuType; const ASibling: TGEOSDoMenuItem);
    begin
    if  not Assigned(ASibling) then
        raise Exception.Create('A sibling menu item must be supplied.');

    GEOSDesignerIdents.Add(AIdent);
    FIdentifier:= AIdent;

    FControlItem:= ASibling.FControlItem;

    FAlignment:= FControlItem.FAlignment;
    FVisible:= FControlItem.FVisible;
    FBounds:= FControlItem.FBounds;

    FMenuType:= AType;

    FParent:= ASibling.FParent;
    FElement:= ASibling.FElement;

    FSubItems:= TObjectList.Create(True);

    if  Assigned(FParent) then
        FParent.FSubItems.Add(Self)
    else
        FElement.FItems.Add(Self);

    FElement.DoChanged;
    end;

constructor TGEOSDoMenuItem.Create(const AIdent: string;
        const AType: TGEOSMenuType; const AParent: TGEOSDoMenuItem;
        const AAlign: TGEOSMenuAlignment);
    begin
    if  not Assigned(AParent) then
        raise Exception.Create('A parent menu item must be supplied.');

    if  AParent.FSubItems.Count > 0 then
        begin
        FControlItem:= TGEOSDoMenuItem(AParent.FSubItems[0]);
        if  FControlItem.Alignment <> AAlign then
            raise Exception.Create('Peer alignment differs to expectation.');

        FVisible:= FControlItem.FVisible;
        FBounds:= FControlItem.FBounds;
        end
    else
        FControlItem:= Self;

    FAlignment:= AAlign;

    FIdentifier:= AIdent;
    GEOSDesignerIdents.Add(AIdent);

    FMenuType:= AType;

    FParent:= AParent;
    FElement:= AParent.FElement;

    FSubItems:= TObjectList.Create(True);

    FParent.Add(Self);
    end;

constructor TGEOSDoMenuItem.Create(const AIdent: string;
        const AType: TGEOSMenuType; const AMenuElement: TGEOSDoMenuElement;
        const AAlign: TGEOSMenuAlignment);
    begin
    if  not Assigned(AMenuElement) then
        raise Exception.Create('A Menu Element must be supplied.');

    if  AMenuElement.FItems.Count > 0 then
        begin
        FControlItem:= TGEOSDoMenuItem(AMenuElement.FItems[0]);
        if  FControlItem.Alignment <> AAlign then
            raise Exception.Create('Peer alignment differs to expectation.');

        FVisible:= FControlItem.FVisible;
        FBounds:= FControlItem.FBounds;
        end
    else
        FControlItem:= Self;

    FAlignment:= AAlign;

    FIdentifier:= AIdent;
    GEOSDesignerIdents.Add(AIdent);

    FMenuType:= AType;

    FParent:= nil;
    FElement:= AMenuElement;

    FSubItems:= TObjectList.Create(True);

    FElement.Add(Self);
    end;

destructor TGEOSDoMenuItem.Destroy;
    begin
    FSubItems.Free;
    GEOSDesignerIdents.Delete(GEOSDesignerIdents.IndexOf(FIdentifier));

    inherited Destroy;
    end;

function TGEOSDoMenuItem.Remove(AMenuItem: TGEOSDoMenuItem): Integer;
    var
    i: Integer;
    m,
    c: TGEOSDoMenuItem;

    begin
    Result:= FSubItems.IndexOf(AMenuItem);

    if  (Result = 0)
    and (FSubItems.Count > 1) then
        begin
        c:= TGEOSDoMenuItem(FSubItems[1]);

        for i:= 1 to FSubItems.Count - 1 do
            begin
            m:= TGEOSDoMenuItem(FSubItems[i]);
            m.FControlItem:= c;
            m.FAlignment:= FAlignment;
            m.FBounds:= FBounds;
            m.FVisible:= FVisible;
            end;
        end;

    FSubItems.Remove(AMenuItem);
    FElement.DoChanged;
    end;

{ TGEOSGraphicsStrElement }

function TGEOSGraphicsStrElement.GetCount: Integer;
    begin
    Result:= FItems.Count;
    end;

function TGEOSGraphicsStrElement.GetItems(
        const AIndex: Integer): PGEOSGraphicsInstr;
    begin
    Result:= PGEOSGraphicsInstr(FItems[AIndex]);
    end;

constructor TGEOSGraphicsStrElement.Create(const AIdent: string);
    begin
    inherited Create(AIdent);

    FDefaultMode:= ggiGraphics;
    FMode:= ggiGraphics;
    FItems:= TList.Create;
    end;

destructor TGEOSGraphicsStrElement.Destroy;
    var
    i: Integer;

    begin
    for i:= FItems.Count - 1 downto 0 do
        Dispose(PGEOSGraphicsInstr(FItems[i]));

    FreeAndNil(FItems);

    inherited Destroy;
    end;

procedure TGEOSGraphicsStrElement.AddItem(const AType: TGEOSGraphicsInstrType;
    const ACmd: Byte; const AData: array of Byte; const ADoubleW: Boolean;
    const AAdd1W: Boolean);
    var
    i: Integer;
    r: PGEOSGraphicsInstr;

    begin
    if  ((FMode = ggiString)
    and  (AType = ggiGraphics))
    or  ((FMode = ggiGraphics)
    and  (AType = ggiString)) then
        raise Exception.Create('Invalid instruction type for current mode.');

    if  (AType = ggiGraphics)
    and not (ACmd in [1..3,5..7]) then
        raise Exception.Create('Invalid command for instruction type.');

    if  (AType = ggiString)
    and not (ACmd in [8..27, 127, 128, 255]) then
        raise Exception.Create('Invalid command for instruction type.');

    if  (FMode = ggiGraphics)
    and (ACmd = VAL_CMD_GEOSGSTR_ESCPTS) then
        FMode:= ggiString
    else if (FMode = ggiString)
    and (ACmd = VAL_CMD_GEOSPSTR_ESCGRP) then
        FMode:= ggiGraphics;

    New(r);

    r^.InstrType:= AType;
    r^.InstrCmd:= ACmd;

    i:= Length(AData);
    SetLength(r^.InstrData, i);
    if  i > 0 then
        Move(AData[0], r^.InstrData[0], i);

    r^.DoubleW:= ADoubleW;
    r^.Add1W:= AAdd1W;

    FItems.Add(r);

    DoChanged;
    end;

procedure TGEOSGraphicsStrElement.DeleteItem(const AIndex: Integer);
    var
    p,
    i: Integer;
    r: PGEOSGraphicsInstr;

    begin
    r:= FItems[AIndex];
    FItems.Delete(AIndex);
    Dispose(r);

    p:= -1;
    for i:= FItems.Count - 1 downto 0 do
        begin
        r:= FItems[i];
        if  (r^.InstrCmd = VAL_CMD_GEOSGSTR_ESCPTS)
        or  (r^.InstrCmd = VAL_CMD_GEOSPSTR_ESCGRP) then
            begin
            p:= i;
            Break;
            end;
        end;

    if  p > -1 then
        if  PGEOSGraphicsInstr(FItems[p])^.InstrCmd = VAL_CMD_GEOSGSTR_ESCPTS then
            FMode:= ggiString
        else
            FMode:= ggiGraphics
    else
        FMode:= FDefaultMode;

    DoChanged;
    end;

class function TGEOSGraphicsStrElement.ElementName: string;
    begin
    Result:= LIT_CAP_GEOSELEMGRPHSTR;
    end;

procedure TGEOSGraphicsStrElement.PreparePreview(const ABitmap: TBitmap);
    var
    i: Integer;
    r: PGEOSGraphicsInstr;

    function  GetWordValue(const AData: array of Byte;
            const AIndex: Integer): Word;
        begin
        Result:= AData[AIndex] or (AData[AIndex + 1] shl 8);
        end;

    procedure DoGraphicsCommand(const ACmd: Byte; const AData: array of Byte);
        begin
        case  ACmd of
            VAL_CMD_GEOSGSTR_MOVETO:
                ABitmap.Canvas.PenPos:= Point(
                        GEOSNormalizeX(GetWordValue(AData, 0), r^.DoubleW, r^.Add1W),
                        GetWordValue(AData, 2));
            VAL_CMD_GEOSGSTR_LINETO:
                GEOSSolidLineTo(ABitmap.Canvas,GetWordValue(AData, 0),
                        GetWordValue(AData, 2), r^.DoubleW, r^.Add1W);
            VAL_CMD_GEOSGSTR_RECTTO:
                GEOSRectangleTo(ABitmap.Canvas, GetWordValue(AData, 0),
                        GetWordValue(AData, 2), r^.DoubleW, r^.Add1W);
            4:
//              Not used
                ;
            VAL_CMD_GEOSGSTR_NEWPTN:
                GEOSSetPattern(AData[0]);
            VAL_CMD_GEOSGSTR_ESCPTS:
//              Nothing to do
                ;
            VAL_CMD_GEOSGSTR_FMRECT:
                GEOSFrameRectTo(ABitmap.Canvas, GetWordValue(AData, 0),
                        GetWordValue(AData, 2), r^.DoubleW, r^.Add1W);
            end;
        end;

    procedure DoStringCommand(const ACmd: Byte; const AData: array of Byte);
        var
        i: Integer;
        c: Char;
        s: string;
        p: TPoint;

        begin
        case ACmd of
            VAL_CMD_GEOSPSTR_BAKSPC:
                ;
            VAL_CMD_GEOSPSTR_FWDSPC:
                begin
                p:= GEOSSystemFont.TextExtent(' ');
                Inc(p.x, ABitmap.Canvas.PenPos.x);
                p.y:= ABitmap.Canvas.PenPos.y;

                if  p.x >= ARR_REC_GEOSDISPLAYRES[FDispMode].Width then
                    p.x:= ARR_REC_GEOSDISPLAYRES[FDispMode].Width - 1;

                ABitmap.Canvas.PenPos:= p;
                end;
            VAL_CMD_GEOSPSTR_LNFEED:
                begin
                p:= ABitmap.Canvas.PenPos;
                Inc(p.y, GEOSSystemFont.Size);

                if  p.y >= ARR_REC_GEOSDISPLAYRES[FDispMode].Height then
                    p.y:= ARR_REC_GEOSDISPLAYRES[FDispMode].Height - 1;

                ABitmap.Canvas.PenPos:= p;
                end;
            VAL_CMD_GEOSPSTR_HOMEPS:
                ABitmap.Canvas.PenPos:= Point(0, 0);
            VAL_CMD_GEOSPSTR_UPLINE:
                begin
                p:= ABitmap.Canvas.PenPos;
                Dec(p.y, GEOSSystemFont.Size);

                if  p.y < 0 then
                    p.y:= 0;

                ABitmap.Canvas.PenPos:= p;
                end;
            VAL_CMD_GEOSPSTR_CRRTRN:
                begin
                p:= ABitmap.Canvas.PenPos;
                p.x:= 0;
                Inc(p.y, GEOSSystemFont.Size);

                if  p.y >= ARR_REC_GEOSDISPLAYRES[FDispMode].Height then
                    p.y:= ARR_REC_GEOSDISPLAYRES[FDispMode].Height - 1;

                ABitmap.Canvas.PenPos:= p;
                end;
            VAL_CMD_GEOSPSTR_UNDLON:
                GEOSSystemFont.Style:= GEOSSystemFont.Style + [gfsUnderline];
            VAL_CMD_GEOSPSTR_UNDLOF:
                GEOSSystemFont.Style:= GEOSSystemFont.Style - [gfsUnderline];
            VAL_CMD_GEOSPSTR_ESCGRP:
//              Nothing to do
                ;
            VAL_CMD_GEOSPSTR_ESCRLR:
//              Not used here.
                ;
            VAL_CMD_GEOSPSTR_REVSON:
                GEOSSystemFont.Style:= GEOSSystemFont.Style + [gfsReverse];
            VAL_CMD_GEOSPSTR_REVSOF:
                GEOSSystemFont.Style:= GEOSSystemFont.Style - [gfsReverse];
            VAL_CMD_GEOSPSTR_GOTOXP:
                ABitmap.Canvas.PenPos:= Point(
                        GEOSNormalizeX(GetWordValue(AData, 0), r^.DoubleW, r^.Add1W),
                        ABitmap.Canvas.PenPos.y);
            VAL_CMD_GEOSPSTR_GOTOYP:
                ABitmap.Canvas.PenPos:= Point(ABitmap.Canvas.PenPos.x,
                        GetWordValue(AData, 0));
            VAL_CMD_GEOSPSTR_GOTOXY:
                ABitmap.Canvas.PenPos:=Point(
                        GEOSNormalizeX(GetWordValue(AData, 0), r^.DoubleW, r^.Add1W),
                        GetWordValue(AData, 2));
            VAL_CMD_GEOSPSTR_NEWFNT:
                ;
            VAL_CMD_GEOSPSTR_BOLDON:
                GEOSSystemFont.Style:= GEOSSystemFont.Style + [gfsBold];
            VAL_CMD_GEOSPSTR_ITLCON:
                GEOSSystemFont.Style:= GEOSSystemFont.Style + [gfsItalic];
            VAL_CMD_GEOSPSTR_OUTLON:
                GEOSSystemFont.Style:= GEOSSystemFont.Style + [gfsOutline];
            VAL_CMD_GEOSPSTR_PLNTXT:
                GEOSSystemFont.Style:= [];
            VAL_CMD_GEOSPSTR_USELST:
                ;
            VAL_CMD_GEOSPSTR_SHRTCT,
            VAL_CMD_GEOSPSTR_PUTSTR:
                begin
                if  ACmd = VAL_CMD_GEOSPSTR_SHRTCT then
                    s:= string(#$80)
                else
                    begin
                    s:= EmptyStr;
                    for i:= 0 to High(AData) do
                        begin
                        c:= AnsiChar(AData[i]);
                        s:= s + string(c);
                        end;
                    end;

                GEOSSystemFont.TextOut(ABitmap.Canvas, ABitmap.Canvas.PenPos.x,
                        ABitmap.Canvas.PenPos.y, s);

                p:= GEOSSystemFont.TextExtent(s);
                Inc(p.x, ABitmap.Canvas.PenPos.x);
                p.y:= ABitmap.Canvas.PenPos.y;

                if  p.x >= ARR_REC_GEOSDISPLAYRES[FDispMode].Width then
                    p.x:= ARR_REC_GEOSDISPLAYRES[FDispMode].Width - 1;

                ABitmap.Canvas.PenPos:= p;
                end;
            end;
        end;

    begin
    if  FActive then
        for i:= 0 to FItems.Count - 1 do
            begin
            r:= PGEOSGraphicsInstr(FItems[i]);

            if  r^.InstrType = ggiGraphics then
                DoGraphicsCommand(r^.InstrCmd, r^.InstrData)
            else
                DoStringCommand(r^.InstrCmd, r^.InstrData);
            end;
    end;

procedure TGEOSGraphicsStrElement.PrepareCodeInit(const AStrings: TStrings);
    begin
    AStrings.Add(#9#9'LoadW'#9'r0, ' + FIdentifier);
    AStrings.Add(#9#9'jsr'#9'GraphicsString');
    AStrings.Add(EmptyStr);
    end;

procedure TGEOSGraphicsStrElement.PrepareCode(const AStrings: TStrings);
    begin

    end;

procedure TGEOSGraphicsStrElement.PrepareData(const AStrings: TStrings);
    var
    i,
    j: Integer;
    itm: PGEOSGraphicsInstr;
    s: string;

    procedure DoWriteWordX(const AIndex: Integer);
        var
        v: Word;
        s: string;

        begin
        v:= itm^.InstrData[AIndex] or (itm^.InstrData[AIndex + 1] shl 8);
        s:= #9#9'.word'#9'$' + IntToHex(v, 4);
        if  itm^.DoubleW then
            s:= s + ' | DOUBLE_W';
        if  itm^.Add1W then
            s:= s + ' | ADD1_W';

        AStrings.Add(s);
        end;

    begin
    AStrings.Add(FIdentifier + ':');

    for i:= 0 to FItems.Count - 1 do
        begin
        itm:= FItems[i];

        if  itm^.InstrCmd <= VAL_CMD_GEOSPSTR_PLNTXT then
            begin
            s:= ARR_LIT_GEOSGRPHSTRCMDS[itm^.InstrCmd];
            if  Length(s) > 0 then
                AStrings.Add(#9#9'.byte'#9 + UpperCase(s));
            end;

        case itm^.InstrCmd of
            VAL_CMD_GEOSGSTR_MOVETO,
            VAL_CMD_GEOSGSTR_LINETO,
            VAL_CMD_GEOSGSTR_RECTTO,
            VAL_CMD_GEOSGSTR_FMRECT:
                begin
                DoWriteWordX(0);
                AStrings.Add(#9#9'.byte'#9'$' + IntToHex(itm^.InstrData[2], 2));
                end;
            VAL_CMD_GEOSGSTR_NEWPTN:
                AStrings.Add(#9#9'.byte'#9'$' + IntToHex(itm^.InstrData[0], 2));
            VAL_CMD_GEOSGSTR_ESCPTS:
                ;

            VAL_CMD_GEOSPSTR_BAKSPC,
            VAL_CMD_GEOSPSTR_FWDSPC,
            VAL_CMD_GEOSPSTR_LNFEED,
            VAL_CMD_GEOSPSTR_HOMEPS,
            VAL_CMD_GEOSPSTR_UPLINE,
            VAL_CMD_GEOSPSTR_CRRTRN,
            VAL_CMD_GEOSPSTR_UNDLON,
            VAL_CMD_GEOSPSTR_UNDLOF,
            VAL_CMD_GEOSPSTR_ESCGRP,
            VAL_CMD_GEOSPSTR_REVSON,
            VAL_CMD_GEOSPSTR_REVSOF,
            VAL_CMD_GEOSPSTR_BOLDON,
            VAL_CMD_GEOSPSTR_OUTLON,
            VAL_CMD_GEOSPSTR_PLNTXT,
            VAL_CMD_GEOSPSTR_ITLCON:
                ;
            VAL_CMD_GEOSPSTR_ESCRLR:
                ;

            VAL_CMD_GEOSPSTR_GOTOXP:
                DoWriteWordX(0);
            VAL_CMD_GEOSPSTR_GOTOYP:
                AStrings.Add(#9#9'.byte'#9'$' + IntToHex(itm^.InstrData[0], 2));
            VAL_CMD_GEOSPSTR_GOTOXY:
                begin
                DoWriteWordX(0);
                AStrings.Add(#9#9'.byte'#9'$' + IntToHex(itm^.InstrData[2], 2));
                end;

            VAL_CMD_GEOSPSTR_NEWFNT:
                ;

            VAL_CMD_GEOSPSTR_USELST:
                AStrings.Add(#9#9'.byte'#9 + UpperCase(LIT_CMD_GEOSPSTR_USELST));
            VAL_CMD_GEOSPSTR_SHRTCT:
                AStrings.Add(#9#9'.byte'#9 + UpperCase(LIT_CMD_GEOSPSTR_SHRTCT));
            VAL_CMD_GEOSPSTR_PUTSTR:
                begin
                s:= EmptyStr;
                for j:= 0 to High(itm^.InstrData) do
                    s:= s + string(AnsiChar(itm^.InstrData[j]));

                AStrings.Add(#9#9'.byte'#9'"' + s + '"');
                end;
            end;
        end;

    AStrings.Add(#9#9'.byte'#9'NULL');
    AStrings.Add(EmptyStr);
    end;

procedure TGEOSGraphicsStrElement.SaveToXML(const ADoc: TXMLDocument;
            const AParent: TDOMNode);
    var
    it: TDOMNode;
    cn: TDOMElement;
    dn: TDOMText;
    i,
    j: Integer;
    itm: PGEOSGraphicsInstr;
    s: string;

    begin
    cn:= ADoc.CreateElement('items');
    cn.SetAttribute('count', IntToStr(FItems.Count));
    it:= AParent.AppendChild(cn);

    for  i:= 0 to FItems.Count - 1 do
        begin
        itm:= PGEOSGraphicsInstr(FItems[i]);

        cn:= ADoc.CreateElement('item');
        cn.SetAttribute('index', IntToStr(i));
        cn.SetAttribute('instrtype', IntToStr(Ord(itm^.InstrType)));
        cn.SetAttribute('instrcmd', IntToStr(itm^.InstrCmd));
        cn.SetAttribute('doublew', IntToStr(Ord(itm^.DoubleW)));
        cn.SetAttribute('add1w', IntToStr(Ord(itm^.Add1W)));

        cn.SetAttribute('datasize', IntToStr(Length(itm^.InstrData)));

        s:= EmptyStr;
        for j:= 0 to High(itm^.InstrData) do
            s:= s + IntToStr(itm^.InstrData[j]) + ' ';

        dn:= ADoc.CreateTextNode(s);

        cn.AppendChild(dn);
        it.AppendChild(cn);
        end;
    end;

procedure TGEOSGraphicsStrElement.LoadFromXML(const ASource: TDOMNode);
    var
    it,
    cn: TDOMElement;
//  dn: TDOMNode;
    i,
    j,
    k: Integer;
    itm: PGEOSGraphicsInstr;
    s: string;

    begin
    cn:= ASource.FindNode('items') as TDOMElement;

    it:= cn.FirstChild as TDOMElement;
    while Assigned(it) do
        begin
        New(itm);

        j:= StrToInt(it.AttribStrings['instrtype']);
        itm^.InstrType:= TGEOSGraphicsInstrType(j);

        j:= StrToInt(it.AttribStrings['instrcmd']);
        itm^.InstrCmd:= j;

        j:= StrToInt(it.AttribStrings['doublew']);
        itm^.DoubleW:= Boolean(j);

        j:= StrToInt(it.AttribStrings['add1w']);
        itm^.Add1W:= Boolean(j);

        j:= StrToInt(it.AttribStrings['datasize']);
        SetLength(itm^.InstrData, j);

        if  j > 0 then
            begin
//          s:= it.NodeName;
//          dn:= it.FirstChild;
//          s:= dn.NodeName;

            i:= 0;
            s:= it.FirstChild.NodeValue;
            while Length(s) > 0 do
                begin
                j:= Pos(' ', s);
                if  j > 0 then
                    begin
                    k:= StrToInt(Copy(s, 1, j - 1));
                    itm^.InstrData[i]:= k;
                    Inc(i);
                    s:= Copy(s, j + 1, MaxInt);
                    end
                else
                    s:= EmptyStr;
                end;
            end;

        FItems.Add(itm);
        it:= it.NextSibling as TDOMElement;
        end;
    end;


{ TGEOSDesignerElement }

procedure TGEOSDesignerElement.DoChangedUpdate;
    begin
    if  Assigned(GEOSDesignerOnChange) then
        GEOSDesignerOnChange;

    FDirty:= False;
    end;

procedure TGEOSDesignerElement.DoChanged;
    begin
    FDirty:= True;

    if  FLock = 0 then
        DoChangedUpdate;
    end;

procedure TGEOSDesignerElement.SetActive(const AValue: Boolean);
    begin
    if  AValue <> FActive then
        begin
        FActive:= AValue;
        DoChanged;
        end;
    end;

constructor TGEOSDesignerElement.Create(const AIdent: string);
    begin
    FActive:= True;
    FIdentifier:= AIdent;
    FLock:= 0;
    FDirty:= False;

    GEOSDesignerIdents.Add(AIdent);
    end;

destructor TGEOSDesignerElement.Destroy;
    begin
    GEOSDesignerIdents.Delete(GEOSDesignerIdents.IndexOf(FIdentifier));

    inherited Destroy;
    end;

procedure TGEOSDesignerElement.Lock;
    begin
    Inc(FLock);
    end;

procedure TGEOSDesignerElement.Unlock;
    begin
    Dec(FLock);
    if  FLock < 0 then
        FLock:= 0;

    if  FDirty then
        DoChangedUpdate;
    end;


initialization
    GEOSDesignerElements:= TClassList.Create;
    RegisterElements;

    GEOSDesignerOnInit:= nil;
    GEOSDesignerOnChange:= nil;

    FDispMode:= gdm40Column;
    InitialiseFont;

    GEOSDesignerIdents:= TStringList.Create;
    GEOSDesignerIdents.CaseSensitive:= False;
    GEOSDesignerIdents.Duplicates:= dupError;
    GEOSDesignerIdents.Sorted:= True;

finalization
    GEOSDesignerOnChange:= nil;
    GEOSDesignerOnInit:= nil;

    FreeAndNil(GEOSDesignerIdents);

    FreeAndNil(GEOSDesignerElements);

end.

