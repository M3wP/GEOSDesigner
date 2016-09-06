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

        procedure SetActive(const AValue: Boolean);

    public
        constructor Create(const AIdent: string); virtual;
        destructor  Destroy; override;

        class function ElementName: string; virtual; abstract;

        procedure PreparePreview(const ABitmap: TBitmap); virtual; abstract;
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

    { TGEOSDesignerIcon }

    TGEOSDesignerIcon = class(TGEOSBitmap)
    protected
        FIdentifier: string;
        FSystem: Boolean;

    public
        constructor Create(const AIdent: string);
        destructor  Destroy; override;

        property  Indentifier: string read FIdentifier;
        property  System: Boolean read FSystem write FSystem;
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
        'MoveTo',                           //    VAL_CMD_GEOSGSTR_MOVETO = 1;
        'LineTo',                           //    VAL_CMD_GEOSGSTR_LINETO = 2;
        'RectangleTo',                      //    VAL_CMD_GEOSGSTR_RECTTO = 3;
        '',                                 //    4,
        'NewPattern',                       //    VAL_CMD_GEOSGSTR_NEWPTN = 5;
        'EscPutString',                     //    VAL_CMD_GEOSGSTR_ESCPTS = 6;
        'FrameRectTo',                      //    VAL_CMD_GEOSGSTR_FMRECT = 7;

        'BackSpace',                        //    VAL_CMD_GEOSPSTR_BAKSPC = 8;
        'ForwardSpace',                     //    VAL_CMD_GEOSPSTR_FWDSPC = 9;
        'LineFeed',                         //    VAL_CMD_GEOSPSTR_LNFEED = 10;
        'Home',                             //    VAL_CMD_GEOSPSTR_HOMEPS = 11;
        'UpLine',                           //    VAL_CMD_GEOSPSTR_UPLINE = 12;
        'CarriageReturn',                   //    VAL_CMD_GEOSPSTR_CRRTRN = 13;
        'UnderlineOn',                      //    VAL_CMD_GEOSPSTR_UNDLON = 14;
        'UnderlineOff',                     //    VAL_CMD_GEOSPSTR_UNDLOF = 15;
        'EscGraphString',                   //    VAL_CMD_GEOSPSTR_ESCGRP = 16,
        'EscRuler',                         //    VAL_CMD_GEOSPSTR_ESCRLR = 17;
        'ReverseOn',                        //    VAL_CMD_GEOSPSTR_REVSON = 18;
        'ReverseOff',                       //    VAL_CMD_GEOSPSTR_REVSOF = 19;
        'GotoX',                            //    VAL_CMD_GEOSPSTR_GOTOXP = 20;
        'GotoY',                            //    VAL_CMD_GEOSPSTR_GOTOYP = 21;
        'GotoXY',                           //    VAL_CMD_GEOSPSTR_GOTOXY = 22;
        'NewCardset',                       //    VAL_CMD_GEOSPSTR_NEWFNT = 23;
        'BoldOn',                           //    VAL_CMD_GEOSPSTR_BOLDON = 24;
        'ItalicsOn',                        //    VAL_CMD_GEOSPSTR_ITLCON = 25;
        'OutlineOn',                        //    VAL_CMD_GEOSPSTR_OUTLON = 26;
        'PlainText');                       //    VAL_CMD_GEOSPSTR_PLNTXT = 27;
                                            //    VAL_CMD_GEOSPSTR_PUTSTR = $FF;

type

{ TGEOSGraphicsStrElement }

    TGEOSGraphicsStrElement = class(TGEOSDesignerElement)
    private
        FItems: TList;
        FMode: TGEOSGraphicsInstrType;

        function  GetCount: Integer;
        function  GetItems(const AIndex: Integer): PGEOSGraphicsInstr;

    public
        constructor Create(const AIdent: string); override;
        destructor  Destroy; override;

        procedure AddItem(const AType: TGEOSGraphicsInstrType;
                const ACmd: Byte; const AData: array of Byte);
        procedure DeleteItem(const AIndex: Integer);

        class function ElementName: string; override;

        procedure PreparePreview(const ABitmap: TBitmap); override;
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
                const AParent: TGEOSDoMenuItem;
                const AAlign: TGEOSMenuAlignment); overload;
        constructor Create(const AIdent: string; const AType: TGEOSMenuType;
                const AMenuElement: TGEOSDoMenuElement;
                const AAlign: TGEOSMenuAlignment); overload;
        destructor  Destroy; override;

        function  Remove(AMenuItem: TGEOSDoMenuItem): Integer;

        property  Identifier: string read FIdentifier;

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
        procedure Add(AMenuItem: TGEOSDoMenuItem);

    public
        constructor Create(const AIdent: string); override;
        destructor  Destroy; override;

        class function ElementName: string; override;

        procedure PreparePreview(const ABitmap: TBitmap); override;
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
        end;

    private
        FXPos,
        FYPos: Word;
        FDetails: TList;
        FShowMouse: Boolean;

    protected
        procedure SetShowMouse(const AValue: Boolean);

        procedure SetXPos(const AValue: Word);
        procedure SetYPos(const AValue: Word);

        function  GetCount: Integer;
        function  GetIcons(const AIndex: Integer): TGEOSDesignerIcon;
        function  GetIconsXPos(const AIndex: Integer): Word;
        function  GetIconsYPos(const AIndex: Integer): Word;
        function  GetIconsIdent(const AIndex: Integer): string;

    public
        constructor Create(const AIdent: string); override;
        destructor  Destroy; override;

        class function ElementName: string; override;

        procedure PreparePreview(const ABitmap: TBitmap); override;
        procedure PrepareCode(const AStrings: TStrings); override;
        procedure PrepareData(const AStrings: TStrings); override;

        procedure SaveToXML(const ADoc: TXMLDocument;
                const AParent: TDOMNode); override;
        procedure LoadFromXML(const ASourceNode: TDOMNode); override;

        procedure Add(const AX, AY: Word; const AIdent: string;
                const AIcon: TGEOSDesignerIcon);
        procedure Delete(const AIndex: Integer);

        property  ShowMouse: Boolean read FShowMouse write SetShowMouse;
        property  XPos: Word read FXPos write SetXPos;
        property  YPos: Word read FYPos write SetYPos;
        property  Count: Integer read GetCount;
        property  Icons[const AIndex: Integer]: TGEOSDesignerIcon
                read GetIcons; default;
        property  IconsXPos[const AIndex: Integer]: Word read GetIconsXPos;
        property  IconsYPos[const AIndex: Integer]: Word read GetIconsYPos;
        property  IconsIdent[const AIndex: Integer]: string read GetIconsIdent;
    end;


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
    GEOSGraphics, GEOSFont;

const
    LIT_CAP_GEOSELEMGRPHSTR = 'GraphicsString';
    LIT_CAP_GEOSELEMENTMENU = 'DoMenu';
    LIT_CAP_GEOSELEMENTICON = 'DoIcons';

var
    FDispMode: TGEOSDisplayMode;
    FShowingMouse: TGEOSDoIconsElement;


procedure RegisterElements;
    begin
    GEOSDesignerElements.Add(TGEOSGraphicsStrElement);
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
        Result:= FShowingMouse.FXPos
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

        if  Assigned(GEOSDesignerOnChange) then
            GEOSDesignerOnChange;
        end;
    end;


{ TGEOSDoIconsElement }

procedure TGEOSDoIconsElement.SetShowMouse(const AValue: Boolean);
    begin
    if  AValue <> FShowMouse then
        begin
        if  FShowMouse then
            SetGEOSShowMouse(nil)
        else
            SetGEOSShowMouse(Self);

        FShowMouse:= AValue;
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

function TGEOSDoIconsElement.GetCount: Integer;
    begin
    Result:= FDetails.Count;
    end;

function TGEOSDoIconsElement.GetIcons(
        const AIndex: Integer): TGEOSDesignerIcon;
    begin
    Result:= PGEOSIconDetails(FDetails[AIndex])^.Icon;
    end;

function TGEOSDoIconsElement.GetIconsXPos(const AIndex: Integer): Word;
    begin
    Result:= PGEOSIconDetails(FDetails[AIndex])^.X;
    end;

function TGEOSDoIconsElement.GetIconsYPos(const AIndex: Integer): Word;
    begin
    Result:= PGEOSIconDetails(FDetails[AIndex])^.Y;
    end;

function TGEOSDoIconsElement.GetIconsIdent(const AIndex: Integer): string;
    begin
    Result:= PGEOSIconDetails(FDetails[AIndex])^.Ident;
    end;

constructor TGEOSDoIconsElement.Create(const AIdent: string);
    begin
    inherited Create(AIdent);

    FDetails:= TList.Create;
    end;

destructor TGEOSDoIconsElement.Destroy;
    var
    i: Integer;
    d: PGEOSIconDetails;

    begin
    for i:= FDetails.Count - 1 downto 0 do
        begin
        d:= PGEOSIconDetails(FDetails[i]);
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
        GEOSBitmapUp(ABitmap.Canvas, d^.X, d^.Y, d^.Icon);
        end;
    end;

procedure TGEOSDoIconsElement.PrepareCode(const AStrings: TStrings);
    begin

    end;

procedure TGEOSDoIconsElement.PrepareData(const AStrings: TStrings);
    begin

    end;

procedure TGEOSDoIconsElement.SaveToXML(const ADoc: TXMLDocument;
        const AParent: TDOMNode);
    begin

    end;

procedure TGEOSDoIconsElement.LoadFromXML(const ASourceNode: TDOMNode);
    begin

    end;

procedure TGEOSDoIconsElement.Add(const AX, AY: Word; const AIdent: string;
        const AIcon: TGEOSDesignerIcon);
    var
    d: PGEOSIconDetails;

    begin
    GEOSDesignerIdents.Add(AIdent);

    New(d);
    d^.X:= AX;
    d^.Y:= AY;
    d^.Ident:= AIdent;
    d^.Icon:= AIcon;

    FDetails.Add(d);
    DoChanged;
    end;

procedure TGEOSDoIconsElement.Delete(const AIndex: Integer);
    var
    d: PGEOSIconDetails;

    begin
    d:= PGEOSIconDetails(FDetails[AIndex]);
    FDetails.Delete(AIndex);
    GEOSDesignerIdents.Delete(GEOSDesignerIdents.IndexOf(d^.Ident));
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

{ TGEOSDoMenuElement }

function TGEOSDoMenuElement.GetCount: Integer;
    begin
    Result:= FItems.Count;
    end;

function TGEOSDoMenuElement.GetItems(const AIndex: Integer): TGEOSDoMenuItem;
    begin
    Result:= TGEOSDoMenuItem(FItems[AIndex]);
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

procedure TGEOSDoMenuElement.PrepareCode(const AStrings: TStrings);
    begin

    end;

procedure TGEOSDoMenuElement.PrepareData(const AStrings: TStrings);
    begin

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
    begin

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

            GEOSSystemFont.TextOut(ABitmap.Canvas, mp.x, mp.y, mi.FText);

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
        const ACmd: Byte; const AData: array of Byte);
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

//todo TGEOSGraphicsStrElement.AddItem check for valid cmds in type ggiString

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
        FMode:= ggiGraphics;

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
                ABitmap.Canvas.PenPos:= Point(GetWordValue(AData, 0),
                        GetWordValue(AData, 2));
            VAL_CMD_GEOSGSTR_LINETO:
                GEOSSolidLineTo(ABitmap.Canvas, GetWordValue(AData, 0),
                        GetWordValue(AData, 2));
            VAL_CMD_GEOSGSTR_RECTTO:
                GEOSRectangleTo(ABitmap.Canvas, GetWordValue(AData, 0),
                        GetWordValue(AData, 2));
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
                        GetWordValue(AData, 2));
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
                ABitmap.Canvas.PenPos:= Point(GetWordValue(AData, 0),
                        ABitmap.Canvas.PenPos.y);
            VAL_CMD_GEOSPSTR_GOTOYP:
                ABitmap.Canvas.PenPos:= Point(ABitmap.Canvas.PenPos.x,
                        GetWordValue(AData, 0));
            VAL_CMD_GEOSPSTR_GOTOXY:
                ABitmap.Canvas.PenPos:= Point(GetWordValue(AData, 0),
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
                s:= EmptyStr;
                for i:= 0 to High(AData) do
                    begin
                    c:= AnsiChar(AData[i]);
                    s:= s + string(c);
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

procedure TGEOSGraphicsStrElement.PrepareCode(const AStrings: TStrings);
    begin

    end;

procedure TGEOSGraphicsStrElement.PrepareData(const AStrings: TStrings);
    begin

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
    begin

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

