//------------------------------------------------------------------------------
//GEOSGraphics
//============
//Graphics routines and constants for emulation of the GEOS system.
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
unit GEOSGraphics;

{$mode objfpc}{$H+}

interface

uses
    Graphics, Classes, SysUtils, GEOSTypes, GEOSFont;


//dengland  I've made this a function that returns the previous pattern for
//      convienience.
function  GEOSSetPattern(const AValue: Byte): Byte;

function  GEOSNormalizeX(const AX: Word; const ADoubleW, AAdd1W: Boolean): Integer;

//dengland These xxxTo routines are used by GraphicsString and are different to
//      the regular drawing routines in that they use a notion of current
//      position (and don't have some extra features).
procedure GEOSRectangleTo(const ACanvas: TCanvas; const AX, AY: Word;
        const ADoubleW: Boolean = False; const AAdd1W: Boolean = False);
procedure GEOSSolidLineTo(const ACanvas: TCanvas; const AX, AY: Word;
        const ADoubleW: Boolean = False; const AAdd1W: Boolean = False);
procedure GEOSFrameRectTo(const ACanvas: TCanvas; const AX, AY: Word;
        const ADoubleW: Boolean = False; const AAdd1W: Boolean = False);

//dengland  I think technically, these don't exist in GEOS.  I should change
//      them to regular routines that take all required parameters.
procedure GEOSHorzLineTo(const ACanvas: TCanvas; const APattern: Byte;
        const AX: Word; const ADoubleW: Boolean = False;
        const AAdd1W: Boolean = False);
procedure GEOSVertLineTo(const ACanvas: TCanvas; const APattern: Byte;
        const AY: Word);

//dengland These don't exist in GEOS but I need them for handling bitmaps.
procedure GEOSDecompactBitmap(const ADataIn, ADataOut: TStream);
procedure GEOSCompactBitmap(const ADataIn, ADataOut: TStream);

procedure GEOSInvertRectangle(const ACanvas: TCanvas; const ARect: TRect;
        const ADoubleWL: Boolean = False; const AAdd1WL: Boolean = False;
        const ADoubleWR: Boolean = False; const AAdd1WR: Boolean = False);
procedure GEOSBitmapUp(const ACanvas: TCanvas; const AXPos, AYPos: Word;
        const ABitmap: TGEOSBitmap; const ADoubleW: Boolean = False;
        const AAdd1W: Boolean = False; const ADoubleBX: Boolean = False;
        const ATransparent: Boolean = False; const AXIsPixels: Boolean = False);

var
    GEOSCurrPattrn: Byte = 0;
    GEOSSystemFont: TGEOSFont;

const
    ARR_VAL_GEOSSYSICOCANCEL: array[0..$53] of Byte = (
            $05,$FF,$82,$FE,$80,$04,$00,$82,$03,$80,$04,$00,$B8,$03,$87,$C0,
            $00,$00,$00,$E3,$8C,$60,$00,$00,$00,$63,$8C,$07,$9F,$1E,$3C,$63,
            $8C,$0C,$DD,$B3,$66,$63,$8C,$07,$D9,$B0,$66,$63,$8C,$0C,$D9,$B0,
            $7E,$63,$8C,$0C,$D9,$B0,$60,$63,$8C,$6C,$D9,$B3,$66,$63,$87,$C7,
            $D9,$9E,$3C,$63,$80,$04,$00,$82,$03,$80,$04,$00,$81,$03,$06,$FF,
            $81,$7F,$05,$FF);
    ARR_VAL_GEOSSYSICOOK: array[0..$53] of Byte = (
            $05,$FF,$82,$FE,$80,$04,$00,$82,$03,$80,$04,$00,$B8,$03,$80,$00,
            $F8,$C6,$00,$03,$80,$01,$8C,$CC,$00,$03,$80,$01,$8C,$D8,$00,$03,
            $80,$01,$8C,$F0,$00,$03,$80,$01,$8C,$E0,$00,$03,$80,$01,$8C,$F0,
            $00,$03,$80,$01,$8C,$D8,$00,$03,$80,$01,$8C,$CC,$00,$03,$80,$00,
            $F8,$C6,$00,$03,$80,$04,$00,$82,$03,$80,$04,$00,$81,$03,$06,$FF,
            $81,$7F,$05,$FF);
    ARR_VAL_GEOSSYSICONO: array[0..$4A] of Byte = (
            $05,$FF,$81,$FE,$E3,$02,$86,$80,$00,$00,$00,$00,$03,$8C,$80,$01,
            $CC,$7C,$00,$03,$80,$01,$CC,$C6,$00,$03,$E3,$02,$86,$80,$01,$EC,
            $C6,$00,$03,$E3,$02,$86,$80,$01,$BC,$C6,$00,$03,$E3,$02,$86,$80,
            $01,$9C,$C6,$00,$03,$86,$80,$01,$8C,$7C,$00,$03,$E3,$02,$86,$80,
            $00,$00,$00,$00,$03,$06,$FF,$81,$7F,$05,$FF);
    ARR_VAL_GEOSSYSICOYES: array[0..$50] of Byte = (
            $05,$FF,$81,$FE,$E3,$02,$86,$80,$00,$00,$00,$00,$03,$9E,$80,$0C,
            $CF,$C7,$C0,$03,$80,$0C,$CC,$0C,$60,$03,$80,$0C,$CC,$0C,$00,$03,
            $80,$07,$8C,$0C,$00,$03,$80,$07,$8F,$87,$C0,$03,$E3,$02,$86,$80,
            $03,$0C,$00,$60,$03,$8C,$80,$03,$0C,$0C,$60,$03,$80,$03,$0F,$C7,
            $C0,$03,$E3,$02,$86,$80,$00,$00,$00,$00,$03,$06,$FF,$81,$7F,$05,
            $FF);
    ARR_VAL_GEOSSYSICOOPEN: array[0..$52] of Byte = (
            $05,$FF,$81,$FE,$E3,$02,$86,$80,$00,$00,$00,$00,$03,$B6,$80,$3E,
            $00,$00,$00,$03,$80,$63,$00,$00,$00,$03,$80,$63,$7C,$79,$F0,$03,
            $80,$63,$66,$CD,$D8,$03,$80,$63,$66,$CD,$98,$03,$80,$63,$66,$FD,
            $98,$03,$80,$63,$66,$C1,$98,$03,$80,$63,$66,$CD,$98,$03,$80,$3E,
            $7C,$79,$98,$03,$E3,$02,$86,$80,$00,$60,$00,$00,$03,$06,$FF,$81,
            $7F,$05,$FF);
    ARR_VAL_GEOSSYSICODISK: array[0..$52] of Byte = (
            $05,$FF,$81,$FE,$E3,$02,$86,$80,$00,$00,$00,$00,$03,$B6,$80,$1F,
            $0C,$03,$00,$03,$80,$19,$80,$03,$00,$03,$80,$18,$DC,$F3,$30,$03,
            $80,$18,$CD,$9B,$60,$03,$80,$18,$CD,$83,$C0,$03,$80,$18,$CC,$F3,
            $80,$03,$80,$18,$CC,$1B,$C0,$03,$80,$19,$8D,$9B,$60,$03,$80,$1F,
            $0C,$F3,$30,$03,$E3,$02,$86,$80,$00,$00,$00,$00,$03,$06,$FF,$81,
            $7F,$05,$FF);

    ARR_VAL_GEOSSYSMOUSE: array[0..8] of Byte = (
            $88, $FC, $F8, $F0, $F8, $DC, $8E, $07, $03);

implementation

const
    ARR_VAL_GEOSSYSPATNDAT: array[0..$FF] of Byte = (
            $00,$00,$00,$00,$00,$00,$00,$00,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,
            $AA,$55,$AA,$55,$AA,$55,$AA,$55,$99,$42,$24,$99,$99,$24,$42,$99,
            $FB,$F5,$FB,$F5,$FB,$F5,$FB,$F5,$88,$22,$88,$22,$88,$22,$88,$22,
            $77,$DD,$77,$DD,$77,$DD,$77,$DD,$88,$00,$22,$00,$88,$00,$22,$00,
            $77,$FF,$DD,$FF,$77,$FF,$DD,$FF,$FF,$00,$FF,$00,$FF,$00,$FF,$00,
            $55,$55,$55,$55,$55,$55,$55,$55,$01,$02,$04,$08,$10,$20,$40,$80,
            $80,$40,$20,$10,$08,$04,$02,$01,$FE,$FD,$FB,$F7,$EF,$DF,$BF,$7F,
            $7F,$BF,$DF,$EF,$F7,$FB,$FD,$FE,$FF,$88,$88,$88,$FF,$88,$88,$88,
            $FF,$80,$80,$80,$80,$80,$80,$80,$FF,$80,$80,$80,$FF,$08,$08,$08,
            $08,$1C,$22,$C1,$80,$01,$02,$04,$88,$14,$22,$41,$88,$00,$AA,$00,
            $80,$40,$20,$00,$02,$04,$08,$00,$40,$A0,$00,$00,$04,$0A,$00,$00,
            $82,$44,$39,$44,$82,$01,$01,$01,$03,$84,$48,$30,$0C,$02,$01,$01,
            $F8,$74,$22,$47,$8F,$17,$22,$71,$80,$80,$41,$3E,$08,$08,$14,$E3,
            $55,$A0,$40,$40,$55,$0A,$04,$04,$10,$20,$54,$AA,$FF,$02,$04,$08,
            $20,$50,$88,$88,$88,$88,$05,$02,$77,$89,$8F,$8F,$77,$98,$F8,$F8,
            $BF,$00,$BF,$BF,$B0,$B0,$B0,$B0,$00,$08,$14,$2A,$55,$2A,$14,$08);

function GEOSSetPattern(const AValue: Byte): Byte;
    begin
    Result:= GEOSCurrPattrn;
    if  AValue < 32 then
        GEOSCurrPattrn:= AValue;
    end;

function GEOSNormalizeX(const AX: Word; const ADoubleW, AAdd1W: Boolean): Integer;
    begin
//dengland Should do sign checking as per GEOS too, probably.  And should check
//      the AX value for the flags.  Never mind.
    Result:= AX;
    if  ADoubleW then
        Inc(Result, AX);
    if  AAdd1W then
        Inc(Result);
    end;

procedure GEOSInvertRectangle(const ACanvas: TCanvas; const ARect: TRect;
    const ADoubleWL, AAdd1WL, ADoubleWR, AAdd1WR: Boolean);
    var
    i,
    j: Integer;
    pc,
    bc: TColor;
    r: TRect;

    begin
//dengland I'm assuming there should be a sanity check.
    if  (ARect.Right < ARect.Left)
    or  (ARect.Bottom < ARect.Top) then
        Exit;

    pc:= ACanvas.Pen.Color;
    bc:= ACanvas.Brush.Color;

    r:= ARect;
    r.Left:= GEOSNormalizeX(r.Left, ADoubleWL, AAdd1WL);
    r.Right:= GEOSNormalizeX(r.Right, ADoubleWR, AAdd1WR);

//dengland Is inclusive...
    for i:= r.Top to r.Bottom do
        for j:= r.Left to r.Right do
            if  ACanvas.Pixels[j, i] = pc then
                ACanvas.Pixels[j, i]:= bc
            else
                ACanvas.Pixels[j, i]:= pc;
    end;

procedure GEOSBitmapUp(const ACanvas: TCanvas; const AXPos, AYPos: Word;
        const ABitmap: TGEOSBitmap; const ADoubleW: Boolean;
        const AAdd1W: Boolean; const ADoubleBX: Boolean;
        const ATransparent: Boolean; const AXIsPixels: Boolean);

    var
    x: Integer;
    xp: Word;
    i,
    j: Integer;
    m: TMemoryStream;
    b,
    d: Byte;
    pc,
    bc: TColor;

    begin
    if  AXIsPixels then
        xp:= AXPos
    else
        xp:= AXPos * 8;

    xp:= GEOSNormalizeX(xp, ADoubleBX, False);

    m:= TMemoryStream.Create;
    try
        ABitmap.Data.Position:= 0;
        GEOSDecompactBitmap(ABitmap.Data, m);

        pc:= ACanvas.Pen.Color;
        bc:= ACanvas.Brush.Color;

        m.Position:= 0;
        b:= m.ReadByte;
        d:= 128;
        for i:= AYPos to AYPos + ABitmap.Height - 1 do
            begin
            x:= xp;

            for j:= xp to xp + ABitmap.Width - 1 do
                begin
                if  (b and d) <> 0 then
                    ACanvas.Pixels[x, i]:= pc
                else if not ATransparent then
                    ACanvas.Pixels[x, i]:= bc;

                if  ADoubleW then
                    begin
                    Inc(x);
                    if  (b and d) <> 0 then
                        ACanvas.Pixels[x, i]:= pc
                    else if not ATransparent then
                        ACanvas.Pixels[x, i]:= bc;
                    Inc(x);
                    end
                else
                    Inc(x);

                if  d = 1 then
                    begin
                    if  m.Position < m.Size then
                        b:= m.ReadByte;
                    d:= 128;
                    end
                else
                    d:= d shr 1;
                end;
            end;
        finally
        m.Free;
        end;
    end;

procedure GEOSRectangleTo(const ACanvas: TCanvas; const AX, AY: Word;
    const ADoubleW: Boolean; const AAdd1W: Boolean);
    var
    xs,
    xo,
    yo,
    x,
    x1,
    y,
    y1: Integer;
    d: Byte;
    c,
    i,
    j: Integer;
    pc,
    bc: TColor;

    begin
//  Must fill a rectangluar area (inclusive) with the current pattern.  The
//      pattern must be used on "card" boundaries.

    x:= ACanvas.PenPos.x;
    y:= ACanvas.PenPos.y;

    x1:= GEOSNormalizeX(AX, ADoubleW, AAdd1W);
    y1:= AY;

    if  x > x1 then
        begin
        i:= x;
        x:= x1;
        x1:= i;
        end;

    if  y > y1 then
        begin
        i:= y;
        y:= y1;
        y1:= y;
        end;

//dengland I'm assuming this sanity logic is there
//todo GEOSRectangleTo Check if can draw to a point higher/behind the current.
    ACanvas.PenPos:= Point(x1, y1);

    xs:= 7 - (x mod 8);
    yo:= y mod 8;

    pc:= ACanvas.Pen.Color;
    bc:= ACanvas.Brush.Color;

    for i:= y to y1 do
        begin
        xo:= xs;
        d:= 1 shl xo;
        c:= GEOSCurrPattrn * 8 + yo;
        for j:= x to x1 do
            begin
            if  (ARR_VAL_GEOSSYSPATNDAT[c] and d) <> 0 then
                ACanvas.Pixels[j, i]:= pc
            else
                ACanvas.Pixels[j, i]:= bc;

            if  xo = 0 then
                xo:= 7
            else
                Dec(xo);

            d:= 1 shl xo;
            end;

        if  yo = 7 then
            yo:= 0
        else
            Inc(yo);
        end;
    end;

procedure GEOSSolidLineTo(const ACanvas: TCanvas; const AX, AY: Word;
    const ADoubleW: Boolean; const AAdd1W: Boolean);
    var
//  sp: TPoint;
    ep: TPoint;

    begin
//  ACanvas.Pen.Color:= clC64DkGrey;
    ACanvas.Pen.Style:= psSolid;

//  GEOS points are inclusive.
//dengland It seems that when drawing lines, the points are inclusive.
    ep.x:= GEOSNormalizeX(AX, ADoubleW, AAdd1W);
    ep.y:= AY;

//dengland  This may or may not be the same algorithm used by GEOS.  Hopefully
//      it is close enough.
    ACanvas.LineTo(ep);
    ACanvas.PenPos:= Point(ep.x, ep.y);
    end;

procedure GEOSFrameRectTo(const ACanvas: TCanvas; const AX, AY: Word;
    const ADoubleW: Boolean; const AAdd1W: Boolean);
    var
    sp: TPoint;

    begin
    sp:= ACanvas.PenPos;

    GEOSSolidLineTo(ACanvas, AX, ACanvas.PenPos.y, ADoubleW, AAdd1W);
    GEOSSolidLineTo(ACanvas, AX, AY, ADoubleW, AAdd1W);
    GEOSSolidLineTo(ACanvas, sp.x, ACanvas.PenPos.y, False, False);
    GEOSSolidLineTo(ACanvas, sp.x, sp.y, False, False);

    ACanvas.PenPos:= Point(GEOSNormalizeX(AX, ADoubleW, AAdd1W), AY);
    end;

procedure GEOSHorzLineTo(const ACanvas: TCanvas; const APattern: Byte;
        const AX: Word; const ADoubleW: Boolean; const AAdd1W: Boolean);
    var
    xo,
    x1,
    y,
    x2: Integer;
    d: Byte;
    i: Integer;
    pc,
    bc: TColor;

    begin
    x1:= ACanvas.PenPos.x;
    y:= ACanvas.PenPos.y;

    x2:= GEOSNormalizeX(AX, ADoubleW, AAdd1W);

    if  x2 < x1 then
        begin
        x1:= x2;
        x2:= ACanvas.PenPos.x;
        end;

    pc:= ACanvas.Pen.Color;
    bc:= ACanvas.Brush.Color;

    xo:= 7 - (x1 mod 8);
    d:= 1 shl xo;

    for i:= x1 to x2 do
        begin
        if  (d and APattern) <> 0 then
            ACanvas.Pixels[i, y]:= pc
        else
            ACanvas.Pixels[i, y]:= bc;

        if  xo = 0 then
            xo:= 7
        else
            Dec(xo);

        d:= 1 shl xo;
        end;

    ACanvas.PenPos:= Point(AX, y);
    end;

procedure GEOSVertLineTo(const ACanvas: TCanvas; const APattern: Byte;
        const AY: Word);
    var
    yo,
    x,
    y1,
    y2: Integer;
    d: Byte;
    i: Integer;
    pc,
    bc: TColor;

    begin
    y1:= ACanvas.PenPos.y;
    x:= ACanvas.PenPos.x;

    if  AY < y1 then
        begin
        y1:= AY;
        y2:= ACanvas.PenPos.y;
        end
    else
        y2:= AY;

    pc:= ACanvas.Pen.Color;
    bc:= ACanvas.Brush.Color;

    yo:= (y1 mod 8);
    d:= 1 shl yo;

    for i:= y1 to y2 do
        begin
        if  (d and APattern) <> 0 then
            ACanvas.Pixels[x, i]:= pc
        else
            ACanvas.Pixels[x, i]:= bc;

        if  yo = 7 then
            yo:= 0
        else
            Inc(yo);

        d:= 1 shl yo;
        end;

    ACanvas.PenPos:= Point(x, AY);
    end;

procedure DoDecodeToTemp(const ACount: Integer; const ADataIn: TStream;
        const t: TStream);
    var
    i: Integer;
    c,
    b: Byte;
    r: Boolean;

    begin
    r:= True;
    while r do
        begin
        c:= ADataIn.ReadByte;

        if  c in [0..127] then
            begin
            b:= ADataIn.ReadByte;

            for i:= 0 to c - 1 do
                t.WriteByte(b);
            end
        else if c in [128..220] then
            begin
            c:= c - 128;
            for i:= 0 to c - 1 do
                begin
                b:= ADataIn.ReadByte;
                t.WriteByte(b);
                end;
            end
        else
            raise Exception.Create('BIGCOUNT format unexpected at this time.');

        if  (ACount > 0)
        and (t.Size > ACount) then
            raise Exception.Create('Data format error with BIGCOUNT bytes.');

        r:= (ACount > 0) and (t.Size < ACount);
        end;
    end;

procedure GEOSDecompactBitmap(const ADataIn, ADataOut: TStream);
    var
    t: TMemoryStream;
    i: Integer;
    c,
    b: Byte;

    begin
    t:= TMemoryStream.Create;
    try
        while ADataIn.Position < ADataIn.Size do
            begin
            t.Clear;

            b:= 1;
            c:= ADataIn.ReadByte;
            ADataIn.Position:= ADataIn.Position - 1;

            if  c in [221..255] then
                begin
//dengland      This is what the documentation says but the data says otherwise.
//              c:= ADataIn.ReadByte - 220;
                c:= ADataIn.ReadByte - 221;
                b:= ADataIn.ReadByte;
                DoDecodeToTemp(c, ADataIn, t);
                end
            else
                DoDecodeToTemp(-1, ADataIn, t);

            for i:= 0 to b - 1 do
                begin
                t.Position:= 0;
                ADataOut.CopyFrom(t, t.Size);
                end;
            end;

        finally
        t.Free;
        end;
    end;

const
    VAL_SIZ_MAXWINDOW = 92;
type
    PGEOSBitmapCmd = ^TGEOSBitmapCmd;
    TGEOSBitmapCmd = record
        Cmd: Byte;
        Data: array of Byte;
    end;

function DoCollateRunLength(const AList: TList; const ADataIn: TStream): Boolean;
    var
    c: PGEOSBitmapCmd;
    b,
    d: Byte;
    idx: Int64;
    cnt: Byte;

    begin
    idx:= ADataIn.Position;
    b:= ADataIn.ReadByte;
    d:= b;
    cnt:= 1;
    Result:= ADataIn.Position < ADataIn.Size;
    while Result do
        begin
        b:= ADataIn.ReadByte;
        if  (b <> d) then
            begin
            Result:= cnt > 1;
            Break;
            end;

        idx:= ADataIn.Position;
        Inc(cnt);

        if  (cnt = 127)
        or  (ADataIn.Position = ADataIn.Size) then
            begin
            Result:= cnt > 1;
            Break;
            end;
        end;

    ADataIn.Position:= idx;

    if  Result then
        begin
        New(c);
        c^.Cmd:= cnt;
        SetLength(c^.Data, 1);
        c^.Data[0]:= d;
        AList.Add(c);
        end;
    end;

procedure DoProcessSequence(const AList: TList; const AWindow: array of Byte;
        ADataIn: TStream);
    var
    i,
    j,
    idxs,
    idxe,
    idxt,
    idxr,
    idxx: Integer;
    rcnt: Byte;
    f: Boolean;
    c: PGEOSBitmapCmd;

    begin
    idxr:= 0;
    idxt:= 0;
    idxs:= 0;
    idxe:= 1;
    idxx:= 1;
    rcnt:= 1;

    while  idxs < Length(AWindow) do
        begin
        while (idxe < Length(AWindow)) and ((idxe - idxs) < 34)  do
            begin
            i:= 1 + idxe;
            f:= False;
            if  (i + (idxe - idxs)) < Length(AWindow) then
                begin
                f:= True;
                for j:= idxs to idxe do
                    if  AWindow[j] <> AWindow[i + j - idxs] then
                        begin
                        f:= False;
                        Break;
                        end;
                end;

            if  not f
            and (idxt > 0) then
                begin
                New(c);
                SetLength(c^.Data, idxt);
                c^.Cmd:= 128 + Length(c^.Data);
                for j:= 0 to idxt - 1 do
                    c^.Data[j]:= AWindow[j];
                AList.Add(c);

                idxt:= idxs;
                end;

            if  not f
            and (rcnt > 1) then
                begin
                New(c);
                SetLength(c^.Data, idxx - idxr + 3);
                c^.Cmd:= 221 + Length(c^.Data) - 2;
                c^.Data[0]:= rcnt;
                c^.Data[1]:= 128 + idxx - idxr + 1;
                for j:= 2 to Length(c^.Data) - 1 do
                    c^.Data[j]:= AWindow[j + idxr - 2];
                AList.Add(c);

                idxs:= idxe;
                idxt:= idxs;
                idxr:= idxs;
                rcnt:= 1;

//dengland      I'm exiting out of this routine here and backing out the rest of
//                  the window instead in the hope that there will be better
//                  pattern matching if I do.
//              Break;
                j:= High(AWindow);
                ADataIn.Position:= ADataIn.Position - (j - idxs);
                Exit;
                end;

            if f then
                begin
                if  rcnt = 1 then
                    begin
                    idxr:= idxs;
                    idxx:= idxe;
                    idxt:= idxs;
                    end;
                Inc(rcnt);

                idxs:= i;
                idxe:= i + idxx - idxr;
                end
            else
                Inc(idxe);
            end;

        Inc(idxs);
        idxe:= idxs + 1;
        end;

//dengland  Hopefully, this won't get used.
    if  idxt < Length(AWindow) then
        begin
        j:= Length(AWindow);
        New(c);
        SetLength(c^.Data, Length(AWindow) - idxt);
        c^.Cmd:= 128 + Length(c^.Data);
        for i:= 0 to Length(c^.Data) - 1 do
            c^.Data[i]:= AWindow[idxt + i];
        AList.Add(c);
        end;
    end;

procedure DoCollateSequence(const AList: TList; const ADataIn: TStream);
    var
    idxs,
    idxe: Int64;
    rcnt: Byte;
    d,
    b: Byte;
    w: array of Byte;

    begin
    idxs:= ADataIn.Position;
    rcnt:= 1;
    b:= ADataIn.ReadByte;
    idxe:= ADataIn.Position;
    d:= b;
    while (ADataIn.Position < ADataIn.Size) and
            ((idxe - idxs) < VAL_SIZ_MAXWINDOW) and (rcnt < 3) do
        begin
        b:= ADataIn.ReadByte;
        if  b = d then
            Inc(rcnt)
        else
            begin
            d:= b;
            rcnt:= 1;
            end;
        idxe:= ADataIn.Position;
        end;

    if  rcnt > 2 then
        Dec(idxe, rcnt);

    if  idxe > idxs then
        begin
        SetLength(w, idxe - idxs);
        ADataIn.Position:= idxs;
        ADataIn.ReadBuffer(w[0], Length(w));

        DoProcessSequence(AList, w, ADataIn);
        end;
    end;

procedure DoOutputCmdList(const AList: TList; const ADataOut: TStream);
    var
    i,
    j: Integer;
    c: PGEOSBitmapCmd;

    begin
    for i:= 0 to AList.Count - 1 do
        begin
        c:= PGEOSBitmapCmd(AList[i]);
        ADataOut.WriteByte(c^.Cmd);

        for j:= 0 to High(c^.Data) do
            ADataOut.WriteByte(c^.Data[j]);
        end;
    end;

procedure GEOSCompactBitmap(const ADataIn, ADataOut: TStream);
    var
    l: TList;
    i: Integer;

    begin
    l:= TList.Create;
    try
        while ADataIn.Position < ADataIn.Size do
            begin
            if  DoCollateRunLength(l, ADataIn) then
                Continue
            else
                DoCollateSequence(l, ADataIn);
            end;

        DoOutputCmdList(l, ADataOut);

        finally
        for i:= l.Count - 1 downto 0 do
            Dispose(PGEOSBitmapCmd(l[i]));

        l.Free;
        end;
    end;

initialization
    GEOSSystemFont:= TGEOSFont.Create(clC64DkGrey, clC64LtGrey);

finalization
    FreeAndNil(GEOSSystemFont);


end.

