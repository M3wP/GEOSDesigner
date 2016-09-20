//------------------------------------------------------------------------------
//GEOSFont
//========
//Classes and types for supporing GEOS fonts.
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
unit GEOSFont;

{$mode objfpc}{$H+}

interface

uses
    Graphics, Classes, SysUtils, GEOSTypes;

type

    TGEOSFontStyle = (gfsUnderline, gfsReverse, gfsBold, gfsItalic, gfsOutline);
    TGEOSFontStyles = set of TGEOSFontStyle;

{ TGEOSFont }

    TGEOSFont = class(TObject)
    private type
        TCharWidths = array of Byte;
        TCharOffsets = array of Word;
        TFontDetail = record
            Baseline: Byte;
            BytesPerLine: Word;
            Size: TGEOSFontSize;
//          OffsetsOffset: Word;
            PixelsOffset: Word;
            Offsets: TCharOffsets;
//dengland  Widths is actually not stored in the font data, I've calculated them
//              here for convienience.
            Widths: TCharWidths;
            Bitmap: TBitmap;
//dengland  We're going to keep the data so that we can rebuild the bitmaps as
//              the Pen and Brush colours change.  This is a speed optimisation;
//              I don't want to muck around with accessing the pixels via the
//              canvas interface (would be dreadfully slow) when displaying
//              (plain) text.  It does use a "lot" more memory, however.
            Data: TMemoryStream;
        end;

    private
        FName: string;
        FVersion: TGEOSVersion;
        FID: TGEOSFontID;
        FDetails: array of TFontDetail;
        FSizes: array of TGEOSFontSize;
        FSizeIdx: Integer;
        FPenColor: TColor;
        FBrushColor: TColor;
        FStyle: TGEOSFontStyles;
        FDirty: Boolean;
        FLocked: Integer;
        FTemp1,
        FTemp2: TBitmap;

        function  GetSize: TGEOSFontSize;
        procedure SetSize(const AValue: TGEOSFontSize);
        function  GetBaseline: Integer;
        procedure SetPenColor(const AValue: TColor);
        procedure SetBrushColor(const AValue: TColor);
        function  GetGlyphCount: Integer;
        function  GetSizesCount: Integer;
        function  GetSizes(const AIndex: Integer): TGEOSFontSize;
        procedure SetStyles(const AValue: TGEOSFontStyles);

        procedure SetDirty(const AValue: Boolean);

        procedure DoPrepareDetails(const AIndex: Integer);
        procedure DoFreeDetails;
        procedure DoBuildSizes;
        procedure DoBuildBitmaps;

    public
        constructor Create(const APen: TColor = clBlack;
                const ABrush: TColor = clWhite);
        destructor  Destroy; override;

        procedure LoadSystemFont(const AMode: TGEOSDisplayMode);

        procedure Lock;
        procedure Unlock;

        function  SetStyle(const AValue: TGEOSFontStyles): TGEOSFontStyles;

        procedure TextOut(const ACanvas: TCanvas; AX, AY: Integer;
                const AText: string);
        function  TextFitInfo(const AText: string; AMaxWidth: Integer): Integer;
        function  TextExtent(const AText: string): TPoint;

        property  Name: string read FName;
        property  Version: TGEOSVersion read FVersion;
        property  ID: TGEOSFontID read FID;
        property  Size: TGEOSFontSize read GetSize write SetSize;
        property  Baseline: Integer read GetBaseline;
        property  GlyphCount: Integer read GetGlyphCount;
        property  SizesCount: Integer read GetSizesCount;
        property  Sizes[const AIndex: Integer]: TGEOSFontSize read GetSizes;
        property  PenColor: TColor read FPenColor write SetPenColor;
        property  BrushColor: TColor read FBrushColor write SetBrushColor;
        property  Style: TGEOSFontStyles read FStyle write SetStyles;
    end;

implementation

const
    ARR_VAL_DAT_GEOSBSW40_9: array[0..$2E7] of Byte = (
        $06,$3C,$00,$09,$08,$00,$CC,$00,$00,$00,$05,$00,$07,$00,$0B,$00,
        $11,$00,$17,$00,$1D,$00,$23,$00,$25,$00,$29,$00,$2D,$00,$33,$00,
        $39,$00,$3C,$00,$41,$00,$43,$00,$4A,$00,$4F,$00,$52,$00,$56,$00,
        $5A,$00,$5F,$00,$63,$00,$68,$00,$6D,$00,$72,$00,$77,$00,$79,$00,
        $7C,$00,$80,$00,$84,$00,$88,$00,$8E,$00,$94,$00,$9A,$00,$9F,$00,
        $A4,$00,$A9,$00,$AD,$00,$B1,$00,$B6,$00,$BC,$00,$BE,$00,$C2,$00,
        $C8,$00,$CC,$00,$D4,$00,$DA,$00,$E0,$00,$E5,$00,$EB,$00,$F0,$00,
        $F5,$00,$F9,$00,$FE,$00,$04,$01,$0C,$01,$12,$01,$18,$01,$1E,$01,
        $21,$01,$29,$01,$2C,$01,$32,$01,$3A,$01,$3E,$01,$43,$01,$48,$01,
        $4D,$01,$52,$01,$57,$01,$5A,$01,$5F,$01,$64,$01,$66,$01,$68,$01,
        $6D,$01,$6F,$01,$77,$01,$7C,$01,$82,$01,$87,$01,$8C,$01,$8F,$01,
        $93,$01,$96,$01,$9B,$01,$A1,$01,$A9,$01,$AF,$01,$B4,$01,$BA,$01,
        $BE,$01,$C0,$01,$C4,$01,$CA,$01,$D2,$01,$DD,$01,$02,$A5,$1E,$C1,
        $88,$A0,$80,$00,$00,$0C,$59,$82,$E2,$79,$8C,$00,$00,$38,$E1,$1C,
        $67,$3B,$99,$14,$51,$44,$14,$4E,$71,$CE,$3B,$A5,$14,$14,$51,$7D,
        $A0,$30,$00,$10,$08,$00,$40,$40,$85,$42,$00,$00,$00,$00,$00,$00,
        $00,$00,$00,$05,$42,$40,$07,$80,$02,$A5,$28,$CA,$09,$12,$A0,$00,
        $00,$52,$C4,$46,$84,$0A,$52,$01,$04,$45,$12,$92,$94,$A2,$25,$14,
        $52,$46,$36,$51,$4A,$29,$41,$25,$14,$14,$51,$05,$10,$10,$00,$08,
        $08,$00,$40,$80,$80,$42,$00,$00,$00,$00,$08,$00,$00,$00,$00,$09,
        $25,$80,$0F,$80,$02,$0F,$98,$12,$42,$09,$C2,$00,$00,$96,$44,$4A,
        $C8,$12,$52,$02,$72,$09,$74,$52,$84,$A2,$21,$14,$54,$45,$55,$51,
        $4A,$29,$21,$25,$14,$12,$91,$09,$08,$11,$00,$04,$EE,$31,$CC,$CE,
        $E5,$4A,$EC,$E3,$9C,$72,$6D,$28,$A4,$A2,$97,$89,$20,$00,$1C,$70,
        $02,$05,$08,$21,$E2,$0B,$E2,$00,$01,$1A,$48,$92,$2E,$21,$8E,$94,
        $01,$11,$54,$5C,$84,$BB,$A1,$F4,$58,$44,$94,$D1,$4A,$29,$11,$25,
        $14,$91,$0A,$11,$04,$12,$80,$01,$29,$4A,$52,$92,$95,$52,$92,$94,
        $52,$94,$89,$28,$A4,$94,$91,$11,$10,$00,$1C,$00,$02,$0F,$8C,$42,
        $42,$09,$CF,$87,$82,$12,$50,$5E,$29,$22,$42,$02,$72,$11,$77,$D2,
        $84,$A2,$2D,$14,$54,$44,$14,$51,$72,$2E,$09,$25,$15,$52,$84,$21,
        $02,$14,$40,$01,$29,$42,$5E,$92,$95,$62,$92,$94,$52,$94,$49,$28,
        $A4,$88,$92,$11,$10,$00,$1C,$70,$00,$05,$0A,$9A,$42,$0A,$A2,$00,
        $04,$12,$50,$42,$29,$22,$44,$01,$04,$01,$04,$52,$84,$A2,$25,$14,
        $52,$44,$14,$51,$42,$2A,$09,$24,$A6,$34,$44,$41,$01,$10,$00,$01,
        $29,$42,$50,$92,$95,$52,$92,$94,$52,$94,$29,$25,$24,$94,$94,$09,
        $20,$00,$0F,$80,$02,$05,$3C,$19,$C1,$10,$82,$10,$28,$0C,$5D,$82,
        $C6,$21,$88,$90,$00,$10,$E4,$5C,$77,$3A,$1D,$15,$91,$74,$14,$4E,
        $41,$C9,$71,$1C,$44,$14,$44,$7D,$00,$90,$00,$00,$AE,$39,$CE,$8E,
        $95,$4A,$92,$93,$9C,$74,$C4,$E2,$1B,$22,$77,$89,$20,$00,$07,$80,
        $00,$00,$08,$00,$00,$A0,$00,$20,$00,$00,$00,$00,$00,$00,$00,$20,
        $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$40,$00,$00,
        $00,$00,$00,$01,$80,$30,$00,$00,$00,$00,$00,$02,$01,$00,$00,$00,
        $10,$10,$00,$00,$00,$00,$10,$05,$40,$00,$00,$00,$00,$00,$00,$00,
        $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,
        $00,$00,$00,$00,$00,$00,$00,$00,$00,$20,$00,$00,$00,$00,$00,$00,
        $00,$00,$3F,$C0,$00,$00,$00,$04,$02,$00,$00,$00,$10,$10,$00,$00,
        $00,$00,$20,$01,$00,$00,$00,$00);


    ARR_VAL_DAT_GEOSBSW80_9: array[0..$3A4] of Byte = (
        $06,$51,$00,$09,$08,$00,$CC,$00,$00,$00,$06,$00,$09,$00,$0F,$00,
        $17,$00,$1F,$00,$26,$00,$2E,$00,$31,$00,$36,$00,$3B,$00,$43,$00,
        $4A,$00,$4E,$00,$54,$00,$57,$00,$5F,$00,$66,$00,$6B,$00,$71,$00,
        $77,$00,$7E,$00,$84,$00,$8B,$00,$92,$00,$99,$00,$A0,$00,$A3,$00,
        $A7,$00,$AC,$00,$B1,$00,$B6,$00,$BD,$00,$C5,$00,$CD,$00,$D4,$00,
        $DB,$00,$E2,$00,$E8,$00,$EE,$00,$F5,$00,$FD,$00,$00,$01,$05,$01,
        $0D,$01,$12,$01,$1C,$01,$24,$01,$2C,$01,$33,$01,$3B,$01,$42,$01,
        $48,$01,$4F,$01,$56,$01,$5E,$01,$68,$01,$70,$01,$77,$01,$7E,$01,
        $83,$01,$8C,$01,$91,$01,$99,$01,$A2,$01,$A7,$01,$AE,$01,$B5,$01,
        $BC,$01,$C3,$01,$CA,$01,$CF,$01,$D6,$01,$DD,$01,$E0,$01,$E3,$01,
        $EA,$01,$ED,$01,$F8,$01,$FF,$01,$07,$02,$0E,$02,$15,$02,$1A,$02,
        $20,$02,$25,$02,$2C,$02,$34,$02,$3F,$02,$47,$02,$4E,$02,$55,$02,
        $5B,$02,$5E,$02,$64,$02,$6C,$02,$78,$02,$83,$02,$01,$B6,$6C,$7E,
        $C0,$F9,$8D,$83,$80,$00,$00,$00,$78,$CF,$3C,$1D,$F1,$CF,$CF,$1E,
        $00,$00,$00,$F1,$F0,$E3,$E3,$CF,$9F,$7C,$F3,$1B,$1B,$1B,$18,$36,
        $33,$E7,$C7,$CF,$8F,$7E,$CD,$8D,$83,$63,$66,$FD,$EC,$07,$80,$00,
        $18,$01,$80,$00,$60,$0E,$01,$83,$6C,$18,$00,$00,$00,$00,$00,$00,
        $00,$00,$00,$00,$00,$00,$00,$6D,$83,$30,$00,$3C,$00,$01,$B6,$6C,
        $D0,$CD,$81,$98,$CD,$60,$00,$00,$06,$CD,$C1,$86,$3D,$83,$00,$D9,
        $B3,$00,$30,$31,$1B,$39,$B3,$36,$6C,$D8,$61,$9B,$1B,$1B,$33,$1C,
        $77,$36,$36,$6C,$6C,$D8,$18,$CD,$8D,$83,$63,$66,$0D,$86,$01,$80,
        $00,$0C,$01,$80,$00,$60,$18,$01,$80,$0C,$18,$00,$00,$00,$00,$00,
        $00,$60,$00,$00,$00,$00,$00,$01,$8C,$66,$E0,$00,$7C,$00,$01,$80,
        $FE,$70,$19,$98,$30,$67,$C3,$00,$00,$0C,$CC,$C1,$86,$6D,$E6,$01,
        $99,$B3,$00,$67,$98,$33,$7B,$1B,$36,$0C,$D8,$61,$83,$1B,$1B,$63,
        $1B,$B7,$B6,$36,$6C,$6C,$CC,$18,$CD,$8D,$83,$36,$66,$19,$83,$01,
        $8E,$00,$06,$7D,$F1,$E3,$E7,$9E,$7D,$F3,$6C,$DB,$EE,$7C,$7C,$F8,
        $F9,$CF,$7B,$36,$36,$66,$C6,$CD,$F9,$8C,$60,$00,$00,$E3,$80,$01,
        $80,$6C,$38,$30,$FC,$30,$63,$83,$00,$00,$18,$DC,$C7,$1C,$CC,$37,
        $C3,$0F,$1F,$66,$C0,$0C,$63,$5B,$1B,$E6,$0C,$DF,$7D,$83,$FB,$1B,
        $C3,$19,$36,$F6,$36,$6C,$6C,$C6,$18,$CD,$8D,$93,$1C,$3C,$31,$81,
        $81,$9B,$00,$00,$CD,$9B,$36,$6C,$D8,$CD,$9B,$6D,$9B,$33,$66,$C6,
        $CD,$9B,$18,$63,$36,$36,$66,$6C,$CC,$1B,$0C,$30,$00,$00,$E0,$00,
        $01,$80,$FE,$1C,$61,$98,$30,$67,$CF,$C1,$F0,$30,$EC,$CC,$06,$FC,
        $36,$63,$19,$83,$00,$67,$98,$63,$7B,$FB,$36,$0C,$D8,$61,$BB,$1B,
        $1B,$63,$18,$36,$76,$37,$CC,$6F,$83,$18,$CD,$8D,$BB,$36,$18,$61,
        $80,$C1,$B1,$80,$00,$CD,$9B,$06,$6F,$D8,$CD,$9B,$6F,$1B,$33,$66,
        $C6,$CD,$9B,$0E,$63,$36,$36,$66,$38,$CC,$63,$0C,$30,$00,$00,$E3,
        $80,$00,$00,$6C,$16,$CD,$98,$30,$6D,$63,$00,$00,$60,$CC,$CC,$06,
        $0C,$36,$63,$19,$86,$00,$30,$30,$03,$03,$1B,$36,$0C,$D8,$61,$9B,
        $1B,$1B,$33,$18,$36,$36,$36,$0C,$6D,$83,$18,$CC,$D9,$C7,$63,$18,
        $C1,$80,$61,$80,$00,$00,$CD,$9B,$06,$6C,$18,$CD,$9B,$6D,$9B,$33,
        $66,$C6,$CD,$9B,$03,$63,$33,$66,$66,$6C,$CD,$81,$8C,$60,$00,$00,
        $7C,$00,$01,$80,$6C,$FC,$0C,$F0,$18,$C3,$83,$0C,$06,$C0,$79,$EF,
        $BC,$0D,$E3,$C3,$0F,$1C,$66,$00,$00,$61,$F3,$1B,$E3,$EF,$9F,$60,
        $F3,$1B,$73,$1B,$D8,$36,$33,$E6,$07,$CC,$DE,$18,$7C,$71,$83,$63,
        $18,$FD,$80,$31,$80,$00,$00,$6D,$F1,$F3,$E7,$D8,$7D,$9B,$6C,$DB,
        $33,$66,$7C,$F8,$FB,$1E,$39,$F1,$C3,$DC,$C6,$7D,$F9,$8C,$60,$00,
        $00,$3C,$00,$00,$00,$00,$00,$00,$00,$0D,$80,$00,$18,$00,$00,$00,
        $00,$00,$00,$00,$00,$00,$00,$0C,$00,$00,$00,$00,$00,$00,$00,$00,
        $00,$00,$00,$00,$00,$00,$00,$00,$00,$01,$80,$00,$00,$00,$00,$00,
        $00,$00,$01,$E0,$07,$80,$00,$00,$00,$00,$00,$00,$00,$18,$00,$60,
        $00,$00,$00,$00,$C0,$18,$00,$00,$00,$00,$00,$00,$0C,$00,$6D,$80,
        $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,
        $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,
        $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$C0,$00,$00,$00,$00,
        $00,$00,$00,$00,$00,$00,$00,$7F,$C0,$00,$00,$00,$00,$00,$30,$00,
        $C0,$00,$00,$00,$00,$C0,$18,$00,$00,$00,$00,$00,$00,$18,$00,$0C,
        $00,$00,$00,$00,$00);

{ TGEOSFont }

function TGEOSFont.GetSize: TGEOSFontSize;
    begin
    Result:= FDetails[FSizeIdx].Size;
    end;

procedure TGEOSFont.SetSize(const AValue: TGEOSFontSize);
    var
    i: Integer;

    begin
    i:= 0;
    while i < Length(FSizes) do
        begin
        if  FSizes[i] >= AValue then
            Break;
        Inc(i);
        end;

    if  (i = Length(FSizes))
    or  (FSizes[i] > AValue) then
        Exit;

    FSizeIdx:= i;
    end;

function TGEOSFont.GetBaseline: Integer;
    begin
    Result:= FDetails[FSizeIdx].Baseline;
    end;

procedure TGEOSFont.SetPenColor(const AValue: TColor);
    begin
    if  AValue <> FPenColor then
        begin
        FPenColor:= AValue;
        SetDirty(True);
        end;
    end;

procedure TGEOSFont.SetBrushColor(const AValue: TColor);
    begin
    if  AValue <> FBrushColor then
        begin
        FBrushColor:= AValue;
        SetDirty(True);
        end;
    end;

function TGEOSFont.GetGlyphCount: Integer;
    begin
    Result:= Length(FDetails[FSizeIdx].Widths);
    end;

function TGEOSFont.GetSizesCount: Integer;
    begin
    Result:= Length(FSizes)
    end;

function TGEOSFont.GetSizes(const AIndex: Integer): TGEOSFontSize;
    begin
    Result:= FSizes[AIndex];
    end;

procedure TGEOSFont.SetStyles(const AValue: TGEOSFontStyles);
    begin
    FStyle:= AValue;
    end;

procedure TGEOSFont.SetDirty(const AValue: Boolean);
    begin
    FDirty:= AValue;

    if  FDirty
    and (FLocked = 0) then
        DoBuildBitmaps;
    end;

procedure TGEOSFont.DoBuildSizes;
    var
    i: Integer;

    begin
    SetLength(FSizes, Length(FDetails));

    for i:= 0 to High(FDetails) do
        FSizes[i]:= FDetails[i].Size;
    end;

procedure TGEOSFont.DoPrepareDetails(const AIndex: Integer);
    var
    j: Integer;
    o: Word;
    g: Word;

    begin
    FDetails[AIndex].Data.Position:= 0;

    FDetails[AIndex].Baseline:= FDetails[AIndex].Data.ReadByte;
    FDetails[AIndex].BytesPerLine:= FDetails[AIndex].Data.ReadByte or
            (FDetails[AIndex].Data.ReadByte shl 8);
    FDetails[AIndex].Size:= FDetails[AIndex].Data.ReadByte;

    o:= FDetails[AIndex].Data.ReadByte or
            (FDetails[AIndex].Data.ReadByte shl 8);

    FDetails[AIndex].PixelsOffset:= FDetails[AIndex].Data.ReadByte or
            (FDetails[AIndex].Data.ReadByte shl 8);

    g:= (FDetails[AIndex].PixelsOffset - o) div 2;

    if  g = 0 then
        raise Exception.Create('Invalid font file.');

    SetLength(FDetails[AIndex].Offsets, g);
    SetLength(FDetails[AIndex].Widths, g - 1);

    for j:= 0 to High(FDetails[AIndex].Offsets) do
        FDetails[AIndex].Offsets[j]:= FDetails[AIndex].Data.ReadByte or
            (FDetails[AIndex].Data.ReadByte shl 8);

    for j:= 0 to High(FDetails[AIndex].Widths) do
        FDetails[AIndex].Widths[j]:= FDetails[AIndex].Offsets[j + 1] -
                FDetails[AIndex].Offsets[j];
    end;

procedure TGEOSFont.DoFreeDetails;
    var
    i: Integer;

    begin
    for i:= High(FDetails) downto 0 do
        begin
        if  Assigned(FDetails[i].Bitmap) then
            FDetails[i].Bitmap.Free;

        if  Assigned(FDetails[i].Data) then
            FDetails[i].Data.Free;
        end;

    SetLength(FDetails, 0);
    end;

procedure TGEOSFont.DoBuildBitmaps;
    var
    i,
    j,
    k: Integer;
    b: Byte;
    d: Byte;
    x,
    y: Integer;

    begin
    for i:= 0 to High(FDetails) do
        begin
        if  not Assigned(FDetails[i].Bitmap) then
            FDetails[i].Bitmap:= TBitmap.Create;

        FDetails[i].Bitmap.SetSize(FDetails[i].BytesPerLine * 8,
                FDetails[i].Size);

        FDetails[i].Data.Position:= FDetails[i].PixelsOffset;

        for y:= 0 to FDetails[i].Size - 1 do
            begin
            x:= 0;
            for j:= 0 to FDetails[i].BytesPerLine - 1 do
                begin
                d:= 128;
                b:= FDetails[i].Data.ReadByte;
                for k:= 7 downto 0 do
                    begin
                    if  (b and d) <> 0 then
                        FDetails[i].Bitmap.Canvas.Pixels[x, y]:= FPenColor
                    else
                        FDetails[i].Bitmap.Canvas.Pixels[x, y]:= FBrushColor;

                    d:= d shr 1;
                    Inc(x);
                    end;
                end;
            end;
        end;
    end;

constructor TGEOSFont.Create(const APen: TColor; const ABrush: TColor);
    begin
    FPenColor:= APen;
    FBrushColor:= ABrush;

    FName:= '';
    FVersion.Minor:= 0;
    FVersion.Major:= 0;
    FID:= 0;

    SetLength(FDetails, 0);

    FSizeIdx:= -1;
    FStyle:= [];

    FTemp1:= TBitmap.Create;
//dengland  I need a second one for doing outlines.  *Sigh*.
    FTemp2:= TBitmap.Create;
//dengland I recall reading that the actual maxium size for a character is 96
//      so I'll use 150 here, just to be safe (should include space for italics
//      and outline/bold).  I don't want to contantly resize the bitmap.
    FTemp1.SetSize(150, 150);
    FTemp1.Canvas.Brush.Style:= bsSolid;
    FTemp1.Canvas.Pen.Style:= psSolid;
    FTemp2.SetSize(150, 150);
    FTemp2.Canvas.Brush.Style:= bsSolid;
    FTemp2.Canvas.Pen.Style:= psSolid;
    end;

destructor TGEOSFont.Destroy;
    begin
    DoFreeDetails;

    FTemp2.Free;
    FTemp1.Free;

    inherited Destroy;
    end;

procedure TGEOSFont.LoadSystemFont(const AMode: TGEOSDisplayMode);
    procedure DoLoadSystemFontData40;
        begin
        SetLength(FDetails, 1);

        FDetails[0].Data:= TMemoryStream.Create;
        FDetails[0].Data.Write(ARR_VAL_DAT_GEOSBSW40_9,
                SizeOf(ARR_VAL_DAT_GEOSBSW40_9));
        FDetails[0].Data.Position:= 0;
        end;

    procedure DoLoadSystemFontData80;
        begin
        SetLength(FDetails, 1);

        FDetails[0].Data:= TMemoryStream.Create;
        FDetails[0].Data.Write(ARR_VAL_DAT_GEOSBSW80_9,
                SizeOf(ARR_VAL_DAT_GEOSBSW80_9));
        FDetails[0].Data.Position:= 0;
        end;

    begin
    DoFreeDetails;

    FVersion.Minor:= 0;
    FVersion.Major:= 1;

    if  AMode = gdm40Column then
        begin
        FName:= 'BSW';
        FID:= 1;
        DoLoadSystemFontData40;
        end
    else
        begin
        FName:= 'BSW 128';
//dengland  I think this is what I read
        FID:= 1 shl 9;
        DoLoadSystemFontData80;
        end;

    DoPrepareDetails(0);
    DoBuildSizes;

    FSizeIdx:= 0;

    SetDirty(True);
    end;

procedure TGEOSFont.Lock;
    begin
    Inc(FLocked);
    end;

procedure TGEOSFont.Unlock;
    begin
    if  FLocked > 0 then
        Dec(FLocked);

    if  FDirty
    and (FLocked = 0) then
        DoBuildBitmaps;
    end;

function TGEOSFont.SetStyle(const AValue: TGEOSFontStyles): TGEOSFontStyles;
    begin
    Result:= FStyle;
    FStyle:= AValue;
    end;

procedure TGEOSFont.TextOut(const ACanvas: TCanvas; AX, AY: Integer;
        const AText: string);
    var
    i: Integer;
    c: AnsiChar;
    b: Byte;
    w: Byte;
    tr,
    sr,
    dr: TRect;
    tp: TPoint;

    procedure DoDrawTempChar(const AIsItalic, ASecondPass: Boolean;
            const AClampX: Integer);
        var
        i,
        j,
        o,
        x,
        y: Integer;

        begin
        for i:= 0 to FSizes[FSizeIdx] - 1 do
            begin
            if  AIsItalic then
//dengland      RoundingMode is set to "Banker's Rounding" (favours even numbers
//                  when rounding 0.5 instead of always upwards) by default and
//                  that doesn't work and I don't want to include Math to work
//                  around it.   This seems to work alright, anyway.
//              o:= Round((FSizes[FSizeIdx] - i) / 2) - 1
                o:= (FSizes[FSizeIdx] - i) div 2
            else
                o:= 0;

            x:= tr.Left + o + tp.x;
            y:= tr.Top + i + tp.y;

            for j:= sr.Left to sr.Right - 1 do
                begin
                if  FDetails[FSizeIdx].Bitmap.Canvas.Pixels[j, i] = FPenColor then
                    FTemp1.Canvas.Pixels[x, y]:= FPenColor
                else if (not ASecondPass)
                or (x >= AClampX) then
                    FTemp1.Canvas.Pixels[x, y]:= FBrushColor;

                Inc(x);
                end;
            end;
        end;

    procedure DoBuildTempOutline;
        var
        i,
        j: Integer;
        c: TColor;

        begin
        if  gfsItalic in FStyle then
            FTemp2.Canvas.CopyRect(tr, FTemp1.Canvas, tr)
        else
            begin
            FTemp2.Canvas.Brush.Color:= FBrushColor;
            FTemp2.Canvas.FillRect(tr);
            end;

        FTemp2.Canvas.Brush.Color:= FPenColor;

//dengland I wish I didn't have to do this pass.  Do I really need to??  I
//          don't seem to need to anymore.
//      for i:= tr.Top to tr.Bottom - 1 do
//          for j:= tr.Left to tr.Right - 1 do
//              begin
//              c:= FTemp1.Canvas.Pixels[j, i];
//
//              if  c = FBrushColor then
//                  FTemp1.Canvas.Pixels[j, i]:= FBrushColor;
//              end;

//      First pass, create outlined areas but also fills inner areas
        for i:= tr.Top + 1 to tr.Bottom - 2 do
            for j:= tr.Left + 1 to tr.Right - 2 do
                begin
                c:= FTemp1.Canvas.Pixels[j, i];

                if  c = FPenColor then
                    FTemp2.Canvas.FillRect(Rect(j - 1, i - 1, j + 2, i + 2));
                end;

//      Second pass, clear inner areas.
        for i:= tr.Top + 1 to tr.Bottom - 2 do
            for j:= tr.Left + 1 to tr.Right - 2 do
                begin
                c:= FTemp1.Canvas.Pixels[j, i];

                if  c = FPenColor then
                    FTemp2.Canvas.Pixels[j, i]:= FBrushColor;
                end;
        FTemp1.Canvas.CopyRect(tr, FTemp2.Canvas, tr);
        end;

    procedure DoBuildTempReverse;
        var
        i,
        j: Integer;
        c: TColor;
        xc,
        yc: Integer;

        begin
//dengland Need a y "clamp" because otherwise, when using italics, the results
//          get strange.  This may or may not be a kludge but its logical and
//          works to fix the issues otherwise seen when using all styles.
        yc:= tr.Top + FSizes[FSizeIdx] + tp.x - 2;
        xc:= FSizes[FSizeIdx] div 2 + tr.Left;

        for i:= tr.Top to tr.Bottom - 1 do
            for j:= tr.Left to tr.Right - 1 do
                begin
                c:= FTemp1.Canvas.Pixels[j, i];

                if  c = FPenColor then
                    FTemp1.Canvas.Pixels[j, i]:= FBrushColor
                else if (c = FBrushColor)
                or  (j >= xc)
                or  (i >= yc) then
                    FTemp1.Canvas.Pixels[j, i]:= FPenColor;
                end;
        end;

    procedure DoBuildTempUnderline;
        var
        i,
        j: Integer;
        c: TColor;

        begin
        i:= FDetails[FSizeIdx].Baseline + 1 + tp.y;
//dengland Is this a kludge?  I was expecting that tp.y should work but it
//          doesn't seem to.
        if  gfsOutline in FStyle then
            Inc(i);

        for j:= tr.Left to tr.Right - 1 do
            begin
            c:= FTemp1.Canvas.Pixels[j, i];

            if  c = FPenColor then
                FTemp1.Canvas.Pixels[j, i]:= FBrushColor
            else //if c = FBrushColor then
                FTemp1.Canvas.Pixels[j, i]:= FPenColor;
            end;
        end;

    procedure DoCopyItalic;
        var
        i,
        j,
        x,
        y: Integer;
        c: TColor;

        begin
        y:= dr.Top;
        for i:= tr.Top to tr.Bottom - 1 do
            begin
            x:= dr.Left;

            for j:= tr.Left to tr.Right - 1 do
                begin
                c:= FTemp1.Canvas.Pixels[j, i];

                if  c = FPenColor then
                    ACanvas.Pixels[x, y]:= FPenColor
                else if c = FBrushColor then
                    ACanvas.Pixels[x, y]:= FBrushColor;

                Inc(x);
                end;

            Inc(y);
            end;
        end;

    begin
    for i:= 1 to Length(AText) do
        begin
        c:= AnsiChar(AText[i]);
        if  c > #$1F then
            b:= Ord(c) - $20
        else
            b:= $00;

        if  Length(FDetails[FSizeIdx].Widths) > b then
            begin
            w:= FDetails[FSizeIdx].Widths[b];
            sr:= Rect(FDetails[FSizeIdx].Offsets[b], 0,
                    FDetails[FSizeIdx].Offsets[b] + w, FSizes[FSizeIdx]);
            dr:= Rect(AX, AY - (FDetails[FSizeIdx].Baseline + 1), AX + w,
                    AY + FSizes[FSizeIdx] - (FDetails[FSizeIdx].Baseline + 1));

            if  FStyle = [] then
                ACanvas.CopyRect(dr, FDetails[FSizeIdx].Bitmap.Canvas, sr)
            else
                begin
                tr.Top:= 0;
                tr.Left:= 0;
                tr.Bottom:= FSizes[FSizeIdx];
//dengland      w is from inclusive rect space but we'll be using it in exclusive.
                tr.Right:= w + 1;

                tp.x:= 0;
                tp.y:= 0;

                if  gfsBold in FStyle then
                    begin
                    Inc(tr.Right);
                    Inc(dr.Right);
                    Inc(w);
                    end;

                if  gfsOutline in FStyle then
                    begin
                    Inc(tr.Right, 2);
                    Inc(tr.Bottom, 2);
                    Inc(dr.Right, 2);
                    Inc(dr.Bottom, 2);
                    Inc(tp.x, 1);
                    Inc(tp.y, 1);
                    Inc(w, 2);
                    end;

                if  gfsItalic in FStyle then
                    begin
//dengland          RoundingMode is set to "Banker's Rounding" (favours even
//                      numbers when rounding 0.5 instead of always upwards) by
//                      default and that doesn't work and I don't want to
//                      include Math to work around it.   This seems to work
//                      alright, anyway.
//                  b:= Round(FSizes[FSizeIdx] / 2) - 1;
                    b:= FSizes[FSizeIdx] div 2;
                    Inc(tr.Right, b);
                    Inc(dr.Right, b);
                    end
                else
                    b:= 0;

                FTemp1.Canvas.Brush.Color:= clNone;
                FTemp1.Canvas.FillRect(tr);

                DoDrawTempChar(gfsItalic in FStyle, False, tr.Left + b);

                if  gfsBold in FStyle then
                    begin
                    Inc(tp.x);
                    DoDrawTempChar(gfsItalic in FStyle, True, tr.Right - 2);
                    Dec(tp.x);
                    end;

                if  gfsOutline in FStyle then
                    DoBuildTempOutline;

                if  gfsReverse in FStyle then
                    DoBuildTempReverse;

                if  gfsUnderline in FStyle then
                    DoBuildTempUnderline;

                if  gfsItalic in FStyle then
                    DoCopyItalic
                else
                    ACanvas.CopyRect(dr, FTemp1.Canvas, tr);
                end;

            Inc(AX, w);
            end;
        end;
    end;

function TGEOSFont.TextFitInfo(const AText: string;
        AMaxWidth: Integer): Integer;
    var
    i: Integer;
    c: AnsiChar;
    b: Byte;
    w: Byte;
    m: Integer;

    begin
    m:= 0;
    Result:= 0;
    for i:= 1 to Length(AText) do
        begin
        c:= AnsiChar(AText[i]);
        if  c > #$1F then
            b:= Ord(c) - $20
        else
            b:= $00;

        if  Length(FDetails[FSizeIdx].Widths) > b then
            begin
            w:= FDetails[FSizeIdx].Widths[b];

            if  gfsBold in FStyle then
                Inc(w);

            if  gfsOutline in FStyle then
                Inc(w, 2);
            end
        else
            w:= 0;

        if  (m + w) > AMaxWidth then
            Break;

        Inc(Result);
        Inc(m, w);
        end;
    end;

function TGEOSFont.TextExtent(const AText: string): TPoint;
    var
    i: Integer;
    c: AnsiChar;
    b: Byte;
    w: Byte;

    begin
    Result.y:= FDetails[FSizeIdx].Size;
    if  gfsOutline in FStyle then
        Inc(Result.y, 2);

    Result.x:= 0;
    for i:= 1 to Length(AText) do
        begin
        c:= AnsiChar(AText[i]);
        if  c > #$1F then
            b:= Ord(c) - $20
        else
            b:= $00;

        if  Length(FDetails[FSizeIdx].Widths) > b then
            begin
            w:= FDetails[FSizeIdx].Widths[b];

            if  gfsBold in FStyle then
                Inc(w);

            if  gfsOutline in FStyle then
                Inc(w, 2);
            end
        else
            w:= 0;

        Inc(Result.x, w);
        end;
    end;

end.

