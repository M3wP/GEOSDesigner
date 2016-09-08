unit FormGEOSDesignerAddGPStrInstr;

{$mode objfpc}{$H+}

interface

uses
    Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ComboEx,
    StdCtrls, ExtCtrls, Spin, GEOSDesignerCore;

type

{ TGEOSDesignerAddGPStrInstrForm }

    TGEOSDesignerAddGPStrInstrForm = class(TForm)
        BtnOK: TButton;
        BtnCancel: TButton;
        CmbCommand: TComboBox;
        CmbExType: TComboBoxEx;
        CmbExPattern: TComboBoxEx;
        EdtString: TEdit;
        Label1: TLabel;
        LblDataNone: TLabel;
        Label2: TLabel;
        Label3: TLabel;
        Label4: TLabel;
        Label5: TLabel;
        Label6: TLabel;
        Label7: TLabel;
        Label8: TLabel;
        Label9: TLabel;
        NtBkData: TNotebook;
        PgDataY: TPage;
        PgDataString: TPage;
        PgDataX: TPage;
        PgDataXY: TPage;
        PgDataPattern: TPage;
        SpEdtXYXPos: TSpinEdit;
        SpEdtXYYPos: TSpinEdit;
        SpEdtXXPos: TSpinEdit;
        SpEdtYYPos: TSpinEdit;
        procedure CmbCommandChange(Sender: TObject);
        procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
        procedure NtBkDataEnter(Sender: TObject);
    private
        FChanging: Boolean;
        FMode: TGEOSGraphicsInstrType;
        FEdit: Boolean;
        FInstr: PGEOSGraphicsInstr;

        procedure DoXlatDataToCtrls(const ADetails: Integer);
        procedure DoXlatCtrlsToData;

        procedure DoClearControls;
        procedure DoPrepareControls;

        procedure DoInitialise(const AMode: TGEOSGraphicsInstrType;
                const AEdit: Boolean; const AInstr: PGEOSGraphicsInstr);

    public
        function  ShowAdd(const AMode: TGEOSGraphicsInstrType;
                var AInstr: TGEOSGraphicsInstr): TModalResult;
        function  ShowEdit(const AInstr: PGEOSGraphicsInstr): TModalResult;
    end;

var
    GEOSDesignerAddGPStrInstrForm: TGEOSDesignerAddGPStrInstrForm;

implementation

{$R *.lfm}

type
    TGPStrInstrDetails = packed record
        Cmd: Byte;
        Page: Integer;
    end;

const
//Pages:
//      -1 None
//       0 X, Y
//       1 Pattern
//       2 X
//       3 String
//       4 Y
  ARR_REC_GRHPCSTRDET: array[0..5] of TGPStrInstrDetails = (
        (Cmd: 1; Page: 0), (Cmd: 2; Page: 0), (Cmd: 3; Page: 0),
        (Cmd: 5; Page: 1), (Cmd: 6; Page: -1), (Cmd: 7; Page: 0));

  ARR_REC_PUTSTRNGDET: array[0..20] of TGPStrInstrDetails = (
        (Cmd: 8; Page: -1), (Cmd: 9; Page: -1), (Cmd: 10; Page: -1),
        (Cmd: 11; Page: -1), (Cmd: 12; Page: -1), (Cmd: 13; Page: -1),
        (Cmd: 14; Page: -1), (Cmd: 15; Page: -1), (Cmd: 16; Page: -1),
        (Cmd: 18; Page: -1), (Cmd: 19; Page: -1), (Cmd: 20; Page: 2),
        (Cmd: 21; Page: 4), (Cmd: 22; Page: 0), (Cmd: 24; Page: -1),
        (Cmd: 25; Page: -1), (Cmd: 26; Page: -1), (Cmd: 27; Page: -1),
        (Cmd: 127; Page: -1), (Cmd: 128; Page: -1), (Cmd: 255; Page: 3));

{ TGEOSDesignerAddGPStrInstrForm }

procedure TGEOSDesignerAddGPStrInstrForm.CmbCommandChange(Sender: TObject);
    var
    d: TGPStrInstrDetails;

    begin
    if  not FChanging then
        begin
        if  FMode = ggiGraphics then
            d:= ARR_REC_GRHPCSTRDET[CmbCommand.ItemIndex]
        else
            d:= ARR_REC_PUTSTRNGDET[CmbCommand.ItemIndex];

        if  FEdit
        and (((FMode = ggiGraphics)
        and  (d.Cmd = VAL_CMD_GEOSGSTR_ESCPTS))
        or   ((FMode = ggiString)
        and  (d.Cmd = VAL_CMD_GEOSPSTR_ESCGRP))) then
            BtnOK.Enabled:= False
        else
            BtnOK.Enabled:= True;

        NtBkData.PageIndex:= d.Page;
        end;
    end;

procedure TGEOSDesignerAddGPStrInstrForm.FormClose(Sender: TObject;
        var CloseAction: TCloseAction);
    var
    d: TGPStrInstrDetails;

    begin
    if  ModalResult = mrOk then
        begin
        if  FMode = ggiGraphics then
            d:= ARR_REC_GRHPCSTRDET[CmbCommand.ItemIndex]
        else
            d:= ARR_REC_PUTSTRNGDET[CmbCommand.ItemIndex];

        FInstr^.InstrType:= TGEOSGraphicsInstrType(CmbExType.ItemIndex);
        FInstr^.InstrCmd:= d.Cmd;

        DoXlatCtrlsToData;
        end;
    end;

procedure TGEOSDesignerAddGPStrInstrForm.NtBkDataEnter(Sender: TObject);
    begin
    if  NtBkData.PageIndex = 0 then
        ActiveControl:= SpEdtXYXPos
    else if NtBkData.PageIndex = 1 then
        ActiveControl:= CmbExPattern
    else if NtBkData.PageIndex = 2 then
        ActiveControl:= SpEdtXXPos
    else if NtBkData.PageIndex = 3 then
        ActiveControl:= EdtString
    else if NtBkData.PageIndex = 4 then
        ActiveControl:= SpEdtYYPos;
    end;

procedure TGEOSDesignerAddGPStrInstrForm.DoXlatDataToCtrls(
        const ADetails: Integer);
    procedure DoSetSpEdtFromWord(const ASpEdt: TSpinEdit;
            const AData: array of Byte; const AIndex: Integer);
        var
        v: Word;

        begin
        v:= AData[AIndex] or (AData[AIndex + 1] shl 8);
        ASpEdt.Value:= v;
        end;
    var
    p: Integer;
    i: Integer;
    s: string;

    begin
    if  FMode = ggiGraphics then
        p:= ARR_REC_GRHPCSTRDET[ADetails].Page
    else
        p:= ARR_REC_PUTSTRNGDET[ADetails].Page;

    NtBkData.PageIndex:= p;
    LblDataNone.Visible:= p = -1;

    case p of
        0:
            begin
            DoSetSpEdtFromWord(SpEdtXYXPos, FInstr^.InstrData, 0);
            DoSetSpEdtFromWord(SpEdtXYYPos, FInstr^.InstrData, 2);
            end;
        1:
            CmbExPattern.ItemIndex:= FInstr^.InstrData[0];
        2:
            DoSetSpEdtFromWord(SpEdtXXPos, FInstr^.InstrData, 0);
        3:
            begin
            s:= EmptyStr;
            for i:= 0 to High(FInstr^.InstrData) do
                s:= s + string(AnsiChar(FInstr^.InstrData[i]));
            EdtString.Text:= s;
            end;
        4:
            DoSetSpEdtFromWord(SpEdtYYPos, FInstr^.InstrData, 0);
        end
    end;

procedure TGEOSDesignerAddGPStrInstrForm.DoXlatCtrlsToData;
    procedure DoSetWordFromSpEdit(const ASpEdt: TSpinEdit;
            var AData: array of Byte; const AIndex: Integer);
        var
        vl,
        vh: Byte;

        begin
        vl:= ASpEdt.Value and $00FF;
        vh:= (ASpEdt.Value and $FF00) shr 8;

        AData[AIndex]:= vl;
        AData[AIndex + 1]:= vh;
        end;

    var
    i: Integer;

    begin
//      -1 None
//       0 X, Y
//       1 Pattern
//       2 X
//       3 String
//       4 Y
    case NtBkData.PageIndex of
        0:
            begin
            SetLength(FInstr^.InstrData, 4);
            DoSetWordFromSpEdit(SpEdtXYXPos, FInstr^.InstrData, 0);
            DoSetWordFromSpEdit(SpEdtXYYPos, FInstr^.InstrData, 2);
            end;
        1:
            begin
            SetLength(FInstr^.InstrData, 1);
            FInstr^.InstrData[0]:= CmbExPattern.ItemIndex;
            end;
        2:
            begin
            SetLength(FInstr^.InstrData, 2);
            DoSetWordFromSpEdit(SpEdtXXPos, FInstr^.InstrData, 0);
            end;
        3:
            begin
            SetLength(FInstr^.InstrData, Length(EdtString.Text));
            for i:= 1 to Length(EdtString.Text) do
                FInstr^.InstrData[i - 1]:= Byte(AnsiChar(EdtString.Text[i]));
            end;
        4:
            begin
            SetLength(FInstr^.InstrData, 2);
            DoSetWordFromSpEdit(SpEdtYYPos, FInstr^.InstrData, 0);
            end;
        else
            SetLength(FInstr^.InstrData, 0);
        end;
    end;

procedure TGEOSDesignerAddGPStrInstrForm.DoClearControls;
    begin
    CmbCommand.Clear;
    LblDataNone.Visible:= True;
    SpEdtXYXPos.Value:= 0;
    SpEdtXYYPos.Value:= 0;
    SpEdtXXPos.Value:= 0;
    SpEdtYYPos.Value:= 0;
    EdtString.Text:= EmptyStr;
    CmbExPattern.ItemIndex:= 0;
    NtBkData.PageIndex:= -1;
    end;

procedure TGEOSDesignerAddGPStrInstrForm.DoPrepareControls;
    var
    d,
    i: Integer;

    begin
    if  FEdit then
        begin
        d:= -1;
        if  FMode = ggiGraphics then
            begin
            for i:= 0 to High(ARR_REC_GRHPCSTRDET) do
                if  FInstr^.InstrCmd = ARR_REC_GRHPCSTRDET[i].Cmd then
                    begin
                    d:= i;
                    Break;
                    end;
            end
        else
            for i:= 0 to High(ARR_REC_PUTSTRNGDET) do
                if  FInstr^.InstrCmd = ARR_REC_PUTSTRNGDET[i].Cmd then
                    begin
                    d:= i;
                    Break;
                    end;

        CmbCommand.ItemIndex:= d;
        DoXlatDataToCtrls(d);
        end;

    ActiveControl:= CmbCommand;
    BtnOK.Enabled:= CmbCommand.ItemIndex > -1;
    end;

procedure TGEOSDesignerAddGPStrInstrForm.DoInitialise(
        const AMode: TGEOSGraphicsInstrType; const AEdit: Boolean;
        const AInstr: PGEOSGraphicsInstr);
    var
    i: Integer;

    begin
    FMode:= AMode;
    FEdit:= AEdit;
    FInstr:= AInstr;

    DoClearControls;

    CmbExType.ItemIndex:= Ord(FMode);
    CmbCommand.Items.BeginUpdate;
    try
        if  FMode = ggiGraphics then
            for i:= 0 to High(ARR_REC_GRHPCSTRDET) do
                CmbCommand.Items.Add(ARR_LIT_GEOSGRPHSTRCMDS[
                        ARR_REC_GRHPCSTRDET[i].Cmd])
        else
            for i:= 0 to High(ARR_REC_PUTSTRNGDET) do
                if  ARR_REC_PUTSTRNGDET[i].Cmd < VAL_CMD_GEOSPSTR_USELST then
                    CmbCommand.Items.Add(ARR_LIT_GEOSGRPHSTRCMDS[
                            ARR_REC_PUTSTRNGDET[i].Cmd])
                else if ARR_REC_PUTSTRNGDET[i].Cmd = VAL_CMD_GEOSPSTR_USELST then
                    CmbCommand.Items.Add('UseLast')
                else if ARR_REC_PUTSTRNGDET[i].Cmd = VAL_CMD_GEOSPSTR_SHRTCT then
                    CmbCommand.Items.Add('ShortCut')
                else if ARR_REC_PUTSTRNGDET[i].Cmd = VAL_CMD_GEOSPSTR_PUTSTR then
                    CmbCommand.Items.Add('PutString');

        finally
        CmbCommand.Items.EndUpdate;
        end;

    DoPrepareControls;
    end;

function TGEOSDesignerAddGPStrInstrForm.ShowAdd(
        const AMode: TGEOSGraphicsInstrType;
        var AInstr: TGEOSGraphicsInstr): TModalResult;
    begin
    DoInitialise(AMode, False, @AInstr);
    Result:= ShowModal;
    end;

function TGEOSDesignerAddGPStrInstrForm.ShowEdit(
        const AInstr: PGEOSGraphicsInstr): TModalResult;
    begin
    DoInitialise(AInstr^.InstrType, True, AInstr);
    Result:= ShowModal;
    end;


end.

