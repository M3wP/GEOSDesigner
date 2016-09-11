unit FormGEOSDesignerAddMenuItm;

{$mode objfpc}{$H+}

interface

uses
    Classes, SysUtils, FileUtil, DividerBevel, Forms, Controls, Graphics,
    Dialogs, StdCtrls, ExtCtrls, Spin;

type

    { TGEOSDesignerAddMenuItmForm }

    TGEOSDesignerAddMenuItmForm = class(TForm)
        BtnCancel: TButton;
        BtnOK: TButton;
        ChkBxCnstrnd: TCheckBox;
        CmbAlignment: TComboBox;
        CmbType: TComboBox;
        DividerBevel1: TDividerBevel;
        DividerBevel2: TDividerBevel;
        EdtText: TEdit;
        EdtIdentifier: TEdit;
        Label10: TLabel;
        Label11: TLabel;
        Label12: TLabel;
        Label13: TLabel;
        Label14: TLabel;
        Label15: TLabel;
        Label6: TLabel;
        Label7: TLabel;
        Label8: TLabel;
        Label9: TLabel;
        Panel1: TPanel;
        Panel2: TPanel;
        PnlLevelDetails: TPanel;
        SpEdtBottom: TSpinEdit;
        SpEdtLeft: TSpinEdit;
        SpEdtRight: TSpinEdit;
        SpEdtTop: TSpinEdit;
        procedure EdtIdentifierKeyPress(Sender: TObject; var Key: char);
        procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    private
        procedure DoClearControls;

    public
        function  ShowAdd(const AMenuLevel: Boolean): TModalResult;
    end;

var
    GEOSDesignerAddMenuItmForm: TGEOSDesignerAddMenuItmForm;

implementation

{$R *.lfm}

uses
    GEOSTypes, GEOSDesignerCore;


{ TGEOSDesignerAddMenuItmForm }

procedure TGEOSDesignerAddMenuItmForm.EdtIdentifierKeyPress(Sender: TObject;
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

procedure TGEOSDesignerAddMenuItmForm.FormClose(Sender: TObject;
        var CloseAction: TCloseAction);
    begin
    if  (ModalResult = mrOk)
    and ((Length(EdtIdentifier.Text) = 0)
    or   (GEOSDesignerIdents.IndexOf(EdtIdentifier.Text) > -1)) then
        begin
        ModalResult:= mrNone;
        CloseAction:= caNone;
        Beep;
        ActiveControl:= EdtIdentifier;
        end;
    end;

procedure TGEOSDesignerAddMenuItmForm.DoClearControls;
    begin
    CmbAlignment.ItemIndex:= 1;
    SpEdtTop.MaxValue:= ARR_REC_GEOSDISPLAYRES[GEOSDispMode].Height - 1;
    SpEdtTop.Value:= 0;
    SpEdtLeft.MaxValue:= ARR_REC_GEOSDISPLAYRES[GEOSDispMode].Width - 1;
    SpEdtLeft.Value:= 0;
    SpEdtBottom.MaxValue:= ARR_REC_GEOSDISPLAYRES[GEOSDispMode].Height - 1;
    SpEdtBottom.Value:= 0;
    SpEdtRight.MaxValue:= ARR_REC_GEOSDISPLAYRES[GEOSDispMode].Width - 1;
    SpEdtRight.Value:= 0;
    ChkBxCnstrnd.Checked:= False;
    EdtIdentifier.Text:= EmptyStr;
    EdtText.Text:= EmptyStr;
    CmbType.ItemIndex:= 0;
    end;

function TGEOSDesignerAddMenuItmForm.ShowAdd(
        const AMenuLevel: Boolean): TModalResult;
    begin
    DoClearControls;

    PnlLevelDetails.Visible:= AMenuLevel;
    if  AMenuLevel then
        ActiveControl:= CmbAlignment
    else
        ActiveControl:= EdtIdentifier;

    Result:= ShowModal;
    end;

end.

