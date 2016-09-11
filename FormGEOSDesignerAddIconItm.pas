unit FormGEOSDesignerAddIconItm;

{$mode objfpc}{$H+}

interface

uses
    Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
    Spin;

type

    { TGEOSDesignerAddIconItmForm }

    TGEOSDesignerAddIconItmForm = class(TForm)
        BtnCancel: TButton;
        BtnOK: TButton;
        ChkBxIconDblB: TCheckBox;
        ChkBxIconDblW: TCheckBox;
        CmbIcon: TComboBox;
        EdtIdentifier: TEdit;
        Label2: TLabel;
        Label24: TLabel;
        Label25: TLabel;
        Label26: TLabel;
        Label27: TLabel;
        Label28: TLabel;
        SpEdtIconXPos: TSpinEdit;
        SpEdtIconYPos: TSpinEdit;
        procedure CmbIconChange(Sender: TObject);
        procedure EdtIdentifierKeyPress(Sender: TObject; var Key: char);
        procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
        procedure FormShow(Sender: TObject);
    private
        FChanging: Boolean;

        procedure DoClearControls;
        procedure DoInitialiseControls;
    public
        { public declarations }
    end;

var
    GEOSDesignerAddIconItmForm: TGEOSDesignerAddIconItmForm;

implementation

{$R *.lfm}

uses
    GEOSTypes, GEOSDesignerCore, DModGEOSDesignerMain;


{ TGEOSDesignerAddIconItmForm }

procedure TGEOSDesignerAddIconItmForm.EdtIdentifierKeyPress(Sender: TObject;
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

procedure TGEOSDesignerAddIconItmForm.CmbIconChange(Sender: TObject);
    begin
    if  not FChanging then
        BtnOK.Enabled:= CmbIcon.ItemIndex > -1;
    end;

procedure TGEOSDesignerAddIconItmForm.FormClose(Sender: TObject;
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

procedure TGEOSDesignerAddIconItmForm.FormShow(Sender: TObject);
    begin
    DoClearControls;
    DoInitialiseControls;
    end;

procedure TGEOSDesignerAddIconItmForm.DoClearControls;
    begin
    FChanging:= True;
    try
        EdtIdentifier.Text:= EmptyStr;
        CmbIcon.Clear;
        SpEdtIconXPos.Value:= 0;
        SpEdtIconXPos.MaxValue:=
                ARR_REC_GEOSDISPLAYRES[GEOSDispMode].Width div 8 - 1;
        SpEdtIconYPos.Value:= 0;
        SpEdtIconYPos.MaxValue:=
                ARR_REC_GEOSDISPLAYRES[GEOSDispMode].Height - 1;
        ChkBxIconDblW.Checked:= False;
        ChkBxIconDblB.Checked:= False;

        finally
        FChanging:= False;
        end;
    end;

procedure TGEOSDesignerAddIconItmForm.DoInitialiseControls;
    var
    i: Integer;

    begin
    FChanging:= True;
    try
        for i:= 0 to GEOSDesignerMainDMod.IconsCount - 1 do
            CmbIcon.Items.Add(GEOSDesignerMainDMod.Icons[i].Identifier);

        finally
        FChanging:= False;
        end;

    CmbIcon.ItemIndex:= -1;
    BtnOK.Enabled:= False;
    ActiveControl:= EdtIdentifier;
    end;

end.

