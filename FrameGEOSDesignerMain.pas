//------------------------------------------------------------------------------
//FrameGEOSDesignerMain
//=====================
//Application main frame for the GEOS Designer application.  Provides the
//standard user interface for the application.
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
unit FrameGEOSDesignerMain;

{$mode objfpc}{$H+}

interface

uses
    Classes, SysUtils, FileUtil, DividerBevel, Forms, Controls, CheckLst,
    ExtCtrls, StdCtrls, ComCtrls, Spin, GEOSDesignerCore;

type

{ TGEOSDesignerMainFrame }

    TGEOSDesignerMainFrame = class(TFrame)
        CheckBox1: TCheckBox;
        ChkBxDoMenuItmCnstrnd: TCheckBox;
        ChkBxDoMenuItmVisible: TCheckBox;
        ChkLstBxElements: TCheckListBox;
        CmbDoMenuItmType: TComboBox;
        CmbDoMenuItmAlign: TComboBox;
        CmbDoIconsIcon: TComboBox;
        DividerBevel1: TDividerBevel;
        DividerBevel2: TDividerBevel;
        DividerBevel3: TDividerBevel;
        DividerBevel4: TDividerBevel;
        EdtDoMenuItmText: TEdit;
        Label1: TLabel;
        Label10: TLabel;
        Label11: TLabel;
        Label12: TLabel;
        Label13: TLabel;
        Label14: TLabel;
        Label15: TLabel;
        Label16: TLabel;
        Label17: TLabel;
        Label18: TLabel;
        Label19: TLabel;
        Label2: TLabel;
        Label20: TLabel;
        Label21: TLabel;
        Label22: TLabel;
        Label23: TLabel;
        LblDoIconItmIdent: TLabel;
        Label24: TLabel;
        Label25: TLabel;
        Label26: TLabel;
        Label27: TLabel;
        Label28: TLabel;
        Label3: TLabel;
        Label4: TLabel;
        Label5: TLabel;
        Label6: TLabel;
        Label7: TLabel;
        Label8: TLabel;
        Label9: TLabel;
        LblDoMenuItmIdent: TLabel;
        LblGrphStrIdent: TLabel;
        LblDoMenuIdent: TLabel;
        LblDoIconsIdent: TLabel;
        LstBxDoIcons: TListBox;
        LstVwGrphStrItems: TListView;
        NtBkDetails: TNotebook;
        Panel10: TPanel;
        Panel11: TPanel;
        Panel3: TPanel;
        Panel4: TPanel;
        Panel5: TPanel;
        Panel6: TPanel;
        Panel7: TPanel;
        Panel8: TPanel;
        Panel9: TPanel;
        PgGraphicsString: TPage;
        PgDoMenu: TPage;
        PgDoIcons: TPage;
        Panel1: TPanel;
        Panel2: TPanel;
        SpEdtDoMenuItmTop: TSpinEdit;
        SpEdtDoMenuItmLeft: TSpinEdit;
        SpEdtDoMenuItmBottom: TSpinEdit;
        SpEdtDoMenuItmRight: TSpinEdit;
        SpEdtDoIconsXPos: TSpinEdit;
        SpEdtDoIconsYPos: TSpinEdit;
        SpEdtDoIconsItmXPos: TSpinEdit;
        SpEdtDoIconsItmYPos: TSpinEdit;
        Splitter1: TSplitter;
        Splitter2: TSplitter;
        Splitter3: TSplitter;
        TlBtnIconDelete: TToolButton;
        TlBtnMenuDelete: TToolButton;
        ToolBar1: TToolBar;
        ToolBar2: TToolBar;
        ToolBar3: TToolBar;
        ToolButton1: TToolButton;
        TlBtnGrphDelete: TToolButton;
        TlBtnGrphEdit: TToolButton;
        ToolButton2: TToolButton;
        ToolButton3: TToolButton;
        TreeVwDoMenu: TTreeView;
        procedure ChkLstBxElementsClick(Sender: TObject);
        procedure ChkLstBxElementsClickCheck(Sender: TObject);
        procedure ChkLstBxElementsItemClick(Sender: TObject; Index: integer);
        procedure LstVwGrphStrItemsResize(Sender: TObject);
        procedure LstVwGrphStrItemsSelectItem(Sender: TObject; Item: TListItem;
                Selected: Boolean);
        procedure TlBtnGrphDeleteClick(Sender: TObject);
        procedure TreeVwDoMenuSelectionChanged(Sender: TObject);
    private
        FSelectedElem: TGEOSDesignerElement;

        procedure ClearDisplay;

        procedure DoInitGrphStrElemView;
        procedure DoInitDoMenuElemView;
        procedure DoInitDoIconElemView;

        procedure DoFindActiveElement;
        procedure DoInitialiseElementView(const AElem: Integer);

    public
        procedure InitialiseDisplay;

        property  SelectedElem: TGEOSDesignerElement read FSelectedElem;
    end;

implementation

{$R *.lfm}

uses
    DModGEOSDesignerMain;


{ TGEOSDesignerMainFrame }

procedure TGEOSDesignerMainFrame.ChkLstBxElementsItemClick(Sender: TObject;
        Index: integer);
    begin
//dengland This isn't working like I expect it to.  Only getting called after
//      clicking on an item's check box which is no good to me because I can
//      detect that in OnClickCheck.
//  DoFindActiveElement;
    end;

procedure TGEOSDesignerMainFrame.LstVwGrphStrItemsResize(Sender: TObject);
    var
    i: Integer;
    w: Integer;

    begin
    w:= 35;
    for i:= 0 to 2 do
        Inc(w, LstVwGrphStrItems.Column[i].Width);

    w:= LstVwGrphStrItems.Width - w;
    if  w < 150 then
        w:= 150;

    LstVwGrphStrItems.Column[3].Width:= w;
    end;

procedure TGEOSDesignerMainFrame.LstVwGrphStrItemsSelectItem(Sender: TObject;
        Item: TListItem; Selected: Boolean);
    begin
    if  Selected
    and (Item.Index = (LstVwGrphStrItems.Items.Count - 1)) then
        TlBtnGrphDelete.Enabled:= True
    else
        TlBtnGrphDelete.Enabled:= False;

    TlBtnGrphEdit.Enabled:= Selected;
    end;

procedure TGEOSDesignerMainFrame.TlBtnGrphDeleteClick(Sender: TObject);
    var
    e: TGEOSGraphicsStrElement;

    begin
    e:= FSelectedElem as TGEOSGraphicsStrElement;

    e.DeleteItem(LstVwGrphStrItems.Items.Count - 1);
    LstVwGrphStrItems.Items.Delete(LstVwGrphStrItems.Items.Count - 1);
    end;

procedure TGEOSDesignerMainFrame.TreeVwDoMenuSelectionChanged(Sender: TObject);
    var
    mi: TGEOSDoMenuItem;

    begin
    if  Assigned(TreeVwDoMenu.Selected) then
        begin
        mi:= TGEOSDoMenuItem(TreeVwDoMenu.Selected.Data);

        CmbDoMenuItmAlign.ItemIndex:= Ord(mi.Alignment);

//todo  TGEOSDesignerMainFrame.TreeVwDoMenuSelectionChanged need set max on
//          spin controls.

        SpEdtDoMenuItmTop.Value:= mi.Bounds.Top;
        SpEdtDoMenuItmLeft.Value:= mi.Bounds.Left;
        SpEdtDoMenuItmBottom.Value:= mi.Bounds.Bottom;
        SpEdtDoMenuItmRight.Value:= mi.Bounds.Right;

        ChkBxDoMenuItmCnstrnd.Checked:= mi.Constrained;
        ChkBxDoMenuItmVisible.Checked:= mi.Visible;

        LblDoMenuItmIdent.Caption:= mi.Identifier;
        EdtDoMenuItmText.Text:= mi.Text;
        CmbDoMenuItmType.ItemIndex:= Ord(mi.MenuType);
        end;
    end;

procedure TGEOSDesignerMainFrame.ChkLstBxElementsClickCheck(Sender: TObject);
    var
    i: Integer;

    begin
    for i:= 0 to ChkLstBxElements.Count - 1 do
        GEOSDesignerMainDMod.Elements[i].Active:= ChkLstBxElements.Checked[i];

    if  Assigned(GEOSDesignerOnChange) then
        GEOSDesignerOnChange;

//dengland  OnClick will be called after this so use that to handle selection
//      changes.
    end;

procedure TGEOSDesignerMainFrame.ChkLstBxElementsClick(Sender: TObject);
    begin
    DoFindActiveElement;
    end;

procedure TGEOSDesignerMainFrame.ClearDisplay;
    begin
    ChkLstBxElements.Items.Clear;
    FSelectedElem:= nil;
    end;

procedure TGEOSDesignerMainFrame.DoInitGrphStrElemView;
    var
    e: TGEOSGraphicsStrElement;
    i,
    j: Integer;
    s: string;
    litm: TListItem;
    inst: PGEOSGraphicsInstr;

    begin
    e:= FSelectedElem as TGEOSGraphicsStrElement;

    LblGrphStrIdent.Caption:= e.Identifier;
    LstVwGrphStrItems.Items.BeginUpdate;
    try
        LstVwGrphStrItems.Clear;

        for i:= 0 to e.Count - 1 do
            begin
            inst:= e.Items[i];

            litm:= LstVwGrphStrItems.Items.Add;
            litm.ImageIndex:= Ord(inst^.InstrType);

            if  inst^.InstrType = ggiGraphics then
                litm.SubItems.Add('GraphicsString')
            else
                litm.SubItems.Add('PutString');

            if  inst^.InstrCmd = VAL_CMD_GEOSPSTR_PUTSTR then
                litm.SubItems.Add('PutString')
            else
                litm.SubItems.Add(ARR_LIT_GEOSGRPHSTRCMDS[inst^.InstrCmd]);

            s:= EmptyStr;
            for j:= 0 to High(inst^.InstrData) do
                s:= s + Format('$%2.2x ', [inst^.InstrData[j]]);

            litm.SubItems.Add(s);
            end;
        finally
        LstVwGrphStrItems.Items.EndUpdate;
        end;
    end;

procedure TGEOSDesignerMainFrame.DoInitDoMenuElemView;
    var
    e: TGEOSDoMenuElement;
    i: Integer;
    tnode: TTreeNode;

    procedure DoRecurseSubItems(const AParent: TTreeNode;
            const AItem: TGEOSDoMenuItem);
        var
        i: Integer;
        tnode: TTreeNode;

        begin
        for i:= 0 to AItem.SubItemsCount - 1 do
            begin
            tnode:= TreeVwDoMenu.Items.AddChildObject(AParent,
                    AItem.SubItems[i].Text, AItem.SubItems[i]);
            if  AItem.SubItems[i].SubItemsCount > 0 then
                DoRecurseSubItems(tnode, AItem.SubItems[i]);
            end;
        end;

    begin
    e:= FSelectedElem as TGEOSDoMenuElement;

    LblDoMenuIdent.Caption:= e.Identifier;
    TreeVwDoMenu.Items.BeginUpdate;
    try
        TreeVwDoMenu.Items.Clear;

        for i:= 0 to e.Count - 1 do
            begin
            tnode:= TreeVwDoMenu.Items.AddChildObject(nil, e.Items[i].Text,
                    e.Items[i]);
            if  e.Items[i].SubItemsCount > 0 then
                DoRecurseSubItems(tnode, e.Items[i]);
            end;

        finally
        TreeVwDoMenu.Items.EndUpdate;
        end;

    TreeVwDoMenu.FullExpand;
    if  TreeVwDoMenu.Items.Count > 0 then
        begin
        TreeVwDoMenu.Selected:= TreeVwDoMenu.Items[0];
        end;
    end;

procedure TGEOSDesignerMainFrame.DoInitDoIconElemView;
    begin

    end;

procedure TGEOSDesignerMainFrame.DoFindActiveElement;
    var
    i: Integer;

    begin
    for i:= 0 to ChkLstBxElements.Count - 1 do
        if  ChkLstBxElements.Selected[i] then
            begin
            DoInitialiseElementView(i);

            Break;
            end;
    end;

procedure TGEOSDesignerMainFrame.DoInitialiseElementView(const AElem: Integer);
    begin
    FSelectedElem:= GEOSDesignerMainDMod.Elements[AElem];

    if  FSelectedElem is TGEOSGraphicsStrElement then
        begin
        DoInitGrphStrElemView;
        NtBkDetails.PageIndex:= 0;
        end
    else if FSelectedElem is TGEOSDoMenuElement then
        begin
        DoInitDoMenuElemView;
        NtBkDetails.PageIndex:= 1;
        end
    else if FSelectedElem is TGEOSDoIconsElement then
        begin
        DoInitDoIconElemView;
        NtBkDetails.PageIndex:= 2;
        end
    else
        NtBkDetails.PageIndex:= -1;
    end;

procedure TGEOSDesignerMainFrame.InitialiseDisplay;
    var
    i: Integer;
    e: TGEOSDesignerElement;

    begin
    ClearDisplay;

    ChkLstBxElements.Items.BeginUpdate;
    try
        for i:= 0 to GEOSDesignerMainDMod.ElementsCount - 1 do
            begin
            e:= GEOSDesignerMainDMod.Elements[i];

            ChkLstBxElements.Items.Add(e.ElementName);
            ChkLstBxElements.Checked[ChkLstBxElements.Items.Count - 1]:=
                    e.Active;
            end;

        finally
        ChkLstBxElements.Items.EndUpdate;
        end;

    if  ChkLstBxElements.Items.Count > 0 then
        begin
        ChkLstBxElements.Selected[0]:= True;
        DoFindActiveElement;
        end
    else
        NtBkDetails.PageIndex:= -1;
    end;

end.

