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
        ChkBxDoIconsDblW: TCheckBox;
        ChkBxDoIconsAdd1W: TCheckBox;
        ChkBxDoIconsShowMouse: TCheckBox;
        ChkBxDoMenuItmCnstrnd: TCheckBox;
        ChkBxDoMenuItmVisible: TCheckBox;
        ChkBxDoIconsItmDblW: TCheckBox;
        ChkBxDoIconsItmDblB: TCheckBox;
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
        TlBtnGrphAdd: TToolButton;
        TlBtnGrphDelete: TToolButton;
        TlBtnGrphEdit: TToolButton;
        ToolButton2: TToolButton;
        ToolButton3: TToolButton;
        TreeVwDoMenu: TTreeView;
        procedure ChkBxDoIconsAdd1WChange(Sender: TObject);
        procedure ChkBxDoIconsDblWChange(Sender: TObject);
        procedure ChkBxDoIconsItmDblBChange(Sender: TObject);
        procedure ChkBxDoIconsItmDblWChange(Sender: TObject);
        procedure ChkBxDoIconsShowMouseChange(Sender: TObject);
        procedure ChkLstBxElementsClick(Sender: TObject);
        procedure ChkLstBxElementsClickCheck(Sender: TObject);
        procedure ChkLstBxElementsItemClick(Sender: TObject; Index: integer);
        procedure CmbDoIconsIconChange(Sender: TObject);
        procedure LstBxDoIconsSelectionChange(Sender: TObject; User: boolean);
        procedure LstVwGrphStrItemsResize(Sender: TObject);
        procedure LstVwGrphStrItemsSelectItem(Sender: TObject; Item: TListItem;
                Selected: Boolean);
        procedure SpEdtDoIconsItmXPosChange(Sender: TObject);
        procedure SpEdtDoIconsItmYPosChange(Sender: TObject);
        procedure SpEdtDoIconsXPosChange(Sender: TObject);
        procedure SpEdtDoIconsYPosChange(Sender: TObject);
        procedure TlBtnGrphAddClick(Sender: TObject);
        procedure TlBtnGrphDeleteClick(Sender: TObject);
        procedure TlBtnGrphEditClick(Sender: TObject);
        procedure TreeVwDoMenuSelectionChanged(Sender: TObject);
    private
        FChanging: Boolean;
        FSelectedElem: TGEOSDesignerElement;
        FSelectedItem: Integer;

        procedure ClearDisplay;

        procedure DoDoIconsItemSelect(const AItem: Integer);

        procedure DoInitGrphStrElemView;
        procedure DoInitDoMenuElemView;
        procedure DoInitDoIconElemView;

        procedure DoFindActiveElement;
        procedure DoInitialiseElementView(const AElem: Integer);

    public
        procedure InitialiseDisplay;
        procedure UpdateElements;
        procedure RedisplayActiveElement;

        property  SelectedElem: TGEOSDesignerElement read FSelectedElem;
    end;

implementation

{$R *.lfm}

uses
    GEOSTypes, DModGEOSDesignerMain, FormGEOSDesignerAddGPStrInstr;


{ TGEOSDesignerMainFrame }

procedure TGEOSDesignerMainFrame.ChkLstBxElementsItemClick(Sender: TObject;
        Index: integer);
    begin
//dengland This isn't working like I expect it to.  Only getting called after
//      clicking on an item's check box which is no good to me because I can
//      detect that in OnClickCheck.
//  DoFindActiveElement;
    end;

procedure TGEOSDesignerMainFrame.CmbDoIconsIconChange(Sender: TObject);
    var
    e: TGEOSDoIconsElement;

    begin
    if  not FChanging then
        begin
        e:= FSelectedElem as TGEOSDoIconsElement;

        e.Icons[FSelectedItem]:=
                GEOSDesignerMainDMod.Icons[CmbDoIconsIcon.ItemIndex];
        end;
    end;

procedure TGEOSDesignerMainFrame.LstBxDoIconsSelectionChange(Sender: TObject;
        User: boolean);
    var
    s,
    i: Integer;

    begin
    s:= -1;
    for i:= 0 to LstBxDoIcons.Count - 1 do
        if  LstBxDoIcons.Selected[i] then
            begin
            s:= i;
            Break;
            end;

    if  s > -1 then
        DoDoIconsItemSelect(s);
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
    var
    e: TGEOSGraphicsStrElement;
    itm: PGEOSGraphicsInstr;

    begin
    if  Selected
    and (Item.Index = (LstVwGrphStrItems.Items.Count - 1)) then
        TlBtnGrphDelete.Enabled:= True
    else
        TlBtnGrphDelete.Enabled:= False;

    if  Selected then
        begin
        e:= FSelectedElem as TGEOSGraphicsStrElement;
        itm:= e[Item.Index];

        TlBtnGrphEdit.Enabled:= ((itm^.InstrType = ggiGraphics) and
             (itm^.InstrCmd <> VAL_CMD_GEOSGSTR_ESCPTS)) or
            ((itm^.InstrType = ggiString) and
             (itm^.InstrCmd <> VAL_CMD_GEOSPSTR_ESCGRP));
        end
    else
        TlBtnGrphEdit.Enabled:= False;
    end;

procedure TGEOSDesignerMainFrame.SpEdtDoIconsItmXPosChange(Sender: TObject);
    var
    e: TGEOSDoIconsElement;

    begin
    if  not FChanging then
        begin
        e:= FSelectedElem as TGEOSDoIconsElement;
        e.IconsXPos[FSelectedItem]:= SpEdtDoIconsItmXPos.Value;
        end;
    end;

procedure TGEOSDesignerMainFrame.SpEdtDoIconsItmYPosChange(Sender: TObject);
    var
    e: TGEOSDoIconsElement;

    begin
    if  not FChanging then
        begin
        e:= FSelectedElem as TGEOSDoIconsElement;
        e.IconsYPos[FSelectedItem]:= SpEdtDoIconsItmYPos.Value;
        end;
    end;

procedure TGEOSDesignerMainFrame.SpEdtDoIconsXPosChange(Sender: TObject);
    var
    e: TGEOSDoIconsElement;

    begin
    if  not FChanging then
        begin
        e:= FSelectedElem as TGEOSDoIconsElement;
        e.XPos:= SpEdtDoIconsXPos.Value;
        end;
    end;

procedure TGEOSDesignerMainFrame.SpEdtDoIconsYPosChange(Sender: TObject);
    var
    e: TGEOSDoIconsElement;

    begin
    if  not FChanging then
        begin
        e:= FSelectedElem as TGEOSDoIconsElement;
        e.YPos:= SpEdtDoIconsYPos.Value;
        end;
    end;

procedure TGEOSDesignerMainFrame.TlBtnGrphAddClick(Sender: TObject);
    var
    e: TGEOSGraphicsStrElement;
    itm: TGEOSGraphicsInstr;

    begin
    e:= FSelectedElem as TGEOSGraphicsStrElement;

    if  GEOSDesignerAddGPStrInstrForm.ShowAdd(e.Mode, itm) = mrOk then
        begin
        e.AddItem(itm.InstrType, itm.InstrCmd, itm.InstrData, itm.DoubleW,
                itm.Add1W);
        DoInitGrphStrElemView;
        GEOSDesignerMainDMod.Changed;
        end;
    end;

procedure TGEOSDesignerMainFrame.TlBtnGrphDeleteClick(Sender: TObject);
    var
    e: TGEOSGraphicsStrElement;

    begin
    e:= FSelectedElem as TGEOSGraphicsStrElement;

    e.DeleteItem(LstVwGrphStrItems.Items.Count - 1);
    LstVwGrphStrItems.Items.Delete(LstVwGrphStrItems.Items.Count - 1);
    end;

procedure TGEOSDesignerMainFrame.TlBtnGrphEditClick(Sender: TObject);
    var
    e: TGEOSGraphicsStrElement;
    i: Integer;

    begin
    e:= FSelectedElem as TGEOSGraphicsStrElement;
    i:= LstVwGrphStrItems.Selected.Index;

    if  GEOSDesignerAddGPStrInstrForm.ShowEdit(e[i]) = mrOk then
        begin
        DoInitGrphStrElemView;
        GEOSDesignerMainDMod.Changed;
        end;
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

//dengland Setting the element's active property will trigger this if necessary.
//  if  Assigned(GEOSDesignerOnChange) then
//      GEOSDesignerOnChange;

//dengland  OnClick will be called after this so use that to handle selection
//      changes.
    end;

procedure TGEOSDesignerMainFrame.ChkLstBxElementsClick(Sender: TObject);
    begin
    DoFindActiveElement;
    end;

procedure TGEOSDesignerMainFrame.ChkBxDoIconsShowMouseChange(Sender: TObject);
    var
    e: TGEOSDoIconsElement;

    begin
    if  not FChanging then
        begin
        e:= FSelectedElem as TGEOSDoIconsElement;
        e.ShowMouse:= ChkBxDoIconsShowMouse.Checked;
        end;
    end;

procedure TGEOSDesignerMainFrame.ChkBxDoIconsItmDblWChange(Sender: TObject);
    var
    e: TGEOSDoIconsElement;

    begin
    if  not FChanging then
        begin
        e:= FSelectedElem as TGEOSDoIconsElement;
        e.IconsDblBWidth[FSelectedItem]:= ChkBxDoIconsItmDblW.Checked;
        end;
    end;

procedure TGEOSDesignerMainFrame.ChkBxDoIconsItmDblBChange(Sender: TObject);
    var
    e: TGEOSDoIconsElement;

    begin
    if  not FChanging then
        begin
        e:= FSelectedElem as TGEOSDoIconsElement;
        e.IconsDblBX[FSelectedItem]:= ChkBxDoIconsItmDblB.Checked;
        end;
    end;

procedure TGEOSDesignerMainFrame.ChkBxDoIconsDblWChange(Sender: TObject);
    var
    e: TGEOSDoIconsElement;

    begin
    if  not FChanging then
        begin
        e:= FSelectedElem as TGEOSDoIconsElement;
        e.DoubleW:= ChkBxDoIconsDblW.Checked;
        end;
    end;

procedure TGEOSDesignerMainFrame.ChkBxDoIconsAdd1WChange(Sender: TObject);
    var
    e: TGEOSDoIconsElement;

    begin
    if  not FChanging then
        begin
        e:= FSelectedElem as TGEOSDoIconsElement;
        e.Add1W:= ChkBxDoIconsAdd1W.Checked;
        end;
    end;

procedure TGEOSDesignerMainFrame.ClearDisplay;
    begin
    ChkLstBxElements.Items.Clear;
    FSelectedElem:= nil;
    end;

procedure TGEOSDesignerMainFrame.DoDoIconsItemSelect(const AItem: Integer);
    var
    e: TGEOSDoIconsElement;

    begin
    e:= FSelectedElem as TGEOSDoIconsElement;

    FSelectedItem:= AItem;

    FChanging:= True;
    try
        LblDoIconItmIdent.Caption:= e.IconsIdent[AItem];
        CmbDoIconsIcon.ItemIndex:= CmbDoIconsIcon.Items.IndexOf(e[AItem].Indentifier);
        SpEdtDoIconsItmXPos.Value:= e.IconsXPos[AItem];
        SpEdtDoIconsItmYPos.Value:= e.IconsYPos[AItem];
        ChkBxDoIconsItmDblW.Checked:= e.IconsDblBWidth[AItem];
        ChkBxDoIconsItmDblB.Checked:= e.IconsDblBX[AItem];

        finally
        FChanging:= False;
        end;
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

            if  inst^.DoubleW then
                s:= s + '(DBLW) ';

            if  inst^.Add1W then
                s:= s + '(ADD1W) ';

            litm.SubItems.Add(s);
            end;
        finally
        LstVwGrphStrItems.Items.EndUpdate;
        end;

    if  LstVwGrphStrItems.Items.Count > 0 then
        LstVwGrphStrItems.Selected:= LstVwGrphStrItems.Items[0]
    else
        begin
        TlBtnGrphEdit.Enabled:= False;
        TlBtnGrphDelete.Enabled:= False;
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
    var
    e: TGEOSDoIconsElement;
    i: Integer;

    begin
    e:= FSelectedElem as TGEOSDoIconsElement;

    SpEdtDoIconsXPos.MaxValue:= ARR_REC_GEOSDISPLAYRES[GEOSDispMode].Width - 1;
    SpEdtDoIconsYPos.MaxValue:= ARR_REC_GEOSDISPLAYRES[GEOSDispMode].Height - 1;
    ChkBxDoIconsDblW.Enabled:= GEOSDispMode = gdm80Column;
    ChkBxDoIconsAdd1W.Enabled:= GEOSDispMode = gdm80Column;
    ChkBxDoIconsItmDblW.Enabled:= GEOSDispMode = gdm80Column;
    ChkBxDoIconsItmDblB.Enabled:= GEOSDispMode = gdm80Column;
    SpEdtDoIconsItmXPos.MaxValue:=
            ARR_REC_GEOSDISPLAYRES[GEOSDispMode].Width div 8 - 1;
    SpEdtDoIconsItmYPos.MaxValue:=
            ARR_REC_GEOSDISPLAYRES[GEOSDispMode].Height - 1;

    FChanging:= True;
    try
        LblDoIconsIdent.Caption:= e.Identifier;
        LstBxDoIcons.Items.BeginUpdate;
        try
            LstBxDoIcons.Clear;

            for i:= 0 to e.Count - 1 do
                LstBxDoIcons.Items.Add(e.IconsIdent[i]);

            finally
            LstBxDoIcons.Items.EndUpdate;
            end;

        SpEdtDoIconsXPos.Value:= e.XPos;
        SpEdtDoIconsYPos.Value:= e.YPos;

        ChkBxDoIconsDblW.Checked:= e.DoubleW;
        ChkBxDoIconsAdd1W.Checked:= e.Add1W;

        ChkBxDoIconsShowMouse.Checked:= e.ShowMouse;

        CmbDoIconsIcon.Items.BeginUpdate;
        try
            CmbDoIconsIcon.Clear;

            for i:= 0 to GEOSDesignerMainDMod.IconsCount - 1 do
                CmbDoIconsIcon.Items.Add(GEOSDesignerMainDMod.Icons[i].Indentifier);

            finally
            CmbDoIconsIcon.Items.EndUpdate;
            end;

        CmbDoIconsIcon.ItemIndex:= -1;
        ChkBxDoIconsItmDblW.Checked:= False;
        SpEdtDoIconsItmXPos.Value:= 0;
        ChkBxDoIconsItmDblB.Checked:= False;
        SpEdtDoIconsItmYPos.Value:= 0;

        finally
        FChanging:= False;
        end;

    if  LstBxDoIcons.Items.Count > 0 then
        begin
        LstBxDoIcons.Selected[0]:= True;
        DoDoIconsItemSelect(0);
        end;
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

            ChkLstBxElements.Items.Add(e.Identifier + ' (' + e.ElementName + ')');
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

procedure TGEOSDesignerMainFrame.UpdateElements;
    var
    i: Integer;

    begin
    for i:= 0 to GEOSDesignerMainDMod.ElementsCount - 1 do
        ChkLstBxElements.Checked[i]:= GEOSDesignerMainDMod.Elements[i].Active;
    end;

procedure TGEOSDesignerMainFrame.RedisplayActiveElement;
    begin
    FSelectedElem:= nil;
    if  ChkLstBxElements.Items.Count > 0 then
        DoFindActiveElement
    else
        NtBkDetails.PageIndex:= -1;
    end;

end.

