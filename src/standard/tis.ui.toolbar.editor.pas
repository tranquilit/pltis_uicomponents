// -----------------------------------------------------------------
//    This file is part of Tranquil IT Software
//    Copyright (C) 2012 - 2022  Tranquil IT https://www.tranquil.it
//    All Rights Reserved.
// ------------------------------------------------------------------
unit tis.ui.toolbar.editor;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,
  // LCL
  LCLProc, LclIntf, Controls, Forms, Graphics, ExtCtrls, Buttons, StdCtrls,
  ComCtrls, Menus, ButtonPanel, ActnList, Dialogs,
  ComponentEditors,
  // LazControls
  TreeFilterEdit,
  // IdeIntf
  ToolBarIntf,
  // TIS
  tis.ui.toolbar.core;

type
  TTisToolBarEditor = class(TForm)
    btnAdd: TSpeedButton;
    btnAddDivider: TSpeedButton;
    btnCancel: TButton;
    btnHelp: TBitBtn;
    btnMoveDown: TSpeedButton;
    btnMoveUp: TSpeedButton;
    btnOK: TButton;
    btnRemove: TSpeedButton;
    FilterEdit: TTreeFilterEdit;
    lblMenuTree: TLabel;
    lblToolbar: TLabel;
    lblSelect: TLabel;
    lvToolbar: TListView;
    miAll: TMenuItem;
    miCustom: TMenuItem;
    miDebug: TMenuItem;
    miDesign: TMenuItem;
    miHTML: TMenuItem;
    pnlButtons: TButtonPanel;
    Splitter1: TSplitter;
    TV: TTreeView;
    ToolBarRestoreButton: TSpeedButton;
    procedure FormCreate(Sender: TObject);
    procedure lvToolbarDblClick(Sender: TObject);
    procedure lvToolbarEnterExit(Sender: TObject);
    procedure TVDblClick(Sender: TObject);
    procedure UpdateButtonsState;
    procedure btnAddClick(Sender: TObject);
    procedure btnAddDividerClick(Sender: TObject);
    procedure btnMoveDownClick(Sender: TObject);
    procedure btnMoveUpClick(Sender: TObject);
    procedure btnRemoveClick(Sender: TObject);
    procedure lvToolbarSelectItem(Sender: TObject; {%H-}Item: TListItem;
      {%H-}Selected: Boolean);
    procedure TVSelectionChanged(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var {%H-}CanClose: boolean);
    procedure ToolBarRestoreButtonClick(Sender: TObject);
  private
    fTarget: TTisToolBar;
    procedure SetTarget(aValue: TTisToolBar);
    procedure AddCommand;
    procedure AddDivider;
    procedure AddToolBarItem(aAction: TAction);
    procedure InsertItem(Item: TListItem);
    procedure MoveUpDown(aOffset: integer);
    function NewLvItem(const aCaption: string): TListItem;
    procedure RemoveCommand;
    procedure SetupCaptions;
    procedure LoadCategories;
    procedure LoadButtons;
    procedure AddMenuItem(aParentNode: TTreeNode; aAction: TAction);
  public
    constructor Create(aOwner: TComponent); override;
    property Target: TTisToolBar read fTarget write SetTarget;
  end;

  /// ToolBar component editor
  TTisToolBarComponentEditor = class(TToolBarComponentEditor)
  protected
    procedure DoShowEditor;
  public
    procedure ExecuteVerb(Index: Integer); override;
    function GetVerb(Index: Integer): string; override;
    function GetVerbCount: Integer; override;
  end;

resourcestring
  rsToolBarConfiguration = 'Toolbar Configuration';
  rsToolBarAvailableCommands = 'Available commands';
  rsToolBarCommands = 'Toolbar commands';
  rsToolBarRestore = 'Restore';

implementation

{$R *.lfm}

const
  DIVIDER_CAPTION = '---------------';

{ TTisToolBarEditor }

procedure TTisToolBarEditor.FormCreate(Sender: TObject);
begin
  inherited;
  pnlButtons.Color := clBtnFace;
  lblSelect.Caption := '';
  btnAddDivider.Caption := '---';
end;

procedure TTisToolBarEditor.lvToolbarDblClick(Sender: TObject);
begin
  RemoveCommand;
end;

procedure TTisToolBarEditor.lvToolbarEnterExit(Sender: TObject);
begin
  UpdateButtonsState;
end;

procedure TTisToolBarEditor.TVDblClick(Sender: TObject);
begin
  AddCommand;
end;

procedure TTisToolBarEditor.UpdateButtonsState;
var
  i: Integer;
begin
  i := lvToolbar.ItemIndex;
  btnAdd.Enabled := Assigned(TV.Selected) and Assigned(TV.Selected.Data);
  btnRemove.Enabled := (i > -1) and (i <= lvToolbar.Items.Count -1);
  btnMoveUp.Enabled := (i > 0) and (i <= lvToolbar.Items.Count -1);
  btnMoveDown.Enabled := (i > -1) and (i < lvToolbar.Items.Count -1);
  btnAddDivider.Enabled := True;
end;

procedure TTisToolBarEditor.TVSelectionChanged(Sender: TObject);
begin
  UpdateButtonsState;
end;

procedure TTisToolBarEditor.FormCloseQuery(Sender: TObject;
  var CanClose: boolean);
var
  style: TToolButtonStyle;
  li: TListItem;
  i: Integer;
begin
  if ModalResult = mrOK then
  begin
    fTarget.RemoveButtons;
    for i := 0 to lvToolbar.Items.Count -1 do
    begin
      li := lvToolbar.Items[i];
      if li.Caption = DIVIDER_CAPTION then
        style := tbsDivider
      else
        style := tbsButton;
      fTarget.AddButton(style, TAction(li.Data));
    end;
  end;
end;

procedure TTisToolBarEditor.ToolBarRestoreButtonClick(Sender: TObject);
begin
  fTarget.RestoreSession;
  LoadButtons;
end;

procedure TTisToolBarEditor.InsertItem(Item: TListItem);
begin
  lvToolbar.ItemIndex := -1;
  lvToolbar.Selected := nil;
  if Item.Index < lvToolbar.Items.Count-1 then
    lvToolbar.ItemIndex := Item.Index+1
  else
    lvToolbar.ItemIndex := Item.Index;
end;

procedure TTisToolBarEditor.btnAddClick(Sender: TObject);
begin
  AddCommand;
end;

function TTisToolBarEditor.NewLvItem(const aCaption: string): TListItem;
var
  I: Integer;
begin
  I := lvToolbar.ItemIndex;
  if I = -1 then
    I := lvToolbar.Items.Count-1;    // Add before the last empty item.
  if I = -1 then
    Result := lvToolbar.Items.Add
  else
    Result := lvToolbar.Items.Insert(I);
  Result.Caption := aCaption;
end;

procedure TTisToolBarEditor.AddCommand;
var
  n: TTreeNode;
  s: string;
  li: TListItem;
begin
  n := TV.Selected;
  if (n = nil) or (n.Data = nil) then
    exit;
  s := TAction(n.Data).Caption;
  DeleteAmpersands(s);
  li := NewLvItem(s);
  li.Data := n.Data;
  li.ImageIndex := n.ImageIndex;
  InsertItem(li);                  // Add the newly created item to ListView.
  // Update selection in TreeView.
  n := TV.Selected.GetNext;
  TV.Selected.Visible := False;
  if n <> nil then
    TV.Selected := n;
  UpdateButtonsState;
end;

procedure TTisToolBarEditor.RemoveCommand;
Var
  a: TAction;
  n: TTreeNode;
  i: Integer;
begin
  i := lvToolbar.ItemIndex;
  if (i < 0)  then
    exit;
  a := TAction(lvToolbar.Items[i].Data);
  lvToolbar.Items.Delete(i);
  {$IF DEFINED(LCLQt) or DEFINED(LCLQt5)}
  lvToolbar.ItemIndex := -1;     // Try to make LCLQt behave.
  lvToolbar.ItemIndex := i;
  {$ENDIF}
  lvToolbar.Selected := lvToolbar.Items[i];
  // Show the command as available again in TreeView.
  if Assigned(a) then
  begin
    n := TV.Items.FindNodeWithData(a);
    if n <> nil then
      n.Visible := True;
  end;
  UpdateButtonsState;
end;

procedure TTisToolBarEditor.btnAddDividerClick(Sender: TObject);
var
  li: TListItem;
begin
  li := NewLvItem(DIVIDER_CAPTION);
  li.ImageIndex := -1;
  InsertItem(li);
  UpdateButtonsState;
end;

procedure TTisToolBarEditor.btnRemoveClick(Sender: TObject);
begin
  RemoveCommand;
end;

procedure TTisToolBarEditor.lvToolbarSelectItem(Sender: TObject;
  Item: TListItem; Selected: Boolean);
var
  RealCount: integer;
begin
  UpdateButtonsState;
  // Update selection status label.
  RealCount := lvToolbar.Items.Count-1;
  if lvToolbar.ItemIndex < RealCount then
    lblSelect.Caption := Format('%d / %d', [lvToolbar.ItemIndex+1, RealCount])
  else
    lblSelect.Caption := Format('%d+ / %d', [lvToolbar.ItemIndex, RealCount])
end;

procedure TTisToolBarEditor.MoveUpDown(aOffset: integer);
var
  Index1,Index2: Integer;
begin
  Index1 := lvToolbar.ItemIndex;
  Index2 := Index1 + aOffset;
  lvToolbar.Items.Exchange(Index1,Index2);
  lvToolbar.Items[Index1].Selected := False;
  lvToolbar.Items[Index2].Selected := False;
  lvToolbar.ItemIndex:= -1;
  lvToolbar.Selected := nil;
  lvToolbar.ItemIndex:= Index2;
  lvToolbar.Selected := lvToolbar.Items[Index2];
end;

procedure TTisToolBarEditor.btnMoveDownClick(Sender: TObject);
begin
  if (lvToolbar.ItemIndex < 0) or (lvToolbar.ItemIndex = lvToolbar.Items.Count-1) then
    exit;
  MoveUpDown(1);
end;

procedure TTisToolBarEditor.btnMoveUpClick(Sender: TObject);
begin
  if (lvToolbar.ItemIndex <= 0) then
    exit;
  MoveUpDown(-1);
end;

procedure TTisToolBarEditor.SetupCaptions;
begin
  Caption := rsToolBarConfiguration;
  lblMenuTree.Caption := rsToolBarAvailableCommands;
  lblToolbar.Caption := rsToolBarCommands;
  ToolBarRestoreButton.Caption := rsToolBarRestore;
end;

procedure TTisToolBarEditor.LoadCategories;
var
  i, x, y: Integer;
  n: TTreeNode;
  categories: TStringList;
  tmp: record
    list: TActionList;
    action: TAction;
    category: record
      name: string;
      hiddens: TStrings;
    end;
  end;
begin
  TV.Items.BeginUpdate;
  categories := TStringList.Create;
  try
    for i := 0 to Target.Actions.Count -1 do
    begin
      tmp.list := Target.Actions.Items[i].List;
      tmp.category.hiddens := Target.Actions.Items[i].HiddenCategories;
      for x := 0 to tmp.list.ActionCount -1 do
      begin
        tmp.action := tmp.list.Actions[x] as TAction;
        if (categories.IndexOf(tmp.action.Category) = -1) and
          (tmp.category.hiddens.IndexOf(tmp.action.Category) = -1) then
          categories.Append(tmp.action.Category);
      end;
    end;
    TV.Items.Clear;
    for x := 0 to categories.Count-1 do
    begin
      tmp.category.name := categories[x];
      DeleteAmpersands(tmp.category.name);
      n := TV.Items.AddChild(nil, tmp.category.name);
      for i := 0 to Target.Actions.Count -1 do
      begin
        tmp.list := Target.Actions.Items[i].List;
        for y := 0 to tmp.list.ActionCount -1 do
        begin
          tmp.action := tmp.list.Actions[y] as TAction;
          if tmp.action.Category = tmp.category.name then
            AddMenuItem(n, tmp.action);
        end;
      end;
    end;
  finally
    categories.Free;
    TV.Items.EndUpdate;
  end;
end;

procedure TTisToolBarEditor.LoadButtons;
var
  i: Integer;
  b: TToolButton;
begin
  if lvToolbar.Items.Count > 0 then
  begin
    lvToolbar.ItemIndex := 0; // point to the first for remove all items
    for i := 0 to lvToolbar.Items.Count -1 do
      RemoveCommand;
  end;
  for i := 0 to fTarget.ButtonCount -1 do
  begin
    b := fTarget.Buttons[i];
    if b.Style = tbsDivider then
      AddDivider
    else
      AddToolBarItem(b.Action as TAction);
  end;
end;

procedure TTisToolBarEditor.AddMenuItem(aParentNode: TTreeNode; aAction: TAction);
var
  n: TTreeNode;
begin
  if aAction.Caption = '-' then
    exit; // divider
  n := TV.Items.AddChild(aParentNode, Format('%s', [aAction.Caption]));
  n.ImageIndex := aAction.ImageIndex;
  n.SelectedIndex := aAction.ImageIndex;
  n.Data := aAction;
end;

constructor TTisToolBarEditor.Create(aOwner: TComponent);
begin
  inherited Create(aOwner);
end;

procedure TTisToolBarEditor.SetTarget(aValue: TTisToolBar);
begin
  if fTarget = aValue then
    exit;
  fTarget := aValue;
  TV.Images := fTarget.Images;
  lvToolbar.SmallImages := fTarget.Images;
  SetupCaptions;
  LoadCategories;
  LoadButtons;
end;

procedure TTisToolBarEditor.AddToolBarItem(aAction: TAction);
var
  n: TTreeNode;
  li: TListItem;
begin
  if aAction = nil then
    exit;
  li := lvToolbar.Items.Add;
  li.Caption := aAction.Caption;
  li.Data := aAction;
  li.ImageIndex := aAction.ImageIndex;
  n := TV.Items.FindNodeWithData(aAction);
  if n <> nil then
    n.Visible := False;
end;

procedure TTisToolBarEditor.AddDivider;
var
  li: TListItem;
begin
  li := lvToolbar.Items.Add;
  li.Caption := DIVIDER_CAPTION;
  li.ImageIndex := -1;
end;

{ TTisToolBarComponentEditor }

procedure TTisToolBarComponentEditor.DoShowEditor;
begin
  (Component as TTisToolBar).ShowEditor;
end;

procedure TTisToolBarComponentEditor.ExecuteVerb(Index: Integer);
begin
  case Index - (inherited GetVerbCount) of
    0: DoShowEditor;
    else
      inherited ExecuteVerb(Index);
  end;
end;

function TTisToolBarComponentEditor.GetVerb(Index: Integer): string;
begin
  case Index - (inherited GetVerbCount) of
    0: result := 'Show Editor...';
  else
    result := inherited GetVerb(Index);
  end;
end;

function TTisToolBarComponentEditor.GetVerbCount: Integer;
begin
  result := inherited GetVerbCount + 1;
end;

end.

