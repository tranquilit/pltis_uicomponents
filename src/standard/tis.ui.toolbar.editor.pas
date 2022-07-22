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
    fPopupReferences: TStringList;
    fDesigner: TComponentEditorDesigner;
    function IsDesignTime: Boolean;
    procedure SetTarget(aValue: TTisToolBar);
    procedure AddCommand;
    procedure AddDivider;
    procedure AddToolBarItem(aButton: TToolButton);
    procedure InsertItem(aItem: TListItem);
    procedure MoveUpDown(aOffset: integer);
    function NewLvItem(const aCaption: string): TListItem;
    procedure RemoveCommand;
    procedure SetupCaptions;
    procedure LoadCategories;
    procedure LoadButtons;
    procedure AddMenuItem(aParentNode: TTreeNode; aAction: TAction);
  private
    const DIVIDER_CAPTION = '---------------';
  protected
    property Designer: TComponentEditorDesigner read fDesigner write fDesigner;
  public
    constructor Create(aOwner: TComponent); override;
    destructor Destroy; override;
    property Target: TTisToolBar read fTarget write SetTarget;
    property PopupReferences: TStringList read fPopupReferences write fPopupReferences;
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
  i, x: Integer;
  vButtonR: record
    Style: TToolButtonStyle;
    Sibling: TToolButton;
  end;
  vButton: TToolButton;
  vListItem: TListItem;
  vAction: TAction;
begin
  if ModalResult = mrOK then
  begin
    if IsDesignTime then
    begin
      for i := fTarget.ButtonCount-1 downto 0 do
      begin
        vButton := fTarget.Buttons[i];
        fDesigner.PropertyEditorHook.DeletePersistent(TPersistent(vButton));
        fDesigner.Modified;
      end;
    end
    else
      fTarget.RemoveButtons;
    for x := 0 to lvToolbar.Items.Count -1 do
    begin
      vListItem := lvToolbar.Items[x];
      if vListItem.Caption = DIVIDER_CAPTION then
        vButtonR.Style := tbsDivider
      else
        vButtonR.Style := tbsButton;
      if Assigned(vListItem.Data) then
        vAction := TAction(vListItem.Data)
      else
        vAction := nil;
      if IsDesignTime then
      begin
        vButton := TToolButton.Create(fTarget.Owner);
        vButton.Name := fDesigner.CreateUniqueComponentName(vButton.ClassName);
        vButton.Caption := vButton.Caption;
        vButton.Style := vButtonR.Style;
        // position the button next to the last one
        if fTarget.ButtonCount > 0 then
        begin
          vButtonR.Sibling := fTarget.Buttons[fTarget.ButtonCount - 1];
          vButton.SetBounds(vButtonR.Sibling.Left + vButtonR.Sibling.Width,
            vButtonR.Sibling.Top, vButton.Width, vButton.Height);
        end;
        vButton.Parent := fTarget;
        vButton.Action := vAction;
        fDesigner.PropertyEditorHook.PersistentAdded(vButton, True);
        fDesigner.Modified;
      end
      else
        vButton := fTarget.AddButton(vListItem.Caption, vButtonR.Style, vAction);
      i := fPopupReferences.IndexOf(vButton.Caption);
      if i > -1 then
      begin
        vButton.Style := tbsButtonDrop;
        vButton.DropdownMenu := fPopupReferences.Objects[i] as TPopupMenu;
      end;
    end;
  end;
end;

procedure TTisToolBarEditor.ToolBarRestoreButtonClick(Sender: TObject);
begin
  fTarget.RestoreSession;
  LoadButtons;
end;

procedure TTisToolBarEditor.InsertItem(aItem: TListItem);
begin
  lvToolbar.ItemIndex := -1;
  lvToolbar.Selected := nil;
  if aItem.Index < lvToolbar.Items.Count-1 then
    lvToolbar.ItemIndex := aItem.Index+1
  else
    lvToolbar.ItemIndex := aItem.Index;
end;

procedure TTisToolBarEditor.btnAddClick(Sender: TObject);
begin
  AddCommand;
end;

function TTisToolBarEditor.NewLvItem(const aCaption: string): TListItem;
var
  i: Integer;
begin
  i := lvToolbar.ItemIndex;
  if i = -1 then
    i := lvToolbar.Items.Count-1;    // Add before the last empty item.
  if i = -1 then
    Result := lvToolbar.Items.Add
  else
    Result := lvToolbar.Items.Insert(i);
  Result.Caption := aCaption;
end;

procedure TTisToolBarEditor.AddCommand;
var
  vNode: TTreeNode;
  vCaption: string;
  vListItem: TListItem;
begin
  vNode := TV.Selected;
  if (vNode = nil) or (vNode.Data = nil) then
    exit;
  vCaption := TAction(vNode.Data).Caption;
  DeleteAmpersands(vCaption);
  vListItem := NewLvItem(vCaption);
  vListItem.Data := vNode.Data;
  vListItem.ImageIndex := vNode.ImageIndex;
  InsertItem(vListItem);
  vNode := TV.Selected.GetNext;
  TV.Selected.Visible := False;
  if vNode <> nil then
    TV.Selected := vNode;
  UpdateButtonsState;
end;

procedure TTisToolBarEditor.RemoveCommand;
var
  i: Integer;
  vAction: TAction;
  vNode: TTreeNode;
begin
  i := lvToolbar.ItemIndex;
  if (i < 0)  then
    exit;
  vAction := TAction(lvToolbar.Items[i].Data);
  lvToolbar.Items.Delete(i);
  {$IF DEFINED(LCLQt) or DEFINED(LCLQt5)}
  lvToolbar.ItemIndex := -1;     // Try to make LCLQt behave.
  lvToolbar.ItemIndex := i;
  {$ENDIF}
  lvToolbar.Selected := lvToolbar.Items[i];
  // Show the command as available again in TreeView.
  if Assigned(vAction) then
  begin
    vNode := TV.Items.FindNodeWithData(vAction);
    if vNode <> nil then
      vNode.Visible := True;
  end;
  UpdateButtonsState;
end;

procedure TTisToolBarEditor.btnAddDividerClick(Sender: TObject);
var
  vListItem: TListItem;
begin
  vListItem := NewLvItem(DIVIDER_CAPTION);
  vListItem.ImageIndex := -1;
  InsertItem(vListItem);
  UpdateButtonsState;
end;

procedure TTisToolBarEditor.btnRemoveClick(Sender: TObject);
begin
  RemoveCommand;
end;

procedure TTisToolBarEditor.lvToolbarSelectItem(Sender: TObject;
  Item: TListItem; Selected: Boolean);
var
  vRealCount: integer;
begin
  UpdateButtonsState;
  // Update selection status label.
  vRealCount := lvToolbar.Items.Count-1;
  if lvToolbar.ItemIndex < vRealCount then
    lblSelect.Caption := Format('%d / %d', [lvToolbar.ItemIndex+1, vRealCount])
  else
    lblSelect.Caption := Format('%d+ / %d', [lvToolbar.ItemIndex, vRealCount])
end;

procedure TTisToolBarEditor.MoveUpDown(aOffset: integer);
var
  vIndex1, vIndex2: Integer;
begin
  vIndex1 := lvToolbar.ItemIndex;
   vIndex2 := vIndex1 + aOffset;
  lvToolbar.Items.Exchange(vIndex1, vIndex2);
  lvToolbar.Items[vIndex1].Selected := False;
  lvToolbar.Items[ vIndex2].Selected := False;
  lvToolbar.ItemIndex:= -1;
  lvToolbar.Selected := nil;
  lvToolbar.ItemIndex:=  vIndex2;
  lvToolbar.Selected := lvToolbar.Items[ vIndex2];
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
  vNode: TTreeNode;
  vCategory: string;
  vActions: TActionList;
  vAction: TAction;
begin
  TV.Items.BeginUpdate;
  try
    TV.Items.Clear;
    for i := 0 to Target.Actions.Count -1 do
    begin
      vActions := Target.Actions.Items[i].List;
      for x := 0 to vActions.ActionCount -1 do
      begin
        vAction := vActions.Actions[x] as TAction;
        vCategory := vAction.Category;
        DeleteAmpersands(vCategory);
        if Target.Actions.Items[i].HiddenCategories.IndexOf(vCategory) = -1 then
        begin
          vNode := TV.Items.FindNodeWithText(vCategory);
          if vNode = nil then
            vNode := TV.Items.AddChild(nil, vCategory);
          AddMenuItem(vNode, vAction);
        end;
      end;
    end;
  finally
    TV.Items.EndUpdate;
  end;
end;

procedure TTisToolBarEditor.LoadButtons;
var
  i: Integer;
  vButton: TToolButton;
begin
  if lvToolbar.Items.Count > 0 then
  begin
    lvToolbar.ItemIndex := 0; // point to the first for remove all items
    for i := 0 to lvToolbar.Items.Count -1 do
      RemoveCommand;
  end;
  for i := 0 to fTarget.ButtonCount -1 do
  begin
    vButton := fTarget.Buttons[i];
    case vButton.Style of
      tbsDivider, tbsSeparator:
        AddDivider;
    else
      AddToolBarItem(vButton);
    end;
  end;
end;

procedure TTisToolBarEditor.AddMenuItem(aParentNode: TTreeNode; aAction: TAction);
var
  vNode: TTreeNode;
begin
  if aAction.Caption = '-' then
    exit; // divider
  vNode := TV.Items.AddChild(aParentNode, Format('%s', [aAction.Caption]));
  vNode.ImageIndex := aAction.ImageIndex;
  vNode.SelectedIndex := aAction.ImageIndex;
  vNode.Data := aAction;
end;

constructor TTisToolBarEditor.Create(aOwner: TComponent);
begin
  inherited Create(aOwner);
end;

destructor TTisToolBarEditor.Destroy;
begin
  inherited Destroy;
end;

function TTisToolBarEditor.IsDesignTime: Boolean;
begin
  result := Assigned(fDesigner) and Assigned(fDesigner.PropertyEditorHook);
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

procedure TTisToolBarEditor.AddToolBarItem(aButton: TToolButton);
var
  vNode: TTreeNode;
  vListItem: TListItem;
  vAction: TAction;
begin
  vListItem := lvToolbar.Items.Add;
  vListItem.Caption := aButton.Caption;
  vAction := aButton.Action as TAction;
  vListItem.Data := vAction;
  if Assigned(vAction) then
    vListItem.ImageIndex := vAction.ImageIndex
  else
    vListItem.ImageIndex := aButton.ImageIndex;
  vNode := TV.Items.FindNodeWithData(vAction);
  if vNode <> nil then
    vNode.Visible := False;
end;

procedure TTisToolBarEditor.AddDivider;
var
  vListItem: TListItem;
begin
  vListItem := lvToolbar.Items.Add;
  vListItem.Caption := DIVIDER_CAPTION;
  vListItem.ImageIndex := -1;
end;

{ TTisToolBarComponentEditor }

procedure TTisToolBarComponentEditor.DoShowEditor;
begin
  with TTisToolBarEditor.Create(Application) do
  try
    Target := (Component as TTisToolBar);
    Designer := GetDesigner;
    ShowModal;
  finally
    Free;
  end;
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

