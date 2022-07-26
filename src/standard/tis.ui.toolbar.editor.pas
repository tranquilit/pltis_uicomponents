// -----------------------------------------------------------------
//    This file is part of Tranquil IT Software
//    Copyright (C) 2012 - 2022  Tranquil IT https://www.tranquil.it
//    All Rights Reserved.
// ------------------------------------------------------------------
unit tis.ui.toolbar.editor;

{$mode objfpc}{$H+}

interface

uses
  Classes,
  SysUtils,
  LCLProc,
  LclIntf,
  Controls,
  Forms,
  Graphics,
  ExtCtrls,
  Buttons,
  StdCtrls,
  ComCtrls,
  Menus,
  ButtonPanel,
  ActnList,
  Dialogs,
  ComponentEditors,
  TreeFilterEdit,
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
    procedure FormShow(Sender: TObject);
    procedure lvToolbarCompare(Sender: TObject; Item1, Item2: TListItem;
      Data: Integer; var Compare: Integer);
  private
    fTarget: TTisToolBar;
    fDesigner: TComponentEditorDesigner;
    function IsDesignTime: Boolean;
    procedure SetTarget(aValue: TTisToolBar);
    procedure AddCommand;
    procedure AddListViewItem(aButton: TToolButton);
    procedure InsertItem(aItem: TListItem);
    procedure MoveUpDown(aOffset: integer);
    function NewListViewItem(const aCaption: string): TListItem;
    procedure CheckButtonDesigntime(aButton: TToolButton);
    function NewButtonBy(aNode: TTreeNode): TToolButton;
    function NewButtonDivider: TToolButton;
    procedure RemoveCommand;
    procedure SetupCaptions;
    procedure LoadActionsAndPopups;
    procedure LoadButtons;
    function FindTreeViewNodeBy(aAction: TAction): TTreeNode; overload;
    function FindTreeViewNodeBy(aPopupItem: TTisPopupMenusItem): TTreeNode; overload;
  private
    const DIVIDER_LISTVIEW_CAPTION = '---------------';
    const DIVIDER_BUTTON_CAPTION = '-';
  protected
    property Designer: TComponentEditorDesigner read fDesigner write fDesigner;
  public
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

const
  rsPopupMenus = 'Popup Menus';

implementation

{$R *.lfm}

type
  /// shared data between TreeView and ListView
  TSharedData = class(TComponent)
  private
    fEditor: TTisToolBarEditor;
  public
    Action: TAction;
    Button: TToolButton;
    Popup: TTisPopupMenusItem;
    constructor Create(aEditor: TTisToolBarEditor); reintroduce;
  end;

{ TSharedData }

constructor TSharedData.Create(aEditor: TTisToolBarEditor);
begin
  inherited Create(aEditor);
  fEditor := aEditor;
end;

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
  vButton, vSibling: TToolButton;
  vListItem: TListItem;
  vFound: Boolean;
begin
  if ModalResult = mrOK then
  begin
    fTarget.BeginUpdate;
    try
      // cleaning the buttons that was removed by user
      for i := fTarget.ButtonCount-1 downto 0 do
      begin
        vFound := False;
        vButton := fTarget.Buttons[i];
        vButton.Parent := nil; // temporarily removed from the target to change its position
        for x := 0 to lvToolbar.Items.Count -1 do
        begin
          vListItem := lvToolbar.Items[x];
          if vButton = TSharedData(vListItem.Data).Button then
          begin
            vFound := True;
            Break;
          end;
        end;
        if not vFound then
        begin
          if IsDesignTime then
          begin
            fDesigner.PropertyEditorHook.DeletePersistent(TPersistent(vButton));
            fDesigner.Modified;
          end
          else
            fTarget.RemoveButton(vButton);
        end;
      end;
      // reorganizing buttons on toolbar
      for x := 0 to lvToolbar.Items.Count -1 do
      begin
        vListItem := lvToolbar.Items[x];
        vButton := TSharedData(vListItem.Data).Button;
        if x = 0 then
          vButton.Left := 1
        else
        begin
          vSibling := fTarget.Buttons[x-1];
          vButton.SetBounds(
            vSibling.Left + vSibling.Width,
            vSibling.Top, vButton.Width, vButton.Height);
        end;
        vButton.AutoSize := True; // should be True, as a new Action caption can be long
        vButton.Parent := fTarget; // show it in the target toolbar, if it is new
      end;
    finally
      fTarget.EndUpdate;
      fTarget.Invalidate;
      if IsDesignTime then
        fDesigner.Modified;
    end;
  end;
end;

procedure TTisToolBarEditor.ToolBarRestoreButtonClick(Sender: TObject);
begin
  fTarget.RestoreSession;
  LoadButtons;
end;

procedure TTisToolBarEditor.FormShow(Sender: TObject);
begin
  ToolBarRestoreButton.Visible := not IsDesignTime;
end;

procedure TTisToolBarEditor.lvToolbarCompare(Sender: TObject; Item1,
  Item2: TListItem; Data: Integer; var Compare: Integer);
var
  vLeft, vRight: Integer;
begin
  vLeft := TSharedData(Item1.Data).Button.Left;
  vRight := TSharedData(Item2.Data).Button.Left;
  Compare := CompareStr(vLeft.ToString, vRight.ToString);
end;

procedure TTisToolBarEditor.InsertItem(aItem: TListItem);
begin
  lvToolbar.ItemIndex := -1;
  lvToolbar.Selected := nil;
  if aItem.Index < lvToolbar.Items.Count -1 then
    lvToolbar.ItemIndex := aItem.Index + 1
  else
    lvToolbar.ItemIndex := aItem.Index;
end;

procedure TTisToolBarEditor.btnAddClick(Sender: TObject);
begin
  AddCommand;
end;

function TTisToolBarEditor.NewListViewItem(const aCaption: string): TListItem;
var
  i: Integer;
begin
  i := lvToolbar.ItemIndex;
  if i = -1 then
    i := lvToolbar.Items.Count -1; // add before the last empty item
  if i = -1 then
    Result := lvToolbar.Items.Add
  else
    Result := lvToolbar.Items.Insert(i);
  result.Caption := aCaption;
end;

procedure TTisToolBarEditor.CheckButtonDesigntime(aButton: TToolButton);
begin
  if IsDesignTime then
  begin
    aButton.Name := fDesigner.CreateUniqueComponentName(aButton.ClassName);
    fDesigner.PropertyEditorHook.PersistentAdded(aButton, True);
    fDesigner.Modified;
  end;
end;

function TTisToolBarEditor.NewButtonBy(aNode: TTreeNode): TToolButton;
var
  vData: TSharedData;
begin
  result := TToolButton.Create(fTarget.Owner);
  result.Style := tbsButton;
  vData := TSharedData(aNode.Data);
  if vData <> nil then
  begin
    if vData.Action <> nil then
    begin
      result.Caption := vData.Action.Caption;
      result.Action := vData.Action;
    end
    else
      result.ImageIndex := aNode.ImageIndex;
    if vData.Popup <> nil then
    begin
      result.Caption := vData.Popup.Category;
      result.DropdownMenu := vData.Popup.PopupMenu;
      result.Style := tbsButtonDrop;
    end;
  end;
  CheckButtonDesigntime(result);
end;

function TTisToolBarEditor.NewButtonDivider: TToolButton;
begin
  result := TToolButton.Create(fTarget.Owner);
  result.Style := tbsDivider;
  CheckButtonDesigntime(result);
end;

procedure TTisToolBarEditor.AddCommand;
var
  vNode: TTreeNode;
  vCaption: string;
  vListItem: TListItem;
  vData: TSharedData;
begin
  vNode := TV.Selected;
  if (vNode = nil) or (vNode.Data = nil) then
    exit;
  vData := TSharedData(vNode.Data);
  if vData.Button = nil then
    vData.Button := NewButtonBy(vNode);
  if vData.Action <> nil then
    vCaption := vData.Action.Caption
  else if vData.Popup <> nil then
    vCaption := vData.Popup.Category
  else
    vCaption := '';
  DeleteAmpersands(vCaption);
  vListItem := NewListViewItem(vCaption);
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
  vData: TSharedData;
  vNode: TTreeNode;
begin
  i := lvToolbar.ItemIndex;
  if (i < 0)  then
    exit;
  vData := TSharedData(lvToolbar.Items[i].Data);
  lvToolbar.Items.Delete(i);
  {$IF DEFINED(LCLQt) or DEFINED(LCLQt5)}
  lvToolbar.ItemIndex := -1;     // Try to make LCLQt behave.
  lvToolbar.ItemIndex := i;
  {$ENDIF}
  lvToolbar.Selected := lvToolbar.Items[i];
  // show the command as available again in TreeView
  if Assigned(vData) then
  begin
    vNode := FindTreeViewNodeBy(vData.Action);
    if vNode = nil then
      vNode := FindTreeViewNodeBy(vData.Popup);
    if vNode <> nil then
      vNode.Data := vData; // overriding data with button instance too
  end;
  UpdateButtonsState;
end;

procedure TTisToolBarEditor.btnAddDividerClick(Sender: TObject);
var
  vListItem: TListItem;
  vData: TSharedData;
begin
  vListItem := NewListViewItem(DIVIDER_LISTVIEW_CAPTION);
  vListItem.ImageIndex := -1;
  InsertItem(vListItem);
  vData := TSharedData.Create(self);
  vData.Button := NewButtonDivider;
  vListItem.Data := vData;
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

procedure TTisToolBarEditor.LoadActionsAndPopups;
var
  i, x: Integer;
  vNode, vPopupRootNode: TTreeNode;
  vCategory: string;
  vAction: TAction;
  vActions: TActionList;
  vActionsItem: TTisActionsItem;
  vPopup: TPopupMenu;
  vPopupMenusItem: TTisPopupMenusItem;
  vPopupItem: TMenuItem;
  vFound: Boolean;
  vData: TSharedData;
begin
  TV.Items.BeginUpdate;
  try
    TV.Items.Clear;
    // adding auto actions
    if eoAutoAddActions in fTarget.EditorOptions then
    begin
      for i := 0 to fTarget.ButtonCount -1 do
      begin
        vAction := fTarget.Buttons[i].Action as TAction;
        if vAction = nil then
          Continue;
        vActions := vAction.ActionList as TActionList;
        vFound := False;
        for x := 0 to fTarget.Actions.Count -1 do
        begin
          if vActions = fTarget.Actions[x].List then
          begin
            vFound := True;
            Break;
          end;
        end;
        if not vFound then
        begin
          vActionsItem := fTarget.Actions.Add as TTisActionsItem;
          vActionsItem.List := vActions;
          if IsDesignTime then
          begin
            fDesigner.PropertyEditorHook.PersistentAdded(vActionsItem, False);
            fDesigner.Modified;
          end;
        end;
      end;
    end;
    // load categories and actions
    for i := 0 to fTarget.Actions.Count -1 do
    begin
      vActions := fTarget.Actions.Items[i].List;
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
          if vAction.Caption <> DIVIDER_BUTTON_CAPTION then // include, if it is not a divider
          begin
            vData := TSharedData.Create(self);
            vData.Action := vAction;
            with TV.Items.AddChild(vNode, Format('%s', [vAction.Caption])) do
            begin
              ImageIndex := vAction.ImageIndex;
              SelectedIndex := vAction.ImageIndex;
              Data := vData;
            end;
          end;
        end;
      end;
    end;
    // adding auto popup menus
    if eoAutoAddPopupMenus in fTarget.EditorOptions then
    begin
      for i := 0 to fTarget.ButtonCount -1 do
      begin
        vPopup := fTarget.Buttons[i].DropdownMenu;
        if vPopup = nil then
          Continue;
        vFound := False;
        for x := 0 to fTarget.PopupMenus.Count -1 do
        begin
          if vPopup = fTarget.PopupMenus[x].PopupMenu then
          begin
            vFound := True;
            Break;
          end;
        end;
        if not vFound then
        begin
          vPopupMenusItem := fTarget.PopupMenus.Add as TTisPopupMenusItem;
          vPopupMenusItem.PopupMenu := vPopup;
          if IsDesignTime then
          begin
            fDesigner.PropertyEditorHook.PersistentAdded(vPopupMenusItem, False);
            fDesigner.Modified;
          end;
        end;
      end;
    end;
    // load popups
    if fTarget.PopupMenus.Count > 0 then
      vPopupRootNode := TV.Items.AddChild(nil, rsPopupMenus);
    for i := 0 to fTarget.PopupMenus.Count -1 do
    begin
      vPopupMenusItem := fTarget.PopupMenus.Items[i];
      vPopup := vPopupMenusItem.PopupMenu;
      vCategory := vPopupMenusItem.Category;
      vData := TSharedData.Create(self);
      vData.Popup := vPopupMenusItem;
      vNode := TV.Items.AddChild(vPopupRootNode, vCategory);
      vNode.Data := vData;
      vNode.ImageIndex := vPopupMenusItem.ImageIndex;
      for x := 0 to vPopup.Items.Count -1 do
      begin
        vPopupItem := vPopup.Items[x];
        vCategory := vPopupItem.Caption;
        if vCategory = DIVIDER_BUTTON_CAPTION then
          Continue;
        DeleteAmpersands(vCategory);
        with TV.Items.AddChild(vNode, vCategory) do
        begin
          ImageIndex := vPopupItem.ImageIndex;
          SelectedIndex := vPopupItem.ImageIndex;
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
begin
  if lvToolbar.Items.Count > 0 then
  begin
    lvToolbar.ItemIndex := 0; // point to the first for remove all items
    for i := 0 to lvToolbar.Items.Count -1 do
      RemoveCommand;
  end;
  for i := 0 to fTarget.ButtonCount -1 do
    AddListViewItem(fTarget.Buttons[i]);
  lvToolbar.Sort; // sort by Button.Left following original design
end;

function TTisToolBarEditor.FindTreeViewNodeBy(aAction: TAction): TTreeNode;
begin
  if aAction = nil then
    exit(nil);
  result := TV.Items.FindTopLvlNode(aAction.Category);
  if result <> nil then
    result := TV.Items.FindNodeWithText(aAction.Caption);
  if result <> nil then
  begin
    result.Visible := True;
    result.Selected := True;
  end;
end;

function TTisToolBarEditor.FindTreeViewNodeBy(aPopupItem: TTisPopupMenusItem): TTreeNode;
begin
  if aPopupItem = nil then
    exit(nil);
  result := TV.Items.FindTopLvlNode(rsPopupMenus);
  if result <> nil then
    result := TV.Items.FindNodeWithText(aPopupItem.Category);
  if result <> nil then
  begin
    result.Visible := True;
    result.Selected := True;
  end;
end;

function TTisToolBarEditor.IsDesignTime: Boolean;
begin
  result := Assigned(fDesigner) and Assigned(fDesigner.PropertyEditorHook);
end;

procedure TTisToolBarEditor.SetTarget(aValue: TTisToolBar);
begin
  fTarget := aValue;
  TV.Images := fTarget.Images;
  lvToolbar.SmallImages := fTarget.Images;
  SetupCaptions;
  LoadActionsAndPopups;
  LoadButtons;
end;

procedure TTisToolBarEditor.AddListViewItem(aButton: TToolButton);
var
  vListItem: TListItem;
  vAction: TAction;
  vData: TSharedData;
  vNode: TTreeNode;
begin
  vListItem := lvToolbar.Items.Add;
  vAction := aButton.Action as TAction;
  vData := TSharedData.Create(self);
  vData.Action := vAction;
  vData.Button := aButton;
  vListItem.Data := vData;
  case aButton.Style of
    tbsDivider, tbsSeparator:
    begin
      vListItem.Caption := DIVIDER_LISTVIEW_CAPTION;
      vListItem.ImageIndex := -1;
    end
    else
    begin
      vListItem.Caption := aButton.Caption;
      if Assigned(vAction) then
        vListItem.ImageIndex := vAction.ImageIndex
      else
        vListItem.ImageIndex := aButton.ImageIndex;
    end;
  end;
  vNode := FindTreeViewNodeBy(vData.Action);
  if vNode <> nil then
    vNode.Visible := False;
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

