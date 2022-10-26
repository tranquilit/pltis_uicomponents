// -----------------------------------------------------------------
//    This file is part of Tranquil IT Software
//    Copyright (C) 2012 - 2022  Tranquil IT https://www.tranquil.it
//    All Rights Reserved.
// ------------------------------------------------------------------
unit tis.ui.toolbar.editor;

{$i tis.ui.defines.inc}

interface

uses
  Classes,
  SysUtils,
  LCLProc,
  LCLType,
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
    ButtonsListView: TListView;
    miAll: TMenuItem;
    miCustom: TMenuItem;
    miDebug: TMenuItem;
    miDesign: TMenuItem;
    miHTML: TMenuItem;
    pnlButtons: TButtonPanel;
    Splitter1: TSplitter;
    ActionsTreeView: TTreeView;
    ToolBarRestoreButton: TSpeedButton;
    procedure FormCreate(Sender: TObject);
    procedure ButtonsListViewDblClick(Sender: TObject);
    procedure lvToolbarEnterExit(Sender: TObject);
    procedure ActionsTreeViewDblClick(Sender: TObject);
    procedure UpdateButtonsState;
    procedure btnAddClick(Sender: TObject);
    procedure btnAddDividerClick(Sender: TObject);
    procedure btnMoveDownClick(Sender: TObject);
    procedure btnMoveUpClick(Sender: TObject);
    procedure btnRemoveClick(Sender: TObject);
    procedure ButtonsListViewSelectItem(Sender: TObject; {%H-}Item: TListItem;
      {%H-}Selected: Boolean);
    procedure ActionsTreeViewSelectionChanged(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var {%H-}CanClose: boolean);
    procedure ToolBarRestoreButtonClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
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
    procedure AddAutoPopups;
    procedure AddAutoActions;
    procedure LoadActions;
    procedure LoadButtons;
    function FindTreeViewNodeBy(aAction: TAction): TTreeNode;
  private
    const DividerListViewCaption = '---------------';
    const DividerButtonCaption = '-';
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
    PopupMenu: TPopupMenu;
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

procedure TTisToolBarEditor.ButtonsListViewDblClick(Sender: TObject);
begin
  RemoveCommand;
end;

procedure TTisToolBarEditor.lvToolbarEnterExit(Sender: TObject);
begin
  UpdateButtonsState;
end;

procedure TTisToolBarEditor.ActionsTreeViewDblClick(Sender: TObject);
begin
  if btnAdd.Enabled then
    AddCommand;
end;

procedure TTisToolBarEditor.UpdateButtonsState;
var
  v1: Integer;
begin
  v1 := ButtonsListView.ItemIndex;
  btnAdd.Enabled := Assigned(ActionsTreeView.Selected) and Assigned(ActionsTreeView.Selected.Data);
  btnRemove.Enabled := (v1 > -1) and (v1 <= ButtonsListView.Items.Count -1);
  btnMoveUp.Enabled := (v1 > 0) and (v1 <= ButtonsListView.Items.Count -1);
  btnMoveDown.Enabled := (v1 > -1) and (v1 < ButtonsListView.Items.Count -1);
  btnAddDivider.Enabled := True;
end;

procedure TTisToolBarEditor.ActionsTreeViewSelectionChanged(Sender: TObject);
begin
  UpdateButtonsState;
end;

procedure TTisToolBarEditor.FormCloseQuery(Sender: TObject;
  var CanClose: boolean);
var
  v1, v2: Integer;
  vButton, vSibling: TToolButton;
  vListItem: TListItem;
  vFound: Boolean;
begin
  if ModalResult = mrOK then
  begin
    fTarget.BeginUpdate;
    try
      // cleaning the buttons that was removed by user
      for v1 := fTarget.ButtonCount-1 downto 0 do
      begin
        vFound := False;
        vButton := fTarget.Buttons[v1];
        vButton.Parent := nil; // temporarily removed from the target to change its bound more below
        for v2 := 0 to ButtonsListView.Items.Count -1 do
        begin
          vListItem := ButtonsListView.Items[v2];
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
      // reorganizing buttons bounds on toolbar
      for v2 := 0 to ButtonsListView.Items.Count -1 do
      begin
        vListItem := ButtonsListView.Items[v2];
        vButton := TSharedData(vListItem.Data).Button;
        if v2 = 0 then
          vButton.Left := 1
        else
        begin
          vSibling := fTarget.Buttons[v2-1];
          vButton.SetBounds(
            vSibling.Left + vSibling.Width,
            vSibling.Top, vButton.Width, vButton.Height);
        end;
        // should be True, as maybe a translated caption could be longer
        vButton.AutoSize := True;
        // show it in the target toolbar
        vButton.Parent := fTarget;
      end;
    finally
      fTarget.EndUpdate;
      if IsDesignTime then
      begin
        fDesigner.Modified;
        // get new default values
        fTarget.DefaultSessionValues := fTarget.SessionValues;
        // force to use new values from DefaultSessionValues
        fTarget.SessionValues := '';
        fTarget.Invalidate;
      end;
    end;
  end;
end;

procedure TTisToolBarEditor.ToolBarRestoreButtonClick(Sender: TObject);
begin
  fTarget.RestoreSession;
  LoadButtons;
  ButtonsListView.SetFocus;
end;

procedure TTisToolBarEditor.FormShow(Sender: TObject);
begin
  ToolBarRestoreButton.Visible := not IsDesignTime;
end;

procedure TTisToolBarEditor.InsertItem(aItem: TListItem);
begin
  ButtonsListView.ItemIndex := -1;
  ButtonsListView.Selected := nil;
  if aItem.Index < ButtonsListView.Items.Count -1 then
    ButtonsListView.ItemIndex := aItem.Index + 1
  else
    ButtonsListView.ItemIndex := aItem.Index;
end;

procedure TTisToolBarEditor.btnAddClick(Sender: TObject);
begin
  AddCommand;
end;

function TTisToolBarEditor.NewListViewItem(const aCaption: string): TListItem;
var
  vIndex: Integer;
begin
  vIndex := ButtonsListView.ItemIndex;
  if vIndex = -1 then
    vIndex := ButtonsListView.Items.Count -1; // add before the last empty item
  if vIndex = -1 then
    Result := ButtonsListView.Items.Add
  else
    Result := ButtonsListView.Items.Insert(vIndex);
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
    if Assigned(vData.Action) then
    begin
      result.Caption := vData.Action.Caption;
      result.Action := vData.Action;
    end
    else
      result.ImageIndex := aNode.ImageIndex;
    if Assigned(vData.PopupMenu) then
    begin
      result.DropdownMenu := vData.PopupMenu;
      if Assigned(result.Action) and Assigned(result.Action.OnExecute) then
        result.Style := tbsDropDown
      else
        result.Style := tbsButtonDrop
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
  vNode := ActionsTreeView.Selected;
  if (vNode = nil) or (vNode.Data = nil) then
    exit;
  vData := TSharedData(vNode.Data);
  if vData.Button = nil then
    vData.Button := NewButtonBy(vNode);
  if Assigned(vData.Action) then
    vCaption := vData.Action.Caption
  else
    vCaption := '';
  DeleteAmpersands(vCaption);
  if vCaption <> '' then
  begin
    vListItem := NewListViewItem(vCaption);
    vListItem.Data := vNode.Data;
    vListItem.ImageIndex := vNode.ImageIndex;
    InsertItem(vListItem);
  end;
  vNode := ActionsTreeView.Selected.GetNext;
  ActionsTreeView.Selected.Visible := False;
  if vNode <> nil then
    ActionsTreeView.Selected := vNode;
  UpdateButtonsState;
end;

procedure TTisToolBarEditor.RemoveCommand;
var
  vIndex: Integer;
  vData: TSharedData;
begin
  vIndex := ButtonsListView.ItemIndex;
  if vIndex < 0  then
    exit;
  vData := TSharedData(ButtonsListView.Items[vIndex].Data);
  ButtonsListView.Items.Delete(vIndex);
  {$IF DEFINED(LCLQt) or DEFINED(LCLQt5)}
  ButtonsListView.ItemIndex := -1;     // try to make LCLQt behave
  ButtonsListView.ItemIndex := vIndex;
  {$ENDIF}
  ButtonsListView.Selected := ButtonsListView.Items[vIndex];
  // show the command as available again in TreeView
  if Assigned(vData) then
    FindTreeViewNodeBy(vData.Action);
  UpdateButtonsState;
end;

procedure TTisToolBarEditor.btnAddDividerClick(Sender: TObject);
var
  vListItem: TListItem;
  vData: TSharedData;
begin
  vListItem := NewListViewItem(DividerListViewCaption);
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

procedure TTisToolBarEditor.ButtonsListViewSelectItem(Sender: TObject;
  Item: TListItem; Selected: Boolean);
begin
  UpdateButtonsState;
  lblSelect.Caption := Format('%d / %d', [ButtonsListView.ItemIndex+1, ButtonsListView.Items.Count])
end;

procedure TTisToolBarEditor.MoveUpDown(aOffset: integer);
var
  vIndex1, vIndex2: Integer;
begin
  vIndex1 := ButtonsListView.ItemIndex;
   vIndex2 := vIndex1 + aOffset;
  ButtonsListView.Items.Exchange(vIndex1, vIndex2);
  ButtonsListView.Items[vIndex1].Selected := False;
  ButtonsListView.Items[ vIndex2].Selected := False;
  ButtonsListView.ItemIndex:= -1;
  ButtonsListView.Selected := nil;
  ButtonsListView.ItemIndex:=  vIndex2;
  ButtonsListView.Selected := ButtonsListView.Items[ vIndex2];
end;

procedure TTisToolBarEditor.btnMoveDownClick(Sender: TObject);
begin
  if (ButtonsListView.ItemIndex < 0) or (ButtonsListView.ItemIndex = ButtonsListView.Items.Count-1) then
    exit;
  MoveUpDown(1);
end;

procedure TTisToolBarEditor.btnMoveUpClick(Sender: TObject);
begin
  if (ButtonsListView.ItemIndex <= 0) then
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

procedure TTisToolBarEditor.AddAutoPopups;
var
  v1, v2: Integer;
  vAction: TAction;
  vPopup: TPopupMenu;
  vPopupMenusItem: TTisPopupMenusItem;
  vFound: Boolean;
begin
  // adding auto popup menus
  if eoAutoAddPopupMenus in fTarget.EditorOptions then
  begin
    for v1 := 0 to fTarget.ButtonCount -1 do
    begin
      with fTarget.Buttons[v1] do
      begin
        vAction := Action as TAction;
        vPopup := DropdownMenu;
      end;
      if (vAction = nil) or (vPopup = nil) then
        continue;
      vFound := False;
      for v2 := 0 to fTarget.PopupMenus.Count -1 do
      begin
        if (vPopup = fTarget.PopupMenus[v2].PopupMenu) and
          (vAction = fTarget.PopupMenus[v2].Action)then
        begin
          vFound := True;
          Break;
        end;
      end;
      if not vFound then
      begin
        vPopupMenusItem := fTarget.PopupMenus.Add as TTisPopupMenusItem;
        vPopupMenusItem.Action := vAction;
        vPopupMenusItem.PopupMenu := vPopup;
        if IsDesignTime then
        begin
          fDesigner.PropertyEditorHook.PersistentAdded(vPopupMenusItem, False);
          fDesigner.Modified;
        end;
      end;
    end;
  end;
end;

procedure TTisToolBarEditor.AddAutoActions;
var
  v1, v2: Integer;
  vAction: TAction;
  vActions: TActionList;
  vActionsItem: TTisActionsItem;
  vFound: Boolean;
begin
  // adding auto actions
  if eoAutoAddActions in fTarget.EditorOptions then
  begin
    for v1 := 0 to fTarget.ButtonCount -1 do
    begin
      vAction := fTarget.Buttons[v1].Action as TAction;
      if vAction = nil then
        Continue;
      vActions := vAction.ActionList as TActionList;
      vFound := False;
      for v2 := 0 to fTarget.Actions.Count -1 do
      begin
        if vActions = fTarget.Actions[v2].List then
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
end;

procedure TTisToolBarEditor.LoadActions;
var
  v1, v2, v3, v4: Integer;
  vNode: TTreeNode;
  vCategory, vCaption: string;
  vAction: TAction;
  vActions: TActionList;
  vPopupItem: TMenuItem;
  vPopupMenusItem: TTisPopupMenusItem;
  vData: TSharedData;
begin
  // load categories and actions
  for v1 := 0 to fTarget.Actions.Count -1 do
  begin
    vActions := fTarget.Actions.Items[v1].List;
    for v2 := 0 to vActions.ActionCount -1 do
    begin
      vAction := vActions.Actions[v2] as TAction;
      vCategory := vAction.Category;
      DeleteAmpersands(vCategory);
      // do not show if the action belongs to a hidden category
      if Target.Actions.Items[v1].HiddenCategories.IndexOf(vCategory) >= 0 then
        continue;
      vNode := ActionsTreeView.Items.FindNodeWithText(vCategory);
      if vNode = nil then
        vNode := ActionsTreeView.Items.AddChild(nil, vCategory);
      vData := TSharedData.Create(self);
      vData.Action := vAction;
      // create the action node
      vNode := ActionsTreeView.Items.AddChild(vNode, Format('%s', [vAction.Caption]));
      vNode.ImageIndex := vAction.ImageIndex;
      vNode.SelectedIndex := vAction.ImageIndex;
      // try to locate the popup menu related to the action
      for v3 := 0 to fTarget.PopupMenus.Count -1 do
      begin
        vPopupMenusItem := fTarget.PopupMenus.Items[v3];
        if Assigned(vPopupMenusItem.PopupMenu) and
           Assigned(vPopupMenusItem.Action) and
          (vPopupMenusItem.Action = vAction) then
        begin
          // show all menu items into the tree view
          for v4 := 0 to vPopupMenusItem.PopupMenu.Items.Count -1 do
          begin
            vPopupItem := vPopupMenusItem.PopupMenu.Items[v4];
            vCaption := vPopupItem.Caption;
            if vCaption = DividerButtonCaption then
              continue;
            DeleteAmpersands(vCaption);
            with ActionsTreeView.Items.AddChild(vNode, vCaption) do
            begin
              ImageIndex := vPopupItem.ImageIndex;
              Data := nil;
            end;
          end;
          // popup matched
          vData.PopupMenu := vPopupMenusItem.PopupMenu;
          continue;
        end;
      end;
      vNode.Data := vData;
    end;
  end;
end;

procedure TTisToolBarEditor.LoadButtons;
var
  v1: Integer;
begin
  if ButtonsListView.Items.Count > 0 then
  begin
    ButtonsListView.ItemIndex := 0; // point to the first for remove all items
    for v1 := 0 to ButtonsListView.Items.Count -1 do
      RemoveCommand;
  end;
  for v1 := 0 to fTarget.ButtonCount -1 do
    AddListViewItem(fTarget.Buttons[v1]);
end;

function TTisToolBarEditor.FindTreeViewNodeBy(aAction: TAction): TTreeNode;
begin
  if aAction = nil then
    exit(nil);
  result := ActionsTreeView.Items.FindTopLvlNode(aAction.Category);
  if Assigned(result) then
    result := ActionsTreeView.Items.FindNodeWithText(aAction.Caption);
  if Assigned(result) then
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
  ActionsTreeView.Images := fTarget.Images;
  ButtonsListView.SmallImages := fTarget.Images;
  SetupCaptions;
  ActionsTreeView.Items.BeginUpdate;
  try
    ActionsTreeView.Items.Clear;
    AddAutoPopups;
    AddAutoActions;
    LoadActions;
  finally
    ActionsTreeView.Items.EndUpdate;
  end;
  LoadButtons;
end;

procedure TTisToolBarEditor.AddListViewItem(aButton: TToolButton);
var
  vListItem: TListItem;
  vAction: TAction;
  vData: TSharedData;
  vNode: TTreeNode;
  vCaption: TTranslateString;
begin
  vListItem := ButtonsListView.Items.Add;
  vAction := aButton.Action as TAction;
  vData := TSharedData.Create(self);
  vData.Action := vAction;
  vData.Button := aButton;
  vData.PopupMenu := aButton.DropdownMenu;
  vListItem.Data := vData;
  case aButton.Style of
    tbsDivider, tbsSeparator:
    begin
      vListItem.Caption := DividerListViewCaption;
      vListItem.ImageIndex := -1;
    end
    else
    begin
      if Assigned(vAction) then
        vCaption := vAction.Caption
      else
        vCaption := aButton.Caption;
      vListItem.Caption := vCaption;
      if Assigned(vAction) then
        vListItem.ImageIndex := vAction.ImageIndex
      else
        vListItem.ImageIndex := aButton.ImageIndex;
    end;
  end;
  vNode := FindTreeViewNodeBy(vData.Action);
  if Assigned(vNode) then
    vNode.Visible := False;
end;

{ TTisToolBarComponentEditor }

procedure TTisToolBarComponentEditor.DoShowEditor;
begin
  with TTisToolBarEditor.Create(Application) do
  try
    Designer := GetDesigner;
    Target := (Component as TTisToolBar);
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

