// -----------------------------------------------------------------
//    This file is part of Tranquil IT Software
//    Copyright (C) 2012 - 2022  Tranquil IT https://www.tranquil.it
//    All Rights Reserved.
// ------------------------------------------------------------------
unit tis.ui.toolbar.core;

{$mode objfpc}{$H+}
{$modeswitch ADVANCEDRECORDS}
{$modeswitch typehelpers}

interface

uses
  Classes,
  SysUtils,
  Forms,
  Controls,
  Graphics,
  Dialogs,
  StdCtrls,
  ExtCtrls,
  Buttons,
  Variants,
  ComCtrls,
  ActnList,
  TextStrings,
  Menus,
  mormot.core.base,
  mormot.core.variants,
  mormot.core.unicode,
  mormot.core.rtti;

type
  // forward class declarations
  TTisToolBar = class;
  TTisActionsCollection = class;

  /// define a item for the collection
  TTisActionsItem = class(TCollectionItem)
  private
    fList: TActionList;
    fHiddenCategories: TStrings;
    procedure SetHiddenCategories(aValue: TStrings);
    procedure SetList(aValue: TActionList);
  protected
    const DefaultReadOnly = False;
    function GetActions: TTisActionsCollection;
  public
    constructor Create(aCollection: TCollection); override;
    destructor Destroy; override;
    procedure Assign(aSource: TPersistent); override;
  published
    property List: TActionList read fList write SetList;
    /// use this list to hide categories on Editor by name
    property HiddenCategories: TStrings read fHiddenCategories write SetHiddenCategories;
  end;

  /// actions collection
  TTisActionsCollection = class(TCollection)
  private
    fControl: TWinControl;
    function GetItems(aIndex: Integer): TTisActionsItem;
    procedure SetItems(aIndex: Integer; aValue: TTisActionsItem);
  protected
    // ------------------------------- inherited methods ----------------------------
    function GetOwner: TPersistent; override;
  public
    constructor Create(aControl: TWinControl); reintroduce;
    // ------------------------------- new methods ----------------------------------
    function LocateAction(const aListOwnerName, aListName, aActionName: string): TAction;
    function GetToolBar: TTisToolBar;
    /// items of the collection
    property Items[aIndex: Integer]: TTisActionsItem read GetItems write SetItems; default;
  end;

  TTisEditorOption = (
    /// user can double-click on toolbar to show editor
    // - only if OnDblClick event was not assigned
    eoShowOnDblClick,
    /// user can right-click to show a popup menu to access editor
    // - it will add a new item at the end of PopupMenu.Items
    // - if PopupMenu was not assigned, it will create it
    eoShowOnPopupMenu
  );

  TTisEditorOptions = set of TTisEditorOption;

  TTisToolBar = class(TToolBar)
  private
    fActions: TTisActionsCollection;
    fEditorOptions: TTisEditorOptions;
    fDefaultSessionValues: string;
    fPopupReferences: TStringList;
  protected
    const DefaultEditorOptions = [eoShowOnPopupMenu];
  protected
    // ------------------------------- inherited methods ----------------------------
    procedure Loaded; override;
    procedure Notification(aComponent: TComponent; aOperation: TOperation); override;
    // ------------------------------- new methods ----------------------------------
    function GetSessionValues: string; virtual;
    procedure SetSessionValues(const aValue: string); virtual;
    procedure SetupDblClick; virtual;
    procedure SetupPopupMenu; virtual;
    procedure SavePopupReferences; virtual;
    procedure ShowEditorCallback({%H-}aSender: TObject); virtual;
  public
    // ------------------------------- inherited methods ----------------------------
    constructor Create(aOwner: TComponent); override;
    destructor Destroy; override;
    procedure Assign(aSource: TPersistent); override;
    // ------------------------------- new methods ----------------------------------
    /// add a new button related to an action
    function AddButton(const aCaption: string; aStyle: TToolButtonStyle;
      aAction: TAction; aPopupMenu: TPopupMenu = nil): TToolButton; overload;
    procedure RemoveButton(aButton: TToolButton); overload; virtual;
    /// remove all buttons
    procedure RemoveButtons;
    /// it shows the Editor to manage buttons/actions
    procedure ShowEditor;
    /// it resets SessionValues to the original design
    procedure RestoreSession;
  published
    // ------------------------------- new properties -------------------------------
    property Actions: TTisActionsCollection read fActions write fActions;
    property EditorOptions: TTisEditorOptions read fEditorOptions write fEditorOptions default DefaultEditorOptions;
    property SessionValues: string read GetSessionValues write SetSessionValues stored False;
  end;

implementation

uses
  tis.ui.toolbar.editor;

resourcestring
  rsCustomizeToolbar = 'Customize the toolbar';

{ TTisActionsItem }

procedure TTisActionsItem.SetHiddenCategories(aValue: TStrings);
begin
  if (aValue <> nil) then
    fHiddenCategories.Assign(aValue);
end;

procedure TTisActionsItem.SetList(aValue: TActionList);
begin
  if fList = aValue then
    exit;
  fList := aValue;
  if fList <> nil then
    fList.FreeNotification(GetActions.GetToolBar);
end;

function TTisActionsItem.GetActions: TTisActionsCollection;
begin
  result := GetOwner as TTisActionsCollection;
end;

constructor TTisActionsItem.Create(aCollection: TCollection);
begin
  inherited Create(aCollection);
  fHiddenCategories := TTextStrings.Create;
end;

destructor TTisActionsItem.Destroy;
begin
  fHiddenCategories.Free;
  inherited Destroy;
end;

procedure TTisActionsItem.Assign(aSource: TPersistent);
begin
  if aSource is TTisActionsItem then
    with aSource as TTisActionsItem do
    begin
      self.List := List;
    end
  else
    inherited Assign(aSource);
end;

{ TTisActionsCollection }

function TTisActionsCollection.GetItems(aIndex: Integer): TTisActionsItem;
begin
  result := TTisActionsItem(inherited Items[aIndex]);
end;

procedure TTisActionsCollection.SetItems(aIndex: Integer; aValue: TTisActionsItem);
begin
  Items[aIndex].Assign(aValue);
end;

function TTisActionsCollection.GetOwner: TPersistent;
begin
  result := fControl;
end;

function TTisActionsCollection.GetToolBar: TTisToolBar;
begin
  result := fControl as TTisToolBar;
end;

constructor TTisActionsCollection.Create(aControl: TWinControl);
begin
  inherited Create(TTisActionsItem);
  fControl := aControl;
end;

function TTisActionsCollection.LocateAction(const aListOwnerName, aListName,
  aActionName: string): TAction;
var
  i: Integer;
  vActions: TActionList;
begin
  result := nil;
  for i := 0 to Count -1 do
  begin
    vActions := Items[i].List;
    if vActions = nil then
      continue;
    if (vActions.Owner.Name = aListOwnerName) and (vActions.Name = aListName) then
    begin
      result := vActions.ActionByName(aActionName) as TAction;
      exit;
    end;
  end;
end;

{ TTisToolBar }

procedure TTisToolBar.Loaded;
begin
  inherited Loaded;
  fDefaultSessionValues := SessionValues;
  // setup things, if there are items in Actions collection
  if not (csDesigning in ComponentState) and (Actions.Count > 0) then
  begin
    SetupDblClick;
    SetupPopupMenu;
    SavePopupReferences;
  end;
end;

procedure TTisToolBar.Notification(aComponent: TComponent;
  aOperation: TOperation);
var
  i: Integer;
  vActionItem: TTisActionsItem;
begin
  inherited Notification(aComponent, aOperation);
  if aOperation = opRemove then
  begin
    for i := 0 to Actions.Count -1 do
    begin
      vActionItem := Actions.Items[i];
      if vActionItem.List = aComponent then
        vActionItem.List := nil;
    end;
  end;
end;

function TTisToolBar.GetSessionValues: string;
var
  i: Integer;
  vDoc: TDocVariantData;
  vAction: TAction;
  vButton: TToolButton;
  vObj: Variant;
  vPopup: TPopupMenu;
begin
  vDoc.InitArray([], JSON_FAST_FLOAT);
  for i := 0 to ButtonCount -1 do
  begin
    vButton := Buttons[i];
    vObj := _ObjFast([
      'caption', vButton.Caption,
      'left', vButton.Left,
      'style', vButton.Style
    ]);
    vAction := vButton.Action as TAction;
    if Assigned(vAction) and
      Assigned(vAction.ActionList) and
      Assigned(vAction.ActionList.Owner) then
    begin
      vObj.action := _ObjFast([
        'owner', vAction.ActionList.Owner.Name,
        'list', vAction.ActionList.Name,
        'name', vAction.Name
      ]);
    end;
    vPopup := vButton.DropdownMenu;
    if Assigned(vPopup) then
    begin
      vObj.popup := _ObjFast([
        'owner', vPopup.Owner.Name,
        'name', vPopup.Name
      ]);
    end;
    vDoc.AddItem(vObj);
  end;
  // by default, the original list order is added by instance not by design
  // - session needs to save buttons by design order
  vDoc.SortArrayByField('left');
  result := Utf8ToString(vDoc.ToJson);
end;

procedure TTisToolBar.SetSessionValues(const aValue: string);

  /// it will try to locate a PopupMenu using the same Actions owners
  function _LocatePopupMenu(const aOwnerName, aPopupName: TComponentName): TPopupMenu;
  var
    i: Integer;
    vActions: TActionList;
  begin
    result := nil;
    for i := 0 to Actions.Count -1 do
    begin
      vActions := Actions.Items[i].List;
      if vActions = nil then
        continue;
      if (vActions.Owner.Name = aOwnerName) then
      begin
        result := vActions.Owner.FindComponent(aPopupName) as TPopupMenu;
        exit;
      end;
    end;
  end;

var
  vDoc: TDocVariantData;
  vAction: TAction;
  vPopup: TPopupMenu;
  vObj: PDocVariantData;
  vObjAction, vObjPopup: PVariant;
begin
  if (csDesigning in ComponentState) or
   (GetSessionValues = aValue) or
   (Actions.Count = 0) then
    exit;
  RemoveButtons;
  try
    if not vDoc.InitJson(StringToUtf8(aValue), JSON_FAST_FLOAT) then
      vDoc.InitJson(StringToUtf8(fDefaultSessionValues), JSON_FAST_FLOAT); // use default values, if aValue is invalid
    for vObj in vDoc.Objects do
    begin
      if vObj^.GetAsPVariant('action', vObjAction) then
        vAction := Actions.LocateAction(vObjAction^.owner, vObjAction^.list, vObjAction^.name)
      else
        vAction := nil;
      if vObj^.GetAsPVariant('popup', vObjPopup) then
        vPopup := _LocatePopupMenu(vObjPopup^.owner, vObjPopup^.name)
      else
        vPopup := nil;
      AddButton(vObj^.S['caption'], TToolButtonStyle(vObj^.I['style']), vAction, vPopup);
    end;
  except
    // shows exception only in designtime
    if csDesigning in ComponentState then
      Application.HandleException(self)
  end;
end;

procedure TTisToolBar.SetupDblClick;
begin
  if (not Assigned(OnDblClick)) and (eoShowOnDblClick in fEditorOptions) then
    OnDblClick := @ShowEditorCallback;
end;

procedure TTisToolBar.SetupPopupMenu;
var
  vMenuItem: TMenuItem;
begin
  if not Assigned(PopupMenu) then
    PopupMenu := TPopupMenu.Create(self);
  if eoShowOnPopupMenu in fEditorOptions then
  begin
    if PopupMenu.Items.Count > 0 then
    begin
      vMenuItem := TMenuItem.Create(self);
      vMenuItem.Caption := '-';
      PopupMenu.Items.Add(vMenuItem);
    end;
    vMenuItem := TMenuItem.Create(self);
    vMenuItem.Tag := -1;
    vMenuItem.Caption := rsCustomizeToolbar;
    vMenuItem.OnClick := @ShowEditorCallback;
    PopupMenu.Items.Add(vMenuItem);
  end;
end;

procedure TTisToolBar.SavePopupReferences;
var
  i: Integer;
  vButton: TToolButton;
begin
  fPopupReferences.Clear;
  for i := 0 to ButtonCount -1 do
  begin
    vButton := Buttons[i];
    if (vButton.Style = tbsButtonDrop) and Assigned(vButton.DropdownMenu) then
      fPopupReferences.AddObject(vButton.Caption, vButton.DropdownMenu);
  end;
end;

procedure TTisToolBar.ShowEditorCallback(aSender: TObject);
begin
  ShowEditor;
end;

function TTisToolBar.AddButton(const aCaption: string;
  aStyle: TToolButtonStyle; aAction: TAction; aPopupMenu: TPopupMenu): TToolButton;
begin
  result := TToolButton.Create(self);
  with result do
  begin
    Parent := self;
    Caption := aCaption;
    Action := aAction;
    Style := aStyle;
    AutoSize := True;
    Left := Parent.Width;
    DropdownMenu := aPopupMenu;
  end;
end;

procedure TTisToolBar.RemoveButton(aButton: TToolButton);
begin
  ButtonList.Remove(aButton);
  RemoveControl(aButton);
end;

procedure TTisToolBar.RemoveButtons;
var
  i: Integer;
begin
  for i := ButtonCount-1 downto 0 do
    RemoveControl(Buttons[i]);
  ButtonList.Clear;
end;

constructor TTisToolBar.Create(aOwner: TComponent);
begin
  inherited Create(aOwner);
  fActions := TTisActionsCollection.Create(self);
  fEditorOptions := DefaultEditorOptions;
  fPopupReferences := TStringList.Create;
end;

destructor TTisToolBar.Destroy;
begin
  fActions.Free;
  fPopupReferences.Free;
  inherited Destroy;
end;

procedure TTisToolBar.Assign(aSource: TPersistent);
begin
  if aSource is TTisToolBar then
    with aSource as TTisToolBar do
    begin
      self.Actions.Assign(Actions);
      self.SessionValues := SessionValues;
    end
  else
    inherited Assign(aSource);
end;

procedure TTisToolBar.ShowEditor;
begin
  with TTisToolBarEditor.Create(Application) do
  try
    Target := self;
    PopupReferences := fPopupReferences;
    ShowModal;
  finally
    Free;
  end;
end;

procedure TTisToolBar.RestoreSession;
begin
  SessionValues := fDefaultSessionValues;
end;

end.
