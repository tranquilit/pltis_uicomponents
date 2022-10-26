// -----------------------------------------------------------------
//    This file is part of Tranquil IT Software
//    Copyright (C) 2012 - 2022  Tranquil IT https://www.tranquil.it
//    All Rights Reserved.
// ------------------------------------------------------------------
unit tis.ui.toolbar.core;

{$i tis.ui.defines.inc}

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
  LCLType,
  mormot.core.base,
  mormot.core.variants,
  mormot.core.unicode,
  mormot.core.text,
  mormot.core.rtti;

type
  // forward class declarations
  TTisToolBar = class;
  TTisActionsCollection = class;
  TTisPopupMenusCollection = class;

  /// define a item for a collection of actions
  TTisActionsItem = class(TCollectionItem)
  private
    fList: TActionList;
    fHiddenCategories: TStrings;
    // ------------------------------- new methods ----------------------------------
    procedure SetHiddenCategories(aValue: TStrings);
    procedure SetList(aValue: TActionList);
  protected
    // ------------------------------- inherited methods ----------------------------
    function GetDisplayName: string; override;
    // ------------------------------- new methods ----------------------------------
    function GetOwnerAsActionsCollection: TTisActionsCollection;
  public
    // ------------------------------- inherited methods ----------------------------
    constructor Create(aCollection: TCollection); override;
    destructor Destroy; override;
    procedure Assign(aSource: TPersistent); override;
  published
    // ------------------------------- new properties -------------------------------
    /// the ActionList available to assign to a button
    property List: TActionList read fList write SetList;
    /// use this list to hide categories on Editor by name
    property HiddenCategories: TStrings read fHiddenCategories write SetHiddenCategories;
  end;

  /// a collection of actions
  TTisActionsCollection = class(TOwnedCollection)
  private
    fControl: TWinControl;
    function GetItems(aIndex: Integer): TTisActionsItem;
    procedure SetItems(aIndex: Integer; aValue: TTisActionsItem);
  public
    constructor Create(aControl: TWinControl); reintroduce;
    // ------------------------------- new methods ----------------------------------
    /// it returns Control as Toolbar
    function GetToolBar: TTisToolBar;
    /// it will try to locate the Action instance by name
    function LocateAction(const aOwnerName, aListName, aActionName: string): TAction;
    // ------------------------------- new properties -------------------------------
    /// items of the collection
    property Items[aIndex: Integer]: TTisActionsItem read GetItems write SetItems; default;
  end;

  /// define a item a collection of popup menus
  TTisPopupMenusItem = class(TCollectionItem)
  private
    fAction: TAction;
    fPopupMenu: TPopupMenu;
    // ------------------------------- new methods ----------------------------------
    procedure SetAction(aValue: TAction);
    procedure SetPopupMenu(aValue: TPopupMenu);
  protected
    // ------------------------------- inherited methods ----------------------------
    function GetDisplayName: string; override;
    // ------------------------------- new methods ----------------------------------
    function GetPopupMenus: TTisPopupMenusCollection;
  public
    // ------------------------------- inherited methods ----------------------------
    constructor Create(aCollection: TCollection); override;
    destructor Destroy; override;
    procedure Assign(aSource: TPersistent); override;
  published
    // ------------------------------- new properties -------------------------------
    /// an action related to the button
    // - if the action has OnExecute implemented, the button style will be tbsDropDown,
    // otherwise tbsButtonDrop
    property Action: TAction read fAction write SetAction;
    /// the PopupMenu available to assign to a button
    property PopupMenu: TPopupMenu read fPopupMenu write SetPopupMenu;
  end;

  /// a collection of popup menus
  TTisPopupMenusCollection = class(TOwnedCollection)
  private
    fControl: TWinControl;
    // ------------------------------- new methods ----------------------------------
    function GetItems(aIndex: Integer): TTisPopupMenusItem;
    procedure SetItems(aIndex: Integer; aValue: TTisPopupMenusItem);
  public
    constructor Create(aControl: TWinControl); reintroduce;
    // ------------------------------- new methods ----------------------------------
    function LocatePopupMenu(const aOwnerName, aPopuMenuName: string): TPopupMenu;
    function GetToolBar: TTisToolBar;
    // ------------------------------- new properties -------------------------------
    /// items of the collection
    property Items[aIndex: Integer]: TTisPopupMenusItem read GetItems write SetItems; default;
  end;

  TTisEditorOption = (
    /// user can double-click on toolbar to show editor
    // - only if OnDblClick event was not assigned
    eoShowOnDblClick,
    /// user can right-click to show a popup menu to access editor
    // - it will add a new item at the end of PopupMenu.Items
    // - if PopupMenu was not assigned, it will create it
    eoShowOnPopupMenu,
    /// it will add Actions, which are already been using on buttons,
    // into the Actions Collection property when open the Editor
    eoAutoAddActions,
    /// it will add PopupMenus, which are already been using on buttons that
    // has an Action assigned, into the Popups Collection property when open the Editor
    eoAutoAddPopupMenus
  );

  TTisEditorOptions = set of TTisEditorOption;

  /// event to manipulate the user session, if it is using a different session
  // version from component instance
  // - the aVersion argument is the current user session version
  TOnSessionChange = procedure(aSender: TTisToolBar; aVersion: Integer) of object;

  TTisToolBar = class(TToolBar)
  private
    fActions: TTisActionsCollection;
    fPopupMenus: TTisPopupMenusCollection;
    fEditorOptions: TTisEditorOptions;
    fDefaultSessionValues: string;
    fOnSessionChange: TOnSessionChange;
    fSessionVersion: Integer;
  protected
    const DefaultEditorOptions = [eoShowOnPopupMenu, eoAutoAddPopupMenus];
    const DefaultSessionVersion = 2;
  protected
    // ------------------------------- inherited methods ----------------------------
    procedure Loaded; override;
    procedure Notification(aComponent: TComponent; aOperation: TOperation); override;
    // ------------------------------- new methods ----------------------------------
    function GetSessionValues: string; virtual;
    procedure SetSessionValues(const aValue: string); virtual;
    procedure SetupDblClick; virtual;
    procedure SetupPopupMenu; virtual;
    procedure ShowEditorCallback({%H-}aSender: TObject); virtual;
    procedure DoSessionChange(aVersion: Integer);
  public
    // ------------------------------- inherited methods ----------------------------
    constructor Create(aOwner: TComponent); override;
    destructor Destroy; override;
    procedure Assign(aSource: TPersistent); override;
    // ------------------------------- new methods ----------------------------------
    /// add a new button related to an action
    function AddButton(const aCaption: string; aStyle: TToolButtonStyle;
      aImageIndex: Integer; aAction: TAction; aPopupMenu: TPopupMenu): TToolButton; overload;
    procedure RemoveButton(aButton: TToolButton); overload; virtual;
    /// remove all buttons
    procedure RemoveButtons;
    /// it shows the Editor to manage buttons/actions
    procedure ShowEditor;
    /// it resets SessionValues to the original design
    procedure RestoreSession;
    /// it keeps SessionValues from original design time
    // - used by Editor, when in design time
    property DefaultSessionValues: string read fDefaultSessionValues write fDefaultSessionValues;
  published
    // ------------------------------- new properties -------------------------------
    property Actions: TTisActionsCollection read fActions write fActions;
    property PopupMenus: TTisPopupMenusCollection read fPopupMenus write fPopupMenus;
    property EditorOptions: TTisEditorOptions read fEditorOptions write fEditorOptions default DefaultEditorOptions;
    property SessionValues: string read GetSessionValues write SetSessionValues stored False;
    property SessionVersion: Integer read fSessionVersion write fSessionVersion default DefaultSessionVersion;
    /// event that will fire after the session has changed
    // - you can use it to fix/add some buttons, popup, actions, or properties in general
    // that maybe do not exist in the SessionValues user machine
    property OnSessionChange: TOnSessionChange read fOnSessionChange write fOnSessionChange;
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
  if Assigned(fList) then
    fList.FreeNotification(GetOwnerAsActionsCollection.GetToolBar);
end;

function TTisActionsItem.GetDisplayName: string;
begin
  if Assigned(List) then
    result := '[' + List.Name + ']'
  else
    result := inherited GetDisplayName;
end;

function TTisActionsItem.GetOwnerAsActionsCollection: TTisActionsCollection;
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
      self.HiddenCategories := HiddenCategories;
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

constructor TTisActionsCollection.Create(aControl: TWinControl);
begin
  inherited Create(aControl, TTisActionsItem);
  fControl := aControl;
end;

function TTisActionsCollection.GetToolBar: TTisToolBar;
begin
  result := fControl as TTisToolBar;
end;

function TTisActionsCollection.LocateAction(const aOwnerName, aListName,
  aActionName: string): TAction;
var
  v1: Integer;
  vActions: TActionList;
begin
  result := nil;
  for v1 := 0 to Count -1 do
  begin
    vActions := Items[v1].List;
    if vActions = nil then
      continue;
    if (vActions.Owner.Name = aOwnerName) and (vActions.Name = aListName) then
    begin
      result := vActions.ActionByName(aActionName) as TAction;
      exit;
    end;
  end;
end;

{ TTisPopupMenusItem }

procedure TTisPopupMenusItem.SetAction(aValue: TAction);
begin
  if fAction = aValue then
    exit;
  fAction := aValue;
  if Assigned(fAction) then
    fAction.FreeNotification(GetPopupMenus.GetToolBar);
end;

procedure TTisPopupMenusItem.SetPopupMenu(aValue: TPopupMenu);
begin
  if fPopupMenu = aValue then
    exit;
  fPopupMenu := aValue;
  if Assigned(fPopupMenu) then
    fPopupMenu.FreeNotification(GetPopupMenus.GetToolBar);
end;

function TTisPopupMenusItem.GetDisplayName: string;
begin
  if Assigned(PopupMenu) then
    result := '[' + PopupMenu.Name + ']'
  else
    result := inherited GetDisplayName;
end;

function TTisPopupMenusItem.GetPopupMenus: TTisPopupMenusCollection;
begin
  result := GetOwner as TTisPopupMenusCollection;
end;

constructor TTisPopupMenusItem.Create(aCollection: TCollection);
begin
  inherited Create(aCollection);
end;

destructor TTisPopupMenusItem.Destroy;
begin
  inherited Destroy;
end;

procedure TTisPopupMenusItem.Assign(aSource: TPersistent);
begin
  if aSource is TTisPopupMenusItem then
    with aSource as TTisPopupMenusItem do
    begin
      self.Action := Action;
      self.PopupMenu := PopupMenu;
    end
  else
    inherited Assign(aSource);
end;

{ TTisPopupMenusCollection }

function TTisPopupMenusCollection.GetItems(aIndex: Integer): TTisPopupMenusItem;
begin
  result := TTisPopupMenusItem(inherited Items[aIndex]);
end;

procedure TTisPopupMenusCollection.SetItems(aIndex: Integer;
  aValue: TTisPopupMenusItem);
begin
  Items[aIndex].Assign(aValue);
end;

constructor TTisPopupMenusCollection.Create(aControl: TWinControl);
begin
  inherited Create(aControl, TTisPopupMenusItem);
  fControl := aControl;
end;

function TTisPopupMenusCollection.LocatePopupMenu(const aOwnerName,
  aPopuMenuName: string): TPopupMenu;
var
  v1: Integer;
  vPopup: TPopupMenu;
begin
  result := nil;
  for v1 := 0 to Count -1 do
  begin
    vPopup := Items[v1].PopupMenu;
    if vPopup = nil then
      Continue;
    if (vPopup.Owner.Name = aOwnerName) and (vPopup.Name = aPopuMenuName) then
    begin
      result := vPopup;
      exit;
    end;
  end;
end;

function TTisPopupMenusCollection.GetToolBar: TTisToolBar;
begin
  result := fControl as TTisToolBar;
end;

{ TTisToolBar }

procedure TTisToolBar.Loaded;
begin
  inherited Loaded;
  fDefaultSessionValues := SessionValues;
  if not (csDesigning in ComponentState) then
  begin
    SetupDblClick;
    SetupPopupMenu;
  end;
end;

procedure TTisToolBar.Notification(aComponent: TComponent;
  aOperation: TOperation);
var
  v1: Integer;
  vActionItem: TTisActionsItem;
begin
  inherited Notification(aComponent, aOperation);
  if aOperation = opRemove then
  begin
    for v1 := 0 to Actions.Count -1 do
    begin
      vActionItem := Actions.Items[v1];
      if vActionItem.List = aComponent then
        vActionItem.List := nil;
    end;
  end;
end;

function TTisToolBar.GetSessionValues: string;
var
  v1: Integer;
  vDoc, vDocButtons: TDocVariantData;
  vAction: TAction;
  vButton: TToolButton;
  vObj: Variant;
  vPopup: TPopupMenu;
  vCaption: TTranslateString;
begin
  vDocButtons.InitArray([], JSON_FAST_FLOAT);
  for v1 := 0 to ButtonCount -1 do
  begin
    vButton := Buttons[v1];
    vAction := vButton.Action as TAction;
    if Assigned(vAction) then
      vCaption := vAction.Caption
    else
      vCaption := vButton.Caption;
    vObj := _ObjFast([
      'caption', vCaption,
      'left', vButton.Left,
      'style', vButton.Style,
      'imageindex', vButton.ImageIndex
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
    vDocButtons.AddItem(vObj);
  end;
  // by default, the original list order is added by instance not by design
  // - session needs to save buttons by design order
  vDocButtons.SortArrayByField('left');
  vDoc.InitFast;
  vDoc.I['version'] := fSessionVersion;
  vDoc.A_['buttons']^ := vDocButtons;
  result := Utf8ToString(vDoc.ToJson);
end;

procedure TTisToolBar.SetSessionValues(const aValue: string);
var
  vDoc: TDocVariantData;
  vAction: TAction;
  vPopup: TPopupMenu;
  vObjButton: PDocVariantData;
  vObjAction, vObjPopup: PVariant;
  vCaption: TTranslateString;
begin
  if (csDesigning in ComponentState) or
   (GetSessionValues = aValue) then
    exit;
  RemoveButtons;
  try
    if not vDoc.InitJson(StringToUtf8(aValue), JSON_FAST_FLOAT) then
    begin
      // use default values, if aValue is invalid
      vDoc.InitJson(StringToUtf8(fDefaultSessionValues), JSON_FAST_FLOAT);
    end;
    if vDoc.I['version'] < DefaultSessionVersion then
    begin
      // old session there is no version
      RestoreSession;
    end
    else
      for vObjButton in vDoc.A_['buttons']^.Objects do
      begin
        if vObjButton^.GetAsPVariant('action', vObjAction) then
          vAction := Actions.LocateAction(vObjAction^.owner, vObjAction^.list, vObjAction^.name)
        else
          vAction := nil;
        if Assigned(vAction) then
          vCaption := vAction.Caption
        else
          vCaption := vObjButton^.S['caption'];
        if vObjButton^.GetAsPVariant('popup', vObjPopup) then
          vPopup := PopupMenus.LocatePopupMenu(vObjPopup^.owner, vObjPopup^.name)
        else
          vPopup := nil;
        AddButton(vCaption, TToolButtonStyle(vObjButton^.I['style']), vObjButton^.I['imageindex'], vAction, vPopup);
      end;
    DoSessionChange(vDoc.I['version']);
  except
    RestoreSession;
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

procedure TTisToolBar.ShowEditorCallback(aSender: TObject);
begin
  ShowEditor;
end;

procedure TTisToolBar.DoSessionChange(aVersion: Integer);
begin
  if Assigned(fOnSessionChange) then
    fOnSessionChange(self, aVersion);
end;

function TTisToolBar.AddButton(const aCaption: string;
  aStyle: TToolButtonStyle; aImageIndex: Integer; aAction: TAction; aPopupMenu: TPopupMenu): TToolButton;
begin
  result := TToolButton.Create(self);
  with result do
  begin
    // should be True, as the a (new) translated caption can be long
    AutoSize := True;
    if Assigned(aAction) then
      Action := aAction
    else
    begin
      Caption := aCaption;
      ImageIndex := aImageIndex;
    end;
    Style := aStyle;
    Left := self.Width;
    DropdownMenu := aPopupMenu;
    // show it in the target toolbar
    Parent := self;
  end;
end;

procedure TTisToolBar.RemoveButton(aButton: TToolButton);
begin
  ButtonList.Remove(aButton);
  RemoveControl(aButton);
end;

procedure TTisToolBar.RemoveButtons;
var
  v1: Integer;
begin
  for v1 := ButtonCount-1 downto 0 do
    RemoveControl(Buttons[v1]);
  ButtonList.Clear;
end;

constructor TTisToolBar.Create(aOwner: TComponent);
begin
  inherited Create(aOwner);
  fActions := TTisActionsCollection.Create(self);
  fPopupMenus := TTisPopupMenusCollection.Create(self);
  fEditorOptions := DefaultEditorOptions;
  fSessionVersion := DefaultSessionVersion;
end;

destructor TTisToolBar.Destroy;
begin
  fActions.Free;
  fPopupMenus.Free;
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
