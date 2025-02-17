// -----------------------------------------------------------------
//    This file is part of Tranquil IT Software
//    Copyright (C) 2012 - 2024  Tranquil IT https://www.tranquil.it
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
  mormot.core.rtti,
  tis.ui.resourcestrings;

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
    // - if the Action has no OnExecute implemented, you should set DisableIfNoHandler property to False,
    // to enable the butto(s) that is(are) using this Action
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

  /// event that will trigger before SessionValues has changed
  TOnBeforeSessionValuesChange = procedure(aSender: TTisToolBar; aCurVersion, aNewVersion: Integer;
    var aHandled: Boolean) of object;

  /// event that will trigger after SessionValues has changed
  TOnAfterSessionValuesChange = procedure(aSender: TTisToolBar) of object;

  /// event that will trigger before show the Editor
  // - use aAbort argumento for do not open it
  TOnBeforeShowEditor = procedure(aSender: TTisToolBar; var aAbort: Boolean) of object;

  /// event that will trigger after close the Editor
  TOnAfterCloseEditor = procedure(aSender: TTisToolBar) of object;

  /// based on ToolBar, this is our version with steroids
  TTisToolBar = class(TToolBar)
  private
    fActions: TTisActionsCollection;
    fPopupMenus: TTisPopupMenusCollection;
    fEditorOptions: TTisEditorOptions;
    fCustomizeMenuItem: TMenuItem; // created at runtime to access the Editor
    fDesigntimeSessionValues: string;
    fRuntimeSessionValues: string;
    fSessionVersion: Integer;
    fOnBeforeSessionValuesChange: TOnBeforeSessionValuesChange;
    fOnAfterSessionValuesChange: TOnAfterSessionValuesChange;
    fOnBeforeShowEditor: TOnBeforeShowEditor;
    fOnAfterCloseEditor: TOnAfterCloseEditor;
  protected const
    // ------------------------------- new constants --------------------------------
    DefaultEditorOptions = [eoShowOnPopupMenu, eoAutoAddPopupMenus];
    DefaultSessionVersion = 1;
  protected
    // ------------------------------- inherited methods ----------------------------
    procedure Loaded; override;
    procedure Notification(aComponent: TComponent; aOperation: TOperation); override;
    procedure DoContextPopup(aMousePos: TPoint; var aHandled: Boolean); override;
    // ------------------------------- new methods ----------------------------------
    function GetSessionValues: string; virtual;
    procedure SetSessionValues(const aValue: string); virtual;
    procedure SetupDblClick; virtual;
    procedure SetupPopupMenu; virtual;
    procedure ShowEditorCallback({%H-}aSender: TObject); virtual;
    function DoBeforeSessionValuesChange(aCurVersion, aNewVersion: Integer): Boolean; virtual;
    procedure DoAfterSessionValuesChange; virtual;
    function DoBeforeShowEditor: Boolean; virtual;
    procedure DoAfterCloseEditor; virtual;
  public
    // ------------------------------- inherited methods ----------------------------
    constructor Create(aOwner: TComponent); override;
    destructor Destroy; override;
    procedure Assign(aSource: TPersistent); override;
    // ------------------------------- new methods ----------------------------------
    /// add a new button
    function AddButton(aStyle: TToolButtonStyle; const aCaption: TTranslateString = '';
      aImageIndex: Integer = -1; const aName: TComponentName = ''; aLeft: Integer = -1): TToolButton; overload; virtual;
    /// add a new button related to an action
    // - the button Style will be defined by arguments: if aPopupMenu was assigned
    // and the Action has OnExecute implemented, the style will be tbsDropDown,
    // otherwise it will be tbsButtonDrop
    // - if the Action has no OnExecute implemented, you should set DisableIfNoHandler property to False,
    // to enable the butto(s) that is(are) using this Action
    // - if aPopupMenu was not assigned, the Style will be tbsButton
    function AddButton(aAction: TAction; aPopupMenu: TPopupMenu = nil;
      const aName: TComponentName = ''; aLeft: Integer = -1): TToolButton; overload; virtual;
    /// remove a button
    // - if it is a design-time button, it will be set to invisible, otherwise it will be removed and disposed
    procedure RemoveButton(aButton: TToolButton); overload; virtual;
    /// remove all buttons
    procedure RemoveButtons;
    /// it shows the Editor to manage buttons/actions
    procedure ShowEditor;
    /// it restores SessionValues to the original design-time
    procedure RestoreSession;
    /// it resets SessionValues to the same SessionValues when the program started
    // - it can be useful when the program changes some Actions dynamically
    // but it should back to its original values
    // - it will be call automatically before showing the Editor
    procedure ResetSession;
    /// it keeps SessionValues from original design-time
    // - used by Editor, when in design-time
    property DesigntimeSessionValues: string read fDesigntimeSessionValues write fDesigntimeSessionValues;
    /// it saves SessionValues after changed
    // - used by Editor, after close it
    property RuntimeSessionValues: string read fRuntimeSessionValues write fRuntimeSessionValues;
  published
    // ------------------------------- new properties -------------------------------
    /// actions collection
    property Actions: TTisActionsCollection read fActions write fActions;
    /// action + popup collections
    property PopupMenus: TTisPopupMenusCollection read fPopupMenus write fPopupMenus;
    /// editor options
    // - use those options to setup how Editor will behave
    property EditorOptions: TTisEditorOptions read fEditorOptions write fEditorOptions default DefaultEditorOptions;
    /// use it to save the layout of the buttons on toolbar
    property SessionValues: string read GetSessionValues write SetSessionValues stored False;
    /// the SessionValues version
    // - it must be incremented when the developer changes and/or removes button names, actions,
    // popups, etc, which could break the compatibility in users machines - in case of the program
    // is storing and restoring SessionValues customized by user
    // - if the user version is minor than SessionVersion, the component will call RestoreSession, unless
    // OnBeforeSessionValuesChange has be implemented and handled
    property SessionVersion: Integer read fSessionVersion write fSessionVersion default DefaultSessionVersion;
    /// event that will trigger before SessionValues has changed
    // - it can be useful to fix/add/delete some actions and/or popusmenus collections
    // that maybe do not exist in the SessionValues user machine
    // - use aCurVersion to know the current version
    // - use aNewVersion to know the new version
    // - set aHandle=TRUE for the component do not automatically restore the buttons as it was designed
    // - see SessionVersion
    property OnBeforeSessionValuesChange: TOnBeforeSessionValuesChange read fOnBeforeSessionValuesChange write fOnBeforeSessionValuesChange;
    /// event that will trigger after SessionValues has changed
    // - it can be useful to change buttons styles, assigned new Actions, etc
    // after the Toolbar has read and recreates all the buttons from SessionValues
    property OnAfterSessionValuesChange: TOnAfterSessionValuesChange read fOnAfterSessionValuesChange write fOnAfterSessionValuesChange;
    /// event that will trigger before show the Editor
    // - use aAbort argumento for do not open it
    property OnBeforeShowEditor: TOnBeforeShowEditor read fOnBeforeShowEditor write fOnBeforeShowEditor;
    /// event that will trigger after close the Editor
    // - it can be useful return to a different state, if something was changed dynamically
    // like Actions, Styles, etc
    property OnAfterCloseEditor: TOnAfterCloseEditor read fOnAfterCloseEditor write fOnAfterCloseEditor;
  end;

implementation

uses
  tis.ui.toolbar.editor;

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
      self.List.Assign(List);
      self.HiddenCategories.Assign(HiddenCategories);
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
      self.Action.Assign(Action);
      self.PopupMenu.Assign(PopupMenu);
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
  fDesigntimeSessionValues := SessionValues;
  if not (csDesigning in ComponentState) then
  begin
    // do not setup options to show Editor, if there will be no Actions available
    if (eoAutoAddActions in fEditorOptions) or (fActions.Count > 0) then
    begin
      SetupDblClick;
      SetupPopupMenu;
    end;
  end;
end;

procedure TTisToolBar.Notification(aComponent: TComponent;
  aOperation: TOperation);
var
  v1: Integer;
  vActionItem: TTisActionsItem;
  vPopupItem: TTisPopupMenusItem;
begin
  inherited Notification(aComponent, aOperation);
  if (aOperation = opRemove) and not (csDestroying in ComponentState) then
  begin
    for v1 := 0 to Actions.Count -1 do
    begin
      vActionItem := Actions.Items[v1];
      if vActionItem.List = aComponent then
        vActionItem.List := nil;
    end;
    for v1 := 0 to PopupMenus.Count -1 do
    begin
      vPopupItem := PopupMenus.Items[v1];
      if vPopupItem.Action = aComponent then
        vPopupItem.Action := nil;
      if vPopupItem.PopupMenu = aComponent then
        vPopupItem.PopupMenu := nil;
    end;
  end;
end;

procedure TTisToolBar.DoContextPopup(aMousePos: TPoint; var aHandled: Boolean);
begin
  if Assigned(fCustomizeMenuItem) then
    fCustomizeMenuItem.Caption := rsToolBarCustomizeToolbar;
  inherited DoContextPopup(aMousePos, aHandled);
end;

function TTisToolBar.GetSessionValues: string;
var
  v1: Integer;
  vSessionDoc, vSessionButtons: TDocVariantData;
  vAction: TAction;
  vButton: TToolButton;
  vObj: Variant;
  vPopup: TPopupMenu;
  vCaption: TTranslateString;
begin
  vSessionButtons.InitArray([], JSON_FAST_FLOAT);
  for v1 := 0 to ButtonCount -1 do
  begin
    vButton := Buttons[v1];
    vAction := vButton.Action as TAction;
    if Assigned(vAction) then
      vCaption := vAction.Caption
    else
      vCaption := vButton.Caption;
    vObj := _ObjFast([
      'name', vButton.Name,
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
    vSessionButtons.AddItem(vObj);
  end;
  // by default, the original list order is added by instance not by design
  // - session needs to save buttons by design order
  vSessionButtons.SortArrayByField('left');
  vSessionDoc.InitFast;
  vSessionDoc.I['version'] := fSessionVersion;
  vSessionDoc.A_['buttons']^ := vSessionButtons;
  result := Utf8ToString(vSessionDoc.ToJson);
end;

procedure TTisToolBar.SetSessionValues(const aValue: string);
var
  vSessionDoc: TDocVariantData;

  procedure _SetupButtons;
    function GetButtonByAction(pAction: TAction): TToolButton;
    var
      vI: Integer;
    begin
      Result := nil;
      for vI := 0 to ButtonCount - 1 do
        if (Buttons[vI].Name = '') and (Buttons[vI].Action = pAction) then
          Exit(Buttons[vI]);
    end;

  var
    vSessionButton: PDocVariantData;
    vSessionAction, vSessionPopup: PVariant;
    vAction: TAction;
    vPopup: TPopupMenu;
    vCaption: TTranslateString;
    vStyle: TToolButtonStyle;
    vLeft: Integer;
    vName: string;
    vComponent: TComponent;
    vBtnAction: TToolButton;
  begin
    // create buttons on the toolbar
    for vSessionButton in vSessionDoc.A_['buttons']^.Objects do
    begin
      if vSessionButton^.GetAsPVariant('action', vSessionAction) then
        vAction := Actions.LocateAction(vSessionAction^.owner, vSessionAction^.list, vSessionAction^.name)
      else
        vAction := nil;
      if Assigned(vAction) then
        vCaption := vAction.Caption
      else
        vCaption := vSessionButton^.S['caption'];
      if vSessionButton^.GetAsPVariant('popup', vSessionPopup) then
        vPopup := PopupMenus.LocatePopupMenu(vSessionPopup^.owner, vSessionPopup^.name)
      else
        vPopup := nil;
      vStyle := TToolButtonStyle(vSessionButton^.I['style']);
      vLeft := vSessionButton^.I['left'];
      vName := vSessionButton^.S['name'];
      vComponent := Owner.FindComponent(vName);
      if Assigned(vComponent) and (vComponent is TToolButton) then
      begin
        with vComponent as TToolButton do
        begin
          // temporarily removed from the target to change its properties
          Parent := nil;
          Caption := vCaption;
          Action := vAction;
          DropdownMenu := vPopup;
          Style := vStyle;
          Left := vLeft;
          Visible := True;
          // show it in the target toolbar
          Parent := self;
        end;
      end
      else
      begin
        if Assigned(vAction) then
        begin
          vBtnAction := GetButtonByAction(vAction);
          if Assigned(vBtnAction) then
          begin
            vBtnAction.Parent := nil;
            vBtnAction.DropdownMenu := vPopup;
            vBtnAction.Style := vStyle;
            vBtnAction.Left := vLeft;
            vBtnAction.Visible := True;
            vBtnAction.Parent := self;
          end
          else
          begin
            AddButton(vAction, vPopup, vName, vLeft);
          end;
        end
        else
        begin
          AddButton(vStyle, vCaption, vSessionButton^.I['imageindex'], vName, vLeft);
        end;
      end;
    end;
  end;

begin
  if (csDesigning in ComponentState) then
    exit;
  RemoveButtons;
  try
    if not vSessionDoc.InitJson(StringToUtf8(aValue), JSON_FAST_FLOAT) then
    begin
      // use default values, if aValue is invalid
      vSessionDoc.InitJson(StringToUtf8(fDesigntimeSessionValues), JSON_FAST_FLOAT);
    end;
    // if user session version is greater than design-time version, do nothing,
    // it means it was customized
    if vSessionDoc.I['version'] < fSessionVersion then
    begin
      // fire an event that allow developer to fix something that could be missing
      // in user machine, as some Action, Popups, etc in the collections
      // - if it was not handled, it must restore for the default SessionValues
      if not DoBeforeSessionValuesChange(vSessionDoc.I['version'], fSessionVersion) then
      begin
        RestoreSession;
        exit;
      end;
      _SetupButtons;
      DoAfterSessionValuesChange;
    end
    else
      _SetupButtons;
    fRuntimeSessionValues := SessionValues;
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
    fCustomizeMenuItem := TMenuItem.Create(self);
    fCustomizeMenuItem.Tag := -1;
    fCustomizeMenuItem.OnClick := @ShowEditorCallback;
    fCustomizeMenuItem.Caption := rsToolBarCustomizeToolbar;
    PopupMenu.Items.Add(fCustomizeMenuItem);
  end;
end;

procedure TTisToolBar.ShowEditorCallback(aSender: TObject);
begin
  ShowEditor;
end;

function TTisToolBar.DoBeforeSessionValuesChange(aCurVersion, aNewVersion: Integer): Boolean;
begin
  result := False;
  if Assigned(fOnBeforeSessionValuesChange) then
    fOnBeforeSessionValuesChange(self, aCurVersion, aNewVersion, result);
end;

procedure TTisToolBar.DoAfterSessionValuesChange;
begin
  if Assigned(fOnAfterSessionValuesChange) then
    fOnAfterSessionValuesChange(self);
end;

function TTisToolBar.DoBeforeShowEditor: Boolean;
var
  vAbort: Boolean;
begin
  vAbort := False;
  if Assigned(fOnBeforeShowEditor) then
    fOnBeforeShowEditor(self, vAbort);
  result := not vAbort;
end;

procedure TTisToolBar.DoAfterCloseEditor;
begin
  if Assigned(fOnAfterCloseEditor) then
    fOnAfterCloseEditor(self);
end;

function TTisToolBar.AddButton(aStyle: TToolButtonStyle;
  const aCaption: TTranslateString; aImageIndex: Integer;
  const aName: TComponentName; aLeft: Integer): TToolButton;
begin
  result := TToolButton.Create(self.Owner);
  with result do
  begin
    Name := aName;
    // should be True, as maybe a translated caption could be longer
    AutoSize := True;
    Caption := aCaption;
    ImageIndex := aImageIndex;
    Style := aStyle;
    if aLeft > -1 then
      Left := aLeft
    else
      Left := self.Width;
    // show it on the toolbar
    Parent := self;
  end;
end;

function TTisToolBar.AddButton(aAction: TAction; aPopupMenu: TPopupMenu;
  const aName: TComponentName; aLeft: Integer): TToolButton;
begin
  result := AddButton(tbsButton, aAction.Caption, aAction.ImageIndex, aName, aLeft);
  with result do
  begin
    Action := aAction;
    DropdownMenu := aPopupMenu;
    if Assigned(DropdownMenu) then
    begin
      if Assigned(Action) and Assigned(Action.OnExecute) then
        Style := tbsDropDown
      else
        Style := tbsButtonDrop
    end;
  end;
end;

procedure TTisToolBar.RemoveButton(aButton: TToolButton);
begin
  if not Assigned(aButton) then
    exit;
  if aButton.Name <> '' then
  begin
    aButton.Visible := False;
    aButton.Parent := nil;
  end
  else
  begin
    aButton.Visible := False;
    aButton.Parent := nil;
  {  ButtonList.Remove(aButton);
    RemoveControl(aButton);
    FreeAndNil(aButton);    }
  end;
end;

procedure TTisToolBar.RemoveButtons;
var
  v1: Integer;
begin
  for v1 := ButtonCount-1 downto 0 do
    RemoveButton(Buttons[v1]);
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
      self.PopupMenus.Assign(PopupMenus);
      self.SessionValues := SessionValues;
      self.DesigntimeSessionValues := DesigntimeSessionValues;
      self.RuntimeSessionValues := RuntimeSessionValues;
    end
  else
    inherited Assign(aSource);
end;

procedure TTisToolBar.ShowEditor;
begin
  if DoBeforeShowEditor then
  begin
    // it is mandatory, as the program might change some actions, popups, etc, dynamically
    ResetSession;
    with TTisToolBarEditor.Create(Application) do
    try
      Target := self;
      ShowModal;
    finally
      Free;
    end;
    DoAfterCloseEditor;
  end;
end;

procedure TTisToolBar.RestoreSession;
begin
  SessionValues := fDesigntimeSessionValues;
end;

procedure TTisToolBar.ResetSession;
begin
  SessionValues := fRuntimeSessionValues;
end;

end.
