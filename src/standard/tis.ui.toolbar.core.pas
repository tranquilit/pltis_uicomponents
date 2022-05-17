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
  mormot.core.base,
  mormot.core.variants,
  mormot.core.unicode,
  mormot.core.rtti;

type
  /// define a item for the collection
  TActionsItem = class(TCollectionItem)
  private
    fList: TActionList;
    fHiddenCategories: TStrings;
    procedure SetHiddenCategories(aValue: TStrings);
  protected
    const DefaultReadOnly = False;
  public
    constructor Create(aCollection: TCollection); override;
    destructor Destroy; override;
    procedure Assign(aSource: TPersistent); override;
  published
    property List: TActionList read fList write fList;
    /// use this list to hide categories on Editor by name
    property HiddenCategories: TStrings read fHiddenCategories write SetHiddenCategories;
  end;

  /// actions collection
  TActionsCollection = class(TCollection)
  private
    fControl: TWinControl;
    function GetItems(aIndex: Integer): TActionsItem;
    procedure SetItems(aIndex: Integer; aValue: TActionsItem);
  protected
    // ------------------------------- inherited methods ----------------------------
    function GetOwner: TPersistent; override;
  public
    constructor Create(aControl: TWinControl); reintroduce;
    function LocateAction(const aListOwnerName, aListName, aActionName: string): TAction;
    /// items of the collection
    property Items[aIndex: Integer]: TActionsItem read GetItems write SetItems; default;
  end;

  TEditorOption = (
    /// user can double-click on toolbar to show editor
    // - only if OnDblClick event was not assigned
    eoShowOnDblClick,
    /// user can right-click to show a popup menu to access editor
    // - only if PopupMenu was not assigned
    eoShowOnPopupMenu
  );

  TEditorOptions = set of TEditorOption;

  TTisToolBar = class(TToolBar)
  private
    fActions: TActionsCollection;
    fEditorOptions: TEditorOptions;
    fDefaultSessionValues: string;
  protected
    const DefaultEditorOptions = [eoShowOnDblClick, eoShowOnPopupMenu];
  protected
    // ------------------------------- inherited methods ----------------------------
    procedure Loaded; override;
    // ------------------------------- new methods ----------------------------------
    function GetSessionValues: string; virtual;
    procedure SetSessionValues(const aValue: string); virtual;
    /// default implementation for OnDblClick event
    // - it will call ShowEditor
    procedure ShowEditorOnDblClick({%H-}sender: TObject); virtual;
  public
    // ------------------------------- inherited methods ----------------------------
    constructor Create(aOwner: TComponent); override;
    destructor Destroy; override;
    procedure Assign(aSource: TPersistent); override;
    // ------------------------------- new methods ----------------------------------
    /// add a new button related to an action
    procedure AddButton(aStyle: TToolButtonStyle; aAction: TAction); overload;
    /// remove all buttons
    procedure RemoveButtons;
    /// it shows the Editor to manage buttons vs. actions
    procedure ShowEditor;
    /// it resets SessionValues to the original design
    procedure RestoreSession;
  published
    // ------------------------------- new properties -------------------------------
    property Actions: TActionsCollection read fActions write fActions;
    property EditorOptions: TEditorOptions read fEditorOptions write fEditorOptions default DefaultEditorOptions;
    property SessionValues: string read GetSessionValues write SetSessionValues stored False;
  end;

implementation

uses
  tis.ui.toolbar.editor;

{ TActionsItem }

procedure TActionsItem.SetHiddenCategories(aValue: TStrings);
begin
  if (aValue <> nil) then
    fHiddenCategories.Assign(aValue);
end;

constructor TActionsItem.Create(aCollection: TCollection);
begin
  inherited Create(aCollection);
  fHiddenCategories := TTextStrings.Create;
end;

destructor TActionsItem.Destroy;
begin
  fHiddenCategories.Free;
  inherited Destroy;
end;

procedure TActionsItem.Assign(aSource: TPersistent);
begin
  if aSource is TActionsItem then
    with aSource as TActionsItem do
    begin
      self.List := List;
    end
  else
    inherited Assign(aSource);
end;

{ TActionsCollection }

function TActionsCollection.GetItems(aIndex: Integer): TActionsItem;
begin
  result := TActionsItem(inherited Items[aIndex]);
end;

procedure TActionsCollection.SetItems(aIndex: Integer; aValue: TActionsItem);
begin
  Items[aIndex].Assign(aValue);
end;

function TActionsCollection.GetOwner: TPersistent;
begin
  result := fControl;
end;

constructor TActionsCollection.Create(aControl: TWinControl);
begin
  inherited Create(TActionsItem);
  fControl := aControl;
end;

function TActionsCollection.LocateAction(const aListOwnerName, aListName,
  aActionName: string): TAction;
var
  i: Integer;
  l: TActionList;
begin
  result := nil;
  for i := 0 to Count -1 do
  begin
    l := Items[i].List;
    if (l.Owner.Name = aListOwnerName) and (l.Name = aListName) then
    begin
      result := l.ActionByName(aActionName) as TAction;
      exit;
    end;
  end;
end;

{ TTisToolBar }

procedure TTisToolBar.Loaded;
begin
  inherited Loaded;
  fDefaultSessionValues := SessionValues; // save default values
  if not (csDesigning in ComponentState) then
  begin
    if (not Assigned(OnDblClick)) and (eoShowOnDblClick in fEditorOptions) then
      OnDblClick := @ShowEditorOnDblClick;
  end;
end;

function TTisToolBar.GetSessionValues: string;
var
  d: TDocVariantData;
  i: Integer;
  a: TAction;
  b: TToolButton;
  o: Variant;
begin
  d.InitArray([], JSON_FAST_FLOAT);
  for i := 0 to ButtonCount -1 do
  begin
    b := Buttons[i];
    o := _ObjFast([
      'left', b.Left,
      'style', b.Style
    ]);
    a := b.Action as TAction;
    // checking all before use it as a valid action
    if (a <> nil) and (a.ActionList <> nil) and (a.ActionList.Owner <> nil) then
    begin
      o.action := _ObjFast([
        'owner', a.ActionList.Owner.Name,
        'list', a.ActionList.Name,
        'name', a.Name
      ]);
    end;
    d.AddItem(o);
  end;
  // by default, the original list order is by instance added, not by design order
  // - session needs to save the buttons design order
  d.SortArrayByField('left');
  result := Utf8ToString(d.ToJson);
end;

procedure TTisToolBar.SetSessionValues(const aValue: string);
var
  d: TDocVariantData;
  o, a: PDocVariantData;
begin
  if (csDesigning in ComponentState) or
   (GetSessionValues = aValue) or
   (Actions.Count = 0) then
    exit;
  RemoveButtons;
  try
    if not d.InitJson(StringToUtf8(aValue), JSON_FAST_FLOAT) then
      d.InitJson(StringToUtf8(fDefaultSessionValues), JSON_FAST_FLOAT); // use default values, if aValue is invalid
    for o in d.Objects do
    begin
      a := o^.O_['action'];
      AddButton(
        TToolButtonStyle(o^.I['style']),
        Actions.LocateAction(a^.S['owner'], a^.S['list'], a^.S['name'])
      );
    end;
  except
    // shows exception only in designtime
    if csDesigning in ComponentState then
      Application.HandleException(self)
    else
      // it should not happen... but if did, do not show exception for users
  end;
end;

procedure TTisToolBar.ShowEditorOnDblClick(sender: TObject);
begin
  ShowEditor;
end;

procedure TTisToolBar.AddButton(aStyle: TToolButtonStyle;
  aAction: TAction);
begin
  with TToolButton.Create(self) do
  begin
    Parent := self;
    Action := aAction;
    Style := aStyle;
    AutoSize := True;
    Left := Parent.Width;
  end;
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
  fActions := TActionsCollection.Create(self);
  fEditorOptions := DefaultEditorOptions;
end;

destructor TTisToolBar.Destroy;
begin
  fActions.Free;
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
