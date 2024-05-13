// -----------------------------------------------------------------
//    This file is part of Tranquil IT Software
//    Copyright (C) 2012 - 2024  Tranquil IT https://www.tranquil.it
//    All Rights Reserved.
// ------------------------------------------------------------------
unit tis.ui.searchedit;

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
  Menus,
  mormot.core.variants,
  mormot.core.unicode,
  tis.ui.resourcestrings,
  tis.ui.parts.buttons;

type
  /// event when click in a button of the collection
  TOnButtonClick = procedure (Sender: TObject; aButton: TButtonItem) of object;

  /// event triggered before searching a text typed by the user
  TOnBeforeSearch = procedure(Sender: TObject; const aText: string; var aAbort: Boolean) of object;

  /// event triggered when user is searching a text
  TOnSearch = procedure(Sender: TObject; const aText: string) of object;

  /// component that allow user searching a typed text in asynchronous mode
  // - it will use an internal TTimer instance
  TTisSearchEdit = class(TComboBox, IButtonProperties)
  private
    fTimer: TTimer;
    fAutoSearch: Boolean;
    fButtons: TButtonCollection;
    fData: TDocVariantData;
    fLookupKeyField: string;
    fLookupDisplayField: string;
    fSearchMaxHistory: Integer;
    fOnButtonClick: TOnButtonClick;
    fOnBeforeSearch: TOnBeforeSearch;
    fOnSearch: TOnSearch;
    procedure SetDefault;
    procedure SetUpEdit;
    procedure SetData(aValue: TDocVariantData);
    function GetKeyValue: Variant;
    procedure SetKeyValue(aValue: Variant);
    // -------- Timer events begin --------
    function GetSearchInterval: Cardinal;
    procedure SetSearchInterval(aValue: Cardinal);
    function GetOnStartSearch: TNotifyEvent;
    procedure SetOnStartSearch(aValue: TNotifyEvent);
    function GetOnStopSearch: TNotifyEvent;
    procedure SetOnStopSearch(aValue: TNotifyEvent);
    // -------- Timer events end --------
  protected
    const DefaultSearchMaxHistory = 8;
    const DefaultSearchInterval = 300;
  protected
    // ------------------------------- inherited methods ----------------------------
    procedure Loaded; override;
    procedure SetParent(aNewParent: TWinControl); override;
    procedure SetSorted(aValue: boolean); override;
    procedure SetItemIndex(const aValue: integer); override;
    procedure DoSetBounds(aLeft, aTop, aWidth, aHeight: Integer); override;
    procedure Select; override;
    procedure DoAutoCompleteSelect; override;
    procedure FixDesignFontsPPI(const aDesignTimePPI: Integer); override;
    procedure ScaleFontsPPI(const aToPPI: Integer; const aProportion: Double); override;
    // ------------------------------- new methods ----------------------------------
    /// it will add aText for each new typing, if Data.IsVoid = TRUE
    procedure AddHistory(const aText: string); virtual;
    /// it triggers OnBeforeSearch event
    function DoBeforeSearch: Boolean; virtual;
    /// it triggers OnSearch event
    // - first it will test DoBeforeSearch result
    procedure DoSearch(Sender: TObject); virtual;
    /// it triggers button clicks
    procedure DoButtonClick(Sender: TObject); virtual;
    /// it implements IButtonProperties.Setup
    procedure Setup(aButton: TButtonItem); virtual;
    /// it implements a Popup menu for Clear buttons to clean all
    procedure SetupClearPopupMenu; virtual;
    /// callback to Popup menu to clear all
    procedure DoClearCallback(aSender: TObject);
  public
    // ------------------------------- inherited methods ----------------------------
    constructor Create(aOwner: TComponent); override;
    destructor Destroy; override;
    /// it will clear Items, Data and everything else related
    procedure Clear; override;
    /// it triggers RefreshSearch if aKey=#13, even if AutoSearch=FALSE
    procedure KeyPress(var aKey: Char); override;
    // ------------------------------- new methods ----------------------------------
    /// it triggers OnSearch event directly
    // - you might want to call RefreshSearch instead, for using AutoSearch flag behavior
    procedure Search; virtual;
    /// refresh items using Data content
    // - you should call LoadData, if you change Data content directly
    procedure LoadData; virtual;
    /// it will refresh the search
    // - if AutoSearch or aForceDelayed is TRUE, it will enable the timer, otherwise it will call Search directly
    procedure RefreshSearch(aForceDelayed: Boolean = False); virtual;
    /// it will sort Items and Data by LookupDisplayField
    procedure Sort; virtual;
    // ------------------------------- new properties -------------------------------
    /// direct access to the low-level internal data
    // - if you change its content directly, you should call LoadData for VirtualTree be aware about it
    property Data: TDocVariantData read fData write SetData;
    /// it will return the key field value from Data
    // - the ItemIndex will be use as index for Data array
    property KeyValue: Variant read GetKeyValue write SetKeyValue;
  published
    // ------------------------------- new properties -------------------------------
    /// if TRUE, it will start the Timer when user start typing
    property AutoSearch: Boolean read fAutoSearch write fAutoSearch default True;
    /// a collection of buttons
    property Buttons: TButtonCollection read fButtons write fButtons;
    property LookupKeyField: string read fLookupKeyField write fLookupKeyField;
    property LookupDisplayField: string read fLookupDisplayField write fLookupDisplayField;
    /// the max history items that it will keep
    property SearchMaxHistory: Integer read fSearchMaxHistory write fSearchMaxHistory default DefaultSearchMaxHistory;
    /// the interval of the internal Timer
    property SearchInterval: Cardinal read GetSearchInterval write SetSearchInterval default DefaultSearchInterval;
    // ------------------------------- new events -----------------------------------
    /// an event that will be trigger for bkCustom Kind buttons
    property OnButtonClick: TOnButtonClick read fOnButtonClick write fOnButtonClick;
    /// an event that will be trigger before start searching
    // - you have an option to abort the operation
    property OnBeforeSearch: TOnBeforeSearch read fOnBeforeSearch write fOnBeforeSearch;
    /// an event that will be trigger when the Timer starts
    property OnStartSearch: TNotifyEvent read GetOnStartSearch write SetOnStartSearch;
    /// an event that will call the user's algorithm for searching
    property OnSearch: TOnSearch read fOnSearch write fOnSearch;
    /// an event that will be trigger when the Timer stops
    property OnStopSearch: TNotifyEvent read GetOnStopSearch write SetOnStopSearch;
  end;

implementation

{ TTisSearchEdit }

procedure TTisSearchEdit.SetDefault;
begin
  Width := 130;
  Height := 24;
end;

procedure TTisSearchEdit.SetUpEdit;
begin
  Text := '';
  ControlStyle := ControlStyle - [csSetCaption];
end;

function TTisSearchEdit.GetSearchInterval: Cardinal;
begin
  result := fTimer.Interval;
end;

procedure TTisSearchEdit.SetSearchInterval(aValue: Cardinal);
begin
  fTimer.Interval := aValue;
end;

function TTisSearchEdit.GetOnStartSearch: TNotifyEvent;
begin
  result := fTimer.OnStartTimer;
end;

procedure TTisSearchEdit.SetOnStartSearch(aValue: TNotifyEvent);
begin
  fTimer.OnStartTimer := aValue;
end;

function TTisSearchEdit.GetOnStopSearch: TNotifyEvent;
begin
  result := fTimer.OnStopTimer;
end;

procedure TTisSearchEdit.SetOnStopSearch(aValue: TNotifyEvent);
begin
  fTimer.OnStopTimer := aValue;
end;

function TTisSearchEdit.GetKeyValue: Variant;
begin
  result := NULL;
  if fData.IsVoid then
    exit;
  if ItemIndex in [0..fData.Count-1] then
    result := _Safe(fData.Values[ItemIndex])^.Value[fLookupKeyField];
end;

procedure TTisSearchEdit.SetKeyValue(aValue: Variant);
begin
  ItemIndex := fData.SearchItemByProp(StringToUtf8(fLookupKeyField), VarToStr(aValue), True);
end;

procedure TTisSearchEdit.SetData(aValue: TDocVariantData);
begin
  if fData.Equals(aValue) then
    exit;
  fData := aValue;
  LoadData;
end;

procedure TTisSearchEdit.Loaded;
begin
  inherited Loaded;
  fButtons.Invalidate;
  if not (csDesigning in ComponentState) then
    SetupClearPopupMenu;
end;

procedure TTisSearchEdit.SetParent(aNewParent: TWinControl);
begin
  inherited SetParent(aNewParent);
  if csDestroying in ComponentState then
    exit;
  fButtons.Invalidate;
end;

procedure TTisSearchEdit.SetSorted(aValue: boolean);
begin
  inherited SetSorted(aValue);
  if aValue then
    Sort;
end;

procedure TTisSearchEdit.SetItemIndex(const aValue: integer);
begin
  inherited SetItemIndex(aValue);
  Change; // refresh to change KeyValue
end;

procedure TTisSearchEdit.DoSetBounds(aLeft, aTop, aWidth, aHeight: Integer);
begin
  inherited DoSetBounds(aLeft, aTop, aWidth, aHeight);
  if Assigned(fButtons) then
    fButtons.Invalidate;
end;

procedure TTisSearchEdit.Select;
begin
  inherited Select;
  RefreshSearch;
end;

procedure TTisSearchEdit.DoAutoCompleteSelect;
begin
  // do nothing
end;

procedure TTisSearchEdit.FixDesignFontsPPI(const aDesignTimePPI: Integer);
begin
  inherited FixDesignFontsPPI(aDesignTimePPI);
  fButtons.Invalidate;
end;

procedure TTisSearchEdit.ScaleFontsPPI(const aToPPI: Integer;
  const aProportion: Double);
begin
  inherited ScaleFontsPPI(aToPPI, aProportion);
  fButtons.Invalidate;
end;

procedure TTisSearchEdit.AddHistory(const aText: string);
begin
  if fData.IsVoid then
    AddHistoryItem(aText, fSearchMaxHistory, True,
      cbactSearchCaseSensitive in AutoCompleteText);
end;

function TTisSearchEdit.DoBeforeSearch: Boolean;
var
  vAborted: Boolean;
begin
  vAborted := False;
  if Assigned(fOnBeforeSearch) then
    fOnBeforeSearch(self, Text, vAborted);
  result := not vAborted;
end;

procedure TTisSearchEdit.DoSearch(Sender: TObject);
begin
  fTimer.Enabled := False;
  if DoBeforeSearch and Assigned(fOnSearch) then
    fOnSearch(self, Text);
  fTimer.Enabled := False;
end;

procedure TTisSearchEdit.DoButtonClick(Sender: TObject);
var
  vButton: TButtonItem;
begin
  vButton := fButtons.Items[(Sender as TComponent).Tag];
  case vButton.Kind of
    bkCustom:
      if Assigned(fOnButtonClick) then
        fOnButtonClick(self, vButton);
    bkSearch:
    begin
      RefreshSearch;
      AddHistory(Text);
    end;
    bkClear:
    begin
      Text := '';
      RefreshSearch;
    end;
  end;
end;

procedure TTisSearchEdit.Setup(aButton: TButtonItem);
begin
  aButton.Button.OnClick := @DoButtonClick;
end;

procedure TTisSearchEdit.SetupClearPopupMenu;
var
  v1: Integer;
  vMenuItem: TMenuItem;
  vButton: TButtonItem;
begin
  for v1 := 0 to fButtons.Count -1 do
  begin
    vButton := fButtons[v1];
    if vButton.Kind = bkClear then
    begin
      if not Assigned(vButton.Button.PopupMenu) then
        vButton.Button.PopupMenu := TPopupMenu.Create(self);
      if vButton.Button.PopupMenu.Items.Count > 0 then
      begin
        vMenuItem := TMenuItem.Create(self);
        vMenuItem.Caption := '-';
        vButton.Button.PopupMenu.Items.Add(vMenuItem);
      end;
      vMenuItem := TMenuItem.Create(self);
      vMenuItem.Tag := -1;
      vMenuItem.Caption := rsSearchEditClearAll;
      vMenuItem.OnClick := @DoClearCallback;
      vButton.Button.PopupMenu.Items.Add(vMenuItem);
    end;
  end;
end;

procedure TTisSearchEdit.DoClearCallback(aSender: TObject);
begin
  Clear;
end;

constructor TTisSearchEdit.Create(aOwner: TComponent);
begin
  inherited Create(aOwner);
  fTimer := TTimer.Create(nil);
  fTimer.Interval := DefaultSearchInterval;
  fTimer.Enabled := False;
  fTimer.OnTimer := @DoSearch;
  fButtons := TButtonCollection.Create(self);
  fAutoSearch := True;
  fSearchMaxHistory := DefaultSearchMaxHistory;
  SetDefault;
  SetUpEdit;
  Clear;
end;

destructor TTisSearchEdit.Destroy;
begin
  fTimer.Free;
  fButtons.Free;
  inherited Destroy;
end;

procedure TTisSearchEdit.Clear;
begin
  inherited Clear;
  fData.Clear;
  fData.InitArray([], JSON_FAST_FLOAT);
end;

procedure TTisSearchEdit.KeyPress(var aKey: Char);
begin
  inherited KeyPress(aKey);
  fTimer.Enabled := False;
  if aKey = #13 then
  begin
    SelectAll;
    RefreshSearch;
    AddHistory(Text);
  end
  else if aKey <> #0 then
    fTimer.Enabled := fAutoSearch;
end;

procedure TTisSearchEdit.Search;
begin
  DoSearch(self);
end;

procedure TTisSearchEdit.LoadData;
var
  vObj: PDocVariantData;
begin
  if not fData.IsVoid then
    inherited Clear; // clear Items
  if Sorted then
    Sort;
  for vObj in fData.Objects do
    Items.Add(vObj^.S[StringToUtf8(fLookupDisplayField)]);
end;

procedure TTisSearchEdit.RefreshSearch(aForceDelayed: Boolean);
begin
  fTimer.Enabled := False;
  if fAutoSearch or aForceDelayed then
    fTimer.Enabled := True
  else
    Search;
end;

procedure TTisSearchEdit.Sort;
begin
  fData.SortArrayByField(StringToUtf8(fLookupDisplayField));
end;

end.
