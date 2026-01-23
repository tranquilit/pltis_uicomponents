// -----------------------------------------------------------------
//    This file is part of Tranquil IT Software
//    Copyright (C) 2012 - 2026  Tranquil IT https://www.tranquil.it
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
  ImgList,
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

  /// event triggered when the combobox items are created from internal data
  TOnGetItemText = procedure(Sender: TObject; const aItem: TDocVariantData; var aText: string) of object;

  /// callback called when sorting the search edit data array
  TItemComparer = function(Sender: TObject; const V1, V2: TDocVariantData): PtrInt of object;

  /// callback allowing user to handle the completion by himself
  TOnGetCompletionText = function(Sender: TObject; const aText: string): string of object;
  /// callback allowing user to validate or not a match given the current text and a tried item
  // - Warning: sUserText is in UpperCase if CaseSensitive is False
  TOnIsMatchingText = function (const sCompareText, sUserText: string; CaseSensitive, OnlyPrefix: Boolean): Boolean of object;

  /// component that allow user searching a typed text in asynchronous mode
  // - it will use an internal TTimer instance

  { TTisSearchEdit }

  TTisSearchEdit = class(TComboBox, IButtonProperties)
  private
    fTimer: TTimer;
    fAutoSearch: Boolean;
    fButtons: TButtonCollection;
    fImageList: TCustomImageList;
    fData: TDocVariantData;
    fLookupKeyField: string;
    fLookupDisplayField: string;
    fSearchMaxHistory: Integer;
    fOnButtonClick: TOnButtonClick;
    fOnBeforeSearch: TOnBeforeSearch;
    fOnSearch: TOnSearch;
    fOnGetItemText: TOnGetItemText;
    fItemComparer: TItemComparer;
    fOnGetCompletionText: TOnGetCompletionText;
    fOnIsMatchingText: TOnIsMatchingText;
    fAllowMiddleAutoComplete: Boolean;
    fSelStart: Integer;
    procedure SetImageList(const aValue: TCustomImageList);
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
    function ApplyPreviewedTextCompletion: Boolean;
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
    procedure KeyUp(var Key: Word; Shift: TShiftState); override;
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
    /// workaround to apply the completion when leaving the edit
    procedure DoExit; override;
    procedure SetSelStart(Val: integer); override;
    /// wrapper to call user's comparison function with TDocVariantData instances
    function RowComparer(const V1, V2: Variant): PtrInt;
    /// callback to get the completed text based on the current user text
    function GetCompleteText(aText: string; iSelStart: Integer): string;
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
    /// it will sort Items and Data
    // - will call ItemComparer if assigned
    // - otherwise sort by LookupDisplayField
    procedure Sort; virtual;
    // ------------------------------- new properties -------------------------------
    /// direct access to the low-level internal data
    // - if you change its content directly, you should call LoadData for VirtualTree be aware about it
    property Data: TDocVariantData read fData write SetData;
    /// it will return the key field value from Data
    // - the ItemIndex will be use as index for Data array
    property KeyValue: Variant read GetKeyValue write SetKeyValue;
    /// it will return the object value from Data
    function KeyObject: PDocVariantData;
  published
    // ------------------------------- new properties -------------------------------
    /// if TRUE, it will start the Timer when user start typing
    property AutoSearch: Boolean read fAutoSearch write fAutoSearch default True;
    /// a collection of buttons
    property Buttons: TButtonCollection read fButtons write fButtons;
    property Images: TCustomImageList read fImageList write SetImageList;
    /// doc variant field used for KeyValue property
    property LookupKeyField: string read fLookupKeyField write fLookupKeyField;
    /// doc variant field used to populate Completion texts instead of OnGetItemText
    property LookupDisplayField: string read fLookupDisplayField write fLookupDisplayField;
    /// the max history items that it will keep
    property SearchMaxHistory: Integer read fSearchMaxHistory write fSearchMaxHistory default DefaultSearchMaxHistory;
    /// the interval of the internal Timer
    property SearchInterval: Cardinal read GetSearchInterval write SetSearchInterval default DefaultSearchInterval;
    /// whether completion must strictly be made from start of items
    // - Set this to False to allow matching in the middle of the item text
    property AllowMiddleAutoComplete: Boolean read fAllowMiddleAutoComplete write fAllowMiddleAutoComplete default False;
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
    /// an event that will call the user's custom text association method
    property OnGetItemText: TOnGetItemText read fOnGetItemText write fOnGetItemText;
    /// a callback used when sorting the items
    property ItemComparer: TItemComparer read fItemComparer write fItemComparer;
    /// an event that will call the user's algorithm for completion
    property OnGetCompletionText: TOnGetCompletionText read fOnGetCompletionText write fOnGetCompletionText;
    /// an event that will call the user's algorithm for completion item matching
    property OnIsMatchingText: TOnIsMatchingText read fOnIsMatchingText write fOnIsMatchingText;
  end;

implementation

uses
  LCLType,
  LCLProc,
  LazUTF8;

{ TTisSearchEdit }

procedure TTisSearchEdit.SetImageList(const aValue: TCustomImageList);
begin
  if fImageList = aValue then
    Exit;

  fImageList := aValue;
  fButtons.Images := fImageList;
  Self.Invalidate;
end;

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

function TTisSearchEdit.ApplyPreviewedTextCompletion: Boolean;
var
  aItem: String;
begin
  if not AllowMiddleAutoComplete then
    Exit;

  if fSelStart > 0 then
  begin
    aItem := Copy(Text, fSelStart + 1);
    Result := SelectItem(aItem);
    if Result then
      EditingDone;
    fSelStart := 0;
  end;
end;

function TTisSearchEdit.KeyObject: PDocVariantData;
begin
  result := nil;
  if fData.IsVoid then
    exit;
  if ItemIndex in [0..fData.Count-1] then
    result := _Safe(fData.Values[ItemIndex]);
end;

function TTisSearchEdit.GetKeyValue: Variant;
var
  aObject: PDocVariantData;
begin
  aObject := KeyObject;
  result := NULL;
  if not Assigned(aObject) then
    exit;
  result := aObject^.Value[fLookupKeyField];
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

procedure TTisSearchEdit.DoExit;
begin
  ApplyPreviewedTextCompletion;
  inherited DoExit;
end;

procedure TTisSearchEdit.SetSelStart(Val: integer);
begin
  inherited SetSelStart(Val);
  fSelStart := Val;
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
  fImageList := nil;
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
    ApplyPreviewedTextCompletion;
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
  aItemText: string;
begin
  if not fData.IsVoid then
    inherited Clear; // clear Items
  if Sorted then
    Sort;
  for vObj in fData.Objects do
  begin
    if fLookupDisplayField <> '' then
      aItemText := vObj^.S[StringToUtf8(fLookupDisplayField)]
    else
      aItemText := '';
    if Assigned(fOnGetItemText) then
      fOnGetItemText(Self, vObj^, aItemText);
    Items.Add(aItemText);
  end;
end;

procedure TTisSearchEdit.RefreshSearch(aForceDelayed: Boolean);
begin
  fTimer.Enabled := False;
  if fAutoSearch or aForceDelayed then
    fTimer.Enabled := True
  else
    Search;
end;

function TTisSearchEdit.RowComparer(const V1, V2: Variant): PtrInt;
begin
  Result := ItemComparer(Self, _Safe(V1)^, _Safe(V2)^);
end;

function TTisSearchEdit.GetCompleteText(aText: string; iSelStart: Integer): string;

  function IsMatchingUserText(const sCompareText, sUserText: string; OnlyPrefix: Boolean): Boolean;
  var
    sTempText: string;
  begin
    Result := False;
    if OnlyPrefix then
    begin
      if cbactSearchCaseSensitive in AutoCompleteText then
        Result := StartWithExact(sCompareText, sUserText)
      else
        Result := StartWith(sCompareText, sUserText);
    end else
    begin
      if cbactSearchCaseSensitive in AutoCompleteText then
        Result := Pos(sUserText, sCompareText) > 0
      else
        Result := PosI(PUtf8Char(sUserText), sCompareText) > 0;
    end;
  end;

var
  sPrefixText: string;
  Idx, i: Integer;
  OnlyPrefix, CaseSensitive: Boolean;
begin
  Result := aText;
  if aText = '' then
    Exit;
  // if assigned on get complete call it
  sPrefixText := UTF8Copy(aText, 1, iSelStart);
  if Assigned(OnGetCompletionText) then
  begin
    Result := OnGetCompletionText(Self, sPrefixText);
    Exit;
  end;

  CaseSensitive := cbactSearchCaseSensitive in AutoCompleteText;
  if not CaseSensitive then
    sPrefixText := UTF8UpperCase(sPrefixText);
  for i := 0 to Items.Count - 1 do
  begin
    Idx := i;
    if not (cbactSearchAscending in AutoCompleteText) then
      Idx := Items.Count - i - 1;
    if Assigned(OnIsMatchingText) then
    begin
      if not OnIsMatchingText(Items[Idx], sPrefixText, CaseSensitive, not AllowMiddleAutoComplete) then
        continue;
    end
    else if not IsMatchingUserText(Items[Idx], sPrefixText, not AllowMiddleAutoComplete) then
      continue;

    Result := Items[Idx];
    break;
  end;
end;

procedure TTisSearchEdit.Sort;
begin
  if Assigned(ItemComparer) then
    fData.SortByRow(@RowComparer)
  else
    fData.SortArrayByField(StringToUtf8(fLookupDisplayField));
end;

procedure TTisSearchEdit.KeyUp(var Key: Word; Shift: TShiftState);
var
  iSelStart: Integer; // char position
  sCompleteText, sPrefixText, sResultText: string;
  Utf8TextLen: Integer;
begin
  if Assigned(OnKeyUp) then OnKeyUp(Self, Key, Shift);
  //SelectAll when hitting return key for AutoSelect feature
  if (Key = VK_RETURN) then
  begin
    if ((cbactEnabled in AutoCompleteText) and Style.HasEditBox) then
    begin
      // Only happens with alpha-numeric keys and return key and editable Style
      SelectAll;
    end;
    if AutoSelect then
    begin
     SelectAll;
     if (SelText = Text) then AutoSelected := True;
    end;
  end
  else
  if ((cbactEnabled in AutoCompleteText) and Style.HasEditBox) then
  begin
    //Only happens with alpha-numeric keys and return key and editable Style
    //DebugLn(['TCustomComboBox.KeyUp ',Key,' ',IsEditableTextKey(Key)]);
    if IsEditableTextKey(Key) then
    begin
      iSelStart := SelStart;//Capture original cursor position
      //DebugLn(['TCustomComboBox.UTF8KeyPress SelStart=',SelStart,' Text=',Text]);
      //End of line completion
      Utf8TextLen := UTF8Length(Text);
      if (iSelStart < Utf8TextLen) and (cbactEndOfLineComplete in AutoCompleteText) then
        Exit;
      sPrefixText := UTF8Copy(Text, 1, iSelStart);
      sCompleteText := GetCompleteText(Text, iSelStart);
      if AllowMiddleAutoComplete and (sPrefixText <> sCompleteText) then
        sCompleteText := Format('%s%s', [sPrefixText, sCompleteText]);
      if (sCompleteText <> Text) or (Utf8TextLen = 1) then
      begin
        sResultText := sCompleteText;
        if (cbactEndOfLineComplete in AutoCompleteText)
        and (cbactRetainPrefixCase in AutoCompleteText)
        and not AllowMiddleAutoComplete then
        begin //Retain Prefix Character cases
          UTF8Delete(sResultText, 1, iSelStart);
        end;
        if Utf8TextLen = 1 then
          Text := '';
        Text := sResultText;
        SelStart := iSelStart;
        SelLength := UTF8Length(Text);
        DoAutoCompleteSelect;
      end;
    end;
  end;
end;

end.
