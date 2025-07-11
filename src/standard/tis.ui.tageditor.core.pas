// - modified, improved, and Lazarus converted version (2021) by Tranquil IT
// - modified and improved version (2021) by Daniel C. Dávila - https://github.com/daviladanielc/Delphi_TagEditor
// - first version (2014) by Andreas Rejbrand - https://specials.rejbrand.se/dev/controls/tageditor/

unit tis.ui.tageditor.core;

{$i tis.ui.defines.inc}

interface

uses
  {$ifdef windows}
  Windows,
  {$endif}
  Messages,
  SysUtils,
  Classes,
  Controls,
  StdCtrls,
  Forms,
  Graphics,
  Types,
  Menus,
  Dialogs,
  StrUtils,
  LMessages,
  LCLIntf,
  LCLType,
  TisStrings;

type
  ETagEditor = class(Exception);

  TClickInfo = cardinal;
  TTagIndex = word;

  TTagContext = record
    CanDelete: Boolean;
    BgColor: TColor;
    BorderColor: TColor;
    Value: Variant;
    TextColor: TColor;
  end;

  TTags = class;
  TTisTagEditor = class;

  TTagItem = class(TCollectionItem)
  private
    fCanDelete: Boolean;
    fBgColor: TColor;
    fBorderColor: TColor;
    fValue: Variant;
    fTextColor: TColor;
    fText: string;
    function GetTagEditor: TTisTagEditor;
    procedure UpdateTagEditor;
    procedure SetCanDelete(const aValue: Boolean);
    procedure SetBgColor(const aValue: TColor);
    procedure SetBorderColor(const aValue: TColor);
    procedure SetText(const aValue: string);
    procedure SetTextColor(const aValue: TColor);
  protected
    procedure SetCollection(aValue: TCollection); override;
  public
    constructor Create(aCollection: TCollection); override;
    procedure Assign(aSource: TPersistent); override; overload;
    procedure Assign(aSource: TTagContext); overload;
  published
    property CanDelete: Boolean read fCanDelete write SetCanDelete;
    property BgColor: TColor read fBgColor write SetBgColor;
    property BorderColor: TColor read fBorderColor write SetBorderColor;
    property Value: Variant read fValue write fValue;
    property Text: string read fText write SetText;
    property TextColor: TColor read fTextColor write SetTextColor;
  end;

  TTagItemClass = class of TTagItem;

  TTags = class(TCollection)
  private
    fTagEditor: TTisTagEditor;
    function GetDelimitedText: string;
    procedure SetDelimitedText(const aValue: string);
    function GetTagItem(Index: Integer): TTagItem;
    procedure SetTagItem(Index: Integer; const Value: TTagItem);
  protected
    function GetOwner: TPersistent; override;
  public
    constructor Create(aTagEditor: TTisTagEditor; aTagsItemClass: TTagItemCLass);
    procedure Assign(aSource: TPersistent); override;
    function IndexOf(const aText: string): Integer;
    function Add(aContext: TTagContext; const aText: string = ''): TTagItem; overload;
    function Add(const aText: string = ''): TTagItem; overload;
    procedure DeleteAll;
    property DelimitedText: string read GetDelimitedText write SetDelimitedText;
    property Items[Index: Integer]: TTagItem read GetTagItem write SetTagItem; default;
    property TagEditor: TTisTagEditor read fTagEditor;
  end;

  /// event to execute some code when the user clicks on a tag
  // - use aTag to know which tag was clicked
  TOnTagClick = procedure(Sender: TObject; aTag: TTagItem) of object;

  /// event that occurs before adding a new tag
  // - use aAbort to stop inclusion
  TOnTagBeforeAdd = procedure(Sender: TObject; const aTag: string; var aAbort: Boolean) of object;

  /// event that occurs after adding a new tag
  // - use aTag to change some properties
  TOnTagAfterAdd = procedure(Sender: TObject; aTag: TTagItem) of object;

  /// event that occurs before deleting a new tag
  // - use aAbort to stop deletion
  TOnTagBeforeDelete = procedure(Sender: TObject; aTag: TTagItem; var aAbort: Boolean) of object;

  /// event that occurs after draging a tag
  // - use aTag to change some properties
  // - use aPreIndex abd aNewIndex to know the old and new positions
  TOnTagAfterDrag = procedure (Sender: TObject; aTag: TTagItem; aPreIndex, aNewIndex: Integer) of object;

  /// input options
  TInputOption = (
    ioAllowDragging,
    ioAllowDuplicates,
    ioAllowLeadingSpace,
    ioShowDeleteButton,
    ioTrimText
  );
  TInputOptions = set of TInputOption;

  /// properties and events for ComboBox that shows up inside the editor
  TTagComboBoxOptions = class(TPersistent)
  private
    fAutoDropDown: Boolean;
    fAutoComplete: TComboBoxAutoCompleteText;
    fItems: TStrings;
    fItemWidth: Integer;
    fSorted: Boolean;
    fStyle: TComboBoxStyle;
    fOnDrawItem: TDrawItemEvent;
    fOnMeasureItem: TMeasureItemEvent;
    procedure SetItems(aValue: TStrings);
  protected const
    DefaultAutoComplete = StdCtrls.DefaultComboBoxAutoCompleteText;
  public
    constructor Create;
    destructor Destroy; override;
  published
    // ------- properties compatible with TComboBox properties-----------------
    property AutoDropDown: Boolean read fAutoDropDown write fAutoDropDown default False;
    property AutoComplete: TComboBoxAutoCompleteText
      read fAutoComplete write fAutoComplete default DefaultAutoComplete;
    property Items: TStrings read fItems write SetItems;
    property ItemWidth: Integer read fItemWidth write fItemWidth default 0;
    property Sorted: Boolean read fSorted write fSorted default False;
    /// BUG related with TComboBox: using Style=csSimple typing ENTER will not
    // add a new tag, use TAB instead
    // - if you have Items, ENTER will not choose a item on the list either, if
    // AutoComplete has cbactEnabled
    // - it is better using Sytle=csDropDown with default AutoComplete, if you
    // have a list of items
    property Style: TComboBoxStyle read fStyle write fStyle default csSimple;
    // ------- events compatible with TComboBox events-------------------------
    property OnDrawItem: TDrawItemEvent read fOnDrawItem write fOnDrawItem;
    property OnMeasureItem: TMeasureItemEvent read fOnMeasureItem write fOnMeasureItem;
  end;

  /// tag input properties and events
  TTagInput = class(TPersistent)
  private
    fDeleteIcon: TIcon;
    fDelimiterChars: string;
    fForbiddenChars: string;
    fMaxTags: Integer;
    fOptions: TInputOptions;
    procedure SetDeleteIcon(aValue: TIcon);
    procedure SetDelimiterChars(const aValue: string);
  protected const
    DefaultForbiddenChars = '= !@|():&%$/\[]<>*+?`¨''';
    DefaultDelimiterChars = ',;';
    DefaultOptions = [ioAllowDragging, ioShowDeleteButton, ioTrimText];
    DefaultMaxTags = 0;
  public
    constructor Create;
    destructor Destroy; override;
    /// the first char in DelimiterChars property
    // - it will be used to return tags as strings in Tags.DelimitedText
    function DefaultDelimiter: Char; virtual;
  published
    property DeleteIcon: TIcon read fDeleteIcon write SetDeleteIcon;
    property DelimiterChars: string read fDelimiterChars write SetDelimiterChars;
    property ForbiddenChars: string read fForbiddenChars write fForbiddenChars;
    property MaxTags: Integer read fMaxTags write fMaxTags default DefaultMaxTags;
    property Options: TInputOptions read fOptions write fOptions default DefaultOptions;
  end;

  /// an editor for tags
  // - tags can be created at runtime by typing, of course :)
  // - tags can be created by using Tags property
  // - custom colors can be defined for each tag
  // - for each tag a "hidden" Variant value can be added
  // - allows drag and drop tags among them
  // - each tag can have a "X" that allows you to delete the tag with 1 click
  // - a custom icon can be defined to replace the "X"
  // - custom events, such Before/After add/delete, for handling user actions
  // - input properties to define forbidden chars, max tags, allow duplicates, etc
  // - autocomplete with properties do define items, sorted, style, etc

  { TTisTagEditor }

  TTisTagEditor = class(TCustomControl)
  private
    fActualTagHeight: Integer;
    fAdding: Boolean;
    fAutoHeight: Boolean;
    fBgColor: TColor;
    fBgColorDisabled: TColor;
    fBorderColor: TColor;
    fBorderColorDisabled: TColor;
    fCaretVisible: Boolean;
    fLefts, fRights, fWidths, fTops, fBottoms: array of Integer;
    FOnCloseUp: TNotifyEvent;
    FOnDropDown: TNotifyEvent;
    fCloseBtnLefts, fCloseBtnTops: array of Integer;
    fCloseBtnWidth: Integer;
    fDesiredHeight: Integer;
    fDragging: Boolean;
    fEditorColor: TColor;
    fEditPos: TPoint;
    fTagComboBox: TTagComboBoxOptions;
    fTagInput: TTagInput;
    fMaxHeight: Integer;
    fMouseDownClickInfo: TClickInfo;
    fMultiLine: Boolean;
    fTags: TTags;
    fNumRows: Integer;
    fPopupMenu: TPopupMenu;
    fPrevScrollPos: Integer;
    fReadOnly: Boolean;
    fSavedReadOnly: Boolean;
    fScrollBarVisible: Boolean;
    fScrollInfo: TScrollInfo;
    fShrunk: Boolean;
    fSpacing: Integer;
    fTagBgColor: TColor;
    fTagBgColorDisabled: TColor;
    fTagBorderColor: TColor;
    fTagHeight: Integer;
    fTagRoundBorder: Integer;
    fTagTextColor: TColor;
    fOnTagClick: TOnTagClick;
    fOnTagBeforeAdd: TOnTagBeforeAdd;
    fOnTagAfterAdd: TOnTagAfterAdd;
    fOnTagBeforeDelete: TOnTagBeforeDelete;
    fOnTagAfterDrag: TOnTagAfterDrag;
    fOnChange: TNotifyEvent;
    function GetClickInfoAt(X, Y: Integer): TClickInfo;
    function GetReadOnly: Boolean;
    function GetSeparatorIndexAt(X, Y: Integer): Integer;
    function GetShrunkClientRect(const Amount: Integer): TRect;
    function IsFirstOnRow(TagIndex: Integer): Boolean; inline;
    function IsLastOnRow(TagIndex: Integer): Boolean;
    procedure CreateCaret;
    procedure DestroyCaret;
    procedure DrawFocusRect;
    procedure DoPopupMenuDeleteItem(Sender: TObject);
    procedure SetAutoHeight(const Value: Boolean);
    procedure SetBgColor(const Value: TColor);
    procedure SetBgColorDisabled(aValue: TColor);
    procedure SetBorderColor(const Value: TColor);
    procedure SetBorderColorDisabled(aValue: TColor);
    procedure SetMaxHeight(const Value: Integer);
    procedure SetMultiLine(const Value: Boolean);
    procedure SetTags(const Value: TTags);
    procedure SetReadOnly(const Value: Boolean);
    procedure SetSpacing(const Value: Integer);
    procedure SetTagBgColor(const Value: TColor);
    procedure SetTagBgColorDisabled(aValue: TColor);
    procedure SetTagBorderColor(const Value: TColor);
    procedure SetTagHeight(const Value: Integer);
    procedure SetTagRoundBorder(const Value: Integer);
    procedure SetTagTextColor(const Value: TColor);
    function GetAsArray: TStringArray;
    procedure SetAsArray(aValue: TStringArray);
    procedure HideComboBox;
    procedure ShowComboBox;
    procedure FixPosAndScrollWindow;
    procedure UpdateMetrics;
    procedure UpdateScrollBars;
  protected const
    DefaultSpacing = 8;
    DefaultMaxHeight = 512;
    DefaultTagHeight = 32;
  protected
    fComboBox: TComboBox;
    // ------------------------------- inherited methods ----------------------------
    procedure Loaded; override;
    procedure CreateParams(var Params: TCreateParams); override;
    procedure KeyDown(var Key: word; Shift: TShiftState); override;
    procedure KeyPress(var Key: Char); override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X: Integer;
      Y: Integer); override;
    procedure MouseMove(Shift: TShiftState; X: Integer; Y: Integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X: Integer;
      Y: Integer); override;
    procedure Paint; override;
    procedure WndProc(var Message: TMessage); override;
    // ------------------------------- new methods ----------------------------------
    procedure ComboBoxEnter(Sender: TObject); virtual;
    procedure ComboBoxExit(Sender: TObject); virtual;
    procedure ComboBoxKeyPress(Sender: TObject; var Key: Char); virtual;
    procedure ComboBoxEditingDone(Sender: TObject); virtual;
    procedure ComboBoxEditingDropDown(Sender: TObject); virtual;
    procedure ComboBoxEditingCloseUp(Sender: TObject); virtual;
    function NewTags(aTagEditor: TTisTagEditor): TTags; virtual;
    function NewComboBox: TComboBox; virtual;
    function NewPopupMenu: TPopupMenu; virtual;
    procedure LoadComboBox; virtual;
    /// add a new tag
    // - returns TRUE if tag was included
    function AddTag(const aText: string): Boolean; virtual;
    /// delete a tag using its index
    procedure DeleteTag(aTagIndex: Integer); virtual;
    /// event implementation for OnTagBeforeAdd
    function DoTagBeforeAdd(const aTag: string): Boolean; virtual;
    /// event implementation for OnTagAfterAdd
    procedure DoTagAfterAdd(aTag: TTagItem); virtual;
    /// event implementation for OnChange
    procedure DoChange; virtual;
    /// event implementation for OnTagAfterDrag
    procedure DoAfterDrag(aPreIndex, aNewIndex: Integer); virtual;
  public const
    clBgColorDisabled = $00F0F0F0;
    clBorderColorDisabled = clSilver;
  public
    constructor Create(aOwner: TComponent); override;
    procedure Assign(aSource: TPersistent); override;
    destructor Destroy; override;
    /// it will clear all tags
    // - it will not perform any event, such as OnTagBeforeAdd, OnTagBeforeDelete
    procedure Clear; virtual;
  published
    // ------------------------------- inherited properties ------------------------
    property Anchors;
    property Align;
    property BorderSpacing;
    property Cursor;
    property Enabled;
    property TabOrder;
    property TabStop;
    property Tag;
    // ------------------------------- new properties ------------------------------
    /// use this property to get/set tags as array
    // - if you change its value, it will trigger all events related with adding and deleting tags
    property AsArray: TStringArray read GetAsArray write SetAsArray;
    property AutoHeight: Boolean read fAutoHeight write SetAutoHeight;
    property BgColor: TColor read fBgColor write SetBgColor default clWindow;
    property BgColorDisabled: TColor read fBgColorDisabled write SetBgColorDisabled default clBgColorDisabled;
    property BorderColor: TColor read fBorderColor write SetBorderColor default clWindowFrame;
    property BorderColorDisabled: TColor read fBorderColorDisabled write SetBorderColorDisabled default clBorderColorDisabled;
    property EditorColor: TColor read fEditorColor write fEditorColor default clWindow;
    property MaxHeight: Integer read fMaxHeight write SetMaxHeight default DefaultMaxHeight;
    property MultiLine: Boolean read fMultiLine write SetMultiLine default False;
    property ReadOnly: Boolean read GetReadOnly write SetReadOnly default False;
    property Spacing: Integer read fSpacing write SetSpacing default DefaultSpacing;
    property TagComboBox: TTagComboBoxOptions read fTagComboBox write fTagComboBox;
    property TagInput: TTagInput read fTagInput write fTagInput;
    property TagBgColor: TColor read fTagBgColor write SetTagBgColor default clSkyBlue;
    /// use this property to change the default behavior when Enabled is FALSE
    // - if clDefault, the component will darkening the BgColor in all tags
    // - otherwise, the color chose will be used for all tags
    property TagBgColorDisabled: TColor read fTagBgColorDisabled write SetTagBgColorDisabled default clDefault;
    property TagBorderColor: TColor read fTagBorderColor write SetTagBorderColor default clNavy;
    property TagHeight: Integer read fTagHeight write SetTagHeight default DefaultTagHeight;
    property TagRoundBorder: Integer read fTagRoundBorder write SetTagRoundBorder default 0;
    property TagTextColor: TColor read fTagTextColor write SetTagTextColor default clWhite;
    property Tags: TTags read fTags write SetTags;
    // ------------------------------- new events ----------------------------------
    /// event to execute some code when the user clicks on a tag
    // - use aTag to know which tag was clicked
    property OnTagClick: TOnTagClick read fOnTagClick write fOnTagClick;
    /// event that occurs before adding a new tag
    // - use aAbort to stop inclusion
    property OnTagBeforeAdd: TOnTagBeforeAdd read fOnTagBeforeAdd write fOnTagBeforeAdd;
    /// event that occurs after adding a new tag
    // - use aTag to change some properties
    property OnTagAfterAdd: TOnTagAfterAdd read fOnTagAfterAdd write fOnTagAfterAdd;
    /// event that occurs before deleting a new tag
    // - use aAbort to stop deletion
    property OnTagBeforeDelete: TOnTagBeforeDelete read fOnTagBeforeDelete write fOnTagBeforeDelete;
    /// event that occurs after draging a tag
    // - use aTag to change some properties
    // - use aPreIndex abd aNewIndex to know the old and new positions
    property OnTagAfterDrag: TOnTagAfterDrag read fOnTagAfterDrag write fOnTagAfterDrag;
    /// event that occurs after changing something
    property OnChange: TNotifyEvent read fOnChange write fOnChange;

    property OnDropDown: TNotifyEvent read FOnDropDown write FOnDropDown;
    property OnCloseUp: TNotifyEvent read FOnCloseUp write FOnCloseUp;

    property OnEnter;
    property OnExit;
    property OnKeyDown;
    property OnKeyPress;
    property OnKeyUp;
    property OnMouseDown;
    property OnMouseEnter;
    property OnMouseLeave;
    property OnMouseMove;
    property OnMouseUp;
    property OnMouseWheel;
    property OnMouseWheelDown;
    property OnMouseWheelUp;
  end;

implementation

uses
  Math, Clipbrd;

const
  TAG_LOW = 0;
  TAG_HIGH = MAXWORD - 2;
  EDITOR = MAXWORD - 1;
  NOWHERE = MAXWORD;
  PART_BODY = $00000000;
  PART_REMOVE_BUTTON = $00010000;

function IsKeyDown(const VK: Integer): Boolean;
begin
  IsKeyDown := GetKeyState(VK) and $8000 <> 0;
end;

function GetTagPart(ClickInfo: TClickInfo): cardinal;
begin
  result := ClickInfo and $FFFF0000;
end;

procedure SafeDrawFocusRect(hDC: hDC; const R: TRect);
var
  vOldBkColor, vOldTextColor: COLORREF;
begin
  vOldBkColor := SetBkColor(hDC, clWhite);
  vOldTextColor := SetTextColor(hDC, clBlack);
  DrawFocusRect(hDC, R);
  if vOldBkColor <> CLR_INVALID then
    SetBkColor(hDC, vOldBkColor);
  if vOldTextColor <> CLR_INVALID then
    SetTextColor(hDC, vOldTextColor);
end;

/// round color to white or black
function GetBlackOrWhite(aValue: TColor): TColor;
begin
  if ((GetRValue(longword(aValue)) * 2) +
    (GetGValue(longword(aValue)) * 3) +
    (GetBValue(longword(aValue)) * 2)) < 1000 then
    result := clWhite else
    result := clBlack;
end;

/// returns a darkening color
function GetColorDisabled(aValue: TColor): TColor;
var
  r, g, b: Byte;
begin
  r := GetRValue(aValue);
  g := GetGValue(aValue);
  b := GetBValue(aValue);
  result := RGB(
    r - MulDiv(r, 50, 100),
    g - MulDiv(g, 50, 100),
    b - MulDiv(b, 50, 100)
  );
end;

{ TTagComboBoxOptions }

procedure TTagComboBoxOptions.SetItems(aValue: TStrings);
begin
  if (aValue <> fItems) then
    fItems.Assign(aValue);
end;

constructor TTagComboBoxOptions.Create;
begin
  inherited Create;
  fAutoComplete := DefaultAutoComplete;
  fItems := TStringlist.Create;
  fStyle := csSimple;
end;

destructor TTagComboBoxOptions.Destroy;
begin
  fItems.Free;
  inherited Destroy;
end;

{ TTagItem }

function TTagItem.GetTagEditor: TTisTagEditor;
begin
  if Assigned(Collection) and (Collection is TTags) then
    result := TTags(Collection).fTagEditor
  else
    result := nil;
end;

procedure TTagItem.UpdateTagEditor;
var
  vEditor: TTisTagEditor;
begin
  vEditor := GetTagEditor;
  if assigned(vEditor) then
  begin
    vEditor.Invalidate;
  end;
end;

procedure TTagItem.SetCanDelete(const aValue: Boolean);
begin
  fCanDelete := aValue;
end;

procedure TTagItem.SetBgColor(const aValue: TColor);
begin
  if fBgColor <> aValue then
  begin
    fBgColor := aValue;
    TextColor := GetBlackOrWhite(fBgColor);
    UpdateTagEditor;
  end;
end;

procedure TTagItem.SetBorderColor(const aValue: TColor);
begin
  if fBorderColor <> aValue then
  begin
    fBorderColor := aValue;
    UpdateTagEditor;
  end;
end;

procedure TTagItem.SetText(const aValue: string);
begin
  if fText <> aValue then
  begin
    fText := aValue;
    UpdateTagEditor;
  end;
end;

procedure TTagItem.SetTextColor(const aValue: TColor);
begin
  if fTextColor <> aValue then
  begin
    fTextColor := aValue;
    UpdateTagEditor;
  end;
end;

procedure TTagItem.SetCollection(aValue: TCollection);
begin
  inherited SetCollection(aValue);
  UpdateTagEditor;
end;

constructor TTagItem.Create(aCollection: TCollection);
begin
  inherited Create(aCollection);
  fCanDelete := ioShowDeleteButton in GetTagEditor.TagInput.Options;
  fBgColor := GetTagEditor.fTagBgColor;
  fTextColor := GetTagEditor.fTagTextColor;
end;

procedure TTagItem.Assign(aSource: TPersistent);
begin
  if aSource is TTagItem then
  begin
    with TTagItem(aSource) do
    begin
      self.CanDelete := CanDelete;
      self.BgColor := BgColor;
      self.BorderColor := BorderColor;
      self.Value := Value;
      self.Text := Text;
      self.TextColor := TextColor;
    end;
  end
  else
    inherited Assign(aSource);
end;

procedure TTagItem.Assign(aSource: TTagContext);
begin
  CanDelete := aSource.CanDelete;
  BgColor := aSource.BgColor;
  BorderColor := aSource.BorderColor;
  Value := aSource.Value;
  TextColor := aSource.TextColor;
end;

{ TTags }

function TTags.GetDelimitedText: string;
var
  i: Integer;
begin
  result := '';
  for i := 0 to Count - 1 do
    result := result + IfThen(result <> '', TagEditor.TagInput.DefaultDelimiter) + Items[i].Text;
end;

procedure TTags.SetDelimitedText(const aValue: string);
var
  i: Integer;
  a: TStringArray;
begin
  Clear;
  a := aValue.Split(fTagEditor.TagInput.DefaultDelimiterChars.ToCharArray);
  for i := low(a) to high(a) do
    Add(a[i]);
end;

function TTags.GetTagItem(Index: Integer): TTagItem;
begin
  result := TTagItem(inherited Items[Index]);
end;

procedure TTags.SetTagItem(Index: Integer; const Value: TTagItem);
begin
  Items[Index].Assign(Value);
end;

function TTags.GetOwner: TPersistent;
begin
  result := fTagEditor;
end;

constructor TTags.Create(aTagEditor: TTisTagEditor;
  aTagsItemClass: TTagItemCLass);
begin
  inherited Create(aTagsItemClass);
  fTagEditor := aTagEditor;
end;

procedure TTags.Assign(aSource: TPersistent);
var
  i: Integer;
  s: string;
begin
  Clear;
  if aSource is TTags then
  begin
    with TTags(aSource) do
      for i := 0 to Count-1 do
        self.Add.Assign(Items[i])
  end
  else if aSource is TStrings then
  begin
    for s in TStrings(aSource) do
      self.Add(s)
  end
  else
    inherited Assign(aSource);
end;

function TTags.IndexOf(const aText: string): Integer;
var
  i: Integer;
begin
  result := -1;
  for i := 0 to Self.Count - 1 do
  begin
    if Self.Items[i].Text = aText then
    begin
      result := i;
      break;
    end;
  end;
end;

function TTags.Add(aContext: TTagContext; const aText: string): TTagItem;
begin
  result := TTagItem(inherited Add);
  result.Assign(aContext);
  result.Text := aText;
end;

function TTags.Add(const aText: string): TTagItem;
begin
  result := TTagItem(inherited Add);
  result.Text := aText;
  result.CanDelete := ioShowDeleteButton in fTagEditor.TagInput.Options;
  result.BgColor := fTagEditor.fTagBgColor;
  result.BorderColor := fTagEditor.fTagBorderColor;
  result.TextColor := fTagEditor.fTagTextColor;
end;

procedure TTags.DeleteAll;
begin
  while Self.Count > 0 do
    Self.Delete(0);
end;

{ TTagInput }

procedure TTagInput.SetDeleteIcon(aValue: TIcon);
begin
  if aValue <> nil then
  begin
    if (not(InRange(aValue.Height, 8, 10))) and (not(InRange(aValue.Width, 8, 10))) then
      raise ETagEditor.Create('The icon size should be 8x8 or 10x10');
    fDeleteIcon.Assign(aValue);
  end;
end;

procedure TTagInput.SetDelimiterChars(const aValue: string);
begin
  if fDelimiterChars = aValue then
    exit;
  if Trim(aValue) = '' then
    fDelimiterChars := DefaultDelimiterChars
  else
    fDelimiterChars := aValue;
end;

constructor TTagInput.Create;
begin
  inherited Create;
  fDeleteIcon := TIcon.Create;
  fDelimiterChars := DefaultDelimiterChars;
  fForbiddenChars := DefaultForbiddenChars;
  fMaxTags := DefaultMaxTags;
  fOptions := DefaultOptions;
end;

destructor TTagInput.Destroy;
begin
  fDeleteIcon.Free;
  inherited Destroy;
end;

function TTagInput.DefaultDelimiter: Char;
begin
  result := fDelimiterChars[1];
end;

{ TTagEditor }

constructor TTisTagEditor.Create(aOwner: TComponent);
begin
  inherited Create(aOwner);
  Left := 48;
  Height := 47;
  Top := 48;
  Width := 221;
  fMouseDownClickInfo := NOWHERE;
  fTagComboBox := TTagComboBoxOptions.Create;
  fTagInput := TTagInput.Create;
  fComboBox := NewComboBox;
  fTags := NewTags(Self);
  fPopupMenu := NewPopupMenu;
  fBgColor := clWindow;
  fBgColorDisabled := clBgColorDisabled;
  fBorderColor := clWindowFrame;
  fBorderColorDisabled := clBorderColorDisabled;
  fTagBgColor := clSkyBlue;
  fTagBgColorDisabled := clDefault;
  fTagBorderColor := clNavy;
  fSpacing := DefaultSpacing;
  fTagTextColor := clWhite;
  fMultiLine := False;
  fTagHeight := DefaultTagHeight;
  fShrunk := False;
  fEditorColor := clWindow;
  fMaxHeight := DefaultMaxHeight;
  fCaretVisible := False;
  fDragging := False;
  fPrevScrollPos := 0;
  fScrollInfo.cbSize := sizeof(fScrollInfo);
  fScrollBarVisible := False;
  TabStop := True;
end;

procedure TTisTagEditor.Assign(aSource: TPersistent);
begin
  if aSource is TTisTagEditor then
  begin
    with TTisTagEditor(aSource) do
      self.Tags.Assign(Tags);
  end
  else
    inherited Assign(aSource);
end;

destructor TTisTagEditor.Destroy;
begin
  fTagComboBox.Free;
  fTagInput.Free;
  fTags.Free;
  fTags := nil;
  fPopupMenu.Free;
  fComboBox.Free;
  inherited Destroy;
end;

procedure TTisTagEditor.Clear;
begin
  fTags.Clear;
  DoChange;
end;

procedure TTisTagEditor.DoPopupMenuDeleteItem(Sender: TObject);
begin
  if Sender is TMenuItem then
    DeleteTag(TMenuItem(Sender).Tag);
end;

procedure TTisTagEditor.WndProc(var Message: TMessage);
begin
  case Message.Msg of
    WM_LBUTTONDBLCLK:
      begin
        if csCaptureMouse in ControlStyle then
          MouseCapture := True;
        if csClickEvents in ControlStyle then
          DblClick;
      end;
    WM_SETFOCUS:
      Invalidate;
    WM_KILLFOCUS:
      begin
        if fCaretVisible then
          DestroyCaret;
        fDragging := False;
        Invalidate;
      end;
    WM_COPY:
      Clipboard.AsText := fTags.DelimitedText;
    LM_CLEAR:
      fTags.Clear;
    WM_CUT:
      begin
        Clipboard.AsText := fTags.DelimitedText;
        fTags.DeleteAll;
      end;
    WM_PASTE:
      begin
        if Clipboard.HasFormat(CF_TEXT) then
          fTags.DelimitedText := Clipboard.AsText;
      end;
    WM_SIZE:
      begin
        Invalidate;
        Message.result := 0;
      end;
    WM_VSCROLL:
      begin
        fScrollInfo.fMask := SIF_ALL;
        GetScrollInfo(Handle, SB_VERT, fScrollInfo);
        case Message.WParam of
          SB_TOP:
            fScrollInfo.nPos := fScrollInfo.nMin;
          SB_BOTTOM:
            fScrollInfo.nPos := fScrollInfo.nMax;
          SB_PAGEUP:
            Dec(fScrollInfo.nPos, fScrollInfo.nPage);
          SB_PAGEDOWN:
            Inc(fScrollInfo.nPos, fScrollInfo.nPage);
          SB_LINEUP:
            Dec(fScrollInfo.nPos, fTagHeight);
          SB_LINEDOWN:
            Inc(fScrollInfo.nPos, fTagHeight);
          SB_THUMBTRACK:
            fScrollInfo.nPos := fScrollInfo.nTrackPos;
        end;
        FixPosAndScrollWindow;
        Message.result := 0;
      end;
  end;
  inherited WndProc(Message);
end;

procedure TTisTagEditor.ComboBoxEnter(Sender: TObject);
begin
  if fEditPos.Y + fComboBox.Height > fScrollInfo.nPos + ClientHeight then
    fScrollInfo.nPos := fEditPos.Y + ClientHeight - fComboBox.Height;
  FixPosAndScrollWindow;
end;

procedure TTisTagEditor.ComboBoxExit(Sender: TObject);
begin
  if fComboBox.Text <> '' then
    AddTag(fComboBox.Text);
  HideComboBox;
end;

procedure TTisTagEditor.ComboBoxKeyPress(Sender: TObject; var Key: Char);
begin
  if ((Key = chr(VK_SPACE)) and (fComboBox.Text = '') and not (ioAllowLeadingSpace in fTagInput.Options)) or
    (Pos(Key, fTagInput.ForbiddenChars) > 0) then
  begin
    Key := #0;
    exit;
  end;
  if Pos(Key, fTagInput.DelimiterChars) > 0  then
  begin
    Key := #0;
    ComboBoxEditingDone(fComboBox);
    exit;
  end;
  case ord(Key) of
    VK_BACK:
      begin
        if (fComboBox.Text = '') and (fTags.Count > 0) then
          DeleteTag(fTags.Count-1);
      end;
    VK_ESCAPE:
      begin
        HideComboBox;
        self.SetFocus;
      end;
  end;
end;

procedure TTisTagEditor.ComboBoxEditingDone(Sender: TObject);
begin
  if not fComboBox.Visible then
    exit; // it could be invisible, if user has typed VK_ESCAPE
  if fComboBox.Text <> '' then
  begin
    AddTag(fComboBox.Text);
    ShowComboBox;
  end;
end;

procedure TTisTagEditor.ComboBoxEditingDropDown(Sender: TObject);
begin
  If Assigned(fComboBox) and Assigned(fOnDropDown) then
    fOnDropDown(fComboBox);
end;

procedure TTisTagEditor.ComboBoxEditingCloseUp(Sender: TObject);
begin
  If Assigned(fComboBox) and Assigned(fOnDropDown) then
    FOnCloseUp(fComboBox);
end;

function TTisTagEditor.NewTags(aTagEditor: TTisTagEditor): TTags;
begin
  result := TTags.Create(aTagEditor, TTagItem);
end;

function TTisTagEditor.NewComboBox: TComboBox;
begin
  result := TComboBox.Create(self);
  result.Top := 0;
  result.Left := 0;
  result.Width := 0;
  result.Parent := self;
  result.BorderStyle := bsNone;
  result.Visible := False;
  result.BorderWidth := 0;
  result.OnKeyPress := @ComboBoxKeyPress;
  result.OnEnter := @ComboBoxEnter;
  result.OnExit := @ComboBoxExit;
  result.OnEditingDone := @ComboBoxEditingDone;
  result.OnDropDown := @ComboBoxEditingDropDown;
  result.OnCloseUp := @ComboBoxEditingCloseUp;

end;

function TTisTagEditor.NewPopupMenu: TPopupMenu;
var
  vMenuItem: TMenuItem;
begin
  result := TPopupMenu.Create(Self);
  vMenuItem := TMenuItem.Create(PopupMenu);
  vMenuItem.Caption := 'Delete';
  vMenuItem.OnClick := @DoPopupMenuDeleteItem;
  vMenuItem.Hint := 'Delete selected tag.';
  result.Items.Add(vMenuItem);
end;

procedure TTisTagEditor.LoadComboBox;
begin
  with fTagComboBox do
  begin
    fComboBox.AutoDropDown := AutoDropDown;
    fComboBox.AutoComplete := cbactEnabled in AutoComplete;
    fComboBox.AutoCompleteText := AutoComplete;
    fComboBox.Items.Assign(Items);
    fComboBox.ItemWidth := ItemWidth;
    fComboBox.Sorted := Sorted;
    fComboBox.Style := Style;
    fComboBox.OnDrawItem := OnDrawItem;
    fComboBox.OnMeasureItem := OnMeasureItem;
  end;
end;

function TTisTagEditor.AddTag(const aText: string): Boolean;
var
  vContext: TTagContext;
  s: string;
begin
  result := False;
  // do not continue, if it is in adding mode already
  // - the event combo.OnExit could be trigger, if it loses its focus
  if fAdding then
    exit;
  fAdding := True;
  try
    if (fTags.Count = fTagInput.MaxTags) and (fTagInput.MaxTags > 0) then
      exit;
    s := aText;
    if ioTrimText in fTagInput.Options then
      s := Trim(aText);
    if (s = '') or ((not (ioAllowDuplicates in fTagInput.Options)) and (fTags.IndexOf(s) <> -1)) then
    begin
      beep;
      exit;
    end;
    if DoTagBeforeAdd(s) then
    begin
      vContext.CanDelete := ioShowDeleteButton in fTagInput.Options;
      vContext.BgColor := fTagBgColor;
      vContext.BorderColor := fTagBorderColor;
      vContext.TextColor := fTagTextColor;
      DoTagAfterAdd(fTags.Add(vContext, s));
      result := True;
      DoChange;
    end;
  finally
    fAdding := False;
  end;
end;

procedure TTisTagEditor.DeleteTag(aTagIndex: Integer);
var
  vAborted: Boolean;
begin
  if Assigned(fOnTagBeforeDelete) then
  begin
    vAborted := False;
    fOnTagBeforeDelete(self, fTags.Items[aTagIndex], vAborted);
    if vAborted then
      exit;
  end;
  fTags.Delete(aTagIndex);
  DoChange;
end;

function TTisTagEditor.DoTagBeforeAdd(const aTag: string): Boolean;
var
  vAborted: Boolean;
begin
  result := True;
  if assigned(fOnTagBeforeAdd) then
  begin
    vAborted := False;
    fOnTagBeforeAdd(self, aTag, vAborted);
    result := not vAborted;
  end;
end;

procedure TTisTagEditor.DoTagAfterAdd(aTag: TTagItem);
begin
  if assigned(fOnTagAfterAdd) then
    fOnTagAfterAdd(self, aTag);
end;

procedure TTisTagEditor.DoChange;
begin
  if assigned(fOnChange) then
    fOnChange(self);
  Invalidate;
end;

procedure TTisTagEditor.DoAfterDrag(aPreIndex, aNewIndex: Integer);
begin
  if assigned(fOnTagAfterDrag) then
  begin
    fOnTagAfterDrag(self, fTags.Items[aNewIndex], aPreIndex, aNewIndex);
    DoChange;
  end;
end;

procedure TTisTagEditor.FixPosAndScrollWindow;
var
  r: TRect;
begin
  fScrollInfo.fMask := SIF_POS;
  SetScrollInfo(Handle, SB_VERT, fScrollInfo, True);
  GetScrollInfo(Handle, SB_VERT, fScrollInfo);
  if fScrollInfo.nPos <> fPrevScrollPos then
  begin
    r := GetShrunkClientRect(3);
    ScrollWindowEx(Handle, 0, fPrevScrollPos - fScrollInfo.nPos,
      @r, @r, 0, nil, SW_INVALIDATE);
    fPrevScrollPos := fScrollInfo.nPos;
    Update;
  end;
end;

procedure TTisTagEditor.UpdateScrollBars;
begin
  fScrollInfo.fMask := SIF_RANGE or SIF_PAGE;
  fScrollInfo.nMin := 0;
  fScrollInfo.nMax := IfThen(fMultiLine, fDesiredHeight - 1, 0);
  fScrollInfo.nPage := ClientHeight;
  SetScrollInfo(Handle, SB_VERT, fScrollInfo, True);
  FixPosAndScrollWindow;
end;

procedure TTisTagEditor.Loaded;
begin
  inherited Loaded;
  LoadComboBox;
end;

procedure TTisTagEditor.KeyDown(var Key: word; Shift: TShiftState);
begin
  inherited;
  case Key of
    VK_END:
      ShowComboBox;
    VK_DELETE:
      Perform(LM_CLEAR, 0, 0);
    VK_INSERT:
      Perform(WM_PASTE, 0, 0);
    VK_F2:
      ShowComboBox;
  end;
end;

procedure TTisTagEditor.KeyPress(var Key: Char);
begin
  inherited;
  case Key of
    ^C:
      begin
        Perform(WM_COPY, 0, 0);
        Key := #0;
        exit;
      end;
    ^X:
      begin
        Perform(WM_CUT, 0, 0);
        Key := #0;
        exit;
      end;
    ^V:
      begin
        Perform(WM_PASTE, 0, 0);
        Key := #0;
        exit;
      end;
  end;
  ShowComboBox;
  fComboBox.Perform(WM_CHAR, ord(Key), 0);
end;

function TTisTagEditor.GetClickInfoAt(X, Y: Integer): TClickInfo;
var
  i: Integer;
begin
  result := NOWHERE;
  if (X >= fEditPos.X) and (Y >= fEditPos.Y) then
    exit(EDITOR);

  for i := 0 to fTags.Count - 1 do
    if InRange(X, fLefts[i], fRights[i]) and InRange(Y, fTops[i], fBottoms[i])
    then
    begin
      result := i;
      if InRange(X, fCloseBtnLefts[i], fCloseBtnLefts[i] + fCloseBtnWidth) and
        InRange(Y, fCloseBtnTops[i], fCloseBtnTops[i] + fActualTagHeight) and
        not fShrunk then
        result := result or PART_REMOVE_BUTTON;
      break;
    end;
end;

function TTisTagEditor.GetReadOnly: Boolean;
begin
  result := fReadOnly;
end;

function TTisTagEditor.IsFirstOnRow(TagIndex: Integer): Boolean;
begin
  result := (TagIndex = 0) or (fTops[TagIndex] > fTops[TagIndex - 1]);
end;

function TTisTagEditor.IsLastOnRow(TagIndex: Integer): Boolean;
begin
  result := (TagIndex = fTags.Count - 1) or
    (fTops[TagIndex] < fTops[TagIndex + 1]);
end;

function TTisTagEditor.GetSeparatorIndexAt(X, Y: Integer): Integer;
var
  i: Integer;
begin
  result := fTags.Count;
  Y := Max(Y, fSpacing + 1);
  for i := fTags.Count - 1 downto 0 do
  begin
    if Y < fTops[i] then
      Continue;
    if (IsLastOnRow(i) and (X >= fRights[i])) or
      ((X < fRights[i]) and (IsFirstOnRow(i) or (fRights[i - 1] < X))) then
    begin
      result := i;
      if (IsLastOnRow(i) and (X >= fRights[i])) then
        Inc(result);
      exit;
    end;
  end;
end;

procedure TTisTagEditor.MouseDown(Button: TMouseButton; Shift: TShiftState;
  X: Integer; Y: Integer);
begin
  Inc(Y, fScrollInfo.nPos);
  fMouseDownClickInfo := GetClickInfoAt(X, Y);
  if TTagIndex(fMouseDownClickInfo) <> EDITOR then
    SetFocus;
end;

procedure TTisTagEditor.CreateCaret;
begin
  if not fCaretVisible then
    fCaretVisible := LCLIntf.CreateCaret(Handle, 0, 0, fActualTagHeight);
end;

procedure TTisTagEditor.DestroyCaret;
begin
  if not fCaretVisible then
    exit;
  LCLIntf.DestroyCaret(Handle);
  fCaretVisible := False;
end;

procedure TTisTagEditor.CreateParams(var Params: TCreateParams);
begin
  inherited;
  Params.Style := Params.Style or WS_VSCROLL;
end;

procedure TTisTagEditor.MouseMove(Shift: TShiftState; X: Integer; Y: Integer);
var
  vSepIndex: Integer;
begin
  inherited;
  Inc(Y, fScrollInfo.nPos);
  if IsKeyDown(VK_LBUTTON) and InRange(TTagIndex(fMouseDownClickInfo),
    TAG_LOW, TAG_HIGH) and (ioAllowDragging in fTagInput.Options) then
  begin
    fDragging := True;
    Screen.Cursor := crDrag;
    vSepIndex := GetSeparatorIndexAt(X, Y);
    CreateCaret;
    if vSepIndex = fTags.Count then
      SetCaretPos(fLefts[vSepIndex - 1] + fWidths[vSepIndex - 1] + fSpacing div 2,
        fTops[vSepIndex - 1] - fScrollInfo.nPos)
    else
      SetCaretPos(fLefts[vSepIndex] - fSpacing div 2,
        fTops[vSepIndex] - fScrollInfo.nPos);
    ShowCaret(Handle);
    exit;
  end;
  case TTagIndex(GetClickInfoAt(X, Y)) of
    NOWHERE:
      Cursor := crArrow;
    EDITOR:
      Cursor := crIBeam;
    TAG_LOW .. TAG_HIGH:
      Cursor := crHandPoint;
  end;
end;

procedure TTisTagEditor.MouseUp(Button: TMouseButton; Shift: TShiftState;
  X: Integer; Y: Integer);
var
  i: word;
  vTagPart: cardinal;
  vScreenPoint: TPoint;
  vInfo: TClickInfo;
  vSepIndex: Integer;
  vOldPos, vNewPos: TTagIndex;
begin
  inherited;
  Inc(Y, fScrollInfo.nPos);
  if fDragging then
  begin
    DestroyCaret;
    fDragging := False;
    Screen.Cursor := crDefault;
    vSepIndex := GetSeparatorIndexAt(X, Y);
    if not InRange(vSepIndex, TTagIndex(fMouseDownClickInfo),
      TTagIndex(fMouseDownClickInfo) + 1) then
    begin
      vOldPos := TTagIndex(fMouseDownClickInfo);
      vNewPos := TTagIndex(vSepIndex - IfThen(vSepIndex > TTagIndex(fMouseDownClickInfo), 1, 0));
      fTags.Move(vOldPos, vNewPos);
      DoAfterDrag(vOldPos, vNewPos);
      DoChange;
    end;
    exit;
  end;
  vInfo := GetClickInfoAt(X, Y);
  if vInfo <> fMouseDownClickInfo then
    exit;
  i := TTagIndex(vInfo);
  vTagPart := GetTagPart(vInfo);
  case i of
    EDITOR:
      ShowComboBox;
    NOWHERE:
      ;
  else
    case Button of
      mbLeft:
        begin
          case vTagPart of
            PART_BODY:
              if Assigned(fOnTagClick) then
                fOnTagClick(Self, fTags.Items[i]);
            PART_REMOVE_BUTTON:
              begin
                if not (ioShowDeleteButton in fTagInput.Options) then
                  exit;
                DeleteTag(i);
                Paint;
              end;
          end;
        end;
      mbRight:
        begin
          fPopupMenu.Items[0].Tag := i;
          vScreenPoint := ClientToScreen(Point(X, Y));
          fPopupMenu.Items[0].Caption := 'Delete "' + fTags.Items[i].Text + '"';
          fPopupMenu.Popup(vScreenPoint.X, vScreenPoint.Y - fScrollInfo.nPos);
        end;
    end;
  end;
end;

procedure TTisTagEditor.UpdateMetrics;
const
  cCloseText = 'x';
var
  i, x, y: Integer;
  vMeanWidth: Integer;
  vAdjustedDesiredHeight: Integer;
begin
  SetLength(fLefts, fTags.Count);
  SetLength(fRights, fTags.Count);
  SetLength(fTops, fTags.Count);
  SetLength(fBottoms, fTags.Count);
  SetLength(fWidths, fTags.Count);
  SetLength(fCloseBtnLefts, fTags.Count);
  SetLength(fCloseBtnTops, fTags.Count);
  fCloseBtnWidth := Canvas.TextWidth(cCloseText);
  fShrunk := False;
  fNumRows := 1;
  if fMultiLine then
  begin
    fActualTagHeight := fTagHeight;
    x := fSpacing;
    y := fSpacing;
    for i := 0 to fTags.Count - 1 do
    begin
      fWidths[i] := Canvas.TextWidth(fTags.Items[i].Text +
        IfThen(ioShowDeleteButton in fTagInput.Options, ' ' + cCloseText, '')) + 2 * fSpacing;
      fLefts[i] := x;
      fRights[i] := x + fWidths[i];
      fTops[i] := y;
      fBottoms[i] := y + fTagHeight;
      if x + fWidths[i] + fSpacing > ClientWidth then
      begin
        x := fSpacing;
        Inc(y, fTagHeight + fSpacing);
        Inc(fNumRows);
        fLefts[i] := x;
        fRights[i] := x + fWidths[i];
        fTops[i] := y;
        fBottoms[i] := y + fTagHeight;
      end;
      fCloseBtnLefts[i] := x + fWidths[i] - fCloseBtnWidth - fSpacing;
      fCloseBtnTops[i] := y;
      Inc(x, fWidths[i] + fSpacing);
    end;
  end
  else
  begin
    fActualTagHeight := ClientHeight - 2 * fSpacing;
    x := fSpacing;
    y := fSpacing;
    for i := 0 to fTags.Count - 1 do
    begin
      fWidths[i] := Canvas.TextWidth(fTags.Items[i].Text +
        IfThen(ioShowDeleteButton in fTagInput.Options, ' ' + cCloseText, '')) + 2 * fSpacing;
      fLefts[i] := x;
      fRights[i] := x + fWidths[i];
      fTops[i] := y;
      fBottoms[i] := y + fActualTagHeight;
      Inc(x, fWidths[i] + fSpacing);
      fCloseBtnLefts[i] := fRights[i] - fCloseBtnWidth - fSpacing;
      fCloseBtnTops[i] := y;
    end;
    fShrunk := x + 64 { fComboBox } > ClientWidth;
    if fShrunk then
    begin
      x := fSpacing;
      y := fSpacing;
      for i := 0 to fTags.Count - 1 do
      begin
        fWidths[i] := Canvas.TextWidth(fTags.Items[i].Text) + 2 * fSpacing;
        fLefts[i] := x;
        fRights[i] := x + fWidths[i];
        fTops[i] := y;
        fBottoms[i] := y + fActualTagHeight;
        Inc(x, fWidths[i] + fSpacing);
        fCloseBtnLefts[i] := fRights[i] - fCloseBtnWidth - fSpacing;
        fCloseBtnTops[i] := y;
      end;
      if x + 64 { fComboBox } > ClientWidth then
      begin
        vMeanWidth := (ClientWidth - 2 * fSpacing - 64 { fComboBox } )
          div fTags.Count - fSpacing;
        x := fSpacing;
        for i := 0 to fTags.Count - 1 do
        begin
          fWidths[i] := Min(fWidths[i], vMeanWidth);
          fLefts[i] := x;
          fRights[i] := x + fWidths[i];
          Inc(x, fWidths[i] + fSpacing);
        end;
      end;
    end;
  end;
  fEditPos := Point(fSpacing,
    fSpacing + (fActualTagHeight - fComboBox.Height) div 2);
  if fTags.Count > 0 then
    fEditPos := Point(fRights[fTags.Count - 1] + fSpacing,
      fTops[fTags.Count - 1] + (fActualTagHeight - fComboBox.Height) div 2);
  if fMultiLine and (fEditPos.x + 64 { fComboBox } > ClientWidth) and (fTags.Count > 0) then
  begin
    fEditPos := Point(fSpacing, fTops[fTags.Count - 1] + fTagHeight + fSpacing +
      (fActualTagHeight - fComboBox.Height) div 2);
    Inc(fNumRows);
  end;
  fDesiredHeight := fSpacing + fNumRows * (fTagHeight + fSpacing);
  vAdjustedDesiredHeight := Min(fDesiredHeight, fMaxHeight);
  if fMultiLine and fAutoHeight and (ClientHeight <> vAdjustedDesiredHeight) then
    ClientHeight := vAdjustedDesiredHeight;
  UpdateScrollBars;
end;

procedure TTisTagEditor.Paint;
var
  i: Integer;
  w: Integer;
  x, y, vNewEditWidth: Integer;
  r: TRect;
  s: string;
  vClip: HRGN;
begin
  inherited Paint;
  UpdateMetrics;
  Canvas.Brush.Color := IfThen(Enabled, fBgColor, fBgColorDisabled);
  Canvas.Pen.Color := IfThen(Enabled, fBorderColor, fBorderColorDisabled);
  Canvas.Rectangle(ClientRect);
  Canvas.Font.Assign(Self.Font);
  vClip := CreateRectRgnIndirect(GetShrunkClientRect(3));
  SelectClipRgn(Canvas.Handle, vClip);
  DeleteObject(vClip);
  for i := 0 to fTags.Count - 1 do
  begin
    x := fLefts[i];
    y := fTops[i] - fScrollInfo.nPos;
    w := fWidths[i];
    r := Rect(x, y, x + w, y + fActualTagHeight);
    with fTags.Items[i] do
    begin
      Canvas.Brush.Color := IfThen(
        self.Enabled,
        fBgColor,
        IfThen(
          fTagBgColorDisabled = clDefault,
          GetColorDisabled(fBgColor),
          fTagBgColorDisabled
        )
      );
    end;
    Canvas.Pen.Color := fTags.Items[i].fBorderColor;
    Canvas.RoundRect(r, fTagRoundBorder, fTagRoundBorder);
    Canvas.Font.Color := fTags.Items[i].fTextColor;
    Canvas.Brush.Style := bsClear;
    r.Left := r.Left + fSpacing;
    s := fTags.Items[i].Text;
    if (not fShrunk) and (ioShowDeleteButton in fTagInput.Options) then
    begin
      if fTagInput.DeleteIcon.Empty then
        s := s + ' x'
      else
      {$ifdef windows} //todo
        Windows.DrawIconEx(Canvas.Handle, fCloseBtnLefts[i], fCloseBtnTops[i] + 10,
          fTagInput.DeleteIcon.Handle, fTagInput.DeleteIcon.Width,
          fTagInput.DeleteIcon.Height, 0, DI_NORMAL, DI_NORMAL);
      {$endif}
    end;
    DrawText(Canvas.Handle, PChar(s), -1, r, DT_SINGLELINE or DT_VCENTER or
      DT_LEFT or DT_END_ELLIPSIS or DT_NOPREFIX);
    Canvas.Brush.Style := bsSolid;
  end;
  if fComboBox.Visible then
  begin
    vNewEditWidth := ClientWidth - fEditPos.x - fSpacing;
    if vNewEditWidth < fComboBox.Width then
      fComboBox.Width := vNewEditWidth;
    fComboBox.Left := fEditPos.x;
    if vNewEditWidth > fComboBox.Width then
      fComboBox.Width := vNewEditWidth;
    fComboBox.Top := fEditPos.y - fScrollInfo.nPos;
  end;
  SelectClipRgn(Canvas.Handle, 0);
  if Focused then
    DrawFocusRect;
end;

function TTisTagEditor.GetShrunkClientRect(const Amount: Integer): TRect;
begin
  result := Rect(Amount, Amount, ClientWidth - Amount, ClientHeight - Amount);
end;

procedure TTisTagEditor.DrawFocusRect;
var
  r: TRect;
begin
  r := GetShrunkClientRect(2);
  SafeDrawFocusRect(Canvas.Handle, r);
end;

procedure TTisTagEditor.SetAutoHeight(const Value: Boolean);
begin
  if fAutoHeight <> Value then
  begin
    fAutoHeight := Value;
    Invalidate;
  end;
end;

procedure TTisTagEditor.SetBgColor(const Value: TColor);
begin
  if fBgColor <> Value then
  begin
    fBgColor := Value;
    Invalidate;
  end;
end;

procedure TTisTagEditor.SetBgColorDisabled(aValue: TColor);
begin
  if fBgColorDisabled <> aValue then
  begin
    fBgColorDisabled := aValue;
    Invalidate;
  end;
end;

procedure TTisTagEditor.SetBorderColor(const Value: TColor);
begin
  if fBorderColor <> Value then
  begin
    fBorderColor := Value;
    Invalidate;
  end;
end;

procedure TTisTagEditor.SetBorderColorDisabled(aValue: TColor);
begin
  if fBorderColorDisabled <> aValue then
  begin
    fBorderColorDisabled := aValue;
    Invalidate;
  end;
end;

procedure TTisTagEditor.SetMaxHeight(const Value: Integer);
begin
  if fMaxHeight <> Value then
  begin
    fMaxHeight := Value;
    Invalidate;
  end;
end;

procedure TTisTagEditor.SetMultiLine(const Value: Boolean);
begin
  if fMultiLine <> Value then
  begin
    fMultiLine := Value;
    Invalidate;
  end;
end;

procedure TTisTagEditor.SetTags(const Value: TTags);
begin
  Tags.Assign(Value);
end;

procedure TTisTagEditor.SetReadOnly(const Value: Boolean);
begin
  if fReadOnly <> Value then
  begin
    fReadOnly := Value;
    fComboBox.ReadOnly := Value;
  end;
  fSavedReadOnly := fReadOnly;
end;

procedure TTisTagEditor.SetTagBgColor(const Value: TColor);
begin
  if fTagBgColor <> Value then
  begin
    fTagBgColor := Value;
    TagTextColor := GetBlackOrWhite(fTagBgColor);
    Invalidate;
  end;
end;

procedure TTisTagEditor.SetTagBgColorDisabled(aValue: TColor);
begin
  if fTagBgColorDisabled <> aValue then
  begin
    fTagBgColorDisabled := aValue;
    TagTextColor := GetBlackOrWhite(fTagBgColorDisabled);
    Invalidate;
  end;
end;

procedure TTisTagEditor.SetTagBorderColor(const Value: TColor);
begin
  if fTagBorderColor <> Value then
  begin
    fTagBorderColor := Value;
    Invalidate;
  end;
end;

procedure TTisTagEditor.SetTagHeight(const Value: Integer);
begin
  if fTagHeight <> Value then
  begin
    fTagHeight := Value;
    Invalidate;
  end;
end;

procedure TTisTagEditor.SetTagRoundBorder(const Value: Integer);
begin
  if Value > 10 then
    fTagRoundBorder := 10
  else if Value < 0 then
    fTagRoundBorder := 0
  else
    fTagRoundBorder := Value;
end;

procedure TTisTagEditor.SetTagTextColor(const Value: TColor);
begin
  if fTagTextColor <> Value then
  begin
    fTagTextColor := Value;
    Invalidate;
  end;
end;

function TTisTagEditor.GetAsArray: TStringArray;
var
  i: Integer;
begin
  SetLength(result, fTags.Count);
  for i := 0 to fTags.Count -1 do
    result[i] := fTags.Items[i].Text;
end;

procedure TTisTagEditor.SetAsArray(aValue: TStringArray);
var
  i: Integer;
begin
  for i := fTags.Count -1 downto 0 do
    DeleteTag(i);
  for i := 0 to high(aValue) do
    AddTag(aValue[i]);
  DoChange;
end;

procedure TTisTagEditor.HideComboBox;
begin
  fComboBox.Text := '';
  fComboBox.Hide;
  Invalidate;
end;

procedure TTisTagEditor.ShowComboBox;
begin
  fComboBox.Left := fEditPos.X;
  fComboBox.Top := fEditPos.Y;
  fComboBox.Width := ClientWidth - fComboBox.Left - fSpacing;
  fComboBox.Color := fEditorColor;
  fComboBox.Text := '';
  LoadComboBox;
  fComboBox.Show;
  fComboBox.SetFocus;
  SetWindowRgn(
    fComboBox.Handle,
    CreateRectRgn(2, 2, fComboBox.Width-2, fComboBox.Height-3),
    True
  );
end;

procedure TTisTagEditor.SetSpacing(const Value: Integer);
begin
  if fSpacing <> Value then
  begin
    fSpacing := Value;
    Invalidate;
  end;
end;

initialization
{$ifdef windows}
  Screen.Cursors[crHandPoint] := LoadCursor(0, IDC_HAND);
{$else}
  //todo: form original Tranquil, not sure if it is working on Linux
  // Screen.Cursors[crHandPoint] := LoadCursorFromLazarusResource('HandPoint');
{$endif}

end.
