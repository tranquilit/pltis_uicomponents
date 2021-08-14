// -----------------------------------------------------------------
//    This file is part of Tranquil IT Software
//    Copyright (C) 2012 - 2021  Tranquil IT https://www.tranquil.it
//    All Rights Reserved.
// ------------------------------------------------------------------
//
//    - first version (2014) by Andreas Rejbrand - https://specials.rejbrand.se/dev/controls/tageditor/
//    - modified and improved version (2021) by Daniel C. Dávila - https://github.com/daviladanielc/Delphi_TagEditor
//    - modified, improved, and Lazarus converted version (2021) by Tranquil IT

unit tis.ui.tageditor;

{$i mormot.defines.inc}

interface

uses
  Windows,
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
  LCLType;

type
  TClickInfo = cardinal;
  GetTagIndex = word;

  TTagItemConfig = record
    CanDeleteTag: Boolean;
    TagBgColor: TColor;
    TagBorderColor: TColor;
    TagValue: Variant;
    TextColor: TColor;
  end;

  TTagClickEvent = procedure(Sender: TObject; TagIndex: integer;
    const TagCaption: string) of object;
  TRemoveConfirmEvent = procedure(Sender: TObject; TagIndex: integer;
    const TagCaption: string; var CanRemove: Boolean) of object;
  TTagRemoved = procedure(Sender: TObject; TagIndex: integer) of object;
  TBeforeTagRemove = procedure(Sender: TObject; TagIndex: integer) of object;

  TTags = class;
  TTisTagEditor = class;

  TTagItem = class(TCollectionItem)
  private
    FCanDeleteTag: Boolean;
    FTagBgColor: TColor;
    FTagBorderColor: TColor;
    FTagValue: Variant;
    FTextColor: TColor;
    FText: string;
    function GetTagEditor: TTisTagEditor;
    procedure UpdateTagEditor;
    procedure SetCanDeleteTag(const Value: Boolean);
    procedure SetTagBgColor(const Value: TColor);
    procedure SetTagBorderColor(const Value: TColor);
    procedure SetText(const aValue: string);
    procedure SetTextColor(const Value: TColor);
  protected
    procedure SetCollection(Value: TCollection); override;
  published
    property CanDeleteTag: Boolean read FCanDeleteTag write SetCanDeleteTag;
    property TagBgColor: TColor read FTagBgColor write SetTagBgColor;
    property TagBorderColor: TColor read FTagBorderColor
      write SetTagBorderColor;
    property TagValue: Variant read FTagValue write FTagValue;
    property Text: string read FText write SetText;
    property TextColor: TColor read FTextColor write SetTextColor;
  public
    procedure Assign(Source: TPersistent); override;
    constructor Create(aCollection: TCollection); override;
  end;

  TTagItemClass = class of TTagItem;

  TTags = class(TCollection)
  private
    FTagEditor: TTisTagEditor;
    function GetTagItem(Index: integer): TTagItem;
    procedure SetTagItem(Index: integer; const Value: TTagItem);
  protected
    function GetOwner: TPersistent; override;
  public
    function IndexOf(const aText: string): integer;
    function DelimitedText: string;
    function Add(aItemConfig: TTagItemConfig; const aText: string = ''): TTagItem; overload;
    function Add(const aText: string = ''): TTagItem; overload;
    procedure DeleteAll;
    procedure Move(CurIndex, NewIndex: integer);
    constructor Create(ATagEditor: TTisTagEditor; ATagsItemClass: TTagItemCLass);
    property Items[Index: integer]: TTagItem read GetTagItem
      write SetTagItem; default;
    property TagEditor: TTisTagEditor read FTagEditor;
  end;

  TTisTagEditor = class(TCustomControl)
  private
    FActualTagHeight: integer;
    FAllowDuplicates: Boolean;
    FAutoHeight: Boolean;
    FBeforeTagRemove: TBeforeTagRemove;
    FBgColor: TColor;
    FBorderColor: TColor;
    FCanDragTags: Boolean;
    FCaretVisible: Boolean;
    FLefts, FRights, FWidths, FTops, FBottoms: array of integer;
    FCloseBtnLefts, FCloseBtnTops: array of integer;
    FCloseBtnWidth: integer;
    FCommaAccepts: Boolean;
    FDeleteButtonIcon: TIcon;
    FDeleteTagButton: Boolean;
    FDesiredHeight: integer;
    FDragging: Boolean;
    FEdit: TEdit;
    FEditorColor: TColor;
    FEditPos: TPoint;
    FMaxHeight: integer;
    FMaxTags: integer;
    FMouseDownClickInfo: TClickInfo;
    FMultiLine: Boolean;
    FNoLeadingSpaceInput: Boolean;
    FTags: TTags;
    FNumRows: integer;
    FOnChange: TNotifyEvent;
    FOnRemoveConfirm: TRemoveConfirmEvent;
    FPopupMenu: TPopupMenu;
    FPrevScrollPos: integer;
    FReadOnly: Boolean;
    FSavedReadOnly: Boolean;
    FScrollBarVisible: Boolean;
    FScrollInfo: Windows.TScrollInfo;
    FSemicolonAccepts: Boolean;
    FShrunk: Boolean;
    FSpaceAccepts: Boolean;
    FSpacing: integer;
    FTagAdded: TNotifyEvent;
    FTagBgColor: TColor;
    FTagBorderColor: TColor;
    FTagClickEvent: TTagClickEvent;
    FTagHeight: integer;
    FTagRemoved: TTagRemoved;
    FTagRoundBorder: integer;
    FTextColor: TColor;
    FTrimInput: Boolean;
    function Accept: Boolean;
    function GetClickInfoAt(X, Y: integer): TClickInfo;
    function GetReadOnly: Boolean;
    function GetSeparatorIndexAt(X, Y: integer): integer;
    function GetShrunkClientRect(const Amount: integer): TRect;
    function IsFirstOnRow(TagIndex: integer): Boolean; inline;
    function IsLastOnRow(TagIndex: integer): Boolean;
    procedure CreateCaret;
    procedure DestroyCaret;
    procedure DrawFocusRect;
    procedure EditEnter(Sender: TObject);
    procedure EditExit(Sender: TObject);
    procedure EditKeyPress(Sender: TObject; var Key: Char);
    procedure FixPosAndScrollWindow;
    procedure HideEditor;
    procedure DoPopupMenuDeleteItem(Sender: TObject);
    procedure SetAutoHeight(const Value: Boolean);
    procedure SetBgColor(const Value: TColor);
    procedure SetBorderColor(const Value: TColor);
    procedure SetButtonIcon(const Value: TIcon);
    procedure SetCanDragTags(const Value: Boolean);
    procedure SetCloseTagButton(const Value: Boolean);
    procedure SetMaxHeight(const Value: integer);
    procedure SetMultiLine(const Value: Boolean);
    procedure SetPasteText(AText: string);
    procedure SetTags(const Value: TTags);
    procedure SetReadOnly(const Value: Boolean);
    procedure SetSpacing(const Value: integer);
    procedure SetTagBgColor(const Value: TColor);
    procedure SetTagBorderColor(const Value: TColor);
    procedure SetTagHeight(const Value: integer);
    procedure SetTagRoundBorder(const Value: integer);
    procedure SetTextColor(const Value: TColor);
    procedure ShowEditor;
    procedure TagChange(Sender: TObject);
    procedure UpdateMetrics;
    procedure UpdateScrollBars;
  protected
    function CreateTags(aTagEditor: TTisTagEditor): TTags; virtual;
    function CreateEdit: TEdit; virtual;
    function CreatePopupMenu: TPopupMenu; virtual;
    function GetFontEdit: TFont;
    procedure CreateParams(var Params: TCreateParams); override;
    procedure KeyDown(var Key: word; Shift: TShiftState); override;
    procedure KeyPress(var Key: Char); override;
    procedure Loaded; override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X: integer;
      Y: integer); override;
    procedure MouseMove(Shift: TShiftState; X: integer; Y: integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X: integer;
      Y: integer); override;
    procedure Paint; override;
    procedure WndProc(var Message: TMessage); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    property Align;
    property AllowDuplicates: Boolean read FAllowDuplicates
      write FAllowDuplicates default False;
    property Anchors;
    property AutoHeight: Boolean read FAutoHeight write SetAutoHeight;
    property BgColor: TColor read FBgColor write SetBgColor;
    property BorderColor: TColor read FBorderColor write SetBorderColor;
    property CanDragTags: Boolean read FCanDragTags write SetCanDragTags
      default True;
    property Color;
    property CommaAccepts: Boolean read FCommaAccepts write FCommaAccepts
      default True;
    property Cursor;
    property Tags: TTags read FTags write SetTags;
    property DeleteButtonIcon: TIcon read FDeleteButtonIcon write SetButtonIcon;
    property DeleteTagButton: Boolean read FDeleteTagButton
      write SetCloseTagButton default True;
    property EditorColor: TColor read FEditorColor write FEditorColor
      default clWindow;
    property MaxHeight: integer read FMaxHeight write SetMaxHeight default 512;
    property MaxTags: integer read FMaxTags write FMaxTags default 0;
    property MultiLine: Boolean read FMultiLine write SetMultiLine
      default False;
    property NoLeadingSpaceInput: Boolean read FNoLeadingSpaceInput
      write FNoLeadingSpaceInput default True;
    property OnBeforeTagRemove: TBeforeTagRemove read FBeforeTagRemove
      write FBeforeTagRemove;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
    property OnRemoveConfirm: TRemoveConfirmEvent read FOnRemoveConfirm
      write FOnRemoveConfirm;
    property OnTagAdded: TNotifyEvent read FTagAdded write FTagAdded;
    property OnTagClick: TTagClickEvent read FTagClickEvent
      write FTagClickEvent;
    property OnTagRemoved: TTagRemoved read FTagRemoved write FTagRemoved;
    property ReadOnly: Boolean read GetReadOnly write SetReadOnly default False;
    property SemicolonAccepts: Boolean read FSemicolonAccepts
      write FSemicolonAccepts default True;
    property SpaceAccepts: Boolean read FSpaceAccepts write FSpaceAccepts
      default True;
    property Spacing: integer read FSpacing write SetSpacing;
    property TabOrder;
    property TabStop;
    property Tag;
    property TagBgColor: TColor read FTagBgColor write SetTagBgColor;
    property TagBorderColor: TColor read FTagBorderColor
      write SetTagBorderColor;
    property TagHeight: integer read FTagHeight write SetTagHeight default 32;
    property TagRoundBorder: integer read FTagRoundBorder
      write SetTagRoundBorder;
    property TextColor: TColor read FTextColor write SetTextColor;
    property TrimInput: Boolean read FTrimInput write FTrimInput default True;

  end;

implementation

uses Math, Clipbrd;

const
  TAG_LOW = 0;
  TAG_HIGH = MAXWORD - 2;
  EDITOR = MAXWORD - 1;
  NOWHERE = MAXWORD;
  PART_BODY = $00000000;
  PART_REMOVE_BUTTON = $00010000;

function IsKeyDown(const VK: integer): Boolean;
begin
  IsKeyDown := GetKeyState(VK) and $8000 <> 0;
end;

function GetTagPart(ClickInfo: TClickInfo): cardinal;
begin
  result := ClickInfo and $FFFF0000;
end;

procedure SafeDrawFocusRect(hDC: hDC; const R: TRect);
var
  oldBkColor, oldTextColor: COLORREF;
begin
  oldBkColor := Windows.SetBkColor(hDC, clWhite);
  oldTextColor := Windows.SetTextColor(hDC, clBlack);
  Windows.DrawFocusRect(hDC, R);
  if oldBkColor <> CLR_INVALID then
    Windows.SetBkColor(hDC, oldBkColor);
  if oldTextColor <> CLR_INVALID then
    Windows.SetTextColor(hDC, oldTextColor);
end;

{ TTagEditor }

constructor TTisTagEditor.Create(aOwner: TComponent);
begin
  inherited Create(aOwner);
  Left := 48;
  Height := 47;
  Top := 48;
  Width := 221;
  FEdit := CreateEdit;
  FTags := CreateTags(Self);
  FPopupMenu := CreatePopupMenu;
  FBgColor := clWindow;
  FBorderColor := clWindowFrame;
  FTagBgColor := clSkyBlue;
  FTagBorderColor := clNavy;
  FSpacing := 8;
  FTextColor := clWhite;
  FSpaceAccepts := True;
  FCommaAccepts := True;
  FSemicolonAccepts := True;
  FTrimInput := True;
  FNoLeadingSpaceInput := True;
  FAllowDuplicates := False;
  FMultiLine := False;
  FTagHeight := 32;
  FShrunk := False;
  FEditorColor := clWindow;
  FMaxHeight := 512;
  FCaretVisible := False;
  FDragging := False;
  FPrevScrollPos := 0;
  FScrollInfo.cbSize := sizeof(FScrollInfo);
  FScrollBarVisible := False;
  FCanDragTags := True;
  TabStop := True;
  FDeleteTagButton := True;
  FDeleteButtonIcon := TIcon.Create;
end;

destructor TTisTagEditor.Destroy;
begin
  FTags.Free;
  FTags := nil;
  FPopupMenu.Free;
  FEdit.Free;
  FDeleteButtonIcon.Free;
  inherited Destroy;
end;

procedure TTisTagEditor.EditEnter(Sender: TObject);
begin
  if FEditPos.Y + FEdit.Height > FScrollInfo.nPos + ClientHeight then
    FScrollInfo.nPos := FEditPos.Y + ClientHeight - FEdit.Height;
  FixPosAndScrollWindow;
end;

procedure TTisTagEditor.EditExit(Sender: TObject);
begin
  if FEdit.Text <> '' then
    Accept
  else
    HideEditor;
end;

procedure TTisTagEditor.DoPopupMenuDeleteItem(Sender: TObject);
begin
  if Sender is TMenuItem then
  begin
    if Assigned(FBeforeTagRemove) then
      FBeforeTagRemove(Self, TMenuItem(Sender).Tag);
    FTags.Delete(TMenuItem(Sender).Tag);
    if Assigned(FTagRemoved) then
      FTagRemoved(Self, TMenuItem(Sender).Tag);
  end;
end;

procedure TTisTagEditor.TagChange(Sender: TObject);
begin
  UpdateMetrics;
  Invalidate;
  if Assigned(FOnChange) then
    FOnChange(Self);
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
        if FCaretVisible then
          DestroyCaret;
        FDragging := False;
        Invalidate;
      end;
    WM_COPY:
      Clipboard.AsText := FTags.DelimitedText;
    WM_CLEAR:
      FTags.Clear;
    WM_CUT:
      begin
        Clipboard.AsText := FTags.DelimitedText;
        FTags.DeleteAll;
      end;
    WM_PASTE:
      begin
        if Clipboard.HasFormat(CF_TEXT) then
          SetPasteText(Clipboard.AsText);
      end;
    WM_SIZE:
      begin
        UpdateMetrics;
        Invalidate;
        Message.result := 0;
      end;
    WM_VSCROLL:
      begin
        FScrollInfo.fMask := SIF_ALL;
        GetScrollInfo(Handle, SB_VERT, FScrollInfo);
        case Message.WParamLo of
          SB_TOP:
            FScrollInfo.nPos := FScrollInfo.nMin;
          SB_BOTTOM:
            FScrollInfo.nPos := FScrollInfo.nMax;
          SB_PAGEUP:
            Dec(FScrollInfo.nPos, FScrollInfo.nPage);
          SB_PAGEDOWN:
            Inc(FScrollInfo.nPos, FScrollInfo.nPage);
          SB_LINEUP:
            Dec(FScrollInfo.nPos, FTagHeight);
          SB_LINEDOWN:
            Inc(FScrollInfo.nPos, FTagHeight);
          SB_THUMBTRACK:
            FScrollInfo.nPos := FScrollInfo.nTrackPos;
        end;
        FixPosAndScrollWindow;
        Message.result := 0;
      end;
  end;
  inherited WndProc(Message);
end;

procedure TTisTagEditor.FixPosAndScrollWindow;
begin
  FScrollInfo.fMask := SIF_POS;
  SetScrollInfo(Handle, SB_VERT, FScrollInfo, True);
  GetScrollInfo(Handle, SB_VERT, FScrollInfo);
  if FScrollInfo.nPos <> FPrevScrollPos then
  begin
    ScrollWindowEx(Handle, 0, FPrevScrollPos - FScrollInfo.nPos,
      GetShrunkClientRect(3), GetShrunkClientRect(3), 0, nil, SW_INVALIDATE);
    FPrevScrollPos := FScrollInfo.nPos;
    Update;
  end;
end;

procedure TTisTagEditor.UpdateScrollBars;
begin
  FScrollInfo.fMask := SIF_RANGE or SIF_PAGE;
  FScrollInfo.nMin := 0;
  FScrollInfo.nMax := FDesiredHeight - 1;
  FScrollInfo.nPage := ClientHeight;
  SetScrollInfo(Handle, SB_VERT, FScrollInfo, True);
  FixPosAndScrollWindow;
end;

function TTisTagEditor.Accept: Boolean;
begin
  if (FTags.Count = FMaxTags) and (FMaxTags > 0) then
    Exit(False);
  Assert(FEdit.Visible);
  result := False;
  if FTrimInput then
    FEdit.Text := Trim(FEdit.Text);
  if (FEdit.Text = '') or ((not AllowDuplicates) and
    (FTags.IndexOf(FEdit.Text) <> -1)) then
  begin
    beep;
    Exit;
  end;
  FTags.Add(FEdit.Text);
  UpdateMetrics;
  result := True;
  HideEditor;
  if Assigned(FTagAdded) then
    FTagAdded(Self);
  Invalidate;
end;

procedure TTisTagEditor.EditKeyPress(Sender: TObject; var Key: Char);
begin

  if (Key = chr(VK_SPACE)) and (FEdit.Text = '') and FNoLeadingSpaceInput then
  begin
    Key := #0;
    Exit;
  end;

  if ((Key = chr(VK_SPACE)) and FSpaceAccepts) or
    ((Key = ',') and FCommaAccepts) or ((Key = ';') and FSemicolonAccepts) then
    Key := chr(VK_RETURN);

  case ord(Key) of
    VK_RETURN:
      begin
        Accept;
        ShowEditor;
        Key := #0;
      end;
    VK_BACK:
      begin
        if (FEdit.Text = '') and (FTags.Count > 0) then
        begin
          if Assigned(FBeforeTagRemove) then
            FBeforeTagRemove(Sender, FTags.Count - 1);
          FTags.Delete(FTags.Count - 1);
          if Assigned(FTagRemoved) then
            FTagRemoved(Sender, FTags.Count - 1);
          UpdateMetrics;
          Paint;
        end;
      end;
    VK_ESCAPE:
      begin
        HideEditor;
        Self.SetFocus;
        Key := #0;
      end;
  end;

end;

procedure TTisTagEditor.HideEditor;
begin
  FEdit.Text := '';
  FEdit.Hide;
end;

procedure TTisTagEditor.KeyDown(var Key: word; Shift: TShiftState);
begin
  inherited;
  case Key of
    VK_END:
      ShowEditor;
    VK_DELETE:
      Perform(WM_CLEAR, 0, 0);
    VK_INSERT:
      Perform(WM_PASTE, 0, 0);
    VK_F2:
      ShowEditor;
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
        Exit;
      end;
    ^X:
      begin
        Perform(WM_CUT, 0, 0);
        Key := #0;
        Exit;
      end;
    ^V:
      begin
        Perform(WM_PASTE, 0, 0);
        Key := #0;
        Exit;
      end;
  end;
  ShowEditor;
  FEdit.Perform(WM_CHAR, ord(Key), 0);
end;

procedure TTisTagEditor.Loaded;
begin
  inherited;
  UpdateMetrics;
end;

function TTisTagEditor.GetClickInfoAt(X, Y: integer): TClickInfo;
var
  i: integer;
begin
  result := NOWHERE;
  if (X >= FEditPos.X) and (Y >= FEditPos.Y) then
    Exit(EDITOR);

  for i := 0 to FTags.Count - 1 do
    if InRange(X, FLefts[i], FRights[i]) and InRange(Y, FTops[i], FBottoms[i])
    then
    begin
      result := i;
      if InRange(X, FCloseBtnLefts[i], FCloseBtnLefts[i] + FCloseBtnWidth) and
        InRange(Y, FCloseBtnTops[i], FCloseBtnTops[i] + FActualTagHeight) and
        not FShrunk then
        result := result or PART_REMOVE_BUTTON;
      break;
    end;
end;

function TTisTagEditor.GetFontEdit: TFont;
begin
  result := FEdit.Font;
end;

function TTisTagEditor.GetReadOnly: Boolean;
begin
  result := FReadOnly;
end;

function TTisTagEditor.IsFirstOnRow(TagIndex: integer): Boolean;
begin
  result := (TagIndex = 0) or (FTops[TagIndex] > FTops[TagIndex - 1]);
end;

function TTisTagEditor.IsLastOnRow(TagIndex: integer): Boolean;
begin
  result := (TagIndex = FTags.Count - 1) or
    (FTops[TagIndex] < FTops[TagIndex + 1]);
end;

function TTisTagEditor.GetSeparatorIndexAt(X, Y: integer): integer;
var
  i: integer;
begin
  result := FTags.Count;
  Y := Max(Y, FSpacing + 1);
  for i := FTags.Count - 1 downto 0 do
  begin
    if Y < FTops[i] then
      Continue;
    if (IsLastOnRow(i) and (X >= FRights[i])) or
      ((X < FRights[i]) and (IsFirstOnRow(i) or (FRights[i - 1] < X))) then
    begin
      result := i;
      if (IsLastOnRow(i) and (X >= FRights[i])) then
        Inc(result);
      Exit;
    end;
  end;
end;

procedure TTisTagEditor.MouseDown(Button: TMouseButton; Shift: TShiftState;
  X: integer; Y: integer);
begin
  Inc(Y, FScrollInfo.nPos);
  FMouseDownClickInfo := GetClickInfoAt(X, Y);
  if GetTagIndex(FMouseDownClickInfo) <> EDITOR then
    SetFocus;
end;

procedure TTisTagEditor.CreateCaret;
begin
  if not FCaretVisible then
    FCaretVisible := Windows.CreateCaret(Handle, 0, 0, FActualTagHeight);
end;

procedure TTisTagEditor.DestroyCaret;
begin
  if not FCaretVisible then
    Exit;
  Windows.DestroyCaret;
  FCaretVisible := False;
end;

procedure TTisTagEditor.CreateParams(var Params: TCreateParams);
begin
  inherited;
  Params.Style := Params.Style or WS_VSCROLL;
end;

function TTisTagEditor.CreateTags(aTagEditor: TTisTagEditor): TTags;
begin
  result := TTags.Create(aTagEditor, TTagItem);
end;

function TTisTagEditor.CreateEdit: TEdit;
begin
  result := TEdit.Create(Self);
  result.Top := 0;
  result.Left := 0;
  result.Width := 0;
  result.Parent := Self;
  result.BorderStyle := bsNone;
  result.Visible := False;
  result.OnKeyPress := EditKeyPress;
  result.OnEnter := EditEnter;
  result.OnExit := EditExit;
end;

function TTisTagEditor.CreatePopupMenu: TPopupMenu;
var
  mi: TMenuItem;
begin
  result := TPopupMenu.Create(Self);
  mi := TMenuItem.Create(PopupMenu);
  mi.Caption := 'Delete';
  mi.OnClick := DoPopupMenuDeleteItem;
  mi.Hint := 'Delete selected tag.';
  result.Items.Add(mi);
end;

procedure TTisTagEditor.MouseMove(Shift: TShiftState; X: integer; Y: integer);
var
  SepIndex: integer;
begin
  inherited;
  Inc(Y, FScrollInfo.nPos);
  if IsKeyDown(VK_LBUTTON) and InRange(GetTagIndex(FMouseDownClickInfo),
    TAG_LOW, TAG_HIGH) and (FCanDragTags) then
  begin
    FDragging := True;
    Screen.Cursor := crDrag;
    SepIndex := GetSeparatorIndexAt(X, Y);
    CreateCaret;
    if SepIndex = FTags.Count then
      SetCaretPos(FLefts[SepIndex - 1] + FWidths[SepIndex - 1] + FSpacing div 2,
        FTops[SepIndex - 1] - FScrollInfo.nPos)
    else
      SetCaretPos(FLefts[SepIndex] - FSpacing div 2,
        FTops[SepIndex] - FScrollInfo.nPos);
    ShowCaret(Handle);
    Exit;
  end;
  case GetTagIndex(GetClickInfoAt(X, Y)) of
    NOWHERE:
      Cursor := crArrow;
    EDITOR:
      Cursor := crIBeam;
    TAG_LOW .. TAG_HIGH:
      Cursor := crHandPoint;
  end;
end;

procedure TTisTagEditor.MouseUp(Button: TMouseButton; Shift: TShiftState;
  X: integer; Y: integer);
var
  pnt: TPoint;
  CanRemove: Boolean;
  ClickInfo: TClickInfo;
  i: word;
  p: cardinal;
  SepIndex: integer;
begin
  inherited;
  Inc(Y, FScrollInfo.nPos);
  if FDragging then
  begin
    DestroyCaret;
    FDragging := False;
    Screen.Cursor := crDefault;
    SepIndex := GetSeparatorIndexAt(X, Y);
    if not InRange(SepIndex, GetTagIndex(FMouseDownClickInfo),
      GetTagIndex(FMouseDownClickInfo) + 1) then
    begin
      FTags.Move(GetTagIndex(FMouseDownClickInfo),
        SepIndex - IfThen(SepIndex > GetTagIndex(FMouseDownClickInfo), 1, 0));
      UpdateMetrics;
      Paint;
    end;
    Exit;
  end;
  ClickInfo := GetClickInfoAt(X, Y);
  if ClickInfo <> FMouseDownClickInfo then
    Exit;
  i := GetTagIndex(ClickInfo);
  p := GetTagPart(ClickInfo);
  case i of
    EDITOR:
      ShowEditor;
    NOWHERE:
      ;
  else
    case Button of
      mbLeft:
        begin
          case p of
            PART_BODY:
              if Assigned(FTagClickEvent) then
                FTagClickEvent(Self, i, FTags.Items[i].Text);
            PART_REMOVE_BUTTON:
              begin
                if not FDeleteTagButton then
                  Exit;
                if Assigned(FOnRemoveConfirm) then
                begin
                  CanRemove := False;
                  FOnRemoveConfirm(Self, i, FTags.Items[i].Text, CanRemove);
                  if not CanRemove then
                    Exit;
                end;
                if Assigned(FBeforeTagRemove) then
                  FBeforeTagRemove(Self, i);
                FTags.Delete(i);
                if Assigned(FTagRemoved) then
                  FTagRemoved(Self, i);
                UpdateMetrics;
                Paint;
              end;
          end;
        end;
      mbRight:
        begin
          FPopupMenu.Items[0].Tag := i;
          pnt := ClientToScreen(Point(X, Y));
          FPopupMenu.Items[0].Caption := 'Delete tag "' + FTags.Items[i]
            .Text + '"';
          FPopupMenu.Popup(pnt.X, pnt.Y - FScrollInfo.nPos);
        end;
    end;
  end;

end;

procedure TTisTagEditor.UpdateMetrics;
var
  i: integer;
  X, Y: integer;
  MeanWidth: integer;
  AdjustedFDesiredHeight: integer;
begin
  SetLength(FLefts, FTags.Count);
  SetLength(FRights, FTags.Count);
  SetLength(FTops, FTags.Count);
  SetLength(FBottoms, FTags.Count);
  SetLength(FWidths, FTags.Count);
  SetLength(FCloseBtnLefts, FTags.Count);
  SetLength(FCloseBtnTops, FTags.Count);
  FCloseBtnWidth := Canvas.TextWidth('X');
  FShrunk := False;
  FNumRows := 1;
  if FMultiLine then
  begin
    FActualTagHeight := FTagHeight;
    X := FSpacing;
    Y := FSpacing;
    for i := 0 to FTags.Count - 1 do
    begin
      FWidths[i] := Canvas.TextWidth(FTags.Items[i].Text +
        IfThen(DeleteTagButton, ' ×', '')) + 2 * FSpacing;
      FLefts[i] := X;
      FRights[i] := X + FWidths[i];
      FTops[i] := Y;
      FBottoms[i] := Y + FTagHeight;

      if X + FWidths[i] + FSpacing > ClientWidth then
      begin
        X := FSpacing;
        Inc(Y, FTagHeight + FSpacing);
        Inc(FNumRows);
        FLefts[i] := X;
        FRights[i] := X + FWidths[i];
        FTops[i] := Y;
        FBottoms[i] := Y + FTagHeight;
      end;
      FCloseBtnLefts[i] := X + FWidths[i] - FCloseBtnWidth - FSpacing;
      FCloseBtnTops[i] := Y;
      Inc(X, FWidths[i] + FSpacing);
    end;
  end
  else
  begin
    FActualTagHeight := ClientHeight - 2 * FSpacing;
    X := FSpacing;
    Y := FSpacing;
    for i := 0 to FTags.Count - 1 do
    begin
      FWidths[i] := Canvas.TextWidth(FTags.Items[i].Text +
        IfThen(DeleteTagButton, ' ×', '')) + 2 * FSpacing;
      FLefts[i] := X;
      FRights[i] := X + FWidths[i];
      FTops[i] := Y;
      FBottoms[i] := Y + FActualTagHeight;
      Inc(X, FWidths[i] + FSpacing);
      FCloseBtnLefts[i] := FRights[i] - FCloseBtnWidth - FSpacing;
      FCloseBtnTops[i] := Y;
    end;
    FShrunk := X + 64 { FEdit } > ClientWidth;
    if FShrunk then
    begin
      X := FSpacing;
      Y := FSpacing;
      for i := 0 to FTags.Count - 1 do
      begin
        FWidths[i] := Canvas.TextWidth(FTags.Items[i].Text) + 2 * FSpacing;
        FLefts[i] := X;
        FRights[i] := X + FWidths[i];
        FTops[i] := Y;
        FBottoms[i] := Y + FActualTagHeight;
        Inc(X, FWidths[i] + FSpacing);
        FCloseBtnLefts[i] := FRights[i] - FCloseBtnWidth - FSpacing;
        FCloseBtnTops[i] := Y;
      end;
      if X + 64 { FEdit } > ClientWidth then
      begin
        MeanWidth := (ClientWidth - 2 * FSpacing - 64 { FEdit } )
          div FTags.Count - FSpacing;
        X := FSpacing;
        for i := 0 to FTags.Count - 1 do
        begin
          FWidths[i] := Min(FWidths[i], MeanWidth);
          FLefts[i] := X;
          FRights[i] := X + FWidths[i];
          Inc(X, FWidths[i] + FSpacing);
        end;
      end;
    end;
  end;
  FEditPos := Point(FSpacing,
    FSpacing + (FActualTagHeight - FEdit.Height) div 2);
  if FTags.Count > 0 then
    FEditPos := Point(FRights[FTags.Count - 1] + FSpacing,
      FTops[FTags.Count - 1] + (FActualTagHeight - FEdit.Height) div 2);
  if FMultiLine and (FEditPos.X + 64 > ClientWidth) and (FTags.Count > 0) then
  begin
    FEditPos := Point(FSpacing, FTops[FTags.Count - 1] + FTagHeight + FSpacing +
      (FActualTagHeight - FEdit.Height) div 2);
    Inc(FNumRows);
  end;
  FDesiredHeight := FSpacing + FNumRows * (FTagHeight + FSpacing);
  AdjustedFDesiredHeight := Min(FDesiredHeight, FMaxHeight);
  if FMultiLine and FAutoHeight and (ClientHeight <> AdjustedFDesiredHeight)
  then
    ClientHeight := AdjustedFDesiredHeight;
  UpdateScrollBars;
end;

procedure TTisTagEditor.Paint;
var
  i: integer;
  w: integer;
  X, Y: integer;
  R: TRect;
  S: string;
  clip: HRGN;
begin
  inherited Paint;
  Canvas.Brush.Color := FBgColor;
  Canvas.Pen.Color := FBorderColor;
  Canvas.Rectangle(ClientRect);
  Canvas.Font.Assign(Self.Font);
  clip := CreateRectRgnIndirect(GetShrunkClientRect(3));
  SelectClipRgn(Canvas.Handle, clip);
  DeleteObject(clip);
  for i := 0 to FTags.Count - 1 do
  begin
    X := FLefts[i];
    Y := FTops[i] - FScrollInfo.nPos;
    w := FWidths[i];
    R := Rect(X, Y, X + w, Y + FActualTagHeight);
    Canvas.Brush.Color := FTags.Items[i].FTagBgColor;
    Canvas.Pen.Color := FTags.Items[i].FTagBorderColor;
    Canvas.RoundRect(R, FTagRoundBorder, FTagRoundBorder);
    Canvas.Font.Color := FTags.Items[i].FTextColor;
    Canvas.Brush.Style := bsClear;
    R.Left := R.Left + FSpacing;
    S := FTags.Items[i].Text;
    if (not FShrunk) and (FDeleteTagButton) then
    begin
      if FDeleteButtonIcon.Empty then
        S := S + ' X'
      else
        DrawIconEx(Canvas.Handle, FCloseBtnLefts[i], FCloseBtnTops[i] + 10,
          FDeleteButtonIcon.Handle, FDeleteButtonIcon.Width,
          FDeleteButtonIcon.Height, 0, DI_NORMAL, DI_NORMAL);
    end;
    DrawText(Canvas.Handle, PChar(S), -1, R, DT_SINGLELINE or DT_VCENTER or
      DT_LEFT or DT_END_ELLIPSIS or DT_NOPREFIX);
    Canvas.Brush.Style := bsSolid;
  end;
  if FEdit.Visible then
  begin
    FEdit.Left := FEditPos.X;
    FEdit.Top := FEditPos.Y - FScrollInfo.nPos;
    FEdit.Width := ClientWidth - FEdit.Left - FSpacing;
  end;
  SelectClipRgn(Canvas.Handle, 0);
  if Focused then
    DrawFocusRect;
end;

function TTisTagEditor.GetShrunkClientRect(const Amount: integer): TRect;
begin
  result := Rect(Amount, Amount, ClientWidth - Amount, ClientHeight - Amount);
end;

procedure TTisTagEditor.DrawFocusRect;
var
  R: TRect;
begin
  R := GetShrunkClientRect(2);
  SafeDrawFocusRect(Canvas.Handle, R);
end;

procedure TTisTagEditor.SetAutoHeight(const Value: Boolean);
begin
  if FAutoHeight <> Value then
  begin
    FAutoHeight := Value;
    UpdateMetrics;
    Invalidate;
  end;
end;

procedure TTisTagEditor.SetBgColor(const Value: TColor);
begin
  if FBgColor <> Value then
  begin
    FBgColor := Value;
    Invalidate;
  end;
end;

procedure TTisTagEditor.SetBorderColor(const Value: TColor);
begin
  if FBorderColor <> Value then
  begin
    FBorderColor := Value;

  end;
end;

procedure TTisTagEditor.SetButtonIcon(const Value: TIcon);
begin
  if Value <> nil then
  begin
    if (not(InRange(Value.Height, 8, 10))) and (not(InRange(Value.Width, 8, 10))) then
      raise Exception.Create('The icon size should be 8x8 or 10x10');
    FDeleteButtonIcon.Assign(Value);
  end;
end;

procedure TTisTagEditor.SetCanDragTags(const Value: Boolean);
begin
  FCanDragTags := Value;
end;

procedure TTisTagEditor.SetCloseTagButton(const Value: Boolean);
begin
  FDeleteTagButton := Value;
end;

procedure TTisTagEditor.SetMaxHeight(const Value: integer);
begin
  if FMaxHeight <> Value then
  begin
    FMaxHeight := Value;
    UpdateMetrics;
    Invalidate;
  end;
end;

procedure TTisTagEditor.SetMultiLine(const Value: Boolean);
begin
  if FMultiLine <> Value then
  begin
    FMultiLine := Value;
    UpdateMetrics;
    Invalidate;
  end;
end;

procedure TTisTagEditor.SetPasteText(AText: string);
var
  LStrList: TStringList;
  i: integer;
  LTagConf: TTagItemConfig;
begin
  if AText = '' then
    Exit;
  LStrList := TStringList.Create;
  LTagConf.CanDeleteTag := FDeleteTagButton;
  LTagConf.TagBgColor := FTagBgColor;
  LTagConf.TagBorderColor := FTagBorderColor;
  LTagConf.TextColor := FTextColor;
  try
    LStrList.DelimitedText := AText;
    for i := 0 to LStrList.Count - 1 do
    begin
      FTags.Add(LTagConf, LStrList[i]);
    end;
  finally
    LStrList.Free;
    UpdateMetrics;
    Paint;
  end;
end;

procedure TTisTagEditor.SetTags(const Value: TTags);
begin
  Tags.Assign(Value);
end;

procedure TTisTagEditor.SetReadOnly(const Value: Boolean);
begin
  if FReadOnly <> Value then
  begin
    FReadOnly := Value;
    FEdit.ReadOnly := Value;
  end;
  FSavedReadOnly := FReadOnly;
end;

procedure TTisTagEditor.SetTagBgColor(const Value: TColor);
begin
  if FTagBgColor <> Value then
  begin
    FTagBgColor := Value;
    Invalidate;
  end;
end;

procedure TTisTagEditor.SetTagBorderColor(const Value: TColor);
begin
  if FTagBorderColor <> Value then
  begin
    FTagBorderColor := Value;
    Invalidate;
  end;
end;

procedure TTisTagEditor.SetTagHeight(const Value: integer);
begin
  if FTagHeight <> Value then
  begin
    FTagHeight := Value;
    UpdateMetrics;
    Invalidate;
  end;
end;

procedure TTisTagEditor.SetTagRoundBorder(const Value: integer);
begin
  if Value > 10 then
    FTagRoundBorder := 10
  else if Value < 0 then
    FTagRoundBorder := 0
  else
    FTagRoundBorder := Value;
end;

procedure TTisTagEditor.SetTextColor(const Value: TColor);
begin
  if FTextColor <> Value then
  begin
    FTextColor := Value;
    Invalidate;
  end;
end;

procedure TTisTagEditor.ShowEditor;
begin
  FEdit.Left := FEditPos.X;
  FEdit.Top := FEditPos.Y;
  FEdit.Width := ClientWidth - FEdit.Left - FSpacing;
  FEdit.Color := FEditorColor;
  FEdit.Text := '';
  FEdit.Show;
  FEdit.SetFocus;
end;

procedure TTisTagEditor.SetSpacing(const Value: integer);
begin
  if FSpacing <> Value then
  begin
    FSpacing := Value;
    UpdateMetrics;
    Invalidate;
  end;
end;

{ TTags }

type
{$HINTS OFF}
  TCollectionHack = class(TPersistent);
  //private
  //  FItemClass: TCollectionItemClass;
  //  FItems: TList;
  //end;
{$HINTS ON}

function TTags.Add(aItemConfig: TTagItemConfig; const aText: string): TTagItem;
begin
  result := TTagItem(inherited Add);
  result.FText := AText;
  result.FCanDeleteTag := aItemConfig.CanDeleteTag;
  result.FTagBgColor := aItemConfig.TagBgColor;
  result.FTagBorderColor := aItemConfig.TagBorderColor;
  result.FTextColor := aItemConfig.TextColor;
end;

function TTags.Add(const aText: string): TTagItem;
begin
  result := TTagItem(inherited Add);
  result.FText := aText;
  result.FCanDeleteTag := FTagEditor.FDeleteTagButton;
  result.FTagBgColor := FTagEditor.FTagBgColor;
  result.FTagBorderColor := FTagEditor.FTagBorderColor;
  result.FTextColor := FTagEditor.FTextColor;
end;

constructor TTags.Create(ATagEditor: TTisTagEditor;
  ATagsItemClass: TTagItemCLass);
begin
  inherited Create(ATagsItemClass);
  FTagEditor := ATagEditor;
end;

procedure TTags.DeleteAll;
begin
  while Self.Count > 0 do
    Self.Delete(0);
end;

function TTags.DelimitedText: string;
var
  i: integer;
begin
  result := '';
  for i := 0 to Self.Count - 1 do
  begin
    result := result + IfThen(result <> '', ',') + Self.Items[i].Text;
  end;
end;

function TTags.GetOwner: TPersistent;
begin
  result := FTagEditor;
end;

function TTags.GetTagItem(Index: integer): TTagItem;
begin
  result := TTagItem(inherited Items[Index]);
end;

function TTags.IndexOf(const aText: string): integer;
var
  Index: integer;
begin
  result := -1;
  for index := 0 to Self.Count - 1 do
  begin
    if Self.Items[index].Text = aText then
    begin
      result := index;
      break;
    end;
  end;
end;

procedure TTags.Move(CurIndex, NewIndex: integer);
//var
//  TempList: TList;
begin
  //TempList := TCollectionHack(Self).FItems;  //todo
  //TempList.Exchange(CurIndex, NewIndex);
end;

procedure TTags.SetTagItem(Index: integer; const Value: TTagItem);
begin
  Items[Index].Assign(Value);
end;

{ TTagItem }

procedure TTagItem.Assign(Source: TPersistent);
begin
  inherited Assign(Source);
end;

constructor TTagItem.Create(aCollection: TCollection);
begin
  inherited Create(aCollection);
  FCanDeleteTag := GetTagEditor.DeleteTagButton;
  FTagBgColor := GetTagEditor.FTagBgColor;
  FTextColor := GetTagEditor.FTextColor;
end;

function TTagItem.GetTagEditor: TTisTagEditor;
begin
  if Assigned(Collection) and (Collection is TTags) then
    result := TTags(Collection).FTagEditor
  else
    result := nil;
end;

procedure TTagItem.UpdateTagEditor;
var
  e: TTisTagEditor;
begin
  e := GetTagEditor;
  if assigned(e) then
  begin
    e.UpdateMetrics;
    e.Invalidate;
  end;
end;

procedure TTagItem.SetCanDeleteTag(const Value: Boolean);
begin
  FCanDeleteTag := Value;
end;

procedure TTagItem.SetTagBgColor(const Value: TColor);
begin
  if FTagBgColor <> Value then
  begin
    FTagBgColor := Value;
    UpdateTagEditor;
  end;
end;

procedure TTagItem.SetTagBorderColor(const Value: TColor);
begin
  if FTagBorderColor <> Value then
  begin
    FTagBorderColor := Value;
    UpdateTagEditor;
  end;
end;

procedure TTagItem.SetText(const aValue: string);
begin
  if FText <> aValue then
  begin
    FText := aValue;
    UpdateTagEditor;
  end;
end;

procedure TTagItem.SetTextColor(const Value: TColor);
begin
  if FTextColor <> Value then
  begin
    FTextColor := Value;
    UpdateTagEditor;
  end;
end;

procedure TTagItem.SetCollection(Value: TCollection);
begin
  inherited SetCollection(Value);
  UpdateTagEditor;
end;

initialization
  Screen.Cursors[crHandPoint] := LoadCursor(0, IDC_HAND);

end.
