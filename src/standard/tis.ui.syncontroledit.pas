unit tis.ui.syncontroledit;

{$i tis.ui.defines.inc}

interface

uses
  Classes, SysUtils, LResources, Forms, Controls, Graphics, Dialogs, SynEdit,
  SynEditMarkupSelection, SynEditPointClasses, SynEditTypes, mormot.core.base,
  SynEditMiscClasses, mormot.core.data, mormot.core.variants, SynCompletion,
  LazVersion;

type
  TMarkupRecord = record
    /// Key of the property
    key: RawUTF8;
    /// Value of the markup
    value: RawUtf8;
    /// Id of the markup (for key with multiple markups)
    id: Integer;
    /// Markup with the selections style
    markup: TSynEditMarkupSelection;
    /// Bloque with the selection of the value
    bloque: TSynEditSelection;
  end;
  PMarkupRecord = ^TMarkupRecord;
  TMarkupsArray = record
    list: array of TMarkupRecord;
    counts: TDocVariantData;
  end;
  PMarkupsArray = ^TMarkupsArray;
  PSynSelectedColor = ^TSynSelectedColor;

  { TTisControlSynEditor }

  TTisControlSynEditor = class(TSynEdit)
  private
    fErrorMarkups: TMarkupsArray;
    fInfoMarkups: TMarkupsArray;

    procedure clearMarkups(markupArray: PMarkupsArray);
    procedure RemoveMarkup(index: Integer; markupArray: PMarkupsArray);
    procedure setMarkup(key, Value: String; id: Integer;
      markupArray: PMarkupsArray; markupInfo: TSynSelectedColor;
  valuePos: Integer=-1);
    procedure AddMarkup(newMarkup: TMarkupRecord; markupArray: PMarkupsArray);
    function GetMarkup(key: String; markupArray: PMarkupsArray; id:Integer=-1): PMarkupRecord;
    function GetMarkupIndex(key: String; markupArray: PMarkupsArray; id:Integer=-1): Integer;
    function GetValueStartPointOf(key: String): TPoint;
    procedure RemoveMarkup(key: String; markupArray: PMarkupsArray; id:Integer=-1);
  public
    procedure clearMarkups;
    procedure RemoveInfo(key: String);
    procedure AddError(key, value: String);
    procedure AddInfo(key, value: String; valuePos: Integer=-1; bgColor: TColor=clYellow);
    procedure RemoveError(key: String);
    function getValueStartPosOf(key: String): Integer;
    function getCurrentKey: String;
    function getCurrentCompletedWord: String;
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  end;

implementation

{$if laz_fullversion >= 4990000}
uses
  LazEditTextAttributes;
{$endif}

constructor TTisControlSynEditor.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  SetLength(fErrorMarkups.list, 0);
  SetLength(fInfoMarkups.list, 0);
end;

destructor TTisControlSynEditor.Destroy;
begin
  clearMarkups;
  inherited Destroy;
end;

procedure TTisControlSynEditor.clearMarkups(markupArray: PMarkupsArray);
begin
  Delete(markupArray^.list, 0, Length(markupArray^.list));
  markupArray^.counts.Init([dvoReturnNullForUnknownProperty]);
end;

procedure TTisControlSynEditor.clearMarkups;
begin
  clearMarkups(@fErrorMarkups);
  clearMarkups(@fInfoMarkups);
end;

function TTisControlSynEditor.GetMarkupIndex(key: String;
  markupArray: PMarkupsArray; id: Integer): Integer;
var
  i: Integer;
begin
  if Length(markupArray^.list) = 0 then
    Exit(-1);
  for i := 0 to Length(markupArray^.list) - 1 do
    if (markupArray^.list[i].key = key) and ((id = -1) or (markupArray^.list[i].id = id)) then
      Exit(i);
  Exit(-1);
end;

function TTisControlSynEditor.GetMarkup(key: String;
  markupArray: PMarkupsArray; id: Integer): PMarkupRecord;
var
  i: Integer;
begin
  i := GetMarkupIndex(key, markupArray, id);
  if i <> -1 then
    Exit(@markupArray^.list[i]);
  Exit(nil);
end;

procedure TTisControlSynEditor.AddMarkup(newMarkup: TMarkupRecord; markupArray: PMarkupsArray);
begin
  SetLength(markupArray^.list, length(markupArray^.list) + 1);
  markupArray^.list[length(markupArray^.list) - 1] := newMarkup;
end;

procedure TTisControlSynEditor.RemoveMarkup(index: Integer; markupArray: PMarkupsArray);
begin
  if index = -1 then
    Exit;
  MarkupManager.RemoveMarkUp(markupArray^.list[index].markup);
  markupArray^.list[index].markup.Free;
  markupArray^.list[index].bloque.Free;
  Delete(markupArray^.list, index, 1);
end;

procedure TTisControlSynEditor.RemoveMarkup(key: String;
  markupArray: PMarkupsArray; id: Integer);
begin
  RemoveMarkup(GetMarkupIndex(key, markupArray, id), markupArray);
end;

procedure TTisControlSynEditor.RemoveError(key: String);
begin
  RemoveMarkup(key, @fErrorMarkups);
end;

procedure TTisControlSynEditor.setMarkup(key, Value: String; id:Integer;
  markupArray: PMarkupsArray; markupInfo: TSynSelectedColor; valuePos:Integer);
var
  markupPtr: PMarkupRecord;
  newMarkup: TMarkupRecord;
  beginPoint: TPoint;
  startPos: Integer;
begin
  startPos := GetValueStartPosOf(key);
  if startPos = 0 then
      Exit;
  if valuePos = -1 then
    valuePos := Pos(value, Text, startPos)
  else
    valuePos := Pos(value, Text, startPos + valuePos);
  beginPoint := CharIndexToRowCol(valuePos - 1);
  markupPtr := GetMarkup(key, markupArray, id);
  if markupPtr = nil then
  begin
    newMarkup.key := key;
    newMarkup.value:=Value;
    newMarkup.id := id;
    newMarkup.bloque := TSynEditSelection.Create(ViewedTextBuffer, false);
    newMarkup.bloque.InvalidateLinesMethod:= @InvalidateLines;
    newMarkup.bloque.StartLineBytePos := beginPoint;
    newMarkup.bloque.EndLineBytePos := beginPoint.Add(Point(Length(value), 0));
    newMarkup.markup := TSynEditMarkupSelection.Create(self, newMarkup.bloque);
    newMarkup.markup.Enabled := true;
    newMarkup.markup.MarkupInfoSeletion.Foreground := clBlack;
    newMarkup.markup.MarkupInfoSeletion.Background := Color;
    if markupInfo <> nil then
    begin
      newMarkup.markup.MarkupInfo.FrameEdges := markupInfo.FrameEdges;
      newMarkup.markup.MarkupInfo.FrameColor := markupInfo.FrameColor;
      newMarkup.markup.MarkupInfo.FrameStyle := markupInfo.FrameStyle;
      newMarkup.markup.MarkupInfo.Background := markupInfo.Background;
      MarkupManager.AddMarkUp(newMarkup.markup);
    end;
    AddMarkup(newMarkup, markupArray);
  end else if markupInfo <> nil then
  begin
    markupPtr^.bloque.StartLineBytePos := beginPoint;
    markupPtr^.bloque.EndLineBytePos := beginPoint.Add(Point(Length(value), 0));
    markupPtr^.markup.MarkupInfo.FrameEdges := markupInfo.FrameEdges;
    markupPtr^.markup.MarkupInfo.FrameColor := markupInfo.FrameColor;
    markupPtr^.markup.MarkupInfo.FrameStyle := markupInfo.FrameStyle;
    markupPtr^.markup.MarkupInfo.Background := markupInfo.Background;
  end;
end;

procedure TTisControlSynEditor.RemoveInfo(key: String);
var
  count, i: Integer;
begin
  count := fInfoMarkups.counts.I[key];
  if count = 0 then
    Exit;
  for i := 0 to fInfoMarkups.counts.I[key] - 1 do
    RemoveMarkup(key, @fInfoMarkups, i);
  fInfoMarkups.counts.I[key] := 0;
end;


procedure TTisControlSynEditor.AddInfo(key, value: String; valuePos: Integer; bgColor: TColor);
var
  markupInfos: TSynSelectedColor;
  count: Integer;
begin
  count := fInfoMarkups.counts.I[key];
  markupInfos := TSynSelectedColor.Create();
  markupInfos.Background := bgColor;
  setMarkup(key, value, count, @fInfoMarkups, markupInfos, valuePos);
  markupInfos.Free;
  fInfoMarkups.counts.I[key] := count + 1;
end;

procedure TTisControlSynEditor.AddError(key, value: String);
var
  markupInfos: TSynSelectedColor;
begin
  markupInfos := TSynSelectedColor.Create();
  markupInfos.FrameColor:= clRed;
  markupInfos.FrameStyle:=slsWaved;
  markupInfos.FrameEdges:=sfeBottom;
  markupInfos.Background := clNone;
  setMarkup(key, value, -1, @fErrorMarkups, markupInfos);
  markupInfos.Free;
end;

function TTisControlSynEditor.getValueStartPosOf(key: String):Integer;
var
  valuePos, i, j: Integer;
begin
  for i := 0 to Lines.Count - 1 do
  begin
    if not Lines[i].StartsWith(key) then
      continue;
    valuePos := Pos(':', Lines[i], Length(key));
    j := Length(key) + 1;
    while j < valuePos do
    begin
      if Lines[i][j] <> ' ' then
        break;
      inc(j);
    end;
    if j = valuePos then
      Exit(RowColToCharIndex(Point(j, i + 1)));
  end;
  Exit(0);
end;

function TTisControlSynEditor.getCurrentKey: String;
var
  i: Integer;
begin
  if (Lines[CaretY - 1] <> '') and (Lines[CaretY - 1][1] = '#') then
    Exit('');
  for i := CaretY - 1 downto 0 do
  begin
    if (Lines[i] = '') or (Lines[i][1] = ' ') then
      continue;
    Exit(Lines[i].Substring(0, Pos(' ', Lines[i]) - 1));
  end;
  Exit('');
end;

function TTisControlSynEditor.getCurrentCompletedWord: String;
var
  start: TPoint;
  CurLine: String;
begin
  start:=LogicalCaretXY;
  CurLine:=Lines[start.Y - 1];
  while (start.X>1) and (start.X-1<=length(CurLine)) and (IsIdentChar(CurLine[start.X-1])) do
    Dec(start.X);
  if start <> LogicalCaretXY then
      Exit(Lines[start.Y - 1].Substring(start.x - 1, LogicalCaretXY.X - start.x));
  Exit('');
end;

function TTisControlSynEditor.GetValueStartPointOf(key: String): TPoint;
begin
  Exit(CharIndexToRowCol(getValueStartPosOf(key)));
end;

end.
