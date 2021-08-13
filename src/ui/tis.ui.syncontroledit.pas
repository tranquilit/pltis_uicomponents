unit tis.ui.SynControlEdit;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LResources, Forms, Controls, Graphics, Dialogs, SynEdit,
  SynEditMarkupSelection, SynEditPointClasses, SynEditTypes, mormot.core.base;

type
  TErrorMarkupRecord = record
    key: RawUTF8;
    markup: TSynEditMarkupSelection;
    bloque: TSynEditSelection;
  end;
  PErrorMarkupRecord = ^TErrorMarkupRecord;
  TErrorMarkupsArray = array of TErrorMarkupRecord;

  { TTisControlSynEditor }

  TTisControlSynEditor = class(TSynEdit)
  private
    fErrorMarkups: TErrorMarkupsArray;
    procedure AddErrorMarkup(errorMarkup: TErrorMarkupRecord);
    function GetErrorMarkup(key: String): PErrorMarkupRecord;
    function GetErrorMarkupIndex(key: String): Integer;
    function GetValueStartPointOf(key: String): TPoint;
  public
    procedure clearMarkups;
    procedure AddError(key, value: String);
    procedure RemoveError(key: String);
    function getValueStartPosOf(key: String): Integer;
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  end;

implementation

constructor TTisControlSynEditor.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  SetLength(fErrorMarkups, 0);
end;

destructor TTisControlSynEditor.Destroy;
begin
  clearMarkups;
  inherited Destroy;
end;

procedure TTisControlSynEditor.clearMarkups;
begin
  Delete(fErrorMarkups, 0, Length(fErrorMarkups));
end;

function TTisControlSynEditor.GetErrorMarkupIndex(key: String): Integer;
var
  i: Integer;
begin
  if Length(fErrorMarkups) = 0 then
    Exit(-1);
  for i := 0 to Length(fErrorMarkups) - 1 do
    if fErrorMarkups[i].key = key then
      Exit(i);
  Exit(-1);
end;

function TTisControlSynEditor.GetErrorMarkup(key: String): PErrorMarkupRecord;
var
  i: Integer;
begin
  i := GetErrorMarkupIndex(key);
  if i <> -1 then
    Exit(@fErrorMarkups[i]);
  Exit(nil);
end;

procedure TTisControlSynEditor.AddErrorMarkup(errorMarkup: TErrorMarkupRecord);
begin
  SetLength(fErrorMarkups, length(fErrorMarkups) + 1);
  fErrorMarkups[length(fErrorMarkups) - 1] := errorMarkup;
end;

procedure TTisControlSynEditor.RemoveError(key: String);
var
  index: Integer;
begin
  index := GetErrorMarkupIndex(key);
  if index = -1 then
    Exit;
  MarkupManager.RemoveMarkUp(fErrorMarkups[index].markup);
  fErrorMarkups[index].markup.Free;
  fErrorMarkups[index].bloque.Free;
  Delete(fErrorMarkups, index, 1);
end;

procedure TTisControlSynEditor.AddError(key, value: String);
var
  errorMarkupPtr: PErrorMarkupRecord;
  newErrorMarkup: TErrorMarkupRecord;
  beginPoint: TPoint;
  startPos: Integer;
begin
  startPos := GetValueStartPosOf(key);
  if startPos = 0 then
      Exit;
  beginPoint := CharIndexToRowCol(Pos(value, Text, startPos) - 1);
  errorMarkupPtr := GetErrorMarkup(key);
  if errorMarkupPtr = nil then
  begin
    newErrorMarkup.bloque := TSynEditSelection.Create(ViewedTextBuffer, false);
    newErrorMarkup.bloque.InvalidateLinesMethod:= @InvalidateLines;
    newErrorMarkup.markup := TSynEditMarkupSelection.Create(self, newErrorMarkup.bloque);
    newErrorMarkup.bloque.StartLineBytePos := beginPoint;
    newErrorMarkup.bloque.EndLineBytePos := beginPoint.Add(Point(Length(value), 0));
    newErrorMarkup.markup.Enabled := true;
    newErrorMarkup.markup.MarkupInfoSeletion.Foreground := clBlack;
    newErrorMarkup.markup.MarkupInfoSeletion.Background := Color;
    newErrorMarkup.markup.MarkupInfo.FrameColor := clRed;
    newErrorMarkup.markup.MarkupInfo.FrameEdges := sfeBottom;
    newErrorMarkup.markup.MarkupInfo.FrameStyle := slsWaved;
    newErrorMarkup.key := key;
    MarkupManager.AddMarkUp(newErrorMarkup.markup);
    AddErrorMarkup(newErrorMarkup);
  end else
  begin
    errorMarkupPtr^.bloque.StartLineBytePos := beginPoint;
    errorMarkupPtr^.bloque.EndLineBytePos := beginPoint.Add(Point(Length(value), 0));
  end;
end;

function TTisControlSynEditor.getValueStartPosOf(key: String):Integer;
var
  keyPos, valuePos, lineSize, i: Integer;
begin
  lineSize := Pos(':', Text) - 1;
  keyPos:=Pos(key, Text);
  if (keyPos = 0) or (lineSize <= 0) then
    Exit(0);
  while keyPos <> 0 do
  begin
    keyPos := keyPos + Length(key);
    valuePos := Pos(':', Text, keyPos);
    if valuePos = 0 then
      Exit(0);
    i := keyPos;
    while i < valuePos do
    begin
      if Text[i] <> ' ' then
        break;
      inc(i);
    end;
    if i = valuePos then
      Exit(i);
    keyPos:=Pos(key, Text, valuePos);
  end;
  Exit(0);
end;

function TTisControlSynEditor.GetValueStartPointOf(key: String): TPoint;
begin
  Exit(CharIndexToRowCol(getValueStartPosOf(key)));
end;

end.
