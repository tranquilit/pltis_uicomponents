// -----------------------------------------------------------------
//    This file is part of Tranquil IT Software
//    Copyright (C) 2012 - 2024  Tranquil IT https://www.tranquil.it
//    All Rights Reserved.
// ------------------------------------------------------------------
unit tis.ui.tageditor.rtti;

{$i mormot.defines.inc}

interface

uses
  {$ifdef windows}
  Windows,
  {$endif}
  SysUtils,
  Classes,
  Controls,
  ExtCtrls,
  LCLProc,
  StdCtrls,
  PropEdits,
  RTTICtrls,
  tis.ui.tageditor.core;

type
  TTisPropertyLink = class(TPropertyLink)
  private
    fTimer: TTimer;
    function GetTimerInterval: Cardinal;
    procedure SetTimerInterval(aValue: Cardinal);
  protected
    const DefaultOnIdleInterval = 50;
  protected
    procedure TimerCallback(Sender: TObject); virtual;
    procedure OnApplicationIdle(Sender: TObject; var Done: Boolean); override;
  public
    constructor Create(aOwner: TComponent); reintroduce;
  published
    property OnIdleInterval: Cardinal read GetTimerInterval write SetTimerInterval default DefaultOnIdleInterval;
  end;

  TTisTagEditorRtti = class(TTisTagEditor)
  private
    fLink: TTisPropertyLink;
    procedure SetLink(const aValue: TTisPropertyLink);
  protected
    procedure ComboBoxEditingDone(Sender: TObject); override;
    procedure DeleteTag(aTagIndex: Integer); override;
    procedure LinkLoadFromProperty(Sender: TObject); virtual;
    procedure LinkSaveToProperty(Sender: TObject); virtual;
    function LinkTestEditor(const aTestEditor: TPropertyEditor): Boolean; virtual;
    function LinkTestEditing(Sender: TObject): boolean;
  public
    constructor Create(aOwner: TComponent); override;
    destructor Destroy; override;
    procedure Loaded; override;
  published
    property Link: TTisPropertyLink read fLink write SetLink;
  end;

implementation

{ TTisPropertyLink }

function TTisPropertyLink.GetTimerInterval: Cardinal;
begin
  result := fTimer.Interval;
end;

procedure TTisPropertyLink.SetTimerInterval(aValue: Cardinal);
begin
  fTimer.Interval := aValue;
end;

constructor TTisPropertyLink.Create(aOwner: TComponent);
begin
  inherited Create(aOwner);
  fTimer := TTimer.Create(aOwner);
  fTimer.Enabled := False;
  fTimer.Interval := DefaultOnIdleInterval;
  fTimer.OnTimer := TimerCallback;
end;

procedure TTisPropertyLink.TimerCallback(Sender: TObject);
var
  dummy: Boolean;
begin
  inherited OnApplicationIdle(Sender, dummy);
end;

procedure TTisPropertyLink.OnApplicationIdle(Sender: TObject;
  var Done: Boolean);
begin
  fTimer.Enabled := ploReadOnIdle in Options;
  if not fTimer.Enabled then
  begin
    Done := False;
    inherited OnApplicationIdle(Sender, Done);
  end;
end;

{ TTisTagEditorRtti }

procedure TTisTagEditorRtti.SetLink(const aValue: TTisPropertyLink);
begin
  if fLink = aValue then
    exit;
  fLink.Assign(aValue);
end;

procedure TTisTagEditorRtti.ComboBoxEditingDone(Sender: TObject);
begin
  inherited ComboBoxEditingDone(Sender);
  fLink.EditingDone;
end;

procedure TTisTagEditorRtti.DeleteTag(aTagIndex: Integer);
begin
  inherited DeleteTag(aTagIndex);
  fLink.EditingDone;
end;

procedure TTisTagEditorRtti.LinkLoadFromProperty(Sender: TObject);
var
  PropKind: TTypeKind;
  CurObject: TObject;
begin
  if Sender = nil then ;
  if fLink.Editor = nil then
    exit;
  PropKind := fLink.Editor.GetPropType^.Kind;
  if PropKind = tkClass then
  begin
    CurObject := fLink.Editor.GetObjectValue;
    if CurObject is TStrings then
      Tags.Assign(TStrings(CurObject))
  end
  else if PropKind in [tkSString,tkLString,tkAString,tkWString] then
    Tags.DelimitedText := fLink.GetAsText;
end;

procedure TTisTagEditorRtti.LinkSaveToProperty(Sender: TObject);
var
  PropKind: TTypeKind;
  CurObject: TObject;
begin
  if Sender = nil then ;
  if fLink.Editor = nil then
    exit;
  PropKind := fLink.Editor.GetPropType^.Kind;
  if PropKind = tkClass then
  begin
    CurObject := fLink.Editor.GetObjectValue;
    if CurObject is TStrings then
    begin
      with TStrings(CurObject) do
      begin
        Delimiter := TagInput.DefaultDelimiter;
        DelimitedText := Tags.DelimitedText;
      end;
    end;
  end
  else if PropKind in [tkSString,tkLString,tkAString,tkWString] then
  begin
    fLink.SetAsText(Tags.DelimitedText);
  end;
end;

function TTisTagEditorRtti.LinkTestEditor(const aTestEditor: TPropertyEditor): Boolean;
begin
  result := (aTestEditor is TStringPropertyEditor) or (aTestEditor is TStringsPropertyEditor);
end;

function TTisTagEditorRtti.LinkTestEditing(Sender: TObject): boolean;
begin
  if Sender = nil then ;
  result := Focused or (fComboBox.Visible and fComboBox.Focused);
end;

constructor TTisTagEditorRtti.Create(aOwner: TComponent);
begin
  inherited Create(aOwner);
  fLink := TTisPropertyLink.Create(Self);
  fLink.Filter := [
    {tkUnknown,tkInteger,tkChar,tkEnumeration,}
    {tkFloat,tkSet,tkMethod,}tkSString,tkLString,tkAString,
    tkWString,{tkVariant,tkArray,tkRecord,tkInterface,}
    tkClass{,tkObject,tkWChar,tkBool,tkInt64,}
    {tkQWord},tkDynArray{,tkInterfaceRaw}];
  fLink.OnLoadFromProperty := LinkLoadFromProperty;
  fLink.OnSaveToProperty := LinkSaveToProperty;
  fLink.OnTestEditor := LinkTestEditor;
  fLink.OnTestEditing := LinkTestEditing;
end;

destructor TTisTagEditorRtti.Destroy;
begin
  FreeThenNil(fLink);
  inherited Destroy;
end;

procedure TTisTagEditorRtti.Loaded;
begin
  inherited Loaded;
  fLink.LoadFromProperty;
end;

end.
