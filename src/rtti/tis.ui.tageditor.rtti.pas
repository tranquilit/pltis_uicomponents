// -----------------------------------------------------------------
//    This file is part of Tranquil IT Software
//    Copyright (C) 2012 - 2021  Tranquil IT https://www.tranquil.it
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
  LCLProc,
  StdCtrls,
  PropEdits,
  RTTICtrls,
  tis.ui.tageditor.core;

type
  TTisTagEditorRtti = class(TTisTagEditor)
  private
    fLink: TPropertyLink;
    procedure SetLink(const aValue: TPropertyLink);
  protected
    procedure ComboBoxEditingDone(Sender: TObject); override;
    procedure LinkLoadFromProperty(Sender: TObject); virtual;
    procedure LinkSaveToProperty(Sender: TObject); virtual;
    function LinkTestEditor(const aTestEditor: TPropertyEditor): Boolean; virtual;
  public
    constructor Create(aOwner: TComponent); override;
    destructor Destroy; override;
    procedure Loaded; override;
  published
    property Link: TPropertyLink read fLink write SetLink;
  end;

implementation

{ TTisTagEditorRtti }

procedure TTisTagEditorRtti.SetLink(const aValue: TPropertyLink);
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
  PropKind := FLink.Editor.GetPropType^.Kind;
  if PropKind = tkClass then
  begin
    CurObject := FLink.Editor.GetObjectValue;
    if CurObject is TStrings then
      TStrings(CurObject).DelimitedText := Tags.DelimitedText;
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

constructor TTisTagEditorRtti.Create(aOwner: TComponent);
begin
  inherited Create(aOwner);
  fLink := TPropertyLink.Create(Self);
  fLink.Filter := [
    {tkUnknown,tkInteger,tkChar,tkEnumeration,}
    {tkFloat,tkSet,tkMethod,}tkSString,tkLString,tkAString,
    tkWString,{tkVariant,tkArray,tkRecord,tkInterface,}
    tkClass{,tkObject,tkWChar,tkBool,tkInt64,}
    {tkQWord},tkDynArray{,tkInterfaceRaw}];
  fLink.OnLoadFromProperty := LinkLoadFromProperty;
  fLink.OnSaveToProperty := LinkSaveToProperty;
  fLink.OnTestEditor := LinkTestEditor;
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
