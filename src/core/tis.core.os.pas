// -----------------------------------------------------------------
//    This file is part of Tranquil IT Software
//    Copyright (C) 2012 - 2024  Tranquil IT https://www.tranquil.it
//    All Rights Reserved.
// ------------------------------------------------------------------
unit tis.core.os;

{$i tis.ui.defines.inc}

interface

uses
  Classes,
  SysUtils,
  Dialogs,
  Clipbrd,
  LCLType,
  LCLIntf,
  mormot.core.base,
  mormot.core.unicode;

type
  /// all predefined and extented kinds
  TClipboardKind = (
    /// predefined types -> lcltype.TPredefinedClipboardFormat
    // - do not change this order
    cbkText,
    cbkBitmap,
    cbkPixmap,
    cbkIcon,
    cbkPicture,
    cbkMetaFilePict,
    cbkObject,
    cbkComponent,
    cbkCustomData,
    /// extended types
    // - add more types here - see CLIPBOARD_EXT_KINDS below
    cbkJson
  );

  /// adapter for predefined and extended formats
  TClipboardAdapter = object
  private
    function ToFormat(const aValue: TClipboardKind): TClipboardFormat;
  public
    /// returns TRUE if the buffer match to aValue kind
    function IsValidFor(const aValue: TClipboardKind): Boolean;
    /// convert the buffer into aDest
    function ToStream(const aValue: TClipboardKind; aDest: TStream): Boolean;
    /// open for adding
    procedure Open;
    /// for clear the clipboard before add something
    procedure Clear;
    /// add info to the clipboard
    function Add(const aKind: TClipboardKind; var aBuffer; aSize: Integer): Boolean; overload;
    /// add info to the clipboard
    function Add(const aKind: TClipboardKind; aSource: TStream): Boolean; overload;
    /// close for adding
    procedure Close;
    /// returns the buffer as string
    function AsString: string;
    /// returns the buffer as Utf8
    function AsUtf8: RawUtf8;
    /// returns the buffer as JSON
    function AsJson: RawUtf8;
    /// returns the buffer as Unicode String
    function AsUnicode: SynUnicode;
  end;

implementation

const
  /// predefined types/formats doing a cross between default values and TClipboardTypes
  CLIPBOARD_PRE_TYPES = [cbkText..cbkCustomData];
  CLIPBOARD_PRE_FORMATS: array[cbkText..cbkCustomData] of TPredefinedClipboardFormat = (
    // do not change this order
    pcfText,
    pcfBitmap,
    pcfPixmap,
    pcfIcon,
    pcfPicture,
    pcfMetaFilePict,
    pcfObject,
    pcfComponent,
    pcfCustomData
  );

var
  /// it will holds a pointer for each extended format
  // - it will be initialized with the Format ID for each type on initialization section
  CLIPBOARD_EXT_KINDS: array[cbkJson..cbkJson] of TClipboardFormat;

{ TClipboardAdapter }

function TClipboardAdapter.ToFormat(const aValue: TClipboardKind): TClipboardFormat;
begin
  if aValue in CLIPBOARD_PRE_TYPES then
    result := PredefinedClipboardFormat(CLIPBOARD_PRE_FORMATS[aValue])
  else
    result := CLIPBOARD_EXT_KINDS[aValue];
end;

function TClipboardAdapter.IsValidFor(const aValue: TClipboardKind): Boolean;
begin
  result := Clipboard.HasFormat(ToFormat(aValue));
end;

function TClipboardAdapter.ToStream(const aValue: TClipboardKind; aDest: TStream): Boolean;
begin
  if Assigned(aDest) then
  begin
    result := Clipboard.GetFormat(ToFormat(aValue), aDest);
    aDest.Seek(0, 0);
  end
  else
    result := False;
end;

procedure TClipboardAdapter.Open;
begin
  Clipboard.Open;
end;

procedure TClipboardAdapter.Clear;
begin
  Clipboard.Clear;
end;

function TClipboardAdapter.Add(const aKind: TClipboardKind; var aBuffer;
  aSize: Integer): Boolean;
begin
  result := Clipboard.AddFormat(ToFormat(aKind), aBuffer, aSize);
end;

function TClipboardAdapter.Add(const aKind: TClipboardKind; aSource: TStream): Boolean;
begin
  result := Clipboard.AddFormat(ToFormat(aKind), aSource);
end;

procedure TClipboardAdapter.Close;
begin
  Clipboard.Close;
end;

function TClipboardAdapter.AsString: string;
begin
  result := Clipboard.AsText;
end;

function TClipboardAdapter.AsUtf8: RawUtf8;
begin
  result := StringToUtf8(Clipboard.AsText);
end;

function TClipboardAdapter.AsJson: RawUtf8;
var
  vJson: TRawByteStringStream;
begin
  vJson := TRawByteStringStream.Create('');
  try
    ToStream(cbkJson, vJson);
    if vJson.DataString = '' then
      ToStream(cbkText, vJson);
    result := vJson.DataString;
  finally
    vJson.Free;
  end;
end;

function TClipboardAdapter.AsUnicode: SynUnicode;
begin
  result := Utf8ToSynUnicode(AsUtf8);
end;

initialization
  CLIPBOARD_EXT_KINDS[cbkJson] := RegisterClipboardFormat('application/json');

end.
