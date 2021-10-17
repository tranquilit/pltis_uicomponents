// -----------------------------------------------------------------
//    This file is part of Tranquil IT Software
//    Copyright (C) 2012 - 2021  Tranquil IT https://www.tranquil.it
//    All Rights Reserved.
// ------------------------------------------------------------------
unit tis.ui.parts.buttons;

{$i mormot.defines.inc}

interface

uses
  Classes,
  SysUtils,
  LResources,
  Forms,
  Controls,
  Graphics,
  Dialogs,
  StdCtrls,
  ExtCtrls,
  Buttons;

type
  TButtonKind = (
    bkCustom,
    bkSearch,
    bkClear
  );

  TButtonCollection = class;

  TButtonItem = class(TCollectionItem)
  private
    fButton: TSpeedButton;
    fKind: TButtonKind;
    fName: string;
    function GetFlat: Boolean;
    procedure SetFlat(aValue: Boolean);
    function GetGlyph: TBitmap;
    procedure SetGlyph(aValue: TBitmap);
    function GetVisible: Boolean;
    procedure SetVisible(aValue: Boolean);
    procedure SetKind(aValue: TButtonKind);
  protected
    // ------------------------------- new properties ----------------------------------
    function Buttons: TButtonCollection;
  public
    destructor Destroy; override;
    function Button: TSpeedButton;
  published
    // ------------------------------- new properties ----------------------------------
    property Flat: Boolean read GetFlat write SetFlat default False;
    property Glyph: TBitmap read GetGlyph write SetGlyph;
    property Kind: TButtonKind read fKind write SetKind default bkCustom;
    property Name: string read fName write fName;
    property Visible: Boolean read GetVisible write SetVisible default True;
  end;

  TButtonCollection = class(TCollection)
  private
    fControl: TWinControl;
    function GetButtonItem(aIndex: Integer): TButtonItem;
    procedure SetButtonItem(aIndex: Integer; aValue: TButtonItem);
  protected
    function GetOwner: TPersistent; override;
  public
    constructor Create(aControl: TWinControl); reintroduce;
    function Add: TCollectionItem; reintroduce;
    procedure Invalidate;
    property Control: TWinControl read fControl;
    property Items[aIndex: Integer]: TButtonItem read GetButtonItem write SetButtonItem; default;
  end;

implementation

{$R icons.rc}

{ TButtonItem }

function TButtonItem.GetFlat: Boolean;
begin
  result := Button.Flat;
end;

procedure TButtonItem.SetFlat(aValue: Boolean);
begin
  Button.Flat := aValue;
  Buttons.Invalidate;
end;

function TButtonItem.GetGlyph: TBitmap;
begin
  result := Button.Glyph;
end;

procedure TButtonItem.SetGlyph(aValue: TBitmap);
begin
  Button.Glyph.Assign(aValue);
  Buttons.Invalidate;
end;

procedure TButtonItem.SetKind(aValue: TButtonKind);
var
  img: TImage;
  n: string;
begin
  if fKind = aValue then
    exit;
  Collection.BeginUpdate;
  fKind := aValue;
  img := TImage.Create(nil);
  try
    case fKind of
      bkSearch:
        n := 'searchedit_search';
      bkClear:
        n := 'searchedit_clear';
    else
      exit;
    end;
    img.Picture.LoadFromLazarusResource(n);
    Button.Glyph.Assign(img.Picture.Bitmap);
    Buttons.Invalidate;
  finally
    img.Free;
  end;
end;

function TButtonItem.Buttons: TButtonCollection;
begin
  result := Collection as TButtonCollection;
end;

function TButtonItem.GetVisible: Boolean;
begin
  result := Button.Visible;
end;

procedure TButtonItem.SetVisible(aValue: Boolean);
begin
  Button.Visible := aValue;
  Buttons.Invalidate;
end;

destructor TButtonItem.Destroy;
begin
  fButton.Free;
  inherited Destroy;
end;

function TButtonItem.Button: TSpeedButton;
begin
  if fButton = nil then
  begin
    fButton := TSpeedButton.Create(nil);
    fButton.ControlStyle := fButton.ControlStyle + [csNoDesignSelectable];
  end;
  result := fButton;
end;

{ TButtonCollection }

function TButtonCollection.GetButtonItem(aIndex: Integer): TButtonItem;
begin
  result := TButtonItem(inherited Items[aIndex]);
end;

procedure TButtonCollection.SetButtonItem(aIndex: Integer; aValue: TButtonItem);
begin
  Items[aIndex].Assign(aValue);
end;

function TButtonCollection.GetOwner: TPersistent;
begin
  Result:= fControl;
end;

constructor TButtonCollection.Create(aControl: TWinControl);
begin
  inherited Create(TButtonItem);
  fControl := aControl;
end;

function TButtonCollection.Add: TCollectionItem;
begin
  result := inherited Add;
  Invalidate;
end;

procedure TButtonCollection.Invalidate;
const
  SPACE = 2;
var
  i, m: Integer;
  b: TSpeedButton;
begin
  m := fControl.Left + fControl.Width + SPACE;
  for i := 0 to Count -1 do
  begin
    b := TButtonItem(Items[i]).Button;
    b.SetBounds(fControl.Left, fControl.Top, b.Width, b.Height);
    if b.Visible then
    begin
      b.Left := m;
      inc(m, b.Width + SPACE);
    end;
    b.Parent := fControl.Parent;
    b.Invalidate;
  end;
end;

initialization
  {$I tis.ui.searchedit.lrs}

end.
