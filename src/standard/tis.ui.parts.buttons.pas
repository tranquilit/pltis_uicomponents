// -----------------------------------------------------------------
//    This file is part of Tranquil IT Software
//    Copyright (C) 2012 - 2024  Tranquil IT https://www.tranquil.it
//    All Rights Reserved.
// ------------------------------------------------------------------
unit tis.ui.parts.buttons;

{$i tis.ui.defines.inc}

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
  Buttons,
  mormot.core.unicode,
  mormot.core.rtti;

type
  /// kind that will define some properties of the button
  TButtonKind = (
    bkCustom,
    bkSearch,
    bkClear
  );

  /// class forward
  TButtonCollection = class;

  /// define a item for the collection
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
    const DefaultFlat = True;
  protected
    // ------------------------------- new properties ----------------------------------
    function Buttons: TButtonCollection;
  public
    destructor Destroy; override;
    function Button: TSpeedButton;
  published
    // ------------------------------- new properties ----------------------------------
    /// if the button will be flat mode
    property Flat: Boolean read GetFlat write SetFlat default DefaultFlat;
    /// button icon
    property Glyph: TBitmap read GetGlyph write SetGlyph;
    /// kind that could define some behavior for the button
    property Kind: TButtonKind read fKind write SetKind default bkCustom;
    /// the item name
    property Name: string read fName write fName;
    /// if the button is visible
    // - it will visible/invisible at design time as well
    property Visible: Boolean read GetVisible write SetVisible default True;
  end;

  /// a contract that will allow a better customization when adding
  // a new button to the collection
  IButtonProperties = interface
  ['{D8B143FB-ECD2-4F54-ADA6-E22CB34173D2}']
    /// define any property/event for the item
    procedure Setup(aButton: TButtonItem);
  end;

  /// button collection
  TButtonCollection = class(TCollection)
  private
    fControl: TWinControl;
    function GetButtonItem(aIndex: Integer): TButtonItem;
    procedure SetButtonItem(aIndex: Integer; aValue: TButtonItem);
  protected
    // ------------------------------- inherited methods ----------------------------------
    function GetOwner: TPersistent; override;
  public
    constructor Create(aControl: TWinControl); reintroduce;
    // ------------------------------- new methods ----------------------------------
    function Add: TCollectionItem; reintroduce;
    /// setup the item's properties/events using IButtonProperties implementation
    // from Control
    // - you can override to use another approach
    procedure Setup(aButton: TButtonItem); virtual;
    /// call it when something was changed on items
    procedure Invalidate; virtual;
    // ------------------------------- new properties ----------------------------------
    /// point to the control that created the collection
    property Control: TWinControl read fControl;
    /// items of the collection
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
  vImage: TImage;
  vNewName, vResName: string;
begin
  if fKind = aValue then
    exit;
  fKind := aValue;
  vImage := TImage.Create(nil);
  try
    vNewName := Utf8ToString(LowerCase(GetEnumNameTrimed(TypeInfo(TButtonKind), ord(aValue))));
    vResName := 'searchedit_' + vNewName;
    Name := vNewName + Index.ToString;
    if Assigned(LazarusResources.Find(vResName)) then
    begin
      vImage.Picture.LoadFromLazarusResource(vResName);
      Button.Glyph.Assign(vImage.Picture.Bitmap);
    end
    else
      Button.Glyph.Clear;
    Buttons.Invalidate;
  finally
    vImage.Free;
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
    fButton.Flat := DefaultFlat;
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

procedure TButtonCollection.Setup(aButton: TButtonItem);
var
  vProps: IButtonProperties;
begin
  if Supports(fControl, IButtonProperties, vProps) then
    vProps.Setup(aButton);
end;

procedure TButtonCollection.Invalidate;
const
  cSpace = 2;
var
  v1, vLeftCount: Integer;
  vButton: TButtonItem;
  vSpeedButton: TSpeedButton;
begin
  vLeftCount := fControl.Left + fControl.Width + cSpace;
  for v1 := 0 to Count -1 do
  begin
    vButton := TButtonItem(Items[v1]);
    Setup(vButton);
    vSpeedButton := vButton.Button;
    vSpeedButton.SetBounds(fControl.Left, fControl.Top, vSpeedButton.Width, vSpeedButton.Height);
    if vSpeedButton.Visible then
    begin
      vSpeedButton.Left := vLeftCount;
      inc(vLeftCount, vSpeedButton.Width + cSpace);
    end;
    vSpeedButton.Parent := fControl.Parent;
    vSpeedButton.Tag := v1; // it could be used to locate the corresponding item instance
    vSpeedButton.Invalidate;
  end;
end;

initialization
  {$I tis.ui.searchedit.lrs}

end.
