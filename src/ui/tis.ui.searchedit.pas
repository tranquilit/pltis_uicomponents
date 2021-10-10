// -----------------------------------------------------------------
//    This file is part of Tranquil IT Software
//    Copyright (C) 2012 - 2021  Tranquil IT https://www.tranquil.it
//    All Rights Reserved.
// ------------------------------------------------------------------
unit tis.ui.searchedit;

{$i mormot.defines.inc}

interface

uses
  Classes, SysUtils, LResources, Forms, Controls, Graphics, Dialogs, StdCtrls,
  ExtCtrls, Buttons;

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
    function GetFlat: Boolean;
    procedure SetFlat(aValue: Boolean);
    function GetGlyph: TBitmap;
    procedure SetGlyph(aValue: TBitmap);
    function GetVisible: Boolean;
    procedure SetVisible(aValue: Boolean);
    procedure SetKind(aValue: TButtonKind);
    function GetOnClick: TNotifyEvent;
    procedure SetOnClick(aValue: TNotifyEvent);
  protected
    function Buttons: TButtonCollection;
  public
    destructor Destroy; override;
    function Button: TSpeedButton;
  published
    // ------------------------------- new properties ----------------------------------
    property Flat: Boolean read GetFlat write SetFlat default False;
    property Glyph: TBitmap read GetGlyph write SetGlyph;
    property Kind: TButtonKind read fKind write SetKind default bkCustom;
    property Visible: Boolean read GetVisible write SetVisible default True;
    // ------------------------------- new events ----------------------------------
    property OnClick: TNotifyEvent read GetOnClick write SetOnClick;
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

  TInputOption = (
    ioAutoSearch
  );

  TInputOptions = set of TInputOption;

  TInput = class(TPersistent)
  private
    fMinChars: Integer;
    fOptions: TInputOptions;
    fTimer: TTimer;
  protected
    const DefaultMinChars = 1;
    const DefaultOptions = [ioAutoSearch];
  published
    constructor Create;
    property MinChars: Integer read fMinChars write fMinChars default DefaultMinChars;
    property Options: TInputOptions read fOptions write fOptions default DefaultOptions;
    property Timer: TTimer read fTimer write fTimer;
  end;

  TTisSearchEdit = class(TEdit)
  private
    fButtons: TButtonCollection;
    fInput: TInput;
    procedure SetDefault;
    procedure SetUpEdit;
    procedure SetUpButtons;
  protected
    // ------------------------------- inherited methods ----------------------------------
    procedure SetParent(aNewParent: TWinControl); override;
    procedure DoSetBounds(aLeft, aTop, aWidth, aHeight: Integer); override;
    procedure Loaded; override;
    // ------------------------------- new methods ----------------------------------
    procedure DoSearchClick(Sender: TObject); virtual;
    procedure DoClearClick(Sender: TObject); virtual;
    procedure Searching; virtual;
  public
    // ------------------------------- inherited methods ----------------------------------
    constructor Create(aOwner: TComponent); override;
    destructor Destroy; override;
    procedure TextChanged; override;
    procedure EnabledChanged; override;
    procedure EditingDone; override;
  published
    // ------------------------------- new properties ----------------------------------
    property Buttons: TButtonCollection read fButtons write fButtons;
    property Input: TInput read fInput write fInput;
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

function TButtonItem.GetOnClick: TNotifyEvent;
begin
  result := Button.OnClick;
end;

procedure TButtonItem.SetOnClick(aValue: TNotifyEvent);
begin
  Button.OnClick := aValue;
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

{ TInput }

constructor TInput.Create;
begin
  inherited Create;
  fMinChars := DefaultMinChars;
  fOptions := DefaultOptions;
end;

{ TTisSearchEdit }

procedure TTisSearchEdit.SetDefault;
begin
  Width := 130;
  Height := 24;
end;

procedure TTisSearchEdit.SetUpEdit;
begin
  TextHint := 'Search keywords';
  Text := '';
  ControlStyle := ControlStyle - [csSetCaption];
end;

procedure TTisSearchEdit.SetUpButtons;
var
  i: Integer;
  b: TButtonItem;
begin
  if csDesigning in ComponentState then
    exit;
  for i := 0 to fButtons.Count -1 do
  begin
    b := fButtons.Items[i];
    if not assigned(b.OnClick) then
    begin
      case b.Kind of
        bkSearch: b.OnClick := DoSearchClick;
        bkClear: b.OnClick := DoClearClick;
      end;
    end;
  end;
end;

procedure TTisSearchEdit.SetParent(aNewParent: TWinControl);
begin
  inherited SetParent(aNewParent);
  if csDestroying in ComponentState then
    exit;
  fButtons.Invalidate;
end;

procedure TTisSearchEdit.DoSetBounds(aLeft, aTop, aWidth, aHeight: Integer);
begin
  inherited DoSetBounds(aLeft, aTop, aWidth, aHeight);
  if assigned(fButtons) then
    fButtons.Invalidate;
end;

procedure TTisSearchEdit.Loaded;
begin
  inherited Loaded;
  SetUpButtons;
end;

procedure TTisSearchEdit.DoSearchClick(Sender: TObject);
begin
  Searching;
end;

procedure TTisSearchEdit.DoClearClick(Sender: TObject);
begin
  Clear;
end;

procedure TTisSearchEdit.Searching;
var
  t: TTimer;
begin
  t := fInput.Timer;
  if assigned(t) then
  begin
    t.Enabled := False;
    if (Length(Text) >= fInput.MinChars) then
      t.Enabled := True;
  end;
end;

constructor TTisSearchEdit.Create(aOwner: TComponent);
begin
  inherited Create(AOwner);
  fButtons := TButtonCollection.Create(self);
  fInput := TInput.Create;
  SetDefault;
  SetUpEdit;
end;

destructor TTisSearchEdit.Destroy;
begin
  fButtons.Free;
  fInput.Free;
  inherited Destroy;
end;

procedure TTisSearchEdit.TextChanged;
begin
  inherited TextChanged;
  if ioAutoSearch in fInput.Options then
    Searching;
end;

procedure TTisSearchEdit.EnabledChanged;
begin
  inherited EnabledChanged;
end;

procedure TTisSearchEdit.EditingDone;
begin
  inherited EditingDone;
  Searching;
end;

initialization
  {$I tis.ui.searchedit.lrs}

end.
