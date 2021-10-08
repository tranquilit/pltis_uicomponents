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
  TButton = class(TPersistent)
  private
    fButton: TSpeedButton;
    function GetFlat: Boolean;
    procedure SetFlat(aValue: Boolean);
    function GetGlyph: TBitmap;
    procedure SetGlyph(aValue: TBitmap);
    function GetVisible: Boolean;
    procedure SetVisible(aValue: Boolean);
  public
    constructor Create;
    destructor Destroy; override;
    property Button: TSpeedButton read fButton;
  published
    // ------------------------------- new properties ----------------------------------
    property Flat: Boolean read GetFlat write SetFlat default False;
    property Glyph: TBitmap read GetGlyph write SetGlyph;
    property Visible: Boolean read GetVisible write SetVisible default True;
  end;

  TButtons = class(TPersistent)
  private
    fSearch: TButton;
    fClear: TButton;
    procedure SetUpImages;
  public
    constructor Create;
    destructor Destroy; override;
  published
    property Search: TButton read fSearch write fSearch;
    property Clear: TButton read fClear write fClear;
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
    fButtons: TButtons;
    fInput: TInput;
    fOnButtonSearchClick: TNotifyEvent;
    fOnClearButtonClick: TNotifyEvent;
    procedure SetUpEdit;
    procedure SetUpButtons;
    procedure SetDefault;
    procedure DoSearchClick(aSender: TObject);
    procedure DoClearClick(aSender: TObject);
  protected
    // ------------------------------- inherited methods ----------------------------------
    procedure SetParent(aNewParent: TWinControl); override;
    procedure DoSetBounds(aLeft, aTop, aWidth, aHeight: Integer); override;
    // ------------------------------- new methods ----------------------------------
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
    property Buttons: TButtons read fButtons write fButtons;
    property Input: TInput read fInput write fInput;
    // ------------------------------- new events ----------------------------------
    property OnButtonSearchClick: TNotifyEvent read fOnButtonSearchClick write fOnButtonSearchClick;
    property OnButtonClearClick: TNotifyEvent read fOnClearButtonClick write fOnClearButtonClick;
  end;

implementation

{$R icons.rc}

{ TButton }

function TButton.GetFlat: Boolean;
begin
  result := fButton.Flat;
end;

procedure TButton.SetFlat(aValue: Boolean);
begin
  fButton.Flat := aValue;
end;

function TButton.GetGlyph: TBitmap;
begin
  result := fButton.Glyph;
end;

procedure TButton.SetGlyph(aValue: TBitmap);
begin
  fButton.Glyph.Assign(aValue);
end;

function TButton.GetVisible: Boolean;
begin
  result := fButton.Visible;
end;

procedure TButton.SetVisible(aValue: Boolean);
begin
  fButton.Visible := aValue;
end;

constructor TButton.Create;
begin
  inherited Create;
  fButton := TSpeedButton.Create(nil);
end;

destructor TButton.Destroy;
begin
  fButton.Free;
  inherited Destroy;
end;

{ TButtons }

procedure TButtons.SetUpImages;
var
  img: TImage;
begin
  img := TImage.Create(nil);
  try
    img.Picture.LoadFromResourceName(HINSTANCE, 'TIS_SEARCH_ICON', TPortableNetworkGraphic);
    fSearch.Glyph.Assign(img.Picture.Bitmap);
    //img.Picture.LoadFromResourceName(HINSTANCE, 'TIS_CLEAR_ICON', TPortableNetworkGraphic);
    //fClear.Glyph.Assign(img.Picture.Bitmap);
  finally
    img.Free;
  end;
end;

constructor TButtons.Create;
begin
  inherited Create;
  fSearch := TButton.Create;
  fClear := TButton.Create;
  SetUpImages;
end;

destructor TButtons.Destroy;
begin
  fSearch.Free;
  fClear.Free;
  inherited Destroy;
end;

{ TInput }

constructor TInput.Create;
begin
  inherited Create;
  fMinChars := DefaultMinChars;
  fOptions := DefaultOptions;
end;

{ TTisSearchEdit }

procedure TTisSearchEdit.SetUpEdit;
begin
  TextHint := 'Search keywords';
  Text := '';
  ControlStyle := ControlStyle - [csSetCaption];
end;

procedure TTisSearchEdit.SetUpButtons;
var
  lw: Integer;

  procedure SetUp(aButton: TButton; aClick: TNotifyEvent);
  begin
    with aButton do
    begin
      Button.ControlStyle := ControlStyle + [csNoDesignSelectable];
      Button.OnClick := aClick;
      Button.SetBounds(Left, Top, Button.Width, Button.Height);
      if Button.Visible then
      begin
        Button.Left := lw;
        inc(lw, Button.Width + 2);
      end;
    end;
  end;

begin
  if assigned(fButtons) then // must be checked
  begin
    lw := Left + Width + 2;
    SetUp(fButtons.Search, DoSearchClick);
    SetUp(fButtons.Clear, DoClearClick);
  end;
end;

procedure TTisSearchEdit.SetDefault;
begin
  Width := 130;
  Height := 24;
end;

procedure TTisSearchEdit.DoSearchClick(aSender: TObject);
begin
  if Assigned(fOnButtonSearchClick) then
    fOnButtonSearchClick(aSender)
  else
    Searching;
end;

procedure TTisSearchEdit.DoClearClick(aSender: TObject);
begin
  Text := '';
  if Assigned(fOnClearButtonClick) then
    fOnClearButtonClick(aSender);
end;

procedure TTisSearchEdit.SetParent(aNewParent: TWinControl);
begin
  inherited SetParent(aNewParent);
  if csDestroying in ComponentState then
    exit;
  fButtons.Search.Button.Parent := aNewParent;
  fButtons.Clear.Button.Parent := aNewParent;
end;

procedure TTisSearchEdit.DoSetBounds(aLeft, aTop, aWidth, aHeight: Integer);
begin
  inherited DoSetBounds(aLeft, aTop, aWidth, aHeight);
  SetUpButtons;
end;

procedure TTisSearchEdit.Searching;
begin
  if assigned(fInput.Timer) then
  begin
    fInput.Timer.Enabled := False;
    if (Length(Text) >= fInput.MinChars) then
      fInput.Timer.Enabled := True;
  end;
end;

constructor TTisSearchEdit.Create(aOwner: TComponent);
begin
  inherited Create(AOwner);
  fButtons := TButtons.Create;
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
