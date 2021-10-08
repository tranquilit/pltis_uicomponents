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

  TSearchOption = (
    boShowSearchButton,
    boShowClearButton
  );

  TSearchOptions = set of TSearchOption;

  TTisSearchEdit = class(TEdit)
  private
    fSearchButton: TSpeedButton;
    fClearButton: TSpeedButton;
    fOptions: TSearchOptions;
    fInput: TInput;
    fOnSearchButtonClick: TNotifyEvent;
    fOnClearButtonClick: TNotifyEvent;
    procedure SetUpEdit;
    procedure SetUpImages;
    procedure SetUpButtons;
    procedure SetDefault;
    procedure SetOptions(const aValue: TSearchOptions);
    procedure DoSearch(aSender: TObject);
    procedure DoClear(aSender: TObject);
  protected
    // ------------------------------- inherited methods ----------------------------------
    procedure SetParent(aNewParent: TWinControl); override;
    procedure DoSetBounds(aLeft, aTop, aWidth, aHeight: Integer); override;
    // ------------------------------- new methods ----------------------------------
    procedure Searching; virtual;
  protected
    const DefaultOptions = [boShowSearchButton];
  public
    // ------------------------------- inherited methods ----------------------------------
    constructor Create(aOwner: TComponent); override;
    destructor Destroy; override;
    procedure TextChanged; override;
    procedure EnabledChanged; override;
    procedure EditingDone; override;
  published
    // ------------------------------- new properties ----------------------------------
    property Options: TSearchOptions read fOptions write SetOptions default DefaultOptions;
    property Input: TInput read fInput write fInput;
    // ------------------------------- new events ----------------------------------
    property OnSearchButtonClick: TNotifyEvent read fOnSearchButtonClick write fOnSearchButtonClick;
    property OnClearButtonClick: TNotifyEvent read fOnClearButtonClick write fOnClearButtonClick;
  end;

implementation

{$R icons.rc}

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

procedure TTisSearchEdit.SetUpImages;
var
  img: TImage;
begin
  img := TImage.Create(nil);
  try
    img.Picture.LoadFromResourceName(HINSTANCE, 'TIS_SEARCH_ICON', TPortableNetworkGraphic);
    fSearchButton.Glyph.Assign(img.Picture.Bitmap);
    //img.Picture.LoadFromResourceName(HINSTANCE, 'TIS_CLEAR_ICON', TPortableNetworkGraphic);
    //fClearButton.Glyph.Assign(img.Picture.Bitmap);
  finally
    img.Free;
  end;
end;

procedure TTisSearchEdit.SetUpButtons;
var
  lw: Integer;

  procedure SetUp(aButton: TSpeedButton; aChange: Boolean; aClick: TNotifyEvent);
  begin
    if not assigned(aButton) then // must be checked
      exit;
    aButton.ControlStyle := ControlStyle + [csNoDesignSelectable];
    aButton.Flat := True;
    aButton.OnClick := aClick;
    aButton.SetBounds(Left, Top, aButton.Width, aButton.Height);
    if aChange then
    begin
      aButton.Left := lw;
      inc(lw, aButton.Width + 2);
    end;
  end;

begin
  lw := Left + Width + 2;
  SetUp(fSearchButton, boShowSearchButton in fOptions, DoSearch);
  SetUp(fClearButton, boShowClearButton in fOptions, DoClear);
end;

procedure TTisSearchEdit.SetDefault;
begin
  Width := 130;
  Height := 24;
  Options := DefaultOptions;
end;

procedure TTisSearchEdit.SetOptions(const aValue: TSearchOptions);
begin
  if fOptions = aValue then
    exit;
  fOptions := aValue;
  SetUpButtons;
end;

procedure TTisSearchEdit.DoSearch(aSender: TObject);
begin
  if Assigned(fOnSearchButtonClick) then
    fOnSearchButtonClick(aSender)
  else
    Searching;
end;

procedure TTisSearchEdit.DoClear(aSender: TObject);
begin
  Text := '';
  if Assigned(fOnClearButtonClick) then
    fOnClearButtonClick(aSender);
end;

procedure TTisSearchEdit.SetParent(aNewParent: TWinControl);
begin
  inherited SetParent(aNewParent);
  fSearchButton.Parent := aNewParent;
  fClearButton.Parent := aNewParent;
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
  fInput := TInput.Create;
  fSearchButton := TSpeedButton.Create(self);
  fClearButton := TSpeedButton.Create(self);
  SetDefault;
  SetUpEdit;
  SetUpImages;
end;

destructor TTisSearchEdit.Destroy;
begin
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
