// -----------------------------------------------------------------
//    This file is part of Tranquil IT Software
//    Copyright (C) 2012 - 2021  Tranquil IT https://www.tranquil.it
//    All Rights Reserved.
// ------------------------------------------------------------------
unit tis.ui.searchedit;

{$i mormot.defines.inc}

interface

uses
  Classes,
  SysUtils,
  Forms,
  Controls,
  Graphics,
  Dialogs,
  StdCtrls,
  ExtCtrls,
  Buttons,
  tis.ui.parts.buttons;

type

  TTisSearchEdit = class(TEdit)
  private
    fButtons: TButtonCollection;
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
  SetDefault;
  SetUpEdit;
end;

destructor TTisSearchEdit.Destroy;
begin
  fButtons.Free;
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
