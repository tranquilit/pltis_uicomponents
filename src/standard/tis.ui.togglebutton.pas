// -----------------------------------------------------------------
//    This file is part of Tranquil IT Software
//    Copyright (C) 2012 - 2024  Tranquil IT https://www.tranquil.it
//    All Rights Reserved.
// ------------------------------------------------------------------
unit tis.ui.togglebutton;

{$i tis.ui.defines.inc}

interface

uses
  Classes, LResources, SysUtils, Controls, Graphics, Buttons, ExtCtrls, Dialogs;

type

  { TTisToggleButton }

  TTisToggleButton = class(TCustomSpeedButton)
  private
    FImage1: TButtonGlyph;
    FImage2: TButtonGlyph;
    FClickBox: TPaintBox;
    function getImageOne: TBitmap;
    procedure setImageOne(Value: TBitmap);
    function getImageTwo: TBitmap;
    procedure setImageTwo(Value: TBitmap);
    procedure SetBoxDefault(Box: TPaintBox);
    procedure InterMouseEnter(Sender: TObject);
    procedure InterMouseLeave(Sender: TObject);

  protected
    procedure SetParent(NewParent: TWinControl); override;
    procedure DoSetBounds(ALeft, ATop, AWidth, AHeight: integer); override;
    procedure Loaded; override;
    procedure MouseEnter; override;
    procedure MouseLeave; override;

  public
    ToggleState: Boolean;
    procedure OnClickPan(Sender: TObject);
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure SetButtonState(TState: Boolean);

  published
    property ImageState1: TBitmap read getImageOne write setImageOne;
    property ImageState2: TBitmap read getImageTwo write setImageTwo;
    property Action;
    property Align;
    property AllowAllUp;
    property Anchors;
    property AutoSize;
    property BidiMode;
    property BorderSpacing;
    property Constraints;
    property Caption;
    property Color;
    property Down;
    property Enabled;
    property Flat;
    property Font;
    property GroupIndex;
    property ImageWidth;
    property Layout;
    property Margin;
    property Spacing;
    property Transparent;
    property Visible;
    property OnClick;
    property OnDblClick;
    property OnMouseDown;
    property OnMouseEnter;
    property OnMouseLeave;
    property OnMouseMove;
    property OnMouseUp;
    property OnMouseWheel;
    property OnMouseWheelDown;
    property OnMouseWheelUp;
    property OnPaint;
    property OnResize;
    property OnChangeBounds;
    property ShowCaption;
    property ShowHint;
    property ParentBidiMode;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
  end;

implementation

{ TTisToggleButton }

procedure TTisToggleButton.OnClickPan(Sender: TObject);
begin
  ToggleState := (not ToggleState);
  SetButtonState(ToggleState);
  if Assigned(OnClick) then OnClick(Sender);
end;

procedure TTisToggleButton.Loaded;
begin
  inherited Loaded;
  Glyph := FImage1.Glyph;

  if FClickBox = nil then
  begin
    FClickBox := TPaintBox.Create(self);
    SetBoxDefault(FClickBox);
    SetParent(self.Parent);
    DoSetBounds(Left, Top, Width, Height);
  end;
end;

procedure TTisToggleButton.MouseEnter;
begin
  inherited MouseEnter;
end;

procedure TTisToggleButton.MouseLeave;
begin
  inherited MouseLeave;
end;

function TTisToggleButton.getImageOne: TBitmap;
begin
  Result := FImage1.Glyph;
end;

procedure TTisToggleButton.setImageOne(Value: TBitmap);
begin
  FImage1.Glyph := Value;
  Glyph := Value;
  Invalidate;
end;

function TTisToggleButton.getImageTwo: TBitmap;
begin
  Result := FImage2.Glyph;
end;

procedure TTisToggleButton.setImageTwo(Value: TBitmap);
begin
  FImage2.Glyph := Value;
  Invalidate;
end;

procedure TTisToggleButton.SetBoxDefault(Box: TPaintBox);
begin
  Box.Caption := '';
  Box.Color := clDefault;
  Box.Cursor := crDefault;
  Box.ControlStyle := Box.ControlStyle + [csNoDesignSelectable];
  Box.OnClick := @OnClickPan;
  Box.OnMouseEnter := @InterMouseEnter;
  Box.OnMouseleave := @InterMouseLeave;
end;

procedure TTisToggleButton.InterMouseEnter(Sender: TObject);
begin
  MouseEnter;
end;

procedure TTisToggleButton.InterMouseLeave(Sender: TObject);
begin
  MouseLeave;
end;

procedure TTisToggleButton.SetParent(NewParent: TWinControl);
begin
  inherited SetParent(NewParent);

  if FClickBox = nil then exit;
  FClickBox.Parent := NewParent;
end;

procedure TTisToggleButton.DoSetBounds(ALeft, ATop, AWidth, AHeight: integer);
begin
  inherited DoSetBounds(ALeft, ATop, AWidth, AHeight);

  if FClickBox = nil then Exit;
  FClickBox.SetBounds(ALeft, ATop, AWidth, AHeight);
end;

constructor TTisToggleButton.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  ToggleState := false;
  FImage1 := TButtonGlyph.Create;
  FImage2 := TButtonGlyph.Create;
end;

destructor TTisToggleButton.Destroy;
begin
  inherited Destroy;
  FreeAndNil(FImage1);
  FreeAndNil(FImage2);
end;

procedure TTisToggleButton.SetButtonState(TState: Boolean);
begin
    if TState then
    Glyph := FImage2.Glyph
  else
    Glyph := FImage1.Glyph;
end;

initialization
  {$I tis.ui.togglebutton.lrs}


end.
