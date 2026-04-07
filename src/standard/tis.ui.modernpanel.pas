// -----------------------------------------------------------------
//    This file is part of Tranquil IT Software
//    Copyright (C) 2012 - 2026  Tranquil IT https://www.tranquil.it
//    All Rights Reserved.
// ------------------------------------------------------------------
unit tis.ui.modernpanel;

{$i tis.ui.defines.inc}

interface

uses
  Classes,
  Controls,
  Graphics,
  ExtCtrls;

type

  { TTisModernPanel }

  TTisModernPanel = class(TPanel)
  private
    FRadius: Integer;
    FShadowSize: Integer;
    FFillColor: TColor;
    FBorderColor: TColor;
    FShadowColor: TColor;
    FShowShadow: Boolean;
    procedure SetBorderColor(AValue: TColor);
    procedure SetFillColor(AValue: TColor);
    procedure SetRadius(AValue: Integer);
    procedure SetShadowColor(AValue: TColor);
    procedure SetShadowSize(AValue: Integer);
    procedure SetShowShadow(AValue: Boolean);
  protected
    const DefaultRadius = 16;
    const DefaultShadowSize = 4;
    const DefaultFillColor = clWhite;
    const DefaultBorderColor = $00F0ECE9;
    const DefaultShadowColor = $00E8E3DF;
    const DefaultShowShadow = True;
    procedure Paint; override;
  public
    constructor Create(AOwner: TComponent); override;
  published
    property Radius: Integer read FRadius write SetRadius default DefaultRadius;
    property ShadowSize: Integer read FShadowSize write SetShadowSize default DefaultShadowSize;
    property FillColor: TColor read FFillColor write SetFillColor default DefaultFillColor;
    property BorderColor: TColor read FBorderColor write SetBorderColor default DefaultBorderColor;
    property ShadowColor: TColor read FShadowColor write SetShadowColor default DefaultShadowColor;
    property ShowShadow: Boolean read FShowShadow write SetShowShadow default DefaultShowShadow;
    property Align;
    property Anchors;
    property Visible;
    property Enabled;
  end;

  procedure DrawRoundedRect(ACanvas: TCanvas; const R: TRect; ARadius: Integer;
    AFillColor, ABorderColor: TColor; ABorderWidth: Integer = 1);

implementation

procedure DrawRoundedRect(ACanvas: TCanvas; const R: TRect; ARadius: Integer;
  AFillColor, ABorderColor: TColor; ABorderWidth: Integer);
begin
  ACanvas.AntialiasingMode := amOn;
  ACanvas.Brush.Style := bsSolid;
  ACanvas.Brush.Color := AFillColor;
  ACanvas.Pen.Style := psSolid;
  ACanvas.Pen.Color := ABorderColor;
  ACanvas.Pen.Width := ABorderWidth;
  ACanvas.RoundRect(R.Left, R.Top, R.Right, R.Bottom, ARadius, ARadius);
end;

{ TTisModernPanel }

procedure TTisModernPanel.SetRadius(AValue: Integer);
begin
  if FRadius = AValue then
    Exit;
  FRadius := AValue;
  Invalidate;
end;

procedure TTisModernPanel.SetBorderColor(AValue: TColor);
begin
  if FBorderColor = AValue then
    Exit;
  FBorderColor := AValue;
  Invalidate;
end;

procedure TTisModernPanel.SetFillColor(AValue: TColor);
begin
  if FFillColor = AValue then
    Exit;
  FFillColor := AValue;
  Invalidate;
end;

procedure TTisModernPanel.SetShadowColor(AValue: TColor);
begin
  if FShadowColor = AValue then
    Exit;
  FShadowColor := AValue;
  Invalidate;
end;

procedure TTisModernPanel.SetShadowSize(AValue: Integer);
begin
  if FShadowSize = AValue then
    Exit;
  FShadowSize := AValue;
  Invalidate;
end;

procedure TTisModernPanel.SetShowShadow(AValue: Boolean);
begin
  if FShowShadow = AValue then
    Exit;
  FShowShadow := AValue;
  Invalidate;
end;

procedure TTisModernPanel.Paint;
var
  ShadowRect: TRect;
  MainRect: TRect;
begin
  Canvas.Brush.Style := bsSolid;
  if Parent <> nil then
  begin
    Canvas.Brush.Color := Parent.Color;
    Color := Parent.Color;
  end;
  Canvas.Pen.Style := psClear;
  Canvas.Rectangle(ClientRect);

  if FShowShadow then
  begin
    ShadowRect := Rect(4, 6, Width - 1, Height - 1);
    DrawRoundedRect(Canvas, ShadowRect, FRadius, FShadowColor, FShadowColor, 1);
  end;

  MainRect := Rect(0, 0, Width - FShadowSize, Height - FShadowSize - 2);
  DrawRoundedRect(Canvas, MainRect, FRadius, FFillColor, FBorderColor, 1);
end;

constructor TTisModernPanel.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  ControlStyle := ControlStyle + [csOpaque];
  FRadius := DefaultRadius;
  FShadowSize := DefaultShadowSize;
  FFillColor := DefaultFillColor;
  FBorderColor := DefaultBorderColor;
  FShadowColor := DefaultShadowColor;
  FShowShadow := DefaultShowShadow;
end;

end.
