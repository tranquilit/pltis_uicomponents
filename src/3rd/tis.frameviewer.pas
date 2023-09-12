// -----------------------------------------------------------------
//    This file is part of Tranquil IT Software
//    Copyright (C) 2012 - 2023  Tranquil IT https://www.tranquil.it
//    All Rights Reserved.
// ------------------------------------------------------------------
unit tis.frameviewer;

{$i tis.ui.defines.inc}

interface

{$ifndef FRAMEVIEWER_ENABLED}

uses
  Classes,
  SysUtils,
  Controls,
  StdCtrls,
  tis.ui.grid.controls;

type
  TTisHtmlViewer = class(TMemo); // fake THtmlViewer

{$else FRAMEVIEWER_ENABLED}

uses
  Classes,
  Variants,
  SysUtils,
  Controls,
  Graphics,
  StdCtrls,
  Forms,
  HtmlView,
  tis.ui.grid.controls;

type

  { TTisHtmlViewer }

  TTisHtmlViewer = class(THtmlViewer)
  public
    /// initialize some required properties
    constructor Create(aOwner: TComponent); override;
    procedure HTMLPaintPublic(ACanvas: TCanvas; const ARect: TRect);
  end;

{$endif FRAMEVIEWER_ENABLED}

  /// control used for display HTML
  TTisGridHtmlControl = class(TTisGridControl)
  public
    constructor Create(aOwner: TWinControl); override;
    function GetValue: Variant; override;
    procedure SetValue(const aValue: Variant); override;
    function Edit: TTisHtmlViewer;
  end;

implementation

{$ifdef FRAMEVIEWER_ENABLED}

{ TTisHtmlViewer }

constructor TTisHtmlViewer.Create(aOwner: TComponent);
var
  vParent: TWinControl;
begin
  inherited Create(aOwner);
  vParent := aOwner as TWinControl;
  Visible := False;           // it SHOULD be invisible first, otherwise it will be buggy
  Parent := vParent;          // it needs a valid Parent...
  ScrollBars := ssNone;
  LoadCursor := crNone;
  DefBackground := clWhite;
  DefFontName := Screen.SystemFont.Name;
  DefFontSize := Screen.SystemFont.Size;
end;

procedure TTisHtmlViewer.HTMLPaintPublic(ACanvas: TCanvas; const ARect: TRect);
begin
  HTMLPaint(ACanvas, ARect);
end;

{$endif FRAMEVIEWER_ENABLED}

{ TTisGridHtmlControl }

constructor TTisGridHtmlControl.Create(aOwner: TWinControl);
begin
  inherited Create(aOwner);
  fInternal := TTisHtmlViewer.Create(aOwner);
  Edit.Parent := aOwner;
  Edit.Clear;
end;

function TTisGridHtmlControl.GetValue: Variant;
begin
  if Edit.Text = '' then
    result := NULL
  else
    result := Edit.Text;
end;

procedure TTisGridHtmlControl.SetValue(const aValue: Variant);
begin
  Edit.Text := aValue;
end;

function TTisGridHtmlControl.Edit: TTisHtmlViewer;
begin
  result := fInternal as TTisHtmlViewer;
end;

end.
