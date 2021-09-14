// -----------------------------------------------------------------
//    This file is part of Tranquil IT Software
//    Copyright (C) 2012 - 2021  Tranquil IT https://www.tranquil.it
//    All Rights Reserved.
// ------------------------------------------------------------------
unit tis.ui.searchedit;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LResources, Forms, Controls, Graphics, Dialogs, StdCtrls,
  ExtCtrls, Buttons;

type
  TTisSearchEdit = class(TEdit)
  private
    fEmbeddedImage: TImage;
    fImagePanel: TPanel;
    fSearchIconSize: TConstraintSize;
    fSearchIconSpacingLeft: Integer;
    fSearchIconIsHidden: Boolean;
    fButton: TSpeedButton;
    procedure SetUpEdit;
    procedure SetUpPanel;
    procedure SetUpImage;
    procedure SetDefault;
    procedure SetSearchIconSize(aValue: TConstraintSize);
    procedure SetSearchIconSpacingLeft(aValue: Integer);
    function GetSearchIconVisible: Boolean;
    procedure SetSearchIconVisible(aValue: Boolean);
    function GetOnSearchIconClick: TNotifyEvent;
    procedure SetOnSearchIconClick(aValue: TNotifyEvent);
    function GetFontSize: Integer;
    procedure HideIconForText;
  protected
    procedure SetParent(aNewParent: TWinControl); override;
    procedure DoSetBounds(aLeft, aTop, aWidth, aHeight: Integer); override;
  public
    constructor Create(aOwner: TComponent); override;
    procedure TextChanged; override;
    procedure EnabledChanged; override;
  published
    property SearchIconSize: TConstraintSize read fSearchIconSize write SetSearchIconSize;
    property SearchIconSpacingLeft: Integer read fSearchIconSpacingLeft write SetSearchIconSpacingLeft;
    property SearchIconVisible: Boolean read GetSearchIconVisible write SetSearchIconVisible;
    property OnSearchIconClick: TNotifyEvent read GetOnSearchIconClick write SetOnSearchIconClick;
  end;

implementation

{$R icons.rc}

procedure TTisSearchEdit.SetSearchIconSize(aValue: TConstraintSize);
begin
  if fSearchIconSize = aValue then
    exit;
  fSearchIconSize := aValue;
  DoSetBounds(Left, Top, Width, Height);
end;

procedure TTisSearchEdit.SetSearchIconSpacingLeft(aValue: Integer);
begin
  if fSearchIconSpacingLeft = aValue then
    exit;
  fSearchIconSpacingLeft := aValue;
  if fSearchIconSpacingLeft >= 0 then
    fImagePanel.Color := clNone
  else
    fImagePanel.Color := fImagePanel.Parent.Color;
  DoSetBounds(Left, Top, Width, Height);
end;

function TTisSearchEdit.GetSearchIconVisible: Boolean;
begin
  result := fImagePanel.Visible;
end;

procedure TTisSearchEdit.SetSearchIconVisible(aValue: Boolean);
begin
  fImagePanel.Visible := aValue;
end;

function TTisSearchEdit.GetOnSearchIconClick: TNotifyEvent;
begin
  result := fEmbeddedImage.OnClick;
end;

procedure TTisSearchEdit.SetOnSearchIconClick(aValue: TNotifyEvent);
begin
  fEmbeddedImage.OnClick := aValue;
end;

function TTisSearchEdit.GetFontSize: Integer;
var
  b: TBitmap;
  len: Integer;
begin
  if not (csDestroyingHandle in ControlState) then
  begin
    b := TBitmap.Create;
    try
      b.Canvas.Font.Assign(Font);
      if Length(Text) = 0 then
      begin
        result := 0;
        exit;
      end;
      len := b.Canvas.TextWidth(Text) div length(Text);
      result := b.Canvas.TextWidth(Text) + len;
    finally
      b.Free;
    end;
  end;
end;

procedure TTisSearchEdit.HideIconForText;
var
  maxwidth : Integer;
begin
  if (not (csDestroyingHandle in ControlState)) and (fSearchIconSpacingLeft >= 0) then
  begin
    maxwidth := Width - fSearchIconSize - fSearchIconSpacingLeft;
    if (GetFontSize >= maxwidth) and GetSearchIconVisible then
    begin
      SetSearchIconVisible(False);
      fSearchIconIsHidden := True;
    end
    else if (GetFontSize < maxwidth) and fSearchIconIsHidden then
    begin
      SetSearchIconVisible(True);
      fSearchIconIsHidden := False;
    end;
  end;
end;

procedure TTisSearchEdit.SetUpEdit;
begin
  TextHint := 'Search keywords';
  Text := '';
  ControlStyle := ControlStyle - [csSetCaption];
end;

procedure TTisSearchEdit.SetUpPanel;
begin
  fImagePanel.ControlStyle := fImagePanel.ControlStyle + [csNoDesignSelectable];
  fImagePanel.AutoSize := false;
  fImagePanel.Visible := true;
  fImagePanel.BevelOuter := bvNone;
  fImagePanel.BevelInner := bvNone;
  fImagePanel.Color := clNone;
  fImagePanel.Align := alNone;
  fImagePanel.Anchors := [akRight, akTop];
  fImagePanel.AnchorSide[akRight].Side := asrRight;
  fImagePanel.AnchorSide[akRight].Control := self;
  fImagePanel.AnchorSide[akTop].Side := asrCenter;
  fImagePanel.AnchorSide[akTop].Control := self;
end;

procedure TTisSearchEdit.SetUpImage;
begin
  fEmbeddedImage.ControlStyle := fEmbeddedImage.ControlStyle + [csNoDesignSelectable];
  fEmbeddedImage.Picture.LoadFromResourceName(HINSTANCE,'TIS_SEARCH_ICON',TPortableNetworkGraphic);
  fButton.Glyph.Assign(fEmbeddedImage.Picture.Bitmap);
  fEmbeddedImage.stretch := true;
  fEmbeddedImage.Visible := true;
  fEmbeddedImage.Cursor := crHandPoint;
  fEmbeddedImage.Align := alNone;
  fEmbeddedImage.Anchors := [];
  fSearchIconIsHidden := false;
end;

procedure TTisSearchEdit.SetDefault;
begin
  fSearchIconSize := 15;
  Width := 130;
  Height := 24;
  fSearchIconSpacingLeft := 5;
end;

procedure TTisSearchEdit.SetParent(aNewParent: TWinControl);
begin
  inherited SetParent(aNewParent);
  fImagePanel.Parent := aNewParent;
  fEmbeddedImage.Parent := fImagePanel;
  fButton.Parent := aNewParent;
end;

procedure TTisSearchEdit.DoSetBounds(aLeft, aTop, aWidth, aHeight: Integer);
begin
  inherited DoSetBounds(aLeft, aTop, aWidth, aHeight);
  if assigned(fImagePanel) then
  begin
    fImagePanel.BorderSpacing.Right := fSearchIconSpacingLeft;
    fImagePanel.SetBounds(0, 0, fSearchIconSize, fSearchIconSize);
  end;
  if assigned(fEmbeddedImage) then
    fEmbeddedImage.SetBounds(0, 0,  fSearchIconSize, fSearchIconSize);
  if assigned(fButton) then // should be checked
    fButton.SetBounds(aLeft + aWidth + 2, aTop, 35, aHeight);
end;

constructor TTisSearchEdit.Create(aOwner: TComponent);
begin
  inherited Create(AOwner);
  fButton := TSpeedButton.Create(self);
  fButton.ControlStyle := fButton.ControlStyle + [csNoDesignSelectable];
  fButton.Flat := True;
  SetDefault;
  SetUpEdit;
  if fImagePanel = nil then
  begin
    fImagePanel := TPanel.Create(self);
    SetUpPanel;
  end;
  if fEmbeddedImage = nil then
  begin
    fEmbeddedImage := TImage.Create(fImagePanel);
    SetUpImage;
  end;
end;

procedure TTisSearchEdit.TextChanged;
begin
  inherited TextChanged;
  HideIconForText;
end;

procedure TTisSearchEdit.EnabledChanged;
begin
  inherited EnabledChanged;
  if self.Enabled = False then
    fImagePanel.Color := clDefault
  else
  begin
    if fSearchIconSpacingLeft >= 0 then
      fImagePanel.Color := clNone
    else
      fImagePanel.Color := fImagePanel.Parent.Color;
  end;
end;

initialization
  {$I tis.ui.searchedit.lrs}

end.
