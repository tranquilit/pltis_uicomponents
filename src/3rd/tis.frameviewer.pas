// -----------------------------------------------------------------
//    This file is part of Tranquil IT Software
//    Copyright (C) 2012 - 2024  Tranquil IT https://www.tranquil.it
//    All Rights Reserved.
// ------------------------------------------------------------------
unit tis.frameviewer;

{$i tis.ui.defines.inc}

interface

{$ifndef FRAMEVIEWER_ENABLED}

uses
  Classes,
  Variants,
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
  Menus,
  HtmlView,
  tis.ui.resourcestrings,
  tis.ui.grid.controls;

type

  { TTisHtmlViewer }

  TTisHtmlViewer = class(THtmlViewer)
  protected
    procedure FillPopupMenu; virtual;
    procedure DoSelectAll(aSender: TObject); virtual;
    procedure DoCopy(aSender: TObject); virtual;
    procedure DoCopyAsHtml(aSender: TObject); virtual;
  public
    /// initialize some required properties
    constructor Create(aOwner: TComponent); override;
    procedure PaintHtml(aCanvas: TCanvas; const aRect: TRect);
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

uses
  Clipbrd;

{$ifdef FRAMEVIEWER_ENABLED}

{ TTisHtmlViewer }

procedure TTisHtmlViewer.FillPopupMenu;

  procedure AddItem(const aCaption: string; aShortcut: TShortCut; aEvent: TNotifyEvent);
  var
    vMenuItem: TMenuItem;
  begin
    vMenuItem := TMenuItem.Create(PopupMenu);
    with vMenuItem do
    begin
      Caption := aCaption;
      ShortCut := aShortcut;
      OnClick := aEvent;
    end;
    PopupMenu.Items.Add(vMenuItem);
  end;

begin
  if Assigned(PopupMenu) then
    exit;
  PopupMenu := TPopupMenu.Create(self);
  AddItem(rsFrameViewerSelectAll, ShortCut(Ord('A'), [ssCtrl]), @DoSelectAll);
  AddItem(rsFrameViewerCopy, ShortCut(Ord('C'), [ssCtrl]), @DoCopy);
  AddItem(rsFrameViewerCopyAsHtml, ShortCut(Ord('C'), [ssCtrl, ssShift]), @DoCopyAsHtml);
end;

procedure TTisHtmlViewer.DoSelectAll(aSender: TObject);
begin
  SelectAll;
end;

procedure TTisHtmlViewer.DoCopy(aSender: TObject);
begin
  CopyToClipboard;
end;

procedure TTisHtmlViewer.DoCopyAsHtml(aSender: TObject);
begin
  Clipboard.AsText := SelHtml;
end;

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
  DefBackground := clWindow;
  DefFontColor := clWindowText;
  DefFontName := Screen.SystemFont.Name;
  if Screen.SystemFont.Size>0 then
    DefFontSize := Screen.SystemFont.Size;
  FillPopupMenu;
end;

procedure TTisHtmlViewer.PaintHtml(aCanvas: TCanvas; const aRect: TRect);
begin
  HTMLPaint(aCanvas, aRect);
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
  if VarIsStr(aValue) and (aValue <> '') then
    Edit.Text := aValue // do not set a empty string
  else
    Edit.Clear;
end;

function TTisGridHtmlControl.Edit: TTisHtmlViewer;
begin
  result := fInternal as TTisHtmlViewer;
end;

end.
