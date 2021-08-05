// -----------------------------------------------------------------
//    This file is part of Tranquil IT Software
//    Copyright (C) 2012 - 2021  Tranquil IT https://www.tranquil.it
//    All Rights Reserved.
// ------------------------------------------------------------------
unit main;

{$i mormot.defines.inc}

interface

uses
  classes,
  sysutils,
  forms,
  controls,
  graphics,
  dialogs,
  extctrls,
  stdctrls,
  buttons,
  comctrls,
  Menus,
  ImgList,
  synedit,
  VirtualTrees,
  mormot.core.base,
  mormot.core.text,
  mormot.core.unicode,
  mormot.core.variants,
  tis.core.os,
  tis.ui.grid.core;

type
  TMainForm = class(TForm)
    DeleteRowsButton: TSpeedButton;
    ClipboardLabel1: TLabel;
    AddRowsButton: TSpeedButton;
    Label1: TLabel;
    ClipboardLabel: TLabel;
    GridDataLabel: TLabel;
    GridTotalLabel: TLabel;
    MainPageControl: TPageControl;
    MenuItem1: TMenuItem;
    Panel1: TPanel;
    Panel4: TPanel;
    UserPopupMenu: TPopupMenu;
    SOvsTisTab: TTabSheet;
    InOutputEdit: TSynEdit;
    Grid: TTisGrid;
    Splitter: TSplitter;
    CustomizeButton: TSpeedButton;
    procedure AddRowsButtonClick(Sender: TObject);
    procedure ClipboardLabel1Click(Sender: TObject);
    procedure ClipboardLabelClick(Sender: TObject);
    procedure CustomizeButtonClick(Sender: TObject);
    procedure DeleteRowsButtonClick(Sender: TObject);
    function GridCompareByRow(sender: TTisGrid; const aPropertyName: RawUtf8;
      const aV1, aV2: Variant; aReverse: Boolean): PtrInt;
    procedure GridDataLabelClick(Sender: TObject);
    procedure GridInitNode(Sender: TBaseVirtualTree; ParentNode,
      Node: PVirtualNode; var InitialStates: TVirtualNodeInitStates);
    procedure MenuItem1Click(Sender: TObject);
  end;

var
  MainForm: TMainForm;

implementation

{$R *.lfm}

{ TMainForm }

procedure TMainForm.CustomizeButtonClick(Sender: TObject);
begin
  Grid.Customize;
end;

procedure TMainForm.AddRowsButtonClick(Sender: TObject);
var
  d: PDocVariantData;
begin
  if InOutputEdit.Text <> '' then
  begin
    d := _Safe(_Json(StringToUtf8(InOutputEdit.Text)));
    Grid.AddRows(d);
  end
  else
    ShowMessage('Type a JSON into Input/Output memo.');
end;

procedure TMainForm.DeleteRowsButtonClick(Sender: TObject);
var
  d: PDocVariantData;
begin
  if InOutputEdit.Text <> '' then
  begin
    d := _Safe(_Json(StringToUtf8(InOutputEdit.Text)));
    Grid.DeleteRows(d);
  end
  else
    ShowMessage('Type a JSON into Input/Output memo.');
end;

function TMainForm.GridCompareByRow(sender: TTisGrid;
  const aPropertyName: RawUtf8; const aV1, aV2: Variant; aReverse: Boolean): PtrInt;
begin
  if aReverse then
    result := _Safe(aV2)^.CompareObject(aPropertyName, _Safe(aV1)^)
  else
    result := _Safe(aV1)^.CompareObject(aPropertyName, _Safe(aV2)^)
end;

procedure TMainForm.ClipboardLabel1Click(Sender: TObject);
begin
  InOutputEdit.Lines.Clear;
end;

procedure TMainForm.ClipboardLabelClick(Sender: TObject);
var
  c: TClipboardAdapter;
begin
  InOutputEdit.Lines.Text := c.AsString;
end;

procedure TMainForm.GridDataLabelClick(Sender: TObject);
begin
  InOutputEdit.Lines.Text := Utf8ToString(Grid.Data.ToJson('', '', jsonHumanReadable));
end;

procedure TMainForm.GridInitNode(Sender: TBaseVirtualTree; ParentNode,
  Node: PVirtualNode; var InitialStates: TVirtualNodeInitStates);
begin
  InitialStates := InitialStates + [ivsMultiline];
end;

procedure TMainForm.MenuItem1Click(Sender: TObject);
begin
  ShowMessage('User menu item action');
end;

end.
