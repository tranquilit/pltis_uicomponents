// -----------------------------------------------------------------
//    This file is part of Tranquil IT Software
//    Copyright (C) 2012 - 2021  Tranquil IT https://www.tranquil.it
//    All Rights Reserved.
// ------------------------------------------------------------------
unit demo.grid.frame;

{$i mormot.defines.inc}

interface

uses
  Classes,
  SysUtils,
  Forms,
  Controls,
  ExtCtrls,
  Buttons,
  StdCtrls,
  Dialogs,
  Menus,
  ColorBox,
  SynEdit,
  VirtualTrees,
  mormot.core.base,
  mormot.core.text,
  mormot.core.unicode,
  mormot.core.variants,
  tis.core.os,
  tis.ui.searchedit,
  tis.ui.grid.core;

type
  TGridFrame = class(TFrame)
    ClipboardLabel: TLabel;
    ClipboardLabel1: TLabel;
    Grid: TTisGrid;
    GridDataLabel: TLabel;
    InOutputEdit: TSynEdit;
    Label1: TLabel;
    Panel1: TPanel;
    Panel4: TPanel;
    Splitter: TSplitter;
    UserPopupMenu: TPopupMenu;
    MenuItem1: TMenuItem;
    GroupBox1: TGroupBox;
    CustomizeButton: TSpeedButton;
    AddRowsButton: TSpeedButton;
    DeleteRowsButton: TSpeedButton;
    GroupBox2: TGroupBox;
    SettingsSaveButton: TSpeedButton;
    SettingsLoadButton: TSpeedButton;
    OpenDialog: TOpenDialog;
    SaveDialog: TSaveDialog;
    GridTotalLabel: TLabel;
    GridSelectedRowLabel: TLabel;
    GroupBox3: TGroupBox;
    cbColumnDataType: TComboBox;
    EdDataType: TLabel;
    EditorColorBox: TColorBox;
    Label2: TLabel;
    SearchItemsMemo: TMemo;
    AsynchCheckBox: TCheckBox;
    Label3: TLabel;
    procedure AddRowsButtonClick(Sender: TObject);
    procedure CustomizeButtonClick(Sender: TObject);
    procedure DeleteRowsButtonClick(Sender: TObject);
    function GridCompareByRow(sender: TTisGrid; const aPropertyName: RawUtf8;
      const aRow1, aRow2: TDocVariantData; var aHandled: Boolean): PtrInt;
    procedure GridInitNode(Sender: TBaseVirtualTree; ParentNode,
      Node: PVirtualNode; var InitialStates: TVirtualNodeInitStates);
    procedure ClipboardLabel1Click(Sender: TObject);
    procedure ClipboardLabelClick(Sender: TObject);
    procedure GridDataLabelClick(Sender: TObject);
    procedure MenuItem1Click(Sender: TObject);
    procedure SettingsSaveButtonClick(Sender: TObject);
    procedure SettingsLoadButtonClick(Sender: TObject);
    procedure GridSelectedRowLabelClick(Sender: TObject);
    procedure GridPrepareEditor(sender: TTisGrid;
      const aDataType: TTisColumnDataType; aControl: TWinControl);
    procedure cbColumnDataTypeEnter(Sender: TObject);
    procedure GridEditorSearching(sender: TObject; aEdit: TTisSearchEdit;
      const aText: string);
  end;

implementation

{$R *.lfm}

{ TGridFrame }

procedure TGridFrame.CustomizeButtonClick(Sender: TObject);
begin
  Grid.DefaultNodeHeight := 25;
  Grid.Customize;
end;

procedure TGridFrame.AddRowsButtonClick(Sender: TObject);
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

procedure TGridFrame.DeleteRowsButtonClick(Sender: TObject);
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

function TGridFrame.GridCompareByRow(sender: TTisGrid;
  const aPropertyName: RawUtf8; const aRow1, aRow2: TDocVariantData;
  var aHandled: Boolean): PtrInt;
begin
  // if you do not want to compare manually, just assign False or
  // do not implement this event at all
  aHandled := True;
  // return a customized comparison here
  result := aRow1.CompareObject([aPropertyName], aRow2);
end;

procedure TGridFrame.GridInitNode(Sender: TBaseVirtualTree; ParentNode,
  Node: PVirtualNode; var InitialStates: TVirtualNodeInitStates);
begin
  InitialStates := InitialStates + [ivsMultiline];
end;

procedure TGridFrame.ClipboardLabel1Click(Sender: TObject);
begin
  InOutputEdit.Lines.Clear;
end;

procedure TGridFrame.ClipboardLabelClick(Sender: TObject);
var
  c: TClipboardAdapter;
begin
  InOutputEdit.Lines.Text := c.AsString;
end;

procedure TGridFrame.GridDataLabelClick(Sender: TObject);
begin
  InOutputEdit.Lines.Text := Utf8ToString(Grid.Data.ToJson('', '', jsonHumanReadable));
end;

procedure TGridFrame.MenuItem1Click(Sender: TObject);
begin
  ShowMessage('User menu item action');
end;

procedure TGridFrame.SettingsSaveButtonClick(Sender: TObject);
begin
  if SaveDialog.Execute then
    Grid.SaveSettingsToIni(SaveDialog.FileName);
end;

procedure TGridFrame.SettingsLoadButtonClick(Sender: TObject);
begin
  if OpenDialog.Execute then
    Grid.LoadSettingsFromIni(OpenDialog.FileName);
end;

procedure TGridFrame.GridSelectedRowLabelClick(Sender: TObject);
begin
  InOutputEdit.Lines.Text := Utf8ToString(Grid.SelectedRow.ToJson('', '', jsonHumanReadable));
end;

procedure TGridFrame.GridPrepareEditor(sender: TTisGrid;
  const aDataType: TTisColumnDataType; aControl: TWinControl);
var
  a: TTisColumnDataTypeAdapter;
begin
  if aDataType = a.CaptionToEnum(cbColumnDataType.Text) then
    aControl.Color := EditorColorBox.Selected;
  if aControl is TTisSearchEdit then
  begin
    if not AsynchCheckBox.Checked then
      (aControl as TTisSearchEdit).Items.Text := SearchItemsMemo.Text;
  end;
end;

procedure TGridFrame.cbColumnDataTypeEnter(Sender: TObject);
var
  a: TTisColumnDataTypeAdapter;
begin
  a.EnumsToStrings(cbColumnDataType.Items);
end;

procedure TGridFrame.GridEditorSearching(sender: TObject;
  aEdit: TTisSearchEdit; const aText: string);
begin
  if AsynchCheckBox.Checked then
  begin
    aEdit.Items.Text := SearchItemsMemo.Lines.Text; // do not use Items.Assign()
    aEdit.DroppedDown := True;
  end;
end;

end.
