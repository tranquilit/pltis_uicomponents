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
  tis.ui.grid.controls,
  tis.ui.grid.core;

type
  TGridFrame = class(TFrame)
    ClipboardLabel: TLabel;
    ClipboardLabel1: TLabel;
    Grid: TTisGrid;
    ClipboardGridDataLabel: TLabel;
    InOutputEdit: TSynEdit;
    Label1: TLabel;
    Panel1: TPanel;
    Panel4: TPanel;
    Splitter: TSplitter;
    UserPopupMenu: TPopupMenu;
    MenuItem1: TMenuItem;
    GroupBox1: TGroupBox;
    OpenDialog: TOpenDialog;
    SaveDialog: TSaveDialog;
    GridTotalLabel: TLabel;
    ClipboardSelectedRowLabel: TLabel;
    Panel2: TPanel;
    GroupBox3: TGroupBox;
    cbColumnDataType: TComboBox;
    EdDataType: TLabel;
    EditorColorBox: TColorBox;
    Label2: TLabel;
    GroupBox4: TGroupBox;
    JsonMemo: TMemo;
    KeyFieldEdit: TEdit;
    DisplayFieldEdit: TEdit;
    Label3: TLabel;
    Label4: TLabel;
    AsynchCheckBox: TCheckBox;
    EdDataType1: TLabel;
    ColumnNameEdit: TEdit;
    GridInputPopupMenu: TPopupMenu;
    GridDataAddRowsMenuItem: TMenuItem;
    GridDataDeleteRowsMenuItem: TMenuItem;
    GridSettingsPopupMenu: TPopupMenu;
    GridSettingsSaveMenuItem: TMenuItem;
    GridSettingsLoadMenuItem: TMenuItem;
    FunctionDataLabel: TLabel;
    FunctionSettingsLabel: TLabel;
    FunctionDataLabel1: TLabel;
    GridPropsPopupMenu: TPopupMenu;
    GridDataCustomizeMenuItem1: TMenuItem;
    function GridCompareByRow(sender: TTisGrid; const aPropertyName: RawUtf8;
      const aRow1, aRow2: TDocVariantData; var aHandled: Boolean): PtrInt;
    procedure GridInitNode(Sender: TBaseVirtualTree; ParentNode,
      Node: PVirtualNode; var InitialStates: TVirtualNodeInitStates);
    procedure ClipboardLabel1Click(Sender: TObject);
    procedure ClipboardLabelClick(Sender: TObject);
    procedure ClipboardGridDataLabelClick(Sender: TObject);
    procedure MenuItem1Click(Sender: TObject);
    procedure ClipboardSelectedRowLabelClick(Sender: TObject);
    procedure GridPrepareEditor(sender: TTisGrid;
      aColumn: TTisGridColumn; aControl: TWinControl);
    procedure cbColumnDataTypeEnter(Sender: TObject);
    procedure GridEditorLookup(sender: TTisGrid; aColumn: TTisGridColumn;
      aSearchEdit: TTisSearchEdit; var aHandled: Boolean);
    procedure GridDataAddRowsMenuItemClick(Sender: TObject);
    procedure GridDataDeleteRowsMenuItemClick(Sender: TObject);
    procedure GridSettingsSaveMenuItemClick(Sender: TObject);
    procedure GridSettingsLoadMenuItemClick(Sender: TObject);
    procedure FunctionDataLabelClick(Sender: TObject);
    procedure GridDataCustomizeMenuItem1Click(Sender: TObject);
  private
    procedure DoAsyncSearch(sender: TObject; const aText: string);
  end;

implementation

{$R *.lfm}

{ TGridFrame }

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

procedure TGridFrame.ClipboardGridDataLabelClick(Sender: TObject);
begin
  InOutputEdit.Lines.Text := Utf8ToString(Grid.Data.ToJson('', '', jsonHumanReadable));
end;

procedure TGridFrame.MenuItem1Click(Sender: TObject);
begin
  ShowMessage('User menu item action');
end;

procedure TGridFrame.ClipboardSelectedRowLabelClick(Sender: TObject);
begin
  InOutputEdit.Lines.Text := Utf8ToString(Grid.SelectedRow.ToJson('', '', jsonHumanReadable));
end;

procedure TGridFrame.GridPrepareEditor(sender: TTisGrid;
  aColumn: TTisGridColumn; aControl: TWinControl);
var
  a: TTisColumnDataTypeAdapter;
begin
  if aColumn.DataType = a.CaptionToEnum(cbColumnDataType.Text) then
    aControl.Color := EditorColorBox.Selected;
end;

procedure TGridFrame.cbColumnDataTypeEnter(Sender: TObject);
var
  a: TTisColumnDataTypeAdapter;
begin
  a.EnumsToStrings(cbColumnDataType.Items);
end;

procedure TGridFrame.GridEditorLookup(sender: TTisGrid;
  aColumn: TTisGridColumn; aSearchEdit: TTisSearchEdit; var aHandled: Boolean);
begin
  if aColumn.PropertyName = ColumnNameEdit.Text then
  begin
    aHandled := True;
    aSearchEdit.LookupKeyField := KeyFieldEdit.Text;
    aSearchEdit.LookupDisplayField := DisplayFieldEdit.Text;
    if AsynchCheckBox.Checked then
      aSearchEdit.OnSearch := DoAsyncSearch
    else
    begin
      aSearchEdit.Data.InitJson(StringToUtf8(JsonMemo.Lines.Text), JSON_FAST_FLOAT);
      aSearchEdit.LoadData;
    end;
  end;
end;

procedure TGridFrame.GridDataAddRowsMenuItemClick(Sender: TObject);
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

procedure TGridFrame.GridDataDeleteRowsMenuItemClick(Sender: TObject);
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

procedure TGridFrame.GridSettingsSaveMenuItemClick(Sender: TObject);
begin
  if SaveDialog.Execute then
    Grid.SaveSettingsToIni(SaveDialog.FileName);
end;

procedure TGridFrame.GridSettingsLoadMenuItemClick(Sender: TObject);
begin
  if OpenDialog.Execute then
    Grid.LoadSettingsFromIni(OpenDialog.FileName);
end;

procedure TGridFrame.FunctionDataLabelClick(Sender: TObject);
begin
  (Sender as TLabel).PopupMenu.PopUp;
end;

procedure TGridFrame.GridDataCustomizeMenuItem1Click(Sender: TObject);
begin
  Grid.DefaultNodeHeight := 25;
  Grid.Customize;
end;

procedure TGridFrame.DoAsyncSearch(sender: TObject; const aText: string);
begin
  with sender as TTisSearchEdit do
  begin
    Data.InitJson(StringToUtf8(JsonMemo.Lines.Text), JSON_FAST_FLOAT);
    LoadData;
    DroppedDown := True;
  end;
end;

end.
