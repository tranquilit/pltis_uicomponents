// -----------------------------------------------------------------
//    This file is part of Tranquil IT Software
//    Copyright (C) 2012 - 2026  Tranquil IT https://www.tranquil.it
//    All Rights Reserved.
// ------------------------------------------------------------------
unit demo.grid.frame;

{$i tis.ui.defines.inc}

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
  ComCtrls,
  Spin,
  SynEdit,
  Variants,
  TAGraph,
  VirtualTrees,
  mormot.core.base,
  mormot.core.text,
  mormot.core.unicode,
  mormot.core.variants,
  tis.core.os,
  tis.ui.searchedit,
  tis.ui.grid.chart,
  tis.ui.grid.core,
  tis.ui.grid.controls;

type
  TGridFrame = class(TFrame)
    ClipboardLabel: TLabel;
    ClipboardLabel1: TLabel;
    ClipboardGridDataLabel: TLabel;
    InOutputEdit: TSynEdit;
    Label1: TLabel;
    Panel1: TPanel;
    Splitter: TSplitter;
    MenuItem1: TMenuItem;
    OpenDialog: TOpenDialog;
    SaveDialog: TSaveDialog;
    GridTotalLabel: TLabel;
    ClipboardSelectedRowLabel: TLabel;
    GridInputPopupMenu: TPopupMenu;
    GridDataAddRowsMenuItem: TMenuItem;
    GridInputDeleteRowsMenuItem: TMenuItem;
    GridSettingsPopupMenu: TPopupMenu;
    GridSettingsSaveMenuItem: TMenuItem;
    GridSettingsLoadMenuItem: TMenuItem;
    GridPropsPopupMenu: TPopupMenu;
    GridDataCustomizeMenuItem1: TMenuItem;
    GridSelRowsPopupMenu: TPopupMenu;
    GridSelRowsMenuItem: TMenuItem;
    ClipboardSelectedRowLabel1: TLabel;
    Panel2: TPanel;
    EventsPageControl: TPageControl;
    TabSheet2: TTabSheet;
    EdDataType1: TLabel;
    ColumnNameEdit: TEdit;
    Label3: TLabel;
    KeyFieldEdit: TEdit;
    DisplayFieldEdit: TEdit;
    Label4: TLabel;
    JsonMemo: TMemo;
    AsynchCheckBox: TCheckBox;
    TabSheet1: TTabSheet;
    EdDataType: TLabel;
    cbColumnDataType: TComboBox;
    EditorColorBox: TColorBox;
    Label2: TLabel;
    Panel3: TPanel;
    Splitter1: TSplitter;
    TabSheet3: TTabSheet;
    GroupBox2: TGroupBox;
    Label5: TLabel;
    ValidColumnNameEdit: TEdit;
    Label6: TLabel;
    ValidEditedValueEdit: TEdit;
    Label7: TLabel;
    ValidAbortCheckBox: TCheckBox;
    Label8: TLabel;
    ValidMsgEdit: TEdit;
    PrepareReadOnlyCheckBox: TCheckBox;
    PrepareColumnNameEdid: TEdit;
    EdDataType2: TLabel;
    GridMetaDataPopupMenu: TPopupMenu;
    GridMetaDataSetMenuItem: TMenuItem;
    GridMetaDataGetMenuItem: TMenuItem;
    UserPopupMenu: TPopupMenu;
    Panel4: TPanel;
    Grid: TTisGrid;
    GroupBox1: TGroupBox;
    FunctionDataLabel: TLabel;
    FunctionSettingsLabel: TLabel;
    FunctionDataLabel1: TLabel;
    FunctionSelRowsLabel: TLabel;
    FunctionSelRowsLabel1: TLabel;
    GroupBox3: TGroupBox;
    GridSearchEdit: TTisSearchEdit;
    GroupBox4: TGroupBox;
    Label9: TLabel;
    BgColorBox: TColorBox;
    ZebraLightnessLabel: TLabel;
    ZebraLightnessEdit: TSpinEdit;
    FunctionRefreshLabel: TLabel;
    FromGridDataAsObjectLabel: TLabel;
    function GridCompareByRow(aSender: TTisGrid; const aPropertyName: RawUtf8;
      const aRow1, aRow2: PDocVariantData; var aHandled: Boolean): PtrInt;
    procedure GridInitNode(Sender: TBaseVirtualTree; ParentNode,
      Node: PVirtualNode; var InitialStates: TVirtualNodeInitStates);
    procedure ClipboardLabel1Click(Sender: TObject);
    procedure ClipboardLabelClick(Sender: TObject);
    procedure ClipboardGridDataLabelClick(Sender: TObject);
    procedure MenuItem1Click(Sender: TObject);
    procedure ClipboardSelectedRowLabelClick(Sender: TObject);
    procedure GridPrepareEditor(aSender: TTisGrid;
      aNode: PVirtualNode; aColumn: TTisGridColumn; aControl: TTisGridControl);
    procedure cbColumnDataTypeEnter(Sender: TObject);
    procedure GridEditorLookup(aSender: TTisGrid; aNode: PVirtualNode;
      aColumn: TTisGridColumn; aSearchEdit: TTisSearchEdit; var aHandled: Boolean);
    procedure GridDataAddRowsMenuItemClick(Sender: TObject);
    procedure GridInputDeleteRowsMenuItemClick(Sender: TObject);
    procedure GridSettingsSaveMenuItemClick(Sender: TObject);
    procedure GridSettingsLoadMenuItemClick(Sender: TObject);
    procedure FunctionDataLabelClick(Sender: TObject);
    procedure GridDataCustomizeMenuItem1Click(Sender: TObject);
    procedure GridSelRowsMenuItemClick(Sender: TObject);
    procedure ClipboardSelectedRowLabel1Click(Sender: TObject);
    procedure GridMetaDataGetMenuItemClick(Sender: TObject);
    procedure GridMetaDataSetMenuItemClick(Sender: TObject);
    procedure GridSearchEditSearch(Sender: TObject; const aText: string);
    procedure BgColorBoxChange(Sender: TObject);
    procedure ZebraLightnessEditChange(Sender: TObject);
    procedure FunctionRefreshLabelClick(Sender: TObject);
    procedure FromGridDataAsObjectLabelClick(Sender: TObject);
    procedure GridEditValidated(aSender: TTisGrid; aNode: PVirtualNode;
      aColumn: TTisGridColumn; const aCurValue: Variant;
      var aNewValue: Variant; var aAbort: Boolean);
  private
    procedure DoAsyncSearch(aSender: TObject; const aText: string);
    procedure DoDefaultChartTitle(aSender: TTisGrid; aChart: TChart;
      aColumn: TTisGridColumn; var aFlags: TTisChartChangeFlags; var aChartTitle: string);
  public
    constructor Create(aOwner: TComponent); override;
  end;

implementation

{$R *.lfm}

{ TGridFrame }

function TGridFrame.GridCompareByRow(aSender: TTisGrid;
  const aPropertyName: RawUtf8; const aRow1, aRow2: PDocVariantData;
  var aHandled: Boolean): PtrInt;
begin
  // if you do not want to compare manually, just assign False or
  // do not implement this event at all
  aHandled := True;
  // return a customized comparison here
  result := aRow1^.CompareObject([aPropertyName], aRow2^);
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
  InOutputEdit.Lines.Text := Utf8ToString(Grid.SelectedRows.ToJson('', '', jsonHumanReadable));
end;

procedure TGridFrame.GridPrepareEditor(aSender: TTisGrid;
  aNode: PVirtualNode; aColumn: TTisGridColumn; aControl: TTisGridControl);
var
  a: TTisColumnDataTypeAdapter;
begin
  if aColumn.PropertyName = PrepareColumnNameEdid.Text then
  begin
    aControl.ReadOnly := PrepareReadOnlyCheckBox.Checked;
    if aColumn.DataType = a.CaptionToEnum(cbColumnDataType.Text) then
      aControl.Internal.Color := EditorColorBox.Selected;
  end;
end;

procedure TGridFrame.cbColumnDataTypeEnter(Sender: TObject);
var
  a: TTisColumnDataTypeAdapter;
begin
  a.EnumsToStrings(cbColumnDataType.Items);
end;

procedure TGridFrame.GridEditorLookup(aSender: TTisGrid; aNode: PVirtualNode;
  aColumn: TTisGridColumn; aSearchEdit: TTisSearchEdit; var aHandled: Boolean);
begin
  if aColumn.PropertyName = ColumnNameEdit.Text then
  begin
    aHandled := True;
    aSearchEdit.LookupKeyField := KeyFieldEdit.Text;
    aSearchEdit.LookupDisplayField := DisplayFieldEdit.Text;
    if AsynchCheckBox.Checked then
      aSearchEdit.OnSearch := @DoAsyncSearch
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

procedure TGridFrame.GridInputDeleteRowsMenuItemClick(Sender: TObject);
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

procedure TGridFrame.GridSelRowsMenuItemClick(Sender: TObject);
var
  a: array of string;
  i: Integer;
begin
  a := [];
  SetLength(a, 2);
  if not InputQuery('Update Column', ['Name', 'Value'], a) then
    exit;
  for i := 0 to high(Grid.SelectedObjects) do
  begin
    Grid.SelectedObjects[i]^.S[a[0]] := a[1];
    Grid.LoadData;
  end;
end;

procedure TGridFrame.ClipboardSelectedRowLabel1Click(Sender: TObject);
begin
  InOutputEdit.Lines.Text := Utf8ToString(Grid.ContentToCsv(tstSelected, ','));
end;

procedure TGridFrame.GridMetaDataGetMenuItemClick(Sender: TObject);
begin
  InOutputEdit.Lines.Text := Utf8ToString(Grid.MetaData);
end;

procedure TGridFrame.GridMetaDataSetMenuItemClick(Sender: TObject);
begin
  Grid.MetaData := StringToUtf8(InOutputEdit.Lines.Text);
  ShowMessage('Done! Test your grid now.');
end;

procedure TGridFrame.GridSearchEditSearch(Sender: TObject; const aText: string);
begin
  Grid.Search(aText);
end;

procedure TGridFrame.BgColorBoxChange(Sender: TObject);
begin
  Grid.Color := BgColorBox.Selected;
end;

procedure TGridFrame.ZebraLightnessEditChange(Sender: TObject);
begin
  Grid.ZebraLightness := ZebraLightnessEdit.Value;
end;

procedure TGridFrame.FunctionRefreshLabelClick(Sender: TObject);
begin
  Grid.Refresh;
end;

procedure TGridFrame.FromGridDataAsObjectLabelClick(Sender: TObject);
begin
  InOutputEdit.Lines.Text := Utf8ToString(Grid.GetDataAsJsonObject.ToJson('', '', jsonHumanReadable));
end;

procedure TGridFrame.GridEditValidated(aSender: TTisGrid; aNode: PVirtualNode;
  aColumn: TTisGridColumn; const aCurValue: Variant; var aNewValue: Variant;
  var aAbort: Boolean);
begin
  if aColumn.PropertyName = ValidColumnNameEdit.Text then
  begin
    if VarToStr(aNewValue) = ValidEditedValueEdit.Text then
    begin
      aAbort := ValidAbortCheckBox.Checked;
      if ValidMsgEdit.Text <> '' then
        ShowMessage(ValidMsgEdit.Text);
    end;
  end;
end;

procedure TGridFrame.DoAsyncSearch(aSender: TObject; const aText: string);
begin
  with aSender as TTisSearchEdit do
  begin
    Data.InitJson(StringToUtf8(JsonMemo.Lines.Text), JSON_FAST_FLOAT);
    LoadData;
    DroppedDown := True;
  end;
end;

procedure TGridFrame.DoDefaultChartTitle(aSender: TTisGrid; aChart: TChart;
  aColumn: TTisGridColumn; var aFlags: TTisChartChangeFlags; var aChartTitle: string);
var
  vColumn: TTisGridColumn;
begin
  with aFlags.Values do
  begin
    if Grid.Header.Columns.IsValidColumn(ColumnIndex) then
      vColumn := Grid.FindColumnByIndex(ColumnIndex)
    else
      vColumn := Grid.FocusedColumnObject;
  end;
  if vColumn.Index = aColumn.Index then
    aChartTitle := 'My chart per ' + aColumn.Text
  else
    aChartTitle := 'My chart using ' + vColumn.CaptionText + ' per ' + aColumn.Text;
end;

constructor TGridFrame.Create(aOwner: TComponent);
begin
  inherited Create(aOwner);
  BgColorBox.Selected := Grid.Color;
  ZebraLightnessEdit.Value := Grid.ZebraLightness;
  Grid.OnDefaultChartTitle := @DoDefaultChartTitle;
end;

end.
