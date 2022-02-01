// -----------------------------------------------------------------
//    This file is part of Tranquil IT Software
//    Copyright (C) 2012 - 2021  Tranquil IT https://www.tranquil.it
//    All Rights Reserved.
// ------------------------------------------------------------------
unit tis.ui.grid.editor;

{$mode objfpc}{$H+}
{$modeswitch ADVANCEDRECORDS}
{$modeswitch typehelpers}

interface

uses
  Classes,
  SysUtils,
  LCLIntf,
  FileUtil,
  Forms,
  Controls,
  Graphics,
  Dialogs,
  ButtonPanel,
  ExtCtrls,
  StdCtrls,
  ActnList,
  Menus,
  Buttons,
  Messages,
  MaskEdit,
  LCLType,
  EditBtn, ComCtrls,
  ComponentEditors,
  VirtualTrees,
  mormot.core.base,
  mormot.core.variants,
  mormot.core.unicode,
  mormot.core.text,
  tis.core.os,
  tis.ui.grid.core;

type
  TTisGridEditor = class(TForm)
    ActAddColumn: TAction;
    ActDelColumn: TAction;
    ActCopySettings: TAction;
    ActAddColumns: TAction;
    ActClearAll: TAction;
    ActRemoveAllColumns: TAction;
    ActPasteCSV: TAction;
    ActUpdateColumn: TAction;
    ActPasteJsonTemplate: TAction;
    ActionList: TActionList;
    KeepDataCheckBox: TCheckBox;
    DelColumnsButton: TButton;
    MenuItem8: TMenuItem;
    PasteJsonButton: TButton;
    AddColumnButton: TButton;
    Grid: TTisGrid;
    ButtonPanel: TButtonPanel;
    MenuItem1: TMenuItem;
    MenuItem2: TMenuItem;
    MenuItem3: TMenuItem;
    MenuItem4: TMenuItem;
    MenuItem5: TMenuItem;
    MenuItem6: TMenuItem;
    MenuItem7: TMenuItem;
    Panel1: TPanel;
    PopupMenu1: TPopupMenu;
    ClearAllButton: TButton;
    PropsPageControl: TPageControl;
    ColumnPropsTab: TTabSheet;
    GridPropsTab: TTabSheet;
    MenuItem9: TMenuItem;
    MenuItem10: TMenuItem;
    MenuItem11: TMenuItem;
    MenuItem12: TMenuItem;
    MenuItem13: TMenuItem;
    MenuItem14: TMenuItem;
    MenuItem15: TMenuItem;
    MenuItem16: TMenuItem;
    MenuItem17: TMenuItem;
    MenuItem18: TMenuItem;
    EdColumnTitle: TLabeledEdit;
    EdColumnProperty: TLabeledEdit;
    cbColumnDataType: TComboBox;
    EdDataType: TLabel;
    UpdateColumnButton: TButton;
    EdColumnIndex: TLabeledEdit;
    EdPosition: TEdit;
    DelColumnButton1: TButton;
    Label1: TLabel;
    RequiredCheckBox: TCheckBox;
    AutoSortCheckBox: TCheckBox;
    MultiSelectCheckBox: TCheckBox;
    EditableCheckBox: TCheckBox;
    SortColumnClearLabel: TLabel;
    procedure ActAddColumnExecute(Sender: TObject);
    procedure ActAddColumnsExecute(Sender: TObject);
    procedure ActClearAllExecute(Sender: TObject);
    procedure ActDelColumnExecute(Sender: TObject);
    procedure ActPasteCSVExecute(Sender: TObject);
    procedure ActPasteJsonTemplateExecute(Sender: TObject);
    procedure ActRemoveAllColumnsExecute(Sender: TObject);
    procedure ActUpdateColumnExecute(Sender: TObject);
    procedure GridHeaderDragged(Sender: TVTHeader; Column: TColumnIndex;
      OldPosition: Integer);
    procedure Button6Click(Sender: TObject);
    procedure EdColumnPropertyExit(Sender: TObject);
    procedure EdColumnPropertyKeyPress(Sender: TObject; var Key: char);
    procedure EdColumnTitleExit(Sender: TObject);
    procedure EdColumnTitleKeyPress(Sender: TObject; var Key: char);
    procedure FormCreate(Sender: TObject);
    procedure GridHeaderDragging(Sender: TVTHeader; Column: TColumnIndex;
      var Allowed: Boolean);
    procedure GridHeaderClick(Sender: TVTHeader; HitInfo: TVTHeaderHitInfo);
    procedure EdColumnIndexChange(Sender: TObject);
    procedure AutoSortCheckBoxChange(Sender: TObject);
    procedure GridClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure MultiSelectCheckBoxChange(Sender: TObject);
    procedure EditableCheckBoxChange(Sender: TObject);
    procedure SortColumnClearLabelClick(Sender: TObject);
  private
    procedure SetPropertiesPanel(aColIndex, aColTitle, aColProperty,
      aColPosition: string; const aColDataType: TTisColumnDataType;
      aColRequired: Boolean);
    procedure ClearPropertiesPanel;
    procedure LoadGridCommonProps;
  end;

  TTisGridComponentEditor = class(TComponentEditor)
  protected
    procedure DoShowColumnsEditor;
    procedure DoShowEditor;
  public
    procedure ExecuteVerb(Index: Integer); override;
    function GetVerb(Index: Integer): string; override;
    function GetVerbCount: Integer; override;
  end;

implementation

uses
  PropEdits;

{$R *.lfm}

{ TTisGridEditor }

procedure TTisGridEditor.ActPasteCSVExecute(Sender: TObject);
var
  cb: TClipboardAdapter;
begin
  Grid.Data.InitCsv(cb.AsUtf8, JSON_FAST_FLOAT);
end;

procedure TTisGridEditor.ActAddColumnExecute(Sender: TObject);
var
  col : TTisGridColumn;
begin
  col :=  TTisGridColumn(Grid.Header.Columns.Add);
  col.Text := 'Col ' + IntToStr(col.Index);
  col.PropertyName := 'column' + IntToStr(col.Index);
  Grid.FocusedColumn := col.Index;
end;

procedure TTisGridEditor.ActAddColumnsExecute(Sender: TObject);
begin
  Grid.CreateColumnsFromData(False, False);
end;

procedure TTisGridEditor.ActClearAllExecute(Sender: TObject);
begin
  Grid.ClearAll;
  ClearPropertiesPanel;
end;

procedure TTisGridEditor.ActDelColumnExecute(Sender: TObject);
var
  col: TTisGridColumn;
  idx: Integer;
begin
  col := TTisGridColumn(Grid.Header.Columns[StrToInt(EdColumnIndex.Text)]);
  Grid.Header.Columns.Delete(col.Index);
  idx := col.Index-1;
  if not Grid.Header.Columns.IsValidColumn(idx) then
  begin
    if Grid.Header.Columns.GetLastVisibleColumn >= 0 then
      idx := Grid.Header.Columns.GetLastVisibleColumn;
  end;
  if idx > NoColumn then
  begin
    col := TTisGridColumn(Grid.Header.Columns[idx]);
    SetPropertiesPanel(IntToStr(col.Index), col.Text, col.PropertyName,
      IntToStr(col.Position), col.DataType, col.Required);
  end
  else
    ClearPropertiesPanel;
end;

procedure TTisGridEditor.ActPasteJsonTemplateExecute(Sender: TObject);
const
  ERROR_MSG = 'Clipboard content is not a valid JSON Array of records';
var
  cb: TClipboardAdapter;
  doc: TDocVariantData;
  d: PDocVariantData;
  i, r: PVariant;
begin
  try
    if doc.InitJson(cb.AsUtf8, JSON_FAST_FLOAT) then
    begin
      if doc.Kind = dvArray then
      begin
        for i in doc.Items do // using .Items to get all kind of data, eg: [1,2,3]
        begin
          d := PDocVariantData(i);
          case d^.Kind of
            dvArray:
              for r in d^.Items do
                Grid.Data.AddItem(r^);
            dvObject:
              Grid.Data.AddItem(i^);
          else
            Grid.Data.AddItem(_Json('{"unknown":"' + VariantToUtf8(i^) + '"}'));
          end;
        end;
      end
      else if doc.Kind = dvObject then
      begin
        Grid.Data.AddItem(variant(doc));
      end;
      Grid.LoadData;
      Grid.CreateColumnsFromData(True, False);
    end
    else
      ShowMessage(ERROR_MSG);
  except
    ShowMessage(ERROR_MSG);
  end;
end;

procedure TTisGridEditor.ActRemoveAllColumnsExecute(Sender: TObject);
begin
  Grid.Header.Columns.Clear;
  ClearPropertiesPanel;
end;

procedure TTisGridEditor.ActUpdateColumnExecute(Sender: TObject);
var
  col: TTisGridColumn;
  a: TTisColumnDataTypeAdapter;
begin
  col := TTisGridColumn(Grid.Header.Columns[StrToInt(EdColumnIndex.Text)]);
  if col <> nil then
  begin
    col.Text := EdColumnTitle.Text;
    col.PropertyName := EdColumnProperty.Text;
    col.DataType := a.CaptionToEnum(cbColumnDataType.Text);
    col.Required := RequiredCheckBox.Checked;
  end;
  Grid.Invalidate;
end;

procedure TTisGridEditor.GridHeaderDragged(Sender: TVTHeader;
  Column: TColumnIndex; OldPosition: Integer);
begin
  Grid.ReorderColumns;
end;

function colsort(c1,c2: TCollectionItem): Integer;
begin
  if TTisGridColumn(c1).Position < TTisGridColumn(c2).Position then
    result := -1
  else
  if TTisGridColumn(c1).Position > TTisGridColumn(c2).Position then
    result := 1
  else
    result := 0;
end;

procedure TTisGridEditor.Button6Click(Sender: TObject);
begin
  Grid.ReorderColumns;
end;

procedure TTisGridEditor.EdColumnPropertyExit(Sender: TObject);
begin
  if Grid.FocusedColumnObject <> nil then
  begin
    Grid.FocusedColumnObject.PropertyName := EdColumnProperty.Text;
    Grid.Invalidate;
  end;
end;

procedure TTisGridEditor.EdColumnPropertyKeyPress(Sender: TObject; var Key: char);
begin
  if (key = #13) and (Grid.FocusedColumnObject <> nil)  then
  begin
    Grid.FocusedColumnObject.PropertyName := EdColumnProperty.Text;
    Grid.Invalidate;
    Key := #0;
  end;
end;

procedure TTisGridEditor.EdColumnTitleExit(Sender: TObject);
begin
  if Grid.FocusedColumnObject <> nil then
  begin
    Grid.FocusedColumnObject.Text := EdColumnTitle.Text;
    Grid.Invalidate;
  end;
end;

procedure TTisGridEditor.EdColumnTitleKeyPress(Sender: TObject; var Key: char);
begin
  if (key = #13) and (Grid.FocusedColumnObject <> nil)  then
  begin
    Grid.FocusedColumnObject.Text := EdColumnTitle.Text;
    Grid.Invalidate;
    Key := #0;
  end;
end;

procedure TTisGridEditor.FormCreate(Sender: TObject);
var
  a: TTisColumnDataTypeAdapter;
begin
  ButtonPanel.OKButton.Default := False;
  a.EnumsToStrings(cbColumnDataType.Items);
end;

procedure TTisGridEditor.GridHeaderDragging(Sender: TVTHeader;
  Column: TColumnIndex; var Allowed: Boolean);
begin
  Grid.ReorderColumns;
end;

procedure TTisGridEditor.GridHeaderClick(Sender: TVTHeader;
  HitInfo: TVTHeaderHitInfo);
var
  col: TTisGridColumn;
begin
  PropsPageControl.ActivePage := ColumnPropsTab;
  col := TTisGridColumn(Grid.Header.Columns[HitInfo.Column]);
  if col <> nil then
    SetPropertiesPanel(IntToStr(col.Index), col.Text, col.PropertyName,
      IntToStr(col.Position), col.DataType, col.Required)
  else
    ClearPropertiesPanel;
end;

procedure TTisGridEditor.EdColumnIndexChange(Sender: TObject);
begin
  ActUpdateColumn.Enabled := EdColumnIndex.Text <> '';
  ActDelColumn.Enabled := ActUpdateColumn.Enabled;
end;

procedure TTisGridEditor.AutoSortCheckBoxChange(Sender: TObject);
begin
  with Grid.Header do
    if AutoSortCheckBox.Checked then
      Options := Options + [hoHeaderClickAutoSort]
    else
      Options := Options - [hoHeaderClickAutoSort];
end;

procedure TTisGridEditor.GridClick(Sender: TObject);
begin
  PropsPageControl.ActivePage := GridPropsTab;
end;

procedure TTisGridEditor.FormShow(Sender: TObject);
begin
  LoadGridCommonProps;
end;

procedure TTisGridEditor.MultiSelectCheckBoxChange(Sender: TObject);
begin
  with Grid.TreeOptions do
    if MultiSelectCheckBox.Checked then
      SelectionOptions := SelectionOptions + [toMultiSelect]
    else
      SelectionOptions := SelectionOptions - [toMultiSelect];
end;

procedure TTisGridEditor.EditableCheckBoxChange(Sender: TObject);
begin
  with Grid.TreeOptions do
    if EditableCheckBox.Checked then
      MiscOptions := MiscOptions + [toEditable]
    else
      MiscOptions := MiscOptions - [toEditable];
end;

procedure TTisGridEditor.SortColumnClearLabelClick(Sender: TObject);
begin
  Grid.Header.SortColumn := -1;
end;

procedure TTisGridEditor.SetPropertiesPanel(aColIndex, aColTitle, aColProperty,
  aColPosition: string; const aColDataType: TTisColumnDataType;
  aColRequired: Boolean);
var
  a: TTisColumnDataTypeAdapter;
begin
  EdColumnIndex.Text := aColIndex;
  EdColumnTitle.Text := aColTitle;
  EdColumnProperty.Text := aColProperty;
  cbColumnDataType.ItemIndex := a.EnumToIndex(aColDataType);
  EdPosition.Text := aColPosition;
  RequiredCheckBox.Checked := aColRequired;
end;

procedure TTisGridEditor.ClearPropertiesPanel;
begin
  SetPropertiesPanel('', '', '', '', low(TTisColumnDataType), False);
end;

procedure TTisGridEditor.LoadGridCommonProps;
begin
  AutoSortCheckBox.Checked := hoHeaderClickAutoSort in Grid.Header.Options;
  MultiSelectCheckBox.Checked := toMultiSelect in Grid.TreeOptions.SelectionOptions;
  EditableCheckBox.Checked := toEditable in Grid.TreeOptions.MiscOptions;
end;

{ TTisGridComponentEditor }

procedure TTisGridComponentEditor.DoShowColumnsEditor;
begin
  EditCollection(Component, (Component as TTisGrid).Header.Columns, 'Header.Columns');
end;

procedure TTisGridComponentEditor.DoShowEditor;
begin
  (Component as TTisGrid).Customize;
end;

procedure TTisGridComponentEditor.ExecuteVerb(Index: Integer);
begin
  case Index of
    0: DoShowEditor;
    1: DoShowColumnsEditor;
    2: (Component as TTisGrid).CreateColumnsFromData(False, False);
  end;
end;

function TTisGridComponentEditor.GetVerb(Index: Integer): string;
begin
  case Index of
    0: result := 'Edit grid...';
    1: result := 'Edit columns...';
    2: result := 'Create missing columns from sample data';
  else
    result := 'Unknow';
  end;
end;

function TTisGridComponentEditor.GetVerbCount: Integer;
begin
  result := 3;
end;

end.

