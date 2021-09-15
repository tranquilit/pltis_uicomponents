// -----------------------------------------------------------------
//    This file is part of Tranquil IT Software
//    Copyright (C) 2012 - 2021  Tranquil IT https://www.tranquil.it
//    All Rights Reserved.
// ------------------------------------------------------------------
unit tis.ui.grid.editor;

{$i mormot.defines.inc}

interface

uses
  classes,
  sysutils,
  fileutil,
  forms,
  controls,
  graphics,
  dialogs,
  buttonpanel,
  extctrls,
  stdctrls,
  actnlist,
  menus,
  componenteditors,
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
    UpdateColumnButton: TButton;
    ButtonPanel: TButtonPanel;
    cbEditorType: TComboBox;
    EdColumnTitle: TLabeledEdit;
    EdColumnProperty: TLabeledEdit;
    EdColumnIndex: TLabeledEdit;
    EdPosition: TEdit;
    LstEditorType: TLabel;
    MenuItem1: TMenuItem;
    MenuItem2: TMenuItem;
    MenuItem3: TMenuItem;
    MenuItem4: TMenuItem;
    MenuItem5: TMenuItem;
    MenuItem6: TMenuItem;
    MenuItem7: TMenuItem;
    Panel1: TPanel;
    PropertiesPanel: TPanel;
    PopupMenu1: TPopupMenu;
    DelColumnButton1: TButton;
    Label1: TLabel;
    ClearAllButton: TButton;
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
  private
    procedure SetPropertiesPanel(aColIndex, aColTitle, aColProperty,
      aColPosition: string);
    procedure ClearPropertiesPanel;
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
  propedits;

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
      IntToStr(col.Position));
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
begin
  col := TTisGridColumn(Grid.Header.Columns[StrToInt(EdColumnIndex.Text)]);
  if col <> nil then
  begin
    col.Text := EdColumnTitle.Text;
    col.PropertyName := EdColumnProperty.Text;
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
begin
  ButtonPanel.OKButton.Default := False;
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
  col := TTisGridColumn(Grid.Header.Columns[HitInfo.Column]);
  if col <> nil then
    SetPropertiesPanel(IntToStr(col.Index), col.Text, col.PropertyName,
      IntToStr(col.Position))
  else
    ClearPropertiesPanel;
end;

procedure TTisGridEditor.EdColumnIndexChange(Sender: TObject);
begin
  ActUpdateColumn.Enabled := EdColumnIndex.Text <> '';
  ActDelColumn.Enabled := ActUpdateColumn.Enabled;
end;

procedure TTisGridEditor.SetPropertiesPanel(aColIndex, aColTitle,
  aColProperty, aColPosition: string);
begin
  EdColumnIndex.Text := aColIndex;
  EdColumnTitle.Text := aColTitle;
  EdColumnProperty.Text := aColProperty;
  EdPosition.Text := aColPosition;
end;

procedure TTisGridEditor.ClearPropertiesPanel;
begin
  SetPropertiesPanel('','', '', '');
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

