// -----------------------------------------------------------------
//    This file is part of Tranquil IT Software
//    Copyright (C) 2012 - 2024  Tranquil IT https://www.tranquil.it
//    All Rights Reserved.
// ------------------------------------------------------------------
unit tis.ui.grid.chart;

{$i tis.ui.defines.inc}

interface

uses
  Classes,
  ComCtrls,
  ExtCtrls,
  Spin,
  StdCtrls,
  SysUtils,
  Forms,
  Controls,
  Graphics,
  Dialogs,
  ActnList,
  mormot.core.base,
  mormot.core.rtti,
  mormot.core.buffers,
  mormot.core.variants,
  TAGraph,
  TARadialSeries,
  TASeries,
  TASources,
  TACustomSource,
  TAChartUtils,
  TATextElements,
  TATools;

type
  TTisChartForm = class;

  TTisChartFillSourceFlags = object
  public
    ValueColumnIndex: Integer;
    MostUsedValues: record
      Enabled: Boolean;
      Count: Integer;
    end;
    procedure Init;
  end;

  TTisChartFillSourceEvent = procedure(aChart: TChart; aSource: TListChartSource; var aFlags: TTisChartFillSourceFlags) of object;

  TTisChartChangeFlags = object
  public
    Title: record
      Customized: Boolean;
      Text: string;
    end;
    Values: record
      ColumnIndex: Integer
    end;
    procedure Init;
  end;

  TTisChartChangeEvent = procedure(aChart: TChart; var aFlags: TTisChartChangeFlags) of object;

  TTisChartForm = class(TForm)
    cbMarkAttachment: TComboBox;
    cmbOrientation: TComboBox;
    PieChart: TChart;
    PieChartPieSeries1: TPieSeries;
    ChartToolset: TChartToolset;
    cbRotate: TCheckBox;
    cbMarkPositions: TComboBox;
    Cb3D: TCheckBox;
    cbShowLabels: TCheckBox;
    cbMarkPositionsCentered: TCheckBox;
    lblViewAngle: TLabel;
    lblDistance: TLabel;
    lblStartAngle: TLabel;
    lblAngleRange: TLabel;
    seDepth: TSpinEdit;
    seViewAngle: TSpinEdit;
    seDepthBrightnessDelta: TSpinEdit;
    lblInnerRadius: TLabel;
    lblDepth: TLabel;
    lblDepthBrightnessDelta: TLabel;
    lblWords: TLabel;
    lblLabelAngle: TLabel;
    PieSource: TListChartSource;
    PageControl: TPageControl;
    PiePanel: TPanel;
    seStartAngle: TSpinEdit;
    seAngleRange: TSpinEdit;
    seWords: TSpinEdit;
    seLabelAngle: TSpinEdit;
    seInnerRadius: TSpinEdit;
    seDistance: TSpinEdit;
    tsPie: TTabSheet;
    ActionList: TActionList;
    PieClipboardAction: TAction;
    PieCustomizeAction: TAction;
    ToolBar1: TToolBar;
    PieClipboardButton: TToolButton;
    PieCustomizeButton: TToolButton;
    ToolButton3: TToolButton;
    PieCloseButton: TToolButton;
    ToolButton1: TToolButton;
    PieCloseAction: TAction;
    PieTitleEdit: TEdit;
    Label1: TLabel;
    PieValuesCombo: TComboBox;
    MostUsedCheckbox: TCheckBox;
    TopMostUsedEdit: TSpinEdit;
    TopMostUsedLabel: TLabel;
    PieRestoreButton: TToolButton;
    ToolButton4: TToolButton;
    PieRestoreAction: TAction;
    procedure cbMarkAttachmentChange(Sender: TObject);
    procedure cbMarkPositionsCenteredChange(Sender: TObject);
    procedure cbMarkPositionsChange(Sender: TObject);
    procedure cbRotateChange(Sender: TObject);
    procedure cbShowLabelsChange(Sender: TObject);
    procedure PieChartMouseDown(Sender: TObject; {%H-}Button: TMouseButton;
      {%H-}Shift: TShiftState; X, Y: Integer);
    procedure Cb3DChange(Sender: TObject);
    procedure cmbOrientationChange(Sender: TObject);
    procedure seDepthBrightnessDeltaChange(Sender: TObject);
    procedure seDepthChange(Sender: TObject);
    procedure seDistanceChange(Sender: TObject);
    procedure seAngleRangeChange(Sender: TObject);
    procedure seInnerRadiusChange(Sender: TObject);
    procedure seStartAngleChange(Sender: TObject);
    procedure seViewAngleChange(Sender: TObject);
    procedure seWordsChange(Sender: TObject);
    procedure seLabelAngleChange(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure PieClipboardActionExecute(Sender: TObject);
    procedure PieCustomizeActionExecute(Sender: TObject);
    procedure PieCloseActionExecute(Sender: TObject);
    procedure PieTitleEditChange(Sender: TObject);
    procedure PieValuesComboChange(Sender: TObject);
    procedure MostUsedCheckboxChange(Sender: TObject);
    procedure TopMostUsedEditChange(Sender: TObject);
    procedure PieRestoreActionExecute(Sender: TObject);
  private
    fSavedDataPoints: TStrings;
    fSaveMarks: record
      Style: TSeriesMarksStyle;
      Format: string;
    end;
    fOnChartFillSource: TTisChartFillSourceEvent;
    fOnChartChange: TTisChartChangeEvent;
    fDefaultSettings: RawUtf8;
    function GetSettings: RawUtf8;
    procedure SetSettings(const aValue: RawUtf8);
  protected
    function PieValuesIndexAsColumnIndex: Integer;
    procedure DoChartFillSource(aChart: TChart);
    procedure DoChartChange(aChart: TChart);
  public
    property Settings: RawUtf8 read GetSettings write SetSettings;
    property OnChartFillSource: TTisChartFillSourceEvent read fOnChartFillSource write fOnChartFillSource;
    property OnChartChange: TTisChartChangeEvent read fOnChartChange write fOnChartChange;
  end;

implementation

{$R *.lfm}

{ TTisChartChangeFlags }

procedure TTisChartChangeFlags.Init;
begin
  RecordZero(@self, TypeInfo(TTisChartChangeFlags));
  Values.ColumnIndex := -1;
end;

{ TTisChartFillSourceFlags }

procedure TTisChartFillSourceFlags.Init;
begin
  RecordZero(@self, TypeInfo(TTisChartFillSourceFlags));
end;

{ TTisChartForm }

procedure TTisChartForm.cbMarkAttachmentChange(Sender: TObject);
begin
  PieChartPieSeries1.Marks.Attachment :=
    TChartMarkAttachment(cbMarkAttachment.ItemIndex);
end;

procedure TTisChartForm.cbMarkPositionsCenteredChange(Sender: TObject);
begin
  PieChartPieSeries1.MarkPositionCentered := cbMarkPositionsCentered.Checked;
end;

procedure TTisChartForm.cbMarkPositionsChange(Sender: TObject);
begin
  PieChartPieSeries1.MarkPositions :=
    TPieMarkPositions(cbMarkPositions.ItemIndex);
end;

procedure TTisChartForm.cbRotateChange(Sender: TObject);
begin
  PieChartPieSeries1.RotateLabels := cbRotate.Checked;
end;

procedure TTisChartForm.cbShowLabelsChange(Sender: TObject);
begin
  if cbShowLabels.Checked then
  begin
    with PieChartPieSeries1.Marks do
    begin
      Style := fSaveMarks.Style;
      Format := fSaveMarks.Format;
    end;
  end
  else
    PieChartPieSeries1.Marks.Style := smsNone;
  seWords.Enabled := cbShowLabels.Checked;
  lblWords.Enabled := cbShowLabels.Checked;
  seLabelAngle.Enabled := cbShowLabels.Checked;
  lblLabelAngle.Enabled := cbShowLabels.Checked;
  cbMarkPositions.Enabled := cbShowLabels.Checked;
  cbMarkAttachment.Enabled := cbShowlabels.Checked;
  cbMarkPositionsCentered.Enabled := cbShowLabels.Checked;
  cbRotate.Enabled := cbShowLabels.Checked;
end;

procedure TTisChartForm.Cb3DChange(Sender: TObject);
begin
  if cb3D.Checked then
    PieChartPieSeries1.Depth := seDepth.Value
  else
    PieChartPieSeries1.Depth := 0;
  seDepth.Enabled := cb3D.Checked;
  lblDepth.Enabled := cb3D.Checked;
  seDepthBrightnessDelta.Enabled := cb3D.Checked;
  lblDepthBrightnessDelta.Enabled := cb3D.Checked;
  lblViewAngle.Enabled := cb3D.Checked;
  seViewAngle.Enabled := cb3D.Checked;
  cmbOrientation.Enabled := cb3D.Checked;
end;

procedure TTisChartForm.cmbOrientationChange(Sender: TObject);
begin
  PieChartPieSeries1.Orientation := TPieOrientation(cmbOrientation.ItemIndex);
end;

procedure TTisChartForm.seDepthBrightnessDeltaChange(Sender: TObject);
begin
  PieChartPieSeries1.DepthBrightnessDelta := seDepthBrightnessDelta.Value;
end;

procedure TTisChartForm.seDepthChange(Sender: TObject);
begin
  PieChartPieSeries1.Depth := seDepth.Value;
end;

procedure TTisChartForm.seDistanceChange(Sender: TObject);
begin
  PieChartPieSeries1.Marks.Distance := seDistance.Value;
end;

procedure TTisChartForm.seAngleRangeChange(Sender: TObject);
begin
  PieChartPieSeries1.AngleRange := seAngleRange.Value;
end;

procedure TTisChartForm.seInnerRadiusChange(Sender: TObject);
begin
  PieChartPieSeries1.InnerRadiusPercent := seInnerRadius.Value;
end;

procedure TTisChartForm.PieChartMouseDown(
  Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
  i: Integer;
begin
  i := PieChartPieSeries1.FindContainingSlice(Point(X, Y));
  if i < 0 then exit;
  PieSource.SetXValue(i, 0.2 - PieSource[i]^.X);
  PieChart.Invalidate;
end;

procedure TTisChartForm.seStartAngleChange(Sender: TObject);
begin
  PieChartPieSeries1.StartAngle := seStartAngle.Value;
end;

procedure TTisChartForm.seViewAngleChange(Sender: TObject);
begin
  PieChartPieSeries1.ViewAngle := seViewAngle.Value;
end;

procedure TTisChartForm.seLabelAngleChange(Sender: TObject);
begin
  PieChartPieSeries1.Marks.LabelFont.Orientation := seLabelAngle.Value * 10;
end;

procedure TTisChartForm.FormCreate(Sender: TObject);
begin
  PieChart.Title.Text.Text := PieTitleEdit.Text;
  PieValuesCombo.Items.Clear;
  PieValuesCombo.Items.Add(''); // to user be able to set default, which is to count
  fSavedDataPoints := TStringList.Create;
  with fSaveMarks do
  begin
    Style := smsCustom;
    Format := PieChartPieSeries1.Marks.Format;
  end;
  fDefaultSettings := Settings;
end;

procedure TTisChartForm.FormDestroy(Sender: TObject);
begin
  fSavedDataPoints.Free;
end;

procedure TTisChartForm.FormShow(Sender: TObject);
begin
  DoChartFillSource(PieChart);
  seWordsChange(seWords);
end;

procedure TTisChartForm.PieClipboardActionExecute(Sender: TObject);
begin
  PieChart.CopyToClipboardBitmap;
end;

procedure TTisChartForm.PieCustomizeActionExecute(Sender: TObject);
begin
  PieCustomizeAction.Checked := not PieCustomizeAction.Checked;
  PiePanel.Visible := PieCustomizeAction.Checked;
end;

procedure TTisChartForm.PieCloseActionExecute(Sender: TObject);
begin
  Close;
end;

procedure TTisChartForm.PieTitleEditChange(Sender: TObject);
begin
  PieChart.Title.Text.Text := PieTitleEdit.Text;
  DoChartChange(PieChart);
end;

procedure TTisChartForm.PieValuesComboChange(Sender: TObject);
begin
  DoChartFillSource(PieChart);
  DoChartChange(PieChart);
end;

procedure TTisChartForm.MostUsedCheckboxChange(Sender: TObject);
begin
  TopMostUsedEdit.Enabled := MostUsedCheckbox.Checked;
  DoChartFillSource(PieChart);
  DoChartChange(PieChart);
end;

procedure TTisChartForm.TopMostUsedEditChange(Sender: TObject);
begin
  DoChartFillSource(PieChart);
  DoChartChange(PieChart);
end;

procedure TTisChartForm.PieRestoreActionExecute(Sender: TObject);
begin
  Settings := fDefaultSettings;
end;

function TTisChartForm.GetSettings: RawUtf8;
var
  vObj: TDocVariantData;
begin
  vObj.InitFast(dvObject);
  // form
  with vObj.O_['form']^ do
  begin
    I['top'] := Self.Top;
    I['left'] := Self.Left;
    I['height'] := Self.Height;
    I['width'] := Self.Width;
  end;
  // customization
  vObj.S['title'] := PieTitleEdit.Text;
  vObj.O_['valuescombo']^.I['itemindex'] := PieValuesCombo.ItemIndex;
  with vObj.O_['mostusedvalues']^ do
  begin
    B['enabled'] := MostUsedCheckbox.Checked;
    I['count'] := TopMostUsedEdit.Value;
  end;
  result := BinToBase64(vObj.ToJson);
end;

procedure TTisChartForm.SetSettings(const aValue: RawUtf8);
var
  vObj: TDocVariantData;
begin
  if aValue <> '' then
  begin
    vObj.InitJson(Base64ToBin(aValue), JSON_FAST_FLOAT);
    // form
    with vObj.O_['form']^ do
    begin
      Self.Top := GetValueOrDefault('top', Self.Top);
      Self.Left := GetValueOrDefault('left', Self.Left);
      Self.Height := GetValueOrDefault('height', Self.Height);
      Self.Width := GetValueOrDefault('width', Self.Width);
    end;
    // customization
    PieTitleEdit.Text := vObj.S['title'];
    PieValuesCombo.ItemIndex := vObj.O_['valuescombo']^.GetValueOrDefault('itemindex', -1);
    with vObj.O_['mostusedvalues']^ do
    begin
      MostUsedCheckbox.Checked := GetValueOrDefault('enabled', MostUsedCheckbox.Checked);
      TopMostUsedEdit.Value := GetValueOrDefault('count', TopMostUsedEdit.Increment);
    end;
  end;
end;

function TTisChartForm.PieValuesIndexAsColumnIndex: Integer;
begin
  result := PieValuesCombo.ItemIndex;
  if PieValuesCombo.ItemIndex > 0 then // -1 or 0 is the same as empty
    Dec(result)
  else
    result := -1;
end;

procedure TTisChartForm.seWordsChange(Sender: TObject);

  function ExtractWords(aText: string; aWordCount: Integer): string;
  var
    v1, vPos: Integer;
  begin
    result := '';
    for v1 := 1 to aWordCount do
    begin
      vPos := Pos(' ', aText);
      if vPos > 0 then
      begin
        result += Copy(aText, 1, vPos - 1) + ' ';
        aText := Copy(aText, vPos + 1, Length(aText));
      end
      else
      begin
        result += aText;
        Break;
      end;
    end;
    result := Trim(result);
  end;

var
  v1: Integer;
begin
  if fSavedDataPoints.Count = 0 then
    fSavedDataPoints.Assign(PieSource.DataPoints);
  PieSource.DataPoints.Assign(fSavedDataPoints);
  for v1 := 0 to PieSource.Count - 1 do
  begin
    with PieSource[v1]^ do
      Text := ExtractWords(Text, seWords.Value);
  end;
  PieChart.Invalidate;
end;

procedure TTisChartForm.DoChartFillSource(aChart: TChart);
var
  vFlags: TTisChartFillSourceFlags;
begin
  if Assigned(fOnChartFillSource) then
  begin
    PieSource.Clear;
    vFlags.Init;
    vFlags.ValueColumnIndex := PieValuesIndexAsColumnIndex;
    with vFlags.MostUsedValues do
    begin
      Enabled := MostUsedCheckbox.Checked;
      Count := TopMostUsedEdit.Value;
    end;
    fOnChartFillSource(aChart, PieSource, vFlags);
  end;
end;

procedure TTisChartForm.DoChartChange(aChart: TChart);
var
  vFlags: TTisChartChangeFlags;
begin
  if Assigned(fOnChartChange) then
  begin
    vFlags.Init;
    with vFlags.Title do
    begin
      Customized := PieTitleEdit.Text <> '';
      Text := PieTitleEdit.Text;
    end;
    vFlags.Values.ColumnIndex := PieValuesIndexAsColumnIndex;
    fOnChartChange(aChart, vFlags);
  end;
end;

end.

