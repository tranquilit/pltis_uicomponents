// -----------------------------------------------------------------
//    This file is part of Tranquil IT Software
//    Copyright (C) 2012 - 2023  Tranquil IT https://www.tranquil.it
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
  Dialogs, ActnList,
  TAGraph,
  TARadialSeries,
  TASeries,
  TASources,
  TACustomSource,
  TAChartUtils,
  TATextElements,
  TATools;

type
  TGridChartForm = class;

  TGridFillSourceEvent = procedure(aSender: TGridChartForm) of object;

  TGridChartForm = class(TForm)
    cbMarkAttachment: TComboBox;
    cmbOrientation: TComboBox;
    PieChart: TChart;
    PieChartPieSeries1: TPieSeries;
    ChartToolset1: TChartToolset;
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
    ListChartSource: TListChartSource;
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
  private
    fSavedDataPoints: TStrings;
    fSaveMarks: record
      Style: TSeriesMarksStyle;
      Format: string;
    end;
    fOnFillSource: TGridFillSourceEvent;
  protected
    procedure DoFillSource;
  public
    property OnFillSource: TGridFillSourceEvent read fOnFillSource write fOnFillSource;
  end;

implementation

{$R *.lfm}

{ TGridChartForm }

procedure TGridChartForm.cbMarkAttachmentChange(Sender: TObject);
begin
  PieChartPieSeries1.Marks.Attachment :=
    TChartMarkAttachment(cbMarkAttachment.ItemIndex);
end;

procedure TGridChartForm.cbMarkPositionsCenteredChange(Sender: TObject);
begin
  PieChartPieSeries1.MarkPositionCentered := cbMarkPositionsCentered.Checked;
end;

procedure TGridChartForm.cbMarkPositionsChange(Sender: TObject);
begin
  PieChartPieSeries1.MarkPositions :=
    TPieMarkPositions(cbMarkPositions.ItemIndex);
end;

procedure TGridChartForm.cbRotateChange(Sender: TObject);
begin
  PieChartPieSeries1.RotateLabels := cbRotate.Checked;
end;

procedure TGridChartForm.cbShowLabelsChange(Sender: TObject);
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

procedure TGridChartForm.Cb3DChange(Sender: TObject);
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

procedure TGridChartForm.cmbOrientationChange(Sender: TObject);
begin
  PieChartPieSeries1.Orientation := TPieOrientation(cmbOrientation.ItemIndex);
end;

procedure TGridChartForm.seDepthBrightnessDeltaChange(Sender: TObject);
begin
  PieChartPieSeries1.DepthBrightnessDelta := seDepthBrightnessDelta.Value;
end;

procedure TGridChartForm.seDepthChange(Sender: TObject);
begin
  PieChartPieSeries1.Depth := seDepth.Value;
end;

procedure TGridChartForm.seDistanceChange(Sender: TObject);
begin
  PieChartPieSeries1.Marks.Distance := seDistance.Value;
end;

procedure TGridChartForm.seAngleRangeChange(Sender: TObject);
begin
  PieChartPieSeries1.AngleRange := seAngleRange.Value;
end;

procedure TGridChartForm.seInnerRadiusChange(Sender: TObject);
begin
  PieChartPieSeries1.InnerRadiusPercent := seInnerRadius.Value;
end;

procedure TGridChartForm.PieChartMouseDown(
  Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
  i: Integer;
begin
  i := PieChartPieSeries1.FindContainingSlice(Point(X, Y));
  if i < 0 then exit;
  ListChartSource.SetXValue(i, 0.2 - ListChartSource[i]^.X);
  PieChart.Invalidate;
end;

procedure TGridChartForm.seStartAngleChange(Sender: TObject);
begin
  PieChartPieSeries1.StartAngle := seStartAngle.Value;
end;

procedure TGridChartForm.seViewAngleChange(Sender: TObject);
begin
  PieChartPieSeries1.ViewAngle := seViewAngle.Value;
end;

procedure TGridChartForm.seLabelAngleChange(Sender: TObject);
begin
  PieChartPieSeries1.Marks.LabelFont.Orientation := seLabelAngle.Value * 10;
end;

procedure TGridChartForm.FormCreate(Sender: TObject);
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
end;

procedure TGridChartForm.FormDestroy(Sender: TObject);
begin
  fSavedDataPoints.Free;
end;

procedure TGridChartForm.FormShow(Sender: TObject);
begin
  DoFillSource;
  seWordsChange(seWords);
end;

procedure TGridChartForm.PieClipboardActionExecute(Sender: TObject);
begin
  PieChart.CopyToClipboardBitmap;
end;

procedure TGridChartForm.PieCustomizeActionExecute(Sender: TObject);
begin
  PieCustomizeAction.Checked := not PieCustomizeAction.Checked;
  PiePanel.Visible := PieCustomizeAction.Checked;
end;

procedure TGridChartForm.PieCloseActionExecute(Sender: TObject);
begin
  Close;
end;

procedure TGridChartForm.PieTitleEditChange(Sender: TObject);
begin
  PieChart.Title.Text.Text := PieTitleEdit.Text;
end;

procedure TGridChartForm.PieValuesComboChange(Sender: TObject);
begin
  DoFillSource;
end;

procedure TGridChartForm.seWordsChange(Sender: TObject);

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
    fSavedDataPoints.Assign(ListChartSource.DataPoints);
  ListChartSource.DataPoints.Assign(fSavedDataPoints);
  for v1 := 0 to ListChartSource.Count - 1 do
  begin
    with ListChartSource[v1]^ do
      Text := ExtractWords(Text, seWords.Value);
  end;
  PieChart.Invalidate;
end;

procedure TGridChartForm.DoFillSource;
begin
  ListChartSource.Clear;
  if Assigned(fOnFillSource) then
    fOnFillSource(Self);
end;

end.

