// -----------------------------------------------------------------
//    This file is part of Tranquil IT Software
//    Copyright (C) 2012 - 2026  Tranquil IT https://www.tranquil.it
//    All Rights Reserved.
// ------------------------------------------------------------------
unit demo.searchedit.frame;

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
  Spin,
  Dialogs,
  Variants,
  mormot.core.base,
  mormot.core.variants,
  mormot.core.unicode,
  mormot.core.text,
  tis.ui.parts.buttons,
  tis.ui.searchedit;

type
  TSearchEditFrame = class(TFrame)
    Label1: TLabel;
    GroupBox2: TGroupBox;
    Label4: TLabel;
    IntervalEdit: TSpinEdit;
    Label5: TLabel;
    SearchEdit: TTisSearchEdit;
    AutoSearchCheckBox: TCheckBox;
    SearchingLabel: TLabel;
    DoneLabel: TLabel;
    SearchButtonCheckBox: TCheckBox;
    ClearButtonCheckBox: TCheckBox;
    CustomClickCheckBox: TCheckBox;
    GroupBox1: TGroupBox;
    JsonMemo: TMemo;
    JsonLoadButton: TSpeedButton;
    KeyFieldEdit: TEdit;
    DisplayFieldEdit: TEdit;
    Label2: TLabel;
    Label3: TLabel;
    KeyValueLabel: TLabel;
    SortedCheckBox: TCheckBox;
    procedure SearchButtonCheckBoxChange(Sender: TObject);
    procedure ClearButtonCheckBoxChange(Sender: TObject);
    procedure AutoSearchCheckBoxChange(Sender: TObject);
    procedure IntervalEditEditingDone(Sender: TObject);
    procedure SearchEditSearch(Sender: TObject);
    procedure SearchEditStartSearch(Sender: TObject);
    procedure CustomClickCheckBoxChange(Sender: TObject);
    procedure SearchEditButtonClick(Sender: TObject; aButton: TButtonItem);
    procedure SearchEditStopSearch(Sender: TObject);
    procedure JsonLoadButtonClick(Sender: TObject);
    procedure SearchEditChange(Sender: TObject);
    procedure SortedCheckBoxChange(Sender: TObject);
  public
    constructor Create(aOwner: TComponent); override;
  end;

implementation

{$R *.lfm}

{ TSearchEditFrame }

procedure TSearchEditFrame.SearchButtonCheckBoxChange(Sender: TObject);
begin
  SearchEdit.Buttons[0].Visible := SearchButtonCheckBox.Checked;
end;

procedure TSearchEditFrame.ClearButtonCheckBoxChange(Sender: TObject);
begin
  SearchEdit.Buttons[1].Visible := ClearButtonCheckBox.Checked;
end;

procedure TSearchEditFrame.AutoSearchCheckBoxChange(Sender: TObject);
begin
  SearchEdit.AutoSearch := AutoSearchCheckBox.Checked
end;

procedure TSearchEditFrame.IntervalEditEditingDone(Sender: TObject);
begin
  SearchEdit.SearchInterval := IntervalEdit.Value;
end;

procedure TSearchEditFrame.SearchEditStartSearch(Sender: TObject);
begin
  DoneLabel.Visible := False;
  if AutoSearchCheckBox.Checked then
    SearchingLabel.Visible := SearchEdit.Text <> '';
end;

procedure TSearchEditFrame.CustomClickCheckBoxChange(Sender: TObject);
begin
  if CustomClickCheckBox.Checked then
    SearchEdit.OnButtonClick := @SearchEditButtonClick
  else
    SearchEdit.OnButtonClick := nil;
  SearchEdit.Buttons.Invalidate; // refresh all properties and events
end;

procedure TSearchEditFrame.SearchEditButtonClick(Sender: TObject;
  aButton: TButtonItem);
begin
  if aButton.Name = 'Custom' then
  begin
    if SearchEdit.Data.IsVoid then
    begin
      SearchEdit.Text := InputBox('Search', 'Type a text', '');
      SearchEdit.Search;
    end
    else
      SearchEdit.KeyValue := InputBox('Search', 'Type a Key', '');
  end;
end;

procedure TSearchEditFrame.SearchEditStopSearch(Sender: TObject);
begin
  DoneLabel.Visible := True;
end;

procedure TSearchEditFrame.JsonLoadButtonClick(Sender: TObject);
begin
  SearchEdit.LookupKeyField := KeyFieldEdit.Text;
  SearchEdit.LookupDisplayField := DisplayFieldEdit.Text;
  SearchEdit.Data.InitJson(StringToUtf8(JsonMemo.Text), JSON_FAST_FLOAT);
  SearchEdit.LoadData;
end;

procedure TSearchEditFrame.SearchEditChange(Sender: TObject);
begin
  KeyValueLabel.Visible := not SearchEdit.Data.IsVoid;
  if KeyValueLabel.Visible then
    KeyValueLabel.Caption := 'KeyValue: ' + VarToStr(SearchEdit.KeyValue);
end;

procedure TSearchEditFrame.SortedCheckBoxChange(Sender: TObject);
begin
  SearchEdit.Sorted := SortedCheckBox.Checked;
end;

procedure TSearchEditFrame.SearchEditSearch(Sender: TObject);
begin
  SearchingLabel.Visible := SearchEdit.Text <> '';
  SearchingLabel.Caption := 'Searching for... "' + SearchEdit.Text + '"';
  DoneLabel.Visible := SearchingLabel.Visible;
end;

constructor TSearchEditFrame.Create(aOwner: TComponent);
begin
  inherited Create(aOwner);
  SearchButtonCheckBox.Checked := SearchEdit.Buttons[0].Visible;
  ClearButtonCheckBox.Checked := SearchEdit.Buttons[1].Visible;
  IntervalEdit.Value := SearchEdit.SearchInterval;
  AutoSearchCheckBox.Checked := SearchEdit.AutoSearch;
  CustomClickCheckBox.Checked := Assigned(SearchEdit.OnButtonClick);
  SortedCheckBox.Checked := SearchEdit.Sorted;
end;

end.

