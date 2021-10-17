// -----------------------------------------------------------------
//    This file is part of Tranquil IT Software
//    Copyright (C) 2012 - 2021  Tranquil IT https://www.tranquil.it
//    All Rights Reserved.
// ------------------------------------------------------------------
unit demo.searchedit.frame;

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
  Spin,
  Dialogs,
  mormot.core.base,
  mormot.core.variants,
  mormot.core.unicode,
  mormot.core.text,
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
    GroupBox1: TGroupBox;
    SearchButtonCheckBox: TCheckBox;
    ButtonSearchClickCheckBox: TCheckBox;
    GroupBox3: TGroupBox;
    ClearButtonCheckBox: TCheckBox;
    ButtonClearClickCheckBox: TCheckBox;
    procedure SearchButtonCheckBoxChange(Sender: TObject);
    procedure ClearButtonCheckBoxChange(Sender: TObject);
    procedure AutoSearchCheckBoxChange(Sender: TObject);
    procedure ButtonSearchClickCheckBoxChange(Sender: TObject);
    procedure ButtonClearClickCheckBoxChange(Sender: TObject);
    procedure SearchEditChange(Sender: TObject);
    procedure SearchEditEditingDone(Sender: TObject);
    procedure SearchEditButtons0Click(Sender: TObject);
    procedure SearchEditButtons1Click(Sender: TObject);
    procedure IntervalEditEditingDone(Sender: TObject);
    procedure SearchEditSearch(Sender: TObject);
    procedure SearchEditStartSearch(Sender: TObject);
  private

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

procedure TSearchEditFrame.ButtonSearchClickCheckBoxChange(Sender: TObject);
begin
  with SearchEdit.Buttons[0] do
    if ButtonSearchClickCheckBox.Checked then
      OnClick := SearchEditButtons0Click
    else
      OnClick := nil;
end;

procedure TSearchEditFrame.ButtonClearClickCheckBoxChange(Sender: TObject);
begin
  with SearchEdit.Buttons[1] do
    if ButtonClearClickCheckBox.Checked then
      OnClick := SearchEditButtons1Click
    else
      OnClick := nil;
end;

procedure TSearchEditFrame.SearchEditChange(Sender: TObject);
begin
  if AutoSearchCheckBox.Checked then
    SearchingLabel.Visible := SearchEdit.Text <> '';
end;

procedure TSearchEditFrame.SearchEditEditingDone(Sender: TObject);
begin
  SearchingLabel.Visible := SearchEdit.Text <> '';
end;

procedure TSearchEditFrame.SearchEditButtons0Click(Sender: TObject);
begin
  SearchEdit.Text := InputBox('Search', 'Type a text', '');
end;

procedure TSearchEditFrame.SearchEditButtons1Click(Sender: TObject);
begin
  SearchEdit.Text := InputBox('After clean', 'Type a default text', '');
end;

procedure TSearchEditFrame.IntervalEditEditingDone(Sender: TObject);
begin
  SearchEdit.SearchInterval := IntervalEdit.Value;
end;

procedure TSearchEditFrame.SearchEditStartSearch(Sender: TObject);
begin
  DoneLabel.Visible := False;
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
end;

end.

