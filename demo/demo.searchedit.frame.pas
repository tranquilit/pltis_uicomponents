// -----------------------------------------------------------------
//    This file is part of Tranquil IT Software
//    Copyright (C) 2012 - 2021  Tranquil IT https://www.tranquil.it
//    All Rights Reserved.
// ------------------------------------------------------------------
unit demo.searchedit.frame;

{$i mormot.defines.inc}

interface

uses
  Classes, SysUtils, Forms, Controls, ExtCtrls, Buttons, StdCtrls, Spin,
  Dialogs,
  mormot.core.base,
  mormot.core.variants,
  mormot.core.unicode,
  mormot.core.text,
  tis.ui.grid.core,
  tis.ui.searchedit;

type
  TSearchEditFrame = class(TFrame)
    Label1: TLabel;
    SearchButtonCheckBox: TCheckBox;
    ClearButtonCheckBox: TCheckBox;
    Timer: TTimer;
    GroupBox2: TGroupBox;
    Label4: TLabel;
    DelayEdit: TSpinEdit;
    Label5: TLabel;
    SearchEdit: TTisSearchEdit;
    Label2: TLabel;
    MinCharsEdit: TSpinEdit;
    Label6: TLabel;
    AutoSearchCheckBox: TCheckBox;
    SearchingLabel: TLabel;
    ButtonSearchClickCheckBox: TCheckBox;
    ButtonClearClickCheckBox: TCheckBox;
    DoneLabel: TLabel;
    procedure SearchButtonCheckBoxChange(Sender: TObject);
    procedure ClearButtonCheckBoxChange(Sender: TObject);
    procedure TimerTimer(Sender: TObject);
    procedure MinCharsEditChange(Sender: TObject);
    procedure AutoSearchCheckBoxChange(Sender: TObject);
    procedure ButtonSearchClickCheckBoxChange(Sender: TObject);
    procedure ButtonClearClickCheckBoxChange(Sender: TObject);
    procedure TimerStartTimer(Sender: TObject);
    procedure SearchEditChange(Sender: TObject);
    procedure SearchEditEditingDone(Sender: TObject);
    procedure SearchEditButtons0Click(Sender: TObject);
    procedure SearchEditButtons1Click(Sender: TObject);
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

procedure TSearchEditFrame.TimerTimer(Sender: TObject);
begin
  Timer.Enabled := False;
  SearchingLabel.Visible := SearchEdit.Text <> '';
  SearchingLabel.Caption := 'Searching for... "' + SearchEdit.Text + '"';
  DoneLabel.Visible := SearchingLabel.Visible;
end;

procedure TSearchEditFrame.MinCharsEditChange(Sender: TObject);
begin
  SearchEdit.Input.MinChars := MinCharsEdit.Value;
end;

procedure TSearchEditFrame.AutoSearchCheckBoxChange(Sender: TObject);
begin
  if AutoSearchCheckBox.Checked then
    SearchEdit.Input.Options := SearchEdit.Input.Options + [ioAutoSearch]
  else
    SearchEdit.Input.Options := SearchEdit.Input.Options - [ioAutoSearch];
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

procedure TSearchEditFrame.TimerStartTimer(Sender: TObject);
begin
  DoneLabel.Visible := False;
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

constructor TSearchEditFrame.Create(aOwner: TComponent);
begin
  inherited Create(aOwner);
  SearchButtonCheckBox.Checked := SearchEdit.Buttons[0].Visible;
  ClearButtonCheckBox.Checked := SearchEdit.Buttons[1].Visible;
  DelayEdit.Value := Timer.Interval;
  MinCharsEdit.Value := SearchEdit.Input.MinChars;
  AutoSearchCheckBox.Checked := ioAutoSearch in SearchEdit.Input.Options;
end;

end.

