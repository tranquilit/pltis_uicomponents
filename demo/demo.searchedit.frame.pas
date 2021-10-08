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
    procedure SearchButtonCheckBoxChange(Sender: TObject);
    procedure ClearButtonCheckBoxChange(Sender: TObject);
    procedure TimerTimer(Sender: TObject);
    procedure SearchEditSearchButtonClick(Sender: TObject);
    procedure MinCharsEditChange(Sender: TObject);
    procedure AutoSearchCheckBoxChange(Sender: TObject);
    procedure SearchEditChange(Sender: TObject);
  private

  public
    constructor Create(aOwner: TComponent); override;
  end;

implementation

{$R *.lfm}

{ TSearchEditFrame }

procedure TSearchEditFrame.SearchButtonCheckBoxChange(Sender: TObject);
begin
  if SearchButtonCheckBox.Checked then
    SearchEdit.Options := SearchEdit.Options + [boShowSearchButton]
  else
    SearchEdit.Options := SearchEdit.Options - [boShowSearchButton];
end;

procedure TSearchEditFrame.ClearButtonCheckBoxChange(Sender: TObject);
begin
  if ClearButtonCheckBox.Checked then
    SearchEdit.Options := SearchEdit.Options + [boShowClearButton]
  else
    SearchEdit.Options := SearchEdit.Options - [boShowClearButton];
end;

procedure TSearchEditFrame.TimerTimer(Sender: TObject);
begin
  Timer.Enabled := False;
  SearchingLabel.Visible := True;
  SearchingLabel.Caption := 'Searching for... "' + SearchEdit.Text + '"';
end;

procedure TSearchEditFrame.SearchEditSearchButtonClick(Sender: TObject);
begin
  SearchEdit.Text := InputBox('Search', 'Type a text', '');
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

procedure TSearchEditFrame.SearchEditChange(Sender: TObject);
begin
  SearchingLabel.Visible := SearchEdit.Text <> '';
end;

constructor TSearchEditFrame.Create(aOwner: TComponent);
begin
  inherited Create(aOwner);
  SearchButtonCheckBox.Checked := boShowSearchButton in SearchEdit.Options;
  ClearButtonCheckBox.Checked := boShowClearButton in SearchEdit.Options;
  DelayEdit.Value := Timer.Interval;
  MinCharsEdit.Value := SearchEdit.Input.MinChars;
  AutoSearchCheckBox.Checked := ioAutoSearch in SearchEdit.Input.Options;
end;

end.

