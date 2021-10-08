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
    GroupBox1: TGroupBox;
    DataMemo: TMemo;
    Grid: TTisGrid;
    SearchButtonCheckBox: TCheckBox;
    ClearButtonCheckBox: TCheckBox;
    Timer: TTimer;
    GroupBox2: TGroupBox;
    Label4: TLabel;
    MaxTagsEdit: TSpinEdit;
    Label5: TLabel;
    UpdateButton: TSpeedButton;
    SearchEdit: TTisSearchEdit;
    Label2: TLabel;
    MinCharsEdit: TSpinEdit;
    Label6: TLabel;
    procedure SearchButtonCheckBoxChange(Sender: TObject);
    procedure ClearButtonCheckBoxChange(Sender: TObject);
    procedure TimerTimer(Sender: TObject);
    procedure UpdateButtonClick(Sender: TObject);
    procedure SearchEditSearchButtonClick(Sender: TObject);
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
  //
end;

procedure TSearchEditFrame.UpdateButtonClick(Sender: TObject);
var
  i: Integer;
  d: TDocVariantData;
  r: PVariant;
  res: RawUtf8;
begin
  res := '';
  for i := 0 to DataMemo.Lines.Count -1 do
  begin
    if i > 0 then
      res := res + ',';
    res := res + '{"result":"' + StringToUtf8(DataMemo.Lines[i]) + '"}';
  end;
  d.InitJson('['+res+']', JSON_FAST_FLOAT);
  Grid.Clear;
  for r in d.Items do
    Grid.Data.AddItem(r^);
  Grid.LoadData;
end;

procedure TSearchEditFrame.SearchEditSearchButtonClick(Sender: TObject);
begin
  SearchEdit.Text := InputBox('Search', 'Type a text', '');
end;

constructor TSearchEditFrame.Create(aOwner: TComponent);
begin
  inherited Create(aOwner);
  SearchButtonCheckBox.Checked := boShowSearchButton in SearchEdit.Options;
  ClearButtonCheckBox.Checked := boShowClearButton in SearchEdit.Options;
  MinCharsEdit.Value := SearchEdit.Input.MinChars;
end;

end.

