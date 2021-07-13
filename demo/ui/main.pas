// -----------------------------------------------------------------
//    This file is part of Tranquil IT Software
//    Copyright (C) 2012 - 2021  Tranquil IT https://www.tranquil.it
//    All Rights Reserved.
// ------------------------------------------------------------------
unit main;

{$i mormot.defines.inc}

interface

uses
  classes,
  sysutils,
  forms,
  controls,
  graphics,
  dialogs,
  extctrls,
  stdctrls,
  buttons,
  comctrls,
  synedit,
  virtualtrees,
  mormot.core.text,
  mormot.core.unicode,
  tis.core.os,
  tis.ui.grid.core;

type
  TMainForm = class(TForm)
    ClipboardLabel1: TLabel;
    Label1: TLabel;
    ClipboardLabel: TLabel;
    GridDataLabel: TLabel;
    MainPageControl: TPageControl;
    Panel1: TPanel;
    Panel4: TPanel;
    SOvsTisTab: TTabSheet;
    OutputEdit: TSynEdit;
    Grid: TTisGrid;
    Splitter: TSplitter;
    GridCustomButton: TSpeedButton;
    procedure ClipboardLabel1Click(Sender: TObject);
    procedure ClipboardLabelClick(Sender: TObject);
    procedure GridCustomButtonClick(Sender: TObject);
    procedure GridDataLabelClick(Sender: TObject);
  end;

var
  MainForm: TMainForm;

implementation

{$R *.lfm}

{ TMainForm }

procedure TMainForm.GridCustomButtonClick(Sender: TObject);
begin
  Grid.Customize;
end;

procedure TMainForm.ClipboardLabel1Click(Sender: TObject);
begin
  OutputEdit.Lines.Clear;
end;

procedure TMainForm.ClipboardLabelClick(Sender: TObject);
var
  c: TClipboardAdapter;
begin
  OutputEdit.Lines.Text := c.AsString;
end;

procedure TMainForm.GridDataLabelClick(Sender: TObject);
begin
  OutputEdit.Lines.Text := Utf8ToString(Grid.Data.ToJson('', '', jsonHumanReadable));
end;

end.
