// -----------------------------------------------------------------
//    This file is part of Tranquil IT Software
//    Copyright (C) 2012 - 2021  Tranquil IT https://www.tranquil.it
//    All Rights Reserved.
// ------------------------------------------------------------------
unit demo.main;

{$i mormot.defines.inc}

interface

uses
  classes,
  sysutils,
  forms,
  controls,
  stdctrls,
  buttons,
  comctrls,
  Menus,
  Spin,
  ColorBox,
  demo.grid.frame,
  tis.ui.tageditor.core;

type
  TMainForm = class(TForm)
    Grid: TGridFrame;
    PageControl: TPageControl;
    GridTab: TTabSheet;
    TagEditorTab: TTabSheet;
    TagEditor: TTisTagEditor;
    GroupBox1: TGroupBox;
    AutoHeightCheckBox: TCheckBox;
    AllowDuplicatesCheckBox: TCheckBox;
    MaxTagsEdit: TSpinEdit;
    Label1: TLabel;
    MultiLinesCheckBox: TCheckBox;
    Label2: TLabel;
    TextColorBox: TColorBox;
    Label3: TLabel;
    BgColorBox: TColorBox;
    procedure FormCreate(Sender: TObject);
    procedure AutoHeightCheckBoxClick(Sender: TObject);
    procedure AllowDuplicatesCheckBoxChange(Sender: TObject);
    procedure MaxTagsEditEditingDone(Sender: TObject);
    procedure MultiLinesCheckBoxChange(Sender: TObject);
    procedure BgColorBoxChange(Sender: TObject);
    procedure TextColorBoxChange(Sender: TObject);
  end;

var
  MainForm: TMainForm;

implementation

{$R *.lfm}

{ TMainForm }

procedure TMainForm.FormCreate(Sender: TObject);
begin
  PageControl.ActivePage := GridTab;
  BgColorBox.Selected := TagEditor.TagBgColor;
  TextColorBox.Selected := TagEditor.TagTextColor;
end;

procedure TMainForm.AutoHeightCheckBoxClick(Sender: TObject);
begin
  TagEditor.AutoHeight := AutoHeightCheckBox.Checked;
end;

procedure TMainForm.AllowDuplicatesCheckBoxChange(Sender: TObject);
begin
  TagEditor.AllowDuplicates := AllowDuplicatesCheckBox.Checked;
end;

procedure TMainForm.MaxTagsEditEditingDone(Sender: TObject);
begin
  TagEditor.MaxTags := MaxTagsEdit.Value;
end;

procedure TMainForm.MultiLinesCheckBoxChange(Sender: TObject);
begin
  TagEditor.MultiLine := MultiLinesCheckBox.Checked;
end;

procedure TMainForm.BgColorBoxChange(Sender: TObject);
begin
  TagEditor.TagBgColor := BgColorBox.Selected;
  TextColorBox.Selected := TagEditor.TagTextColor; // text color would change
end;

procedure TMainForm.TextColorBoxChange(Sender: TObject);
begin
  TagEditor.TagTextColor := TextColorBox.Selected;
end;

end.
