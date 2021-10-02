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
  demo.grid.frame,
  demo.tageditor.frame,
  demo.searchedit.frame;

type
  TMainForm = class(TForm)
    PageControl: TPageControl;
    GridTab: TTabSheet;
    TagEditorTab: TTabSheet;
    freGrid: TGridFrame;
    freTagEditor: TTagEditorFrame;
    SearchEditTab: TTabSheet;
    freSearchEdit: TSearchEditFrame;
    procedure FormCreate(Sender: TObject);
  end;

var
  MainForm: TMainForm;

implementation

{$R *.lfm}

{ TMainForm }

procedure TMainForm.FormCreate(Sender: TObject);
begin
  PageControl.ActivePage := GridTab;
  with freTagEditor do
  begin
    BgColorBox.Selected := TagEditor.TagBgColor;
    TextColorBox.Selected := TagEditor.TagTextColor;
  end;
end;

end.
