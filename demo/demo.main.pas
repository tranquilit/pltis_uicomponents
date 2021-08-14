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
  tis.ui.tageditor;

type
  TMainForm = class(TForm)
    Grid: TGridFrame;
    PageControl: TPageControl;
    GridTab: TTabSheet;
    TagEditorTab: TTabSheet;
    TisTagEditor1: TTisTagEditor;
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
end;

end.
