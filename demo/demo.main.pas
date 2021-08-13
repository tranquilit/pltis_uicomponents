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
  demo.gridframe;

type
  TMainForm = class(TForm)
    Grid: TGridFrame;
    MainPageControl: TPageControl;
    GridTab: TTabSheet;
    TagEditTab: TTabSheet;
  end;

var
  MainForm: TMainForm;

implementation

{$R *.lfm}

{ TMainForm }

end.
