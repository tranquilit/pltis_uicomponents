// -----------------------------------------------------------------
//    This file is part of Tranquil IT Software
//    Copyright (C) 2012 - 2022  Tranquil IT https://www.tranquil.it
//    All Rights Reserved.
// ------------------------------------------------------------------
program demo;

{$i mormot.defines.inc}

uses
  interfaces, // this includes the LCL widgetset
  forms,
  demo.main,
  demo.grid.frame,
  demo.tageditor.frame,
  demo.searchedit.frame;

{$R *.res}

begin
  RequireDerivedFormResource := True;
  Application.Scaled := True;
  Application.Initialize;
  Application.CreateForm(TMainForm, MainForm);
  Application.Run;
end.

