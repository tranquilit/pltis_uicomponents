// -----------------------------------------------------------------
//    This file is part of Tranquil IT Software
//    Copyright (C) 2012 - 2026  Tranquil IT https://www.tranquil.it
//    All Rights Reserved.
// ------------------------------------------------------------------
program demo;

{$i tis.ui.defines.inc}

uses
  interfaces, // this includes the LCL widgetset
  forms,
  tis.ui.toolbar.editor,
  demo.main,
  demo.grid.frame,
  demo.tageditor.frame,
  demo.searchedit.frame, FrameViewer09;

{$R *.res}

begin
  RequireDerivedFormResource := True;
  Application.Scaled := True;
  Application.Initialize;
  Application.CreateForm(TMainForm, MainForm);
  Application.Run;
end.

