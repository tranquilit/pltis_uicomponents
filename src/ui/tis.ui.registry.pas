// -----------------------------------------------------------------
//    This file is part of Tranquil IT Software
//    Copyright (C) 2012 - 2021  Tranquil IT https://www.tranquil.it
//    All Rights Reserved.
// ------------------------------------------------------------------
unit tis.ui.registry;

{$i mormot.defines.inc}

interface

uses
  classes,
  sysutils,
  lclproc,
  lcltype,
  lresources,
  lazideintf,
  componenteditors,
  typinfo,
  tis.ui.grid.core,
  tis.ui.grid.editor,
  tis.ui.searchedit,
  tis.ui.tageditor,
  tis.ui.togglebutton;

procedure Register;

implementation

procedure Register;
begin
  RegisterComponents('Tranquil IT', [TTisGrid, TTisSearchEdit, TTisTagEditor, TTisToggleButton]);
  RegisterComponentEditor(TTisGrid, TTisGridComponentEditor);
end;

initialization
  {$i tis.ui.registry.lrs}

end.

