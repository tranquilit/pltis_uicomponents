// -----------------------------------------------------------------
//    This file is part of Tranquil IT Software
//    Copyright (C) 2012 - 2026  Tranquil IT https://www.tranquil.it
//    All Rights Reserved.
// ------------------------------------------------------------------
unit tis.ui.registry;

{$i tis.ui.defines.inc}

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
  tis.ui.tageditor.core,
  tis.ui.searchedit,
  tis.ui.toolbar.core,
  tis.ui.toolbar.editor,
  tis.ui.formtimeout,
  tis.ui.togglebutton,
  tis.ui.syncontroledit,
  tis.ui.lvlgraphcontrol;

procedure Register;

implementation

procedure Register;
begin
  RegisterComponents('Tranquil IT', [
    TTisGrid, TTisGridHeaderPopupMenu, TTisTagEditor,
    TTisSearchEdit, TTisToolBar, TTisFormTimeout,
    TTisToggleButton, TTisControlSynEditor, TTisLvlGraphControl]);
  RegisterComponentEditor(TTisGrid, TTisGridComponentEditor);
  RegisterComponentEditor(TTisToolBar, TTisToolBarComponentEditor);
end;

initialization
  {$i tis.ui.registry.lrs}

end.

