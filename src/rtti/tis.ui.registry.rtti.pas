// -----------------------------------------------------------------
//    This file is part of Tranquil IT Software
//    Copyright (C) 2012 - 2026  Tranquil IT https://www.tranquil.it
//    All Rights Reserved.
// ------------------------------------------------------------------
unit tis.ui.registry.rtti;

{$mode objfpc}{$H+}
{$modeswitch ADVANCEDRECORDS}
{$modeswitch typehelpers}
interface

uses
  classes,
  sysutils,
  lclproc,
  lcltype,
  lresources,
  lazideintf,
  typinfo,
  tis.ui.tageditor.rtti;

procedure Register;

implementation

procedure Register;
begin
  RegisterComponents('Tranquil IT RTTI', [
    TTisTagEditorRtti]);
end;

initialization
  {$i tis.ui.registry.rtti.lrs}

end.

