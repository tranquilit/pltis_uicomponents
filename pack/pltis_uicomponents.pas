{ This file was automatically created by Lazarus. Do not edit!
  This source is only used to compile and install the package.
 }

unit pltis_uicomponents;

{$warn 5023 off : no warning about unused units}
interface

uses
  tis.core.os, tis.core.utils, tis.ui.grid.controls, tis.ui.grid.core, 
  tis.ui.grid.editor, tis.ui.lvlgraphcontrol, tis.ui.parts.buttons, 
  tis.ui.registry, tis.ui.searchedit, tis.ui.syncontroledit, 
  tis.ui.tageditor.core, tis.ui.togglebutton, tis.ui.toolbar.core, 
  tis.ui.toolbar.editor, tis.ui.grid.copyspecial, tis.ui.translator, 
  LazarusPackageIntf;

implementation

procedure Register;
begin
  RegisterUnit('tis.ui.registry', @tis.ui.registry.Register);
end;

initialization
  RegisterPackage('pltis_uicomponents', @Register);
end.
