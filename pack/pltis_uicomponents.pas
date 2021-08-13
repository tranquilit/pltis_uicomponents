{ This file was automatically created by Lazarus. Do not edit!
  This source is only used to compile and install the package.
 }

unit pltis_uicomponents;

{$warn 5023 off : no warning about unused units}
interface

uses
  tis.ui.registry, tis.ui.grid.core, tis.ui.grid.editor, tis.core.os, 
  tis.core.utils, tis.ui.searchedit, tis.ui.tageditor, tis.ui.togglebutton, 
  tis.ui.syncontroledit, LazarusPackageIntf;

implementation

procedure Register;
begin
  RegisterUnit('tis.ui.registry', @tis.ui.registry.Register);
end;

initialization
  RegisterPackage('pltis_uicomponents', @Register);
end.
