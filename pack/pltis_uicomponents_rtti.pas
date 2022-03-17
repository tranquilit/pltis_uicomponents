{ This file was automatically created by Lazarus. Do not edit!
  This source is only used to compile and install the package.
 }

unit pltis_uicomponents_rtti;

{$warn 5023 off : no warning about unused units}
interface

uses
  tis.ui.registry.rtti, tis.ui.tageditor.rtti, LazarusPackageIntf;

implementation

procedure Register;
begin
  RegisterUnit('tis.ui.registry.rtti', @tis.ui.registry.rtti.Register);
end;

initialization
  RegisterPackage('pltis_uicomponents_rtti', @Register);
end.
