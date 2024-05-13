// -----------------------------------------------------------------
//    This file is part of Tranquil IT Software
//    Copyright (C) 2012 - 2024  Tranquil IT https://www.tranquil.it
//    All Rights Reserved.
// ------------------------------------------------------------------
unit tis.core.utils;

{$i tis.ui.defines.inc}

interface

uses
  Classes,
  SysUtils,
  Controls,
  Variants,
  ComCtrls,
  Forms,
  mormot.core.base,
  mormot.core.unicode,
  mormot.core.text;

type
  TWidgetHelper = class helper for TWinControl
  public
    procedure SetFocusSafe;
  end;

implementation

{ TWidgetHelper }

procedure TWidgetHelper.SetFocusSafe;
var
  vControl: TWinControl;
begin
  try
    if Visible and Enabled then
    begin
      vControl := Parent;
      if vControl.InheritsFrom(TFrame) then
        vControl.SetFocusSafe;
      while Assigned(vControl) and vControl.Enabled do
      begin
        if vControl.InheritsFrom(TTabSheet) then
          TPageControl(vControl.Parent).ActivePage := TTabSheet(vControl);
        vControl := vControl.Parent;
      end;
      if CanFocus then
        SetFocus;
    end;
  except
  end;
end;

end.
