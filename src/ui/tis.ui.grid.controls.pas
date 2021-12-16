// -----------------------------------------------------------------
//    This file is part of Tranquil IT Software
//    Copyright (C) 2012 - 2021  Tranquil IT https://www.tranquil.it
//    All Rights Reserved.
// ------------------------------------------------------------------
unit tis.ui.grid.controls;

{$i mormot.defines.inc}

interface

uses
  Classes,
  Variants,
  SysUtils,
  Controls,
  Menus,
  Graphics,
  StdCtrls,
  LCLIntf,
  LCLType,
  LazControls,
  Forms,
  Dialogs,
  ExtCtrls,
  Buttons,
  EditBtn,
  DateTimePicker,
  Spin,
  CheckBoxThemed,
  VirtualTrees,
  mormot.core.datetime,
  mormot.core.variants,
  mormot.core.unicode,
  tis.ui.searchedit;

type
  ETisControls = class(Exception);

  TTisGridControl = class(TObject)
  protected
    fInternal: TWinControl;
  public
    constructor Create; reintroduce; virtual;
    destructor Destroy; override;
    function Internal: TWinControl;
    procedure SetEvents(aOnKeyDown: TKeyEvent; aOnExit: TNotifyEvent); virtual;
    function GetValue: Variant; virtual;
    procedure SetValue(const aValue: Variant); virtual;
  end;

  TTisGridControlClass = class of TTisGridControl;

  TTisGridSearchEditControl = class(TTisGridControl)
  public
    constructor Create; override;
  end;

  TTisGridDateEditControl = class(TTisGridControl)
  protected
    function Internal: TDateEdit;
  public
    constructor Create; override;
    procedure SetEvents(aOnKeyDown: TKeyEvent; aOnExit: TNotifyEvent); override;
    function GetValue: Variant; override;
    procedure SetValue(const aValue: Variant); override;
  end;

  TTisGridDateTimeEditControl = class(TTisGridControl)
  protected
    function Internal: TDateTimePicker;
  public
    constructor Create; override;
    function GetValue: Variant; override;
    procedure SetValue(const aValue: Variant); override;
  end;

  TTisGridSpinEditControl = class(TTisGridControl)
  protected
    function Internal: TSpinEdit;
  public
    constructor Create; override;
    function GetValue: Variant; override;
    procedure SetValue(const aValue: Variant); override;
  end;

  TTisGridSpinFloatEditControl = class(TTisGridControl)
  protected
    function Internal: TFloatSpinEdit;
  public
    constructor Create; override;
    function GetValue: Variant; override;
    procedure SetValue(const aValue: Variant); override;
  end;

  TTisGridBooleanEditControl = class(TTisGridControl)
  protected
    function Internal: TCheckBoxThemed;
  public
    constructor Create; override;
    function GetValue: Variant; override;
    procedure SetValue(const aValue: Variant); override;
  end;

implementation

{ TTisGridControl }

constructor TTisGridControl.Create;
begin
  inherited Create;
end;

destructor TTisGridControl.Destroy;
begin
  Application.ReleaseComponent(fInternal);
  inherited Destroy;
end;

function TTisGridControl.Internal: TWinControl;
begin
  result := fInternal;
end;

procedure TTisGridControl.SetEvents(aOnKeyDown: TKeyEvent; aOnExit: TNotifyEvent);
begin
  fInternal.OnKeyDown := aOnKeyDown;
  fInternal.OnExit := aOnExit;
end;

function TTisGridControl.GetValue: Variant;
begin
  result := fInternal.Caption;
end;

procedure TTisGridControl.SetValue(const aValue: Variant);
begin
  fInternal.Caption := VarToStr(aValue);
end;

{ TTisGridSearchEditControl }

constructor TTisGridSearchEditControl.Create;
begin
  inherited Create;
  fInternal := TTisSearchEdit.Create(nil);
end;

{ TTisGridDateEditControl }

function TTisGridDateEditControl.Internal: TDateEdit;
begin
  result := fInternal as TDateEdit;
end;

constructor TTisGridDateEditControl.Create;
begin
  inherited Create;
  fInternal := TDateEdit.Create(nil);
end;

procedure TTisGridDateEditControl.SetEvents(aOnKeyDown: TKeyEvent;
  aOnExit: TNotifyEvent);
begin
  Internal.OnKeyDown := aOnKeyDown;
  Internal.OnExit := aOnExit;
end;

function TTisGridDateEditControl.GetValue: Variant;
begin
  result := Internal.Date;
end;

procedure TTisGridDateEditControl.SetValue(const aValue: Variant);
begin
  Internal.Date := VarToDateTime(aValue);
end;

{ TTisGridDateTimeEditControl }

function TTisGridDateTimeEditControl.Internal: TDateTimePicker;
begin
  result := fInternal as TDateTimePicker;
end;

constructor TTisGridDateTimeEditControl.Create;
begin
  inherited Create;
  fInternal := TDateTimePicker.Create(nil);
  Internal.Kind := dtkDateTime;
end;

function TTisGridDateTimeEditControl.GetValue: Variant;
begin
  result := Internal.DateTime;
end;

procedure TTisGridDateTimeEditControl.SetValue(const aValue: Variant);
begin
  Internal.DateTime := VarToDateTime(aValue);
end;

{ TTisGridSpinEditControl }

function TTisGridSpinEditControl.Internal: TSpinEdit;
begin
  result := fInternal as TSpinEdit;
end;

constructor TTisGridSpinEditControl.Create;
begin
  inherited Create;
  fInternal := TSpinEdit.Create(nil);
end;

function TTisGridSpinEditControl.GetValue: Variant;
begin
  result := Internal.Value;
end;

procedure TTisGridSpinEditControl.SetValue(const aValue: Variant);
begin
  Internal.Value := aValue;
end;

{ TTisGridSpinFloatEditControl }

function TTisGridSpinFloatEditControl.Internal: TFloatSpinEdit;
begin
  result := fInternal as TFloatSpinEdit;
end;

constructor TTisGridSpinFloatEditControl.Create;
begin
  inherited Create;
  fInternal := TFloatSpinEdit.Create(nil);
end;

function TTisGridSpinFloatEditControl.GetValue: Variant;
begin
  result := Internal.Value;
end;

procedure TTisGridSpinFloatEditControl.SetValue(const aValue: Variant);
begin
  Internal.Value := aValue;
end;

{ TTisGridBooleanEditControl }

function TTisGridBooleanEditControl.Internal: TCheckBoxThemed;
begin
  result := fInternal as TCheckBoxThemed;
end;

constructor TTisGridBooleanEditControl.Create;
begin
  inherited Create;
  fInternal := TCheckBoxThemed.Create(nil);
  Internal.Caption := ' ';
end;

function TTisGridBooleanEditControl.GetValue: Variant;
begin
  result := Internal.Checked;
end;

procedure TTisGridBooleanEditControl.SetValue(const aValue: Variant);
begin
  Internal.Checked := aValue;
end;

end.
