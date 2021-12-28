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
  /// generic exception for the unit
  ETisControls = class(Exception);

  /// this class implements the base for an in-place edit control
  // - use it if you want to implement your own controls
  TTisGridControl = class(TObject)
  protected
    fInternal: TWinControl;
  public
    constructor Create; reintroduce; virtual;
    destructor Destroy; override;
    /// access to the internal (generic) WinControl instance
    function Internal: TWinControl;
    /// this set all events that the grid need to control
    procedure SetEvents(aOnKeyDown: TKeyEvent; aOnExit: TNotifyEvent); virtual;
    /// it returns the value edited by user
    function GetValue: Variant; virtual;
    /// it set the value from grid to the control
    procedure SetValue(const aValue: Variant); virtual;
  end;

  TTisGridControlClass = class of TTisGridControl;

  /// control used for all String data type
  TTisGridSearchEditControl = class(TTisGridControl)
  public
    constructor Create; override;
    function GetValue: Variant; override;
    procedure SetValue(const aValue: Variant); override;
    function Edit: TTisSearchEdit;
  end;

  /// control used for all Date data type
  TTisGridDateEditControl = class(TTisGridControl)
  public
    constructor Create; override;
    procedure SetEvents(aOnKeyDown: TKeyEvent; aOnExit: TNotifyEvent); override;
    function GetValue: Variant; override;
    procedure SetValue(const aValue: Variant); override;
    function Edit: TDateTimePicker;
  end;

  /// control used for all Time data type
  TTisGridTimeEditControl = class(TTisGridDateEditControl)
  public
    constructor Create; override;
    function GetValue: Variant; override;
    procedure SetValue(const aValue: Variant); override;
  end;

  /// control used for all DateTime data type
  TTisGridDateTimeEditControl = class(TTisGridDateEditControl)
  public
    constructor Create; override;
    function GetValue: Variant; override;
    procedure SetValue(const aValue: Variant); override;
  end;

  /// control used for all Integer data type
  TTisGridIntegerEditControl = class(TTisGridControl)
  public
    constructor Create; override;
    function GetValue: Variant; override;
    procedure SetValue(const aValue: Variant); override;
    function Edit: TSpinEdit;
  end;

  /// control used for all Float data type
  TTisGridFloatEditControl = class(TTisGridControl)
  protected
  public
    constructor Create; override;
    function GetValue: Variant; override;
    procedure SetValue(const aValue: Variant); override;
    function Edit: TFloatSpinEdit;
  end;

  /// control used for all Boolean data type
  TTisGridBooleanEditControl = class(TTisGridControl)
  public
    constructor Create; override;
    function GetValue: Variant; override;
    procedure SetValue(const aValue: Variant); override;
    function Edit: TCheckBoxThemed;
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
  result := IfThen<Variant>(fInternal.Caption = '', NULL, fInternal.Caption);
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
  Edit.AutoComplete := True;
end;

function TTisGridSearchEditControl.GetValue: Variant;
begin
  if Edit.LookupKeyField <> '' then
    result := Edit.KeyValue
  else
    result := inherited GetValue;
end;

procedure TTisGridSearchEditControl.SetValue(const aValue: Variant);
begin
  if Edit.LookupKeyField <> '' then
    Edit.KeyValue := aValue
  else
    inherited SetValue(aValue);
end;

function TTisGridSearchEditControl.Edit: TTisSearchEdit;
begin
  result := fInternal as TTisSearchEdit;;
end;

{ TTisGridDateEditControl }

function TTisGridDateEditControl.Edit: TDateTimePicker;
begin
  result := fInternal as TDateTimePicker;
end;

constructor TTisGridDateEditControl.Create;
begin
  inherited Create;
  fInternal := TDateTimePicker.Create(nil);
  Edit.Kind := dtkDate;
end;

procedure TTisGridDateEditControl.SetEvents(aOnKeyDown: TKeyEvent;
  aOnExit: TNotifyEvent);
begin
  inherited SetEvents(aOnKeyDown, aOnExit);
  // replace events to the right inheritance
  Edit.OnKeyDown := aOnKeyDown;
  Edit.OnExit := aOnExit;
end;

function TTisGridDateEditControl.GetValue: Variant;
begin
  result := Edit.Date;
end;

procedure TTisGridDateEditControl.SetValue(const aValue: Variant);
begin
  Edit.Date := VarToDateTime(aValue);
end;

{ TTisGridTimeEditControl }

constructor TTisGridTimeEditControl.Create;
begin
  inherited Create;
  Edit.Kind := dtkTime;
end;

function TTisGridTimeEditControl.GetValue: Variant;
begin
  result := Edit.Time;
end;

procedure TTisGridTimeEditControl.SetValue(const aValue: Variant);
begin
  Edit.Time := VarToDateTime(aValue);
end;

{ TTisGridDateTimeEditControl }

constructor TTisGridDateTimeEditControl.Create;
begin
  inherited Create;
  Edit.Kind := dtkDateTime;
end;

function TTisGridDateTimeEditControl.GetValue: Variant;
begin
  result := Edit.DateTime;
end;

procedure TTisGridDateTimeEditControl.SetValue(const aValue: Variant);
begin
  Edit.DateTime := VarToDateTime(aValue);
end;

{ TTisGridIntegerEditControl }

function TTisGridIntegerEditControl.Edit: TSpinEdit;
begin
  result := fInternal as TSpinEdit;
end;

constructor TTisGridIntegerEditControl.Create;
begin
  inherited Create;
  fInternal := TSpinEdit.Create(nil);
end;

function TTisGridIntegerEditControl.GetValue: Variant;
begin
  result := Edit.Value;
end;

procedure TTisGridIntegerEditControl.SetValue(const aValue: Variant);
begin
  Edit.Value := aValue;
end;

{ TTisGridFloatEditControl }

function TTisGridFloatEditControl.Edit: TFloatSpinEdit;
begin
  result := fInternal as TFloatSpinEdit;
end;

constructor TTisGridFloatEditControl.Create;
begin
  inherited Create;
  fInternal := TFloatSpinEdit.Create(nil);
end;

function TTisGridFloatEditControl.GetValue: Variant;
begin
  result := Edit.Value;
end;

procedure TTisGridFloatEditControl.SetValue(const aValue: Variant);
begin
  Edit.Value := aValue;
end;

{ TTisGridBooleanEditControl }

function TTisGridBooleanEditControl.Edit: TCheckBoxThemed;
begin
  result := fInternal as TCheckBoxThemed;
end;

constructor TTisGridBooleanEditControl.Create;
begin
  inherited Create;
  fInternal := TCheckBoxThemed.Create(nil);
  Edit.Caption := ' ';
end;

function TTisGridBooleanEditControl.GetValue: Variant;
begin
  result := Edit.Checked;
end;

procedure TTisGridBooleanEditControl.SetValue(const aValue: Variant);
begin
  Edit.Checked := aValue;
end;

end.
