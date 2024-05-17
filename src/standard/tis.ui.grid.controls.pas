// -----------------------------------------------------------------
//    This file is part of Tranquil IT Software
//    Copyright (C) 2012 - 2024  Tranquil IT https://www.tranquil.it
//    All Rights Reserved.
// ------------------------------------------------------------------
unit tis.ui.grid.controls;

{.$i tis.ui.defines.inc}
{$i mormot.defines.inc}

interface

uses
  Classes,
  Variants,
  SysUtils,
  Controls,
  Menus,
  StdCtrls,
  LCLIntf,
  LCLType,
  Forms,
  Dialogs,
  Buttons,
  DateTimePicker,
  Spin,
  CheckBoxThemed,
  mormot.core.base,
  mormot.core.datetime,
  mormot.core.variants,
  mormot.core.unicode,
  mormot.core.text,
  tis.ui.searchedit;

type
  /// generic exception for the unit
  ETisControls = class(Exception);

  /// this class implements the base for an in-place edit control
  // - use it if you want to implement your own controls
  TTisGridControl = class(TObject)
  private
    fReadOnly: Boolean;
  protected
    fInternal: TWinControl;
    fOwner: TWinControl;
    procedure SetReadOnly(aValue: Boolean); virtual;
  public
    constructor Create(aOwner: TWinControl); reintroduce; virtual;
    destructor Destroy; override;
    /// access to the internal (generic) WinControl instance
    function Internal: TWinControl;
    /// set OnKeyDown event allowing grid to be in control
    procedure SetOnKeyDown(aEvent: TKeyEvent); virtual;
    /// set OnExit event allowing grid to be in control
    procedure SetOnExit(aEvent: TNotifyEvent); virtual;
    /// it returns the value edited by user
    function GetValue: Variant; virtual;
    /// it set the value from grid to the control
    procedure SetValue(const aValue: Variant); virtual;
    property ReadOnly: Boolean read fReadOnly write SetReadOnly;
  end;

  TTisGridControlClass = class of TTisGridControl;

  /// control used for all String data type
  TTisGridEditControl = class(TTisGridControl)
  protected
    procedure SetReadOnly(aValue: Boolean); override;
  public
    constructor Create(aOwner: TWinControl); override;
    function GetValue: Variant; override;
    procedure SetValue(const aValue: Variant); override;
    function Edit: TEdit;
  end;

  TTisGridPasswordEditControl = class(TTisGridEditControl)
  public
    constructor Create(aOwner: TWinControl); override;
  end;

  /// control used for all Date data type
  TTisGridDateEditControl = class(TTisGridControl)
  protected
    procedure SetReadOnly(aValue: Boolean); override;
  public
    constructor Create(aOwner: TWinControl); override;
    procedure SetOnKeyDown(aEvent: TKeyEvent); override;
    function GetValue: Variant; override;
    procedure SetValue(const aValue: Variant); override;
    function Edit: TDateTimePicker;
  end;

  /// control used for all Time data type
  TTisGridTimeEditControl = class(TTisGridDateEditControl)
  public
    constructor Create(aOwner: TWinControl); override;
    function GetValue: Variant; override;
    procedure SetValue(const aValue: Variant); override;
  end;

  /// control used for all DateTime data type
  TTisGridDateTimeEditControl = class(TTisGridDateEditControl)
  public
    constructor Create(aOwner: TWinControl); override;
    function GetValue: Variant; override;
    procedure SetValue(const aValue: Variant); override;
  end;

  /// control used for all Integer data type
  TTisGridIntegerEditControl = class(TTisGridControl)
  protected
    procedure SetReadOnly(aValue: Boolean); override;
  public
    constructor Create(aOwner: TWinControl); override;
    function GetValue: Variant; override;
    procedure SetValue(const aValue: Variant); override;
    function Edit: TSpinEdit;
  end;

  /// control used for all Float data type
  TTisGridFloatEditControl = class(TTisGridControl)
  protected
    procedure SetReadOnly(aValue: Boolean); override;
  public
    constructor Create(aOwner: TWinControl); override;
    function GetValue: Variant; override;
    procedure SetValue(const aValue: Variant); override;
    function Edit: TFloatSpinEdit;
  end;

  /// control used for all Boolean data type
  TTisGridBooleanEditControl = class(TTisGridControl)
  protected
    procedure SetReadOnly(aValue: Boolean); override;
  public
    constructor Create(aOwner: TWinControl); override;
    function GetValue: Variant; override;
    procedure SetValue(const aValue: Variant); override;
    function Edit: TCheckBoxThemed;
  end;

  /// control used for all Memo data type
  TTisGridMemoControl = class(TTisGridControl)
  protected
    procedure SetReadOnly(aValue: Boolean); override;
  public
    constructor Create(aOwner: TWinControl); override;
    function GetValue: Variant; override;
    procedure SetValue(const aValue: Variant); override;
    function Edit: TMemo;
  end;

  /// control used for Integer data type as a Lookup editor
  // - it is used by default into Grid.OnEditorLookup event
  // - if you want to use a simple ComboBox with a simple string list, 
  // just do not set LookupKeyField/LookupDisplayField, adding strings directly 
  // into Items property, on OnEditorLookup event (syncronous mode)
  // - otherwise, if you want to use a real lookup, you should set LookupKeyField/LookupDisplayField,
  // on the same event
  // - also, you can add dynamic strings into Items using OnSearch event (asynchronous mode)
  TTisGridSearchEditControl = class(TTisGridControl)
  protected
    procedure SetReadOnly(aValue: Boolean); override;
  public
    constructor Create(aOwner: TWinControl); override;
    function GetValue: Variant; override;
    procedure SetValue(const aValue: Variant); override;
    function Edit: TTisSearchEdit;
  end;

implementation

{ TTisGridControl }

procedure TTisGridControl.SetReadOnly(aValue: Boolean);
begin
  if fReadOnly = aValue then
    exit;
  fReadOnly := aValue;
end;

constructor TTisGridControl.Create(aOwner: TWinControl);
begin
  inherited Create;
  fOwner := aOwner;
end;

destructor TTisGridControl.Destroy;
begin
  // finternal.destroy is asynchronous. So be sure we don't refer anymore to this TTisGridControl in events.
  fInternal.OnExit := Nil;
  fInternal.OnKeyDown := Nil;
  Application.ReleaseComponent(fInternal);
  inherited Destroy;
end;

function TTisGridControl.Internal: TWinControl;
begin
  result := fInternal;
end;

procedure TTisGridControl.SetOnKeyDown(aEvent: TKeyEvent);
begin
  fInternal.OnKeyDown := aEvent;
end;

procedure TTisGridControl.SetOnExit(aEvent: TNotifyEvent);
begin
  fInternal.OnExit := aEvent;
end;

function TTisGridControl.GetValue: Variant;
begin
  result := IfThen<Variant>(Trim(fInternal.Caption) = '', NULL, Trim(fInternal.Caption));
end;

procedure TTisGridControl.SetValue(const aValue: Variant);
begin
  fInternal.Caption := VarToStr(aValue);
end;

{ TTisGridEditControl }

procedure TTisGridEditControl.SetReadOnly(aValue: Boolean);
begin
  inherited SetReadOnly(aValue);
  Edit.ReadOnly := aValue;
end;

constructor TTisGridEditControl.Create(aOwner: TWinControl);
begin
  inherited Create(aOwner);
  fInternal := TEdit.Create(nil);
  Edit.Clear;
end;

function TTisGridEditControl.GetValue: Variant;
begin
  if Edit.Text = '' then
    result := NULL
  else
    result := Trim(Edit.Text);
end;

procedure TTisGridEditControl.SetValue(const aValue: Variant);
begin
  if VarIsBool(aValue) then
    Edit.Text := LowerCase(aValue)
  else
    Edit.Text := VarToStr(aValue);
end;

function TTisGridEditControl.Edit: TEdit;
begin
  result := fInternal as TEdit;
end;

{ TTisGridPasswordEditControl }

constructor TTisGridPasswordEditControl.Create;
begin
  inherited Create(aOwner);
  Edit.PasswordChar := '*';
end;

{ TTisGridDateEditControl }

procedure TTisGridDateEditControl.SetReadOnly(aValue: Boolean);
begin
  inherited SetReadOnly(aValue);
  Edit.ReadOnly := aValue;
end;

constructor TTisGridDateEditControl.Create;
begin
  inherited Create(aOwner);
  fInternal := TDateTimePicker.Create(nil);
  Edit.Kind := dtkDate;
end;

procedure TTisGridDateEditControl.SetOnKeyDown(aEvent: TKeyEvent);
begin
  inherited SetOnKeyDown(aEvent);
  Edit.OnKeyDown := aEvent; // replace event to the right inheritance
end;

function TTisGridDateEditControl.GetValue: Variant;
begin
  if Edit.DateIsNull then
    result := NULL
  else
    result := Edit.Date;
end;

procedure TTisGridDateEditControl.SetValue(const aValue: Variant);
begin
  Edit.Date := Iso8601ToDateTime(VarToStrDef(aValue, ''));
end;

function TTisGridDateEditControl.Edit: TDateTimePicker;
begin
  result := fInternal as TDateTimePicker;
end;

{ TTisGridTimeEditControl }

constructor TTisGridTimeEditControl.Create;
begin
  inherited Create(aOwner);
  Edit.Kind := dtkTime;
end;

function TTisGridTimeEditControl.GetValue: Variant;
begin
  if Edit.DateIsNull then
    result := NULL
  else
    result := Edit.Time;
end;

procedure TTisGridTimeEditControl.SetValue(const aValue: Variant);
begin
  Edit.Time := Iso8601ToDateTime(VarToStrDef(aValue, ''));
end;

{ TTisGridDateTimeEditControl }

constructor TTisGridDateTimeEditControl.Create;
begin
  inherited Create(aOwner);
  Edit.Kind := dtkDateTime;
end;

function TTisGridDateTimeEditControl.GetValue: Variant;
begin
  if Edit.DateIsNull then
    result := NULL
  else
    result := Edit.DateTime;
end;

procedure TTisGridDateTimeEditControl.SetValue(const aValue: Variant);
begin
  Edit.DateTime := Iso8601ToDateTime(VarToStrDef(aValue, ''));
end;

{ TTisGridIntegerEditControl }

function TTisGridIntegerEditControl.Edit: TSpinEdit;
begin
  result := fInternal as TSpinEdit;
end;

procedure TTisGridIntegerEditControl.SetReadOnly(aValue: Boolean);
begin
  inherited SetReadOnly(aValue);
  Edit.ReadOnly := aValue;
end;

constructor TTisGridIntegerEditControl.Create;
begin
  inherited Create(aOwner);
  fInternal := TSpinEdit.Create(nil);
end;

function TTisGridIntegerEditControl.GetValue: Variant;
begin
  if Edit.Text = '' then
    result := NULL
  else
    result := Edit.Value;
end;

procedure TTisGridIntegerEditControl.SetValue(const aValue: Variant);
var
  vValue: Int64;
begin
  vValue := 0;
  VariantToInt64(aValue, vValue);
  Edit.Value := vValue;
end;

{ TTisGridFloatEditControl }

function TTisGridFloatEditControl.Edit: TFloatSpinEdit;
begin
  result := fInternal as TFloatSpinEdit;
end;

procedure TTisGridFloatEditControl.SetReadOnly(aValue: Boolean);
begin
  inherited SetReadOnly(aValue);
  Edit.ReadOnly := aValue;
end;

constructor TTisGridFloatEditControl.Create;
begin
  inherited Create(aOwner);
  fInternal := TFloatSpinEdit.Create(nil);
end;

function TTisGridFloatEditControl.GetValue: Variant;
begin
  if Edit.Text = '' then
    result := NULL
  else
    result := Edit.Value;
end;

procedure TTisGridFloatEditControl.SetValue(const aValue: Variant);
begin
  Edit.Value := aValue;
end;

{ TTisGridBooleanEditControl }

procedure TTisGridBooleanEditControl.SetReadOnly(aValue: Boolean);
begin
  inherited SetReadOnly(aValue);
  Edit.Enabled := not aValue;
end;

constructor TTisGridBooleanEditControl.Create;
begin
  inherited Create(aOwner);
  fInternal := TCheckBoxThemed.Create(nil);
  Edit.Caption := ' ';
end;

function TTisGridBooleanEditControl.GetValue: Variant;
begin
  result := Edit.Checked;
end;

procedure TTisGridBooleanEditControl.SetValue(const aValue: Variant);
var
  vValue: Boolean;
begin
  vValue := False;
  VariantToBoolean(aValue, vValue);
  Edit.Checked := vValue;
end;

function TTisGridBooleanEditControl.Edit: TCheckBoxThemed;
begin
  result := fInternal as TCheckBoxThemed;
end;

{ TTisGridMemoControl }

procedure TTisGridMemoControl.SetReadOnly(aValue: Boolean);
begin
  inherited SetReadOnly(aValue);
  Edit.ReadOnly := aValue;
end;

constructor TTisGridMemoControl.Create;
begin
  inherited Create(aOwner);
  fInternal := TMemo.Create(nil);
  Edit.Clear;
end;

function TTisGridMemoControl.GetValue: Variant;
begin
  if Edit.Text = '' then
    result := NULL
  else
    result := Edit.Text;
end;

procedure TTisGridMemoControl.SetValue(const aValue: Variant);
begin
  Edit.Text := VarToStr(aValue);
end;

function TTisGridMemoControl.Edit: TMemo;
begin
  result := fInternal as TMemo;
end;

{ TTisGridSearchEditControl }

procedure TTisGridSearchEditControl.SetReadOnly(aValue: Boolean);
begin
  inherited SetReadOnly(aValue);
  Edit.ReadOnly := aValue;
end;

constructor TTisGridSearchEditControl.Create;
begin
  inherited Create(aOwner);
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
  result := fInternal as TTisSearchEdit;
end;

end.
