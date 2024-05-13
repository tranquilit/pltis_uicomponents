// -----------------------------------------------------------------
//    This file is part of Tranquil IT Software
//    Copyright (C) 2012 - 2024  Tranquil IT https://www.tranquil.it
//    All Rights Reserved.
// ------------------------------------------------------------------
unit tis.ui.formtimeout;

{$i tis.ui.defines.inc}

interface

uses
  Classes,
  SysUtils,
  Forms,
  Controls,
  StdCtrls,
  ExtCtrls,
  Variants,
  tis.ui.resourcestrings;

type
  TTisFormTimeout = class;

  TTisCountingEvent = procedure(aSender: TTisFormTimeout; aCounting: Integer) of object;

  TTisTimeoutEvent = procedure(aSender: TTisFormTimeout; var aHandled: Boolean) of object;

  TTisFormTimeout = class(TComponent)
  private
    fTimeout: Integer;
    fUpdateCaption: Boolean;
    fCounting: Integer;
    fOnCounting: TTisCountingEvent;
    fOnTimeout: TTisTimeoutEvent;
    function GetEnabled: Boolean;
    procedure SetEnabled(aValue: Boolean);
    procedure SetTimeout(aValue: Integer);
    function OwnerAsForm: TCustomForm;
  protected const
    DefaultEnabled = True;
    DefaultTimeout = 30;
    DefaultUpdateCaption = True;
  protected
    fTimer: TTimer;
    fSavedCaption: TCaption;
    procedure DoTimer({%H-}aSender: TObject); virtual;
    procedure DoCounting; virtual;
    procedure DoTimeout(var aHandled: Boolean); virtual;
  public
    constructor Create(aOwner: TComponent); override;
    property Counting: Integer read fCounting;
  published
    property Enabled: Boolean read GetEnabled write SetEnabled default DefaultEnabled;
    property Timeout: Integer read fTimeout write SetTimeout default DefaultTimeout;
    property UpdateCaption: Boolean read fUpdateCaption write fUpdateCaption default DefaultUpdateCaption;
    property OnCounting: TTisCountingEvent read fOnCounting write fOnCounting;
    property OnTimeout: TTisTimeoutEvent read fOnTimeout write fOnTimeout;
  end;

implementation

{ TTisFormTimeout }

function TTisFormTimeout.GetEnabled: Boolean;
begin
  result := fTimer.Enabled;
end;

procedure TTisFormTimeout.SetEnabled(aValue: Boolean);
begin
  OwnerAsForm; // do not continue, if owner is not a form
  fTimer.Enabled := aValue;
  if not aValue then
    fSavedCaption := '';
end;

procedure TTisFormTimeout.SetTimeout(aValue: Integer);
begin
  if fTimeout = aValue then
    Exit;
  fTimeout := aValue;
  fCounting := fTimeout;
end;

function TTisFormTimeout.OwnerAsForm: TCustomForm;
begin
  if Owner is TCustomForm then
    result := Owner as TCustomForm
  else
    raise Exception.Create('The Owner should be an instance of TCustomForm or descendant.');
end;

procedure TTisFormTimeout.DoTimer(aSender: TObject);
var
  vHandled: Boolean;
begin
  if csDesigning in ComponentState then
    Exit;
  if fSavedCaption = '' then
    fSavedCaption := OwnerAsForm.Caption;
  Dec(fCounting);
  DoCounting;
  if fCounting > 0 then
  begin
    if fUpdateCaption then
      OwnerAsForm.Caption := Format(rsFormTimeoutCaption, [fSavedCaption, fCounting]);
  end
  else
  begin
    fTimer.Enabled := False;
    vHandled := False;
    DoTimeout(vHandled);
    if not vHandled then
      OwnerAsForm.Close;
  end;
end;

procedure TTisFormTimeout.DoCounting;
begin
  if Assigned(fOnCounting) then
    fOnCounting(self, fCounting);
end;

procedure TTisFormTimeout.DoTimeout(var aHandled: Boolean);
begin
  if Assigned(fOnTimeout) then
    fOnTimeout(self, aHandled);
end;

constructor TTisFormTimeout.Create(aOwner: TComponent);
begin
  inherited Create(aOwner);
  fTimer := TTimer.Create(self);
  fTimer.Interval := 1000;
  fTimer.Enabled := False;
  fTimer.OnTimer := @DoTimer;
  UpdateCaption := DefaultUpdateCaption;
  Timeout := DefaultTimeout; // set fTimeout and fCounting
  Enabled := DefaultEnabled; // enables the timer
end;

end.
