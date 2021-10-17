// -----------------------------------------------------------------
//    This file is part of Tranquil IT Software
//    Copyright (C) 2012 - 2021  Tranquil IT https://www.tranquil.it
//    All Rights Reserved.
// ------------------------------------------------------------------
unit tis.ui.searchedit;

{$i mormot.defines.inc}

interface

uses
  Classes,
  SysUtils,
  Forms,
  Controls,
  Graphics,
  Dialogs,
  StdCtrls,
  ExtCtrls,
  Buttons,
  tis.ui.parts.buttons;

type
  TOnBeforeSearch = procedure(Sender: TObject; const aText: string; var aAbort: Boolean) of object;

  TOnSearch = procedure(Sender: TObject; const aText: string) of object;

  TTisSearchEdit = class(TEdit, IButtonProperties)
  private
    fTimer: TTimer;
    fButtons: TButtonCollection;
    fAutoSearch: Boolean;
    fOnBeforeSearch: TOnBeforeSearch;
    fOnSearch: TOnSearch;
    procedure SetDefault;
    procedure SetUpEdit;
    // -------- Timer events --------
    function GetSearchInterval: Cardinal;
    procedure SetSearchInterval(aValue: Cardinal);
    function GetOnStartSearch: TNotifyEvent;
    procedure SetOnStartSearch(aValue: TNotifyEvent);
    function GetOnStopSearch: TNotifyEvent;
    procedure SetOnStopSearch(aValue: TNotifyEvent);
  protected
    // ------------------------------- inherited methods ----------------------------------
    procedure SetParent(aNewParent: TWinControl); override;
    procedure DoSetBounds(aLeft, aTop, aWidth, aHeight: Integer); override;
    // ------------------------------- new methods ----------------------------------
    function DoBeforeSearch: Boolean; virtual;
    procedure DoTimer(Sender: TObject); virtual;
    procedure DoButtonClearClick(Sender: TObject); virtual;
    procedure Setup(aButton: TButtonItem); virtual;
  public
    // ------------------------------- inherited methods ----------------------------------
    constructor Create(aOwner: TComponent); override;
    destructor Destroy; override;
    procedure TextChanged; override;
    procedure EnabledChanged; override;
    procedure EditingDone; override;
  published
    // ------------------------------- new properties ----------------------------------
    property AutoSearch: Boolean read fAutoSearch write fAutoSearch default True;
    property Buttons: TButtonCollection read fButtons write fButtons;
    property SearchInterval: Cardinal read GetSearchInterval write SetSearchInterval default 1000;
    // ------------------------------- new events ----------------------------------
    property OnBeforeSearch: TOnBeforeSearch read fOnBeforeSearch write fOnBeforeSearch;
    property OnStartSearch: TNotifyEvent read GetOnStartSearch write SetOnStartSearch;
    property OnSearch: TOnSearch read fOnSearch write fOnSearch;
    property OnStopSearch: TNotifyEvent read GetOnStopSearch write SetOnStopSearch;
  end;

implementation

{ TTisSearchEdit }

procedure TTisSearchEdit.SetDefault;
begin
  Width := 130;
  Height := 24;
end;

procedure TTisSearchEdit.SetUpEdit;
begin
  TextHint := 'Search keywords';
  Text := '';
  ControlStyle := ControlStyle - [csSetCaption];
end;

function TTisSearchEdit.GetSearchInterval: Cardinal;
begin
  result := fTimer.Interval;
end;

procedure TTisSearchEdit.SetSearchInterval(aValue: Cardinal);
begin
  fTimer.Interval := aValue;
end;

function TTisSearchEdit.GetOnStartSearch: TNotifyEvent;
begin
  result := fTimer.OnStartTimer;
end;

procedure TTisSearchEdit.SetOnStartSearch(aValue: TNotifyEvent);
begin
  fTimer.OnStartTimer := aValue;
end;

function TTisSearchEdit.GetOnStopSearch: TNotifyEvent;
begin
  result := fTimer.OnStopTimer;
end;

procedure TTisSearchEdit.SetOnStopSearch(aValue: TNotifyEvent);
begin
  fTimer.OnStopTimer := aValue;
end;

procedure TTisSearchEdit.SetParent(aNewParent: TWinControl);
begin
  inherited SetParent(aNewParent);
  if csDestroying in ComponentState then
    exit;
  fButtons.Invalidate;
end;

procedure TTisSearchEdit.DoSetBounds(aLeft, aTop, aWidth, aHeight: Integer);
begin
  inherited DoSetBounds(aLeft, aTop, aWidth, aHeight);
  if Assigned(fButtons) then
    fButtons.Invalidate;
end;

function TTisSearchEdit.DoBeforeSearch: Boolean;
var
  aborted: Boolean;
begin
  aborted := False;
  if Assigned(fOnBeforeSearch) then
    fOnBeforeSearch(self, Text, aborted);
  result := not aborted;
end;

procedure TTisSearchEdit.DoTimer(Sender: TObject);
begin
  fTimer.Enabled := False;
  if Assigned(fOnSearch) then
    fOnSearch(self, Text);
end;

procedure TTisSearchEdit.DoButtonClearClick(Sender: TObject);
begin
  Clear;
end;

procedure TTisSearchEdit.Setup(aButton: TButtonItem);
var
  b: TSpeedButton;
begin
  b := aButton.Button;
  case aButton.Kind of
    bkSearch:
      b.OnClick := OnEditingDone;
    bkClear:
      b.OnClick := DoButtonClearClick;
  end;
end;

constructor TTisSearchEdit.Create(aOwner: TComponent);
begin
  inherited Create(aOwner);
  fTimer := TTimer.Create(nil);
  fTimer.OnTimer := DoTimer;
  fButtons := TButtonCollection.Create(self);
  fAutoSearch := True;
  SetDefault;
  SetUpEdit;
end;

destructor TTisSearchEdit.Destroy;
begin
  fTimer.Free;
  fButtons.Free;
  inherited Destroy;
end;

procedure TTisSearchEdit.TextChanged;
begin
  inherited TextChanged;
  fTimer.Enabled := fAutoSearch and DoBeforeSearch;
end;

procedure TTisSearchEdit.EnabledChanged;
begin
  inherited EnabledChanged;
end;

procedure TTisSearchEdit.EditingDone;
begin
  inherited EditingDone;
  if not fAutoSearch then
  begin
    if DoBeforeSearch then
      DoTimer(self);
  end;
end;

end.
