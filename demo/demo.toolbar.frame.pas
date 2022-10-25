unit demo.toolbar.frame;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, ActnList, ComCtrls, Buttons, Dialogs,
  StdCtrls, Menus,
  mormot.core.variants,
  mormot.core.unicode,
  mormot.core.text,
  tis.ui.toolbar.core;

type
  TToolBarFrame = class(TFrame)
    ActionList: TActionList;
    GetSessionValuesAction: TAction;
    SetSessionValuesAction: TAction;
    HelloAction: TAction;
    NoHandlerAction: TAction;
    EditorList: TActionList;
    ShowEditorAction: TAction;
    Action5: TAction;
    Memo1: TMemo;
    TisToolBar1: TTisToolBar;
    ToolButton1: TToolButton;
    ToolButton2: TToolButton;
    ToolButton3: TToolButton;
    ToolButton4: TToolButton;
    ToolButton5: TToolButton;
    ToolButton6: TToolButton;
    Label1: TLabel;
    Action6: TAction;
    ToolBarPopup: TPopupMenu;
    MenuItem1: TMenuItem;
    ToolButton8: TToolButton;
    ToolButton9: TToolButton;
    RestoreAction: TAction;
    ToolButton10: TToolButton;
    Action1: TAction;
    Action2: TAction;
    PopupAction: TAction;
    ToolButton11: TToolButton;
    procedure ShowEditorActionExecute(Sender: TObject);
    procedure GetSessionValuesActionExecute(Sender: TObject);
    procedure SetSessionValuesActionExecute(Sender: TObject);
    procedure RestoreActionExecute(Sender: TObject);
    procedure HelloActionExecute(Sender: TObject);
  private

  public

  end;

implementation

{$R *.lfm}

function HumanReadableJson(const aJson: string): string;
var
  d: TDocVariantData;
begin
  d.InitJson(StringToUtf8(aJson), JSON_FAST_FLOAT);
  result := Utf8ToString(d.ToJson('', '', jsonHumanReadable));
end;

{ TToolBarFrame }

procedure TToolBarFrame.ShowEditorActionExecute(Sender: TObject);
begin
  TisToolBar1.ShowEditor;
end;

procedure TToolBarFrame.GetSessionValuesActionExecute(Sender: TObject);
begin
  Memo1.Text := HumanReadableJson(TisToolBar1.SessionValues)
end;

procedure TToolBarFrame.SetSessionValuesActionExecute(Sender: TObject);
begin
  if Memo1.Text = '' then
    ShowMessage('There is no values to update')
  else
    TisToolBar1.SessionValues := Memo1.Text;
end;

procedure TToolBarFrame.RestoreActionExecute(Sender: TObject);
begin
  TisToolBar1.RestoreSession;
end;

procedure TToolBarFrame.HelloActionExecute(Sender: TObject);
begin
  ShowMessage('Hello');
end;

end.

