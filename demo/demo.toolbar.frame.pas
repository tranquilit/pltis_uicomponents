unit demo.toolbar.frame;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, ActnList, ComCtrls, Buttons, Dialogs,
  StdCtrls, tis.ui.toolbar.core;

type
  TToolBarFrame = class(TFrame)
    ActionList1: TActionList;
    GetSessionValuesAction: TAction;
    SetSessionValuesAction: TAction;
    Action3: TAction;
    Action4: TAction;
    ActionList2: TActionList;
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
    procedure ShowEditorActionExecute(Sender: TObject);
    procedure GetSessionValuesActionExecute(Sender: TObject);
    procedure SetSessionValuesActionExecute(Sender: TObject);
  private

  public

  end;

implementation

{$R *.lfm}

{ TToolBarFrame }

procedure TToolBarFrame.ShowEditorActionExecute(Sender: TObject);
begin
  TisToolBar1.ShowEditor;
end;

procedure TToolBarFrame.GetSessionValuesActionExecute(Sender: TObject);
begin
  Memo1.Text := TisToolBar1.SessionValues
end;

procedure TToolBarFrame.SetSessionValuesActionExecute(Sender: TObject);
begin
  if Memo1.Text = '' then
    ShowMessage('There is no values to update')
  else
    TisToolBar1.SessionValues := Memo1.Text;
end;

end.

