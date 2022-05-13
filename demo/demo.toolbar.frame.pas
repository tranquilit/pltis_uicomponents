unit demo.toolbar.frame;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, ActnList, ComCtrls, Buttons, Dialogs,
  StdCtrls, tis.ui.toolbar.core;

type
  TToolBarFrame = class(TFrame)
    ActionList1: TActionList;
    Action1: TAction;
    Action2: TAction;
    Action3: TAction;
    Action4: TAction;
    ReadOnlyActionList: TActionList;
    ShowEditorAction: TAction;
    SpeedButton1: TSpeedButton;
    Action5: TAction;
    Memo1: TMemo;
    SpeedButton2: TSpeedButton;
    TisToolBar1: TTisToolBar;
    ToolButton1: TToolButton;
    ToolButton2: TToolButton;
    ToolButton3: TToolButton;
    ToolButton4: TToolButton;
    ToolButton5: TToolButton;
    procedure ShowEditorActionExecute(Sender: TObject);
    procedure SpeedButton1Click(Sender: TObject);
    procedure SpeedButton2Click(Sender: TObject);
    procedure Action1Execute(Sender: TObject);
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

procedure TToolBarFrame.SpeedButton1Click(Sender: TObject);
begin
  Memo1.Text := TisToolBar1.SessionValues;
end;

procedure TToolBarFrame.SpeedButton2Click(Sender: TObject);
begin
  TisToolBar1.SessionValues := Memo1.Text;
end;

procedure TToolBarFrame.Action1Execute(Sender: TObject);
begin
  ShowMessage((Sender as TAction).Caption);
end;

end.

