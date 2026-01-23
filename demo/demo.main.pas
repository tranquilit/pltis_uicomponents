// -----------------------------------------------------------------
//    This file is part of Tranquil IT Software
//    Copyright (C) 2012 - 2026  Tranquil IT https://www.tranquil.it
//    All Rights Reserved.
// ------------------------------------------------------------------
unit demo.main;

{$i tis.ui.defines.inc}

interface

uses
  Classes,
  SysUtils,
  Forms,
  Controls,
  Buttons,
  ComCtrls,
  LCLType,
  Menus,
  demo.grid.frame,
  demo.tageditor.frame,
  demo.searchedit.frame,
  demo.toolbar.frame;

type
  TMainForm = class(TForm)
    PageControl: TPageControl;
    GridTab: TTabSheet;
    TagEditorTab: TTabSheet;
    freGrid: TGridFrame;
    freTagEditor: TTagEditorFrame;
    SearchEditTab: TTabSheet;
    freSearchEdit: TSearchEditFrame;
    ToolBarTab: TTabSheet;
    freToolBar: TToolBarFrame;
    MainMenu: TMainMenu;
    LangMenuItem: TMenuItem;
    LangEnMenuItem: TMenuItem;
    LangFrMenuItem: TMenuItem;
    LangDeMenuItem: TMenuItem;
    procedure FormCreate(Sender: TObject);
    procedure LangEnMenuItemClick(Sender: TObject);
  end;

var
  MainForm: TMainForm;

implementation

uses
  tis.ui.resourcestrings;

{$R *.lfm}

{ TMainForm }

procedure TMainForm.FormCreate(Sender: TObject);
begin
  PageControl.ActivePage := GridTab;
  with freTagEditor do
  begin
    BgColorBox.Selected := TagEditor.TagBgColor;
    TextColorBox.Selected := TagEditor.TagTextColor;
  end;
end;

procedure TMainForm.LangEnMenuItemClick(Sender: TObject);
var
  vMenu: TMenuItem;
begin
  if Sender is TMenuItem then
  begin
    vMenu := Sender as TMenuItem;
    tis.ui.resourcestrings.Translate('..\languages', vMenu.Caption);
  end;
end;

end.
