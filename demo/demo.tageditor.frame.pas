unit demo.tageditor.frame;

{$i mormot.defines.inc}

interface

uses
  Classes, SysUtils, Forms, Controls, StdCtrls, Spin, ColorBox,
  tis.ui.tageditor.core;

type
  TTagEditorFrame = class(TFrame)
    GroupBox1: TGroupBox;
    AutoHeightCheckBox: TCheckBox;
    AllowDuplicatesCheckBox: TCheckBox;
    MaxTagsEdit: TSpinEdit;
    Label1: TLabel;
    MultiLinesCheckBox: TCheckBox;
    Label2: TLabel;
    TextColorBox: TColorBox;
    Label3: TLabel;
    BgColorBox: TColorBox;
    TagEditor: TTisTagEditor;
    procedure AutoHeightCheckBoxClick(Sender: TObject);
    procedure AllowDuplicatesCheckBoxChange(Sender: TObject);
    procedure MultiLinesCheckBoxChange(Sender: TObject);
    procedure MaxTagsEditEditingDone(Sender: TObject);
    procedure BgColorBoxChange(Sender: TObject);
    procedure TextColorBoxChange(Sender: TObject);
  end;

implementation

{$R *.lfm}

{ TTagEditorFrame }

procedure TTagEditorFrame.AutoHeightCheckBoxClick(Sender: TObject);
begin
  TagEditor.AutoHeight := AutoHeightCheckBox.Checked;
end;

procedure TTagEditorFrame.AllowDuplicatesCheckBoxChange(Sender: TObject);
begin
  TagEditor.AllowDuplicates := AllowDuplicatesCheckBox.Checked;
end;

procedure TTagEditorFrame.MultiLinesCheckBoxChange(Sender: TObject);
begin
  TagEditor.MultiLine := MultiLinesCheckBox.Checked;
end;

procedure TTagEditorFrame.MaxTagsEditEditingDone(Sender: TObject);
begin
  TagEditor.MaxTags := MaxTagsEdit.Value;
end;

procedure TTagEditorFrame.BgColorBoxChange(Sender: TObject);
begin
  TagEditor.TagBgColor := BgColorBox.Selected;
  TextColorBox.Selected := TagEditor.TagTextColor; // text color would change
end;

procedure TTagEditorFrame.TextColorBoxChange(Sender: TObject);
begin
  TagEditor.TagTextColor := TextColorBox.Selected;
end;

end.

