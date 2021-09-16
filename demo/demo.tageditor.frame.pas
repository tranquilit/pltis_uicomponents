unit demo.tageditor.frame;

{$i mormot.defines.inc}

interface

uses
  Classes, SysUtils, Forms, Controls, StdCtrls, Spin, ColorBox, Dialogs,
  ExtCtrls,
  mormot.core.base,
  mormot.core.data,
  mormot.core.text,
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
    GroupBox2: TGroupBox;
    TagClickCheckBox: TCheckBox;
    TagBeforeAddCheckBox: TCheckBox;
    Label6: TLabel;
    TagBeforeAddEdit: TEdit;
    TagBeforeDeleteCheckBox: TCheckBox;
    Label7: TLabel;
    TagBeforeDeleteEdit: TEdit;
    TagAfterDragCheckBox: TCheckBox;
    Bevel1: TBevel;
    Bevel2: TBevel;
    Bevel3: TBevel;
    TagClearAllLabel: TLabel;
    ShowTagsAsArrayLabel: TLabel;
    InputTagsAsArrayLabel: TLabel;
    procedure AutoHeightCheckBoxClick(Sender: TObject);
    procedure AllowDuplicatesCheckBoxChange(Sender: TObject);
    procedure MultiLinesCheckBoxChange(Sender: TObject);
    procedure MaxTagsEditEditingDone(Sender: TObject);
    procedure BgColorBoxChange(Sender: TObject);
    procedure TextColorBoxChange(Sender: TObject);
    procedure TagEditorTagClick(Sender: TObject; aTag: TTagItem);
    procedure TagEditorTagBeforeAdd(Sender: TObject; const aTag: string;
      var aAbort: Boolean);
    procedure TagEditorTagBeforeDelete(Sender: TObject; aTag: TTagItem;
      var aAbort: Boolean);
    procedure TagEditorTagAfterDrag(Sender: TObject; aTag: TTagItem; aPreIndex,
      aNewIndex: Integer);
    procedure TagClearAllLabelClick(Sender: TObject);
    procedure ShowTagsAsArrayLabelClick(Sender: TObject);
    procedure InputTagsAsArrayLabelClick(Sender: TObject);
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

procedure TTagEditorFrame.TagEditorTagClick(Sender: TObject; aTag: TTagItem);
begin
  if TagClickCheckBox.Checked then
    ShowMessage('Tag "' + aTag.Text + '" was clicked');
end;

procedure TTagEditorFrame.TagEditorTagBeforeAdd(Sender: TObject;
  const aTag: string; var aAbort: Boolean);
begin
  if TagBeforeAddCheckBox.Checked then
    if aTag = TagBeforeAddEdit.Text then
    begin
      ShowMessage('Invalid tag name');
      aAbort := True;
    end;
end;

procedure TTagEditorFrame.TagEditorTagBeforeDelete(Sender: TObject;
  aTag: TTagItem; var aAbort: Boolean);
begin
  if TagBeforeDeleteCheckBox.Checked then
    if aTag.Text = TagBeforeDeleteEdit.Text then
    begin
      ShowMessage('You cannot delete "' + TagBeforeDeleteEdit.Text + '"');
      aAbort := True;
    end;
end;

procedure TTagEditorFrame.TagEditorTagAfterDrag(Sender: TObject;
  aTag: TTagItem; aPreIndex, aNewIndex: Integer);
begin
  if TagAfterDragCheckBox.Checked then
    ShowMessage(
      'Tag: ' + aTag.Text + LineEnding +
      '- pre index: ' + aPreIndex.ToString + LineEnding +
      '- new index: ' + aNewIndex.ToString
    );
end;

procedure TTagEditorFrame.TagClearAllLabelClick(Sender: TObject);
begin
  TagEditor.Clear;
end;

procedure TTagEditorFrame.ShowTagsAsArrayLabelClick(Sender: TObject);
var
  a: TRawUtf8DynArray;
begin
  a := nil;
  StringDynArrayToRawUtf8DynArray(TagEditor.AsArray, a);
  ShowMessage(Utf8ToString(RawUtf8ArrayToCsv(a)));
end;

procedure TTagEditorFrame.InputTagsAsArrayLabelClick(Sender: TObject);
var
  input: string;
  sl: TStringList;
  a: TRawUtf8DynArray;
begin
  input := InputBox('Delimited Text', 'Type tags using "," among then', '');
  sl := TStringList.Create;
  try
    sl.DelimitedText := input;
    StringListToRawUtf8DynArray(sl, a);
    TagEditor.AsArray := a;
  finally
    sl.Free;
  end;
end;

end.

