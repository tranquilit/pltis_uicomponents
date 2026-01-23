// -----------------------------------------------------------------
//    This file is part of Tranquil IT Software
//    Copyright (C) 2012 - 2026  Tranquil IT https://www.tranquil.it
//    All Rights Reserved.
// ------------------------------------------------------------------
unit demo.tageditor.frame;

{$i tis.ui.defines.inc}

interface

uses
  Classes,
  SysUtils,
  Forms,
  Controls,
  StdCtrls,
  Spin,
  ColorBox,
  Dialogs,
  ExtCtrls,
  Buttons,
  Graphics,
  ComCtrls,
  mormot.core.base,
  mormot.core.data,
  mormot.core.text,
  mormot.core.unicode,
  tis.ui.tageditor.core,
  tis.ui.tageditor.rtti;

type
  TTagsPersistent = class(TPersistent)
  private
    fTags: string;
  published
    property Tags: string read fTags write fTags;
  end;

  TTagEditorFrame = class(TFrame)
    PageControl1: TPageControl;
    TabSheet1: TTabSheet;
    TabSheet2: TTabSheet;
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
    ItemsMemo: TMemo;
    Label4: TLabel;
    ItemsUpdateButton: TBitBtn;
    DropDownCheckBox: TCheckBox;
    AutoCompleteCheckBox: TCheckBox;
    EnabledCheckBox: TCheckBox;
    Label9: TLabel;
    TagBgColorDisabledBox: TColorBox;
    SpeedButton1: TSpeedButton;
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
    Bevel4: TBevel;
    TagAfterAddCheckBox: TCheckBox;
    TagAfterAddEdit: TEdit;
    Label8: TLabel;
    TagClearAllLabel: TLabel;
    ShowTagsAsArrayLabel: TLabel;
    InputTagsAsArrayLabel: TLabel;
    Label5: TLabel;
    TagEditorRtti: TTisTagEditorRtti;
    Label10: TLabel;
    RttiItemsMemo: TMemo;
    RttiLinkToMemoRadioButton: TRadioButton;
    RttiLinkToEditRadioButton: TRadioButton;
    RttiItemsEdit: TEdit;
    RttiLinkToTPersistentRadioButton: TRadioButton;
    RttiPersistentEdit: TEdit;
    RttiPersistentAddButton: TBitBtn;
    RttiPersistentTimer: TTimer;
    RttiPersistentLabel: TLabel;
    Label11: TLabel;
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
    procedure TagEditorTagAfterAdd(Sender: TObject; aTag: TTagItem);
    procedure ItemsUpdateButtonClick(Sender: TObject);
    procedure DropDownCheckBoxClick(Sender: TObject);
    procedure AutoCompleteCheckBoxClick(Sender: TObject);
    procedure EnabledCheckBoxClick(Sender: TObject);
    procedure TagBgColorDisabledBoxChange(Sender: TObject);
    procedure SpeedButton1Click(Sender: TObject);
    procedure RttiLinkToMemoRadioButtonChange(Sender: TObject);
    procedure RttiLinkToEditRadioButtonChange(Sender: TObject);
    procedure RttiLinkToTPersistentRadioButtonChange(Sender: TObject);
    procedure RttiPersistentAddButtonClick(Sender: TObject);
    procedure RttiPersistentTimerTimer(Sender: TObject);
  private
    fTags: TTagsPersistent;
  public
    constructor Create(aOwner: TComponent); override;
    destructor Destroy; override;
  end;

implementation

uses
  tisstrings;

{$R *.lfm}

{ TTagEditorFrame }

procedure TTagEditorFrame.AutoHeightCheckBoxClick(Sender: TObject);
begin
  TagEditor.AutoHeight := AutoHeightCheckBox.Checked;
end;

procedure TTagEditorFrame.AllowDuplicatesCheckBoxChange(Sender: TObject);
begin
  if AllowDuplicatesCheckBox.Checked then
    TagEditor.TagInput.Options := TagEditor.TagInput.Options + [ioAllowDuplicates]
  else
    TagEditor.TagInput.Options := TagEditor.TagInput.Options - [ioAllowDuplicates];
end;

procedure TTagEditorFrame.MultiLinesCheckBoxChange(Sender: TObject);
begin
  TagEditor.MultiLine := MultiLinesCheckBox.Checked;
end;

procedure TTagEditorFrame.MaxTagsEditEditingDone(Sender: TObject);
begin
  TagEditor.TagInput.MaxTags := MaxTagsEdit.Value;
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
begin
  ShowMessage(TagEditor.AsArray.Join(','));
end;

procedure TTagEditorFrame.InputTagsAsArrayLabelClick(Sender: TObject);
var
  vInput: string;
begin
  vInput := InputBox('Delimited Text', 'Type tags separating them, using one of the TagInput.DelimiterChars property', '');
  if vInput <> '' then
    TagEditor.Tags.DelimitedText := vInput;
end;

procedure TTagEditorFrame.TagEditorTagAfterAdd(Sender: TObject; aTag: TTagItem);
begin
  if TagAfterAddCheckBox.Checked then
    aTag.Text := FormatString('%%', [TagAfterAddEdit.Text, aTag.Text]);
end;

procedure TTagEditorFrame.ItemsUpdateButtonClick(Sender: TObject);
begin
  TagEditor.TagComboBox.Items.Assign(ItemsMemo.Lines);
end;

procedure TTagEditorFrame.DropDownCheckBoxClick(Sender: TObject);
begin
  with TagEditor.TagComboBox do
    if DropDownCheckBox.Checked then
      Style := csDropDown
    else
      Style := csSimple;
end;

procedure TTagEditorFrame.AutoCompleteCheckBoxClick(Sender: TObject);
begin
  with TagEditor.TagComboBox do
    if AutoCompleteCheckBox.Checked then
      AutoComplete := AutoComplete + [cbactEnabled]
    else
      AutoComplete := AutoComplete - [cbactEnabled];
end;

procedure TTagEditorFrame.EnabledCheckBoxClick(Sender: TObject);
begin
  TagEditor.Enabled := EnabledCheckBox.Checked;
end;

procedure TTagEditorFrame.TagBgColorDisabledBoxChange(Sender: TObject);
begin
  TagEditor.TagBgColorDisabled := TagBgColorDisabledBox.Selected;
end;

procedure TTagEditorFrame.SpeedButton1Click(Sender: TObject);
begin
  TagBgColorDisabledBox.Selected := clDefault;
end;

procedure TTagEditorFrame.RttiLinkToMemoRadioButtonChange(Sender: TObject);
begin
  RttiPersistentTimer.Enabled := False;
  TagEditorRtti.Link.SetObjectAndProperty(RttiItemsMemo, 'Lines');
end;

procedure TTagEditorFrame.RttiLinkToEditRadioButtonChange(Sender: TObject);
begin
  RttiPersistentTimer.Enabled := False;
  TagEditorRtti.Link.SetObjectAndProperty(RttiItemsEdit, 'Text');
end;

procedure TTagEditorFrame.RttiLinkToTPersistentRadioButtonChange(Sender: TObject);
begin
  RttiPersistentTimer.Enabled := True;
  TagEditorRtti.Link.SetObjectAndProperty(fTags, 'Tags');
end;

procedure TTagEditorFrame.RttiPersistentAddButtonClick(Sender: TObject);
begin
  fTags.Tags := fTags.Tags + ',' + RttiPersistentEdit.Text;
  RttiPersistentEdit.Clear;
  RttiPersistentEdit.SetFocus;
end;

procedure TTagEditorFrame.RttiPersistentTimerTimer(Sender: TObject);
begin
  RttiPersistentLabel.Caption := fTags.Tags;
end;

constructor TTagEditorFrame.Create(aOwner: TComponent);
begin
  inherited Create(aOwner);
  fTags := TTagsPersistent.Create;
  fTags.Tags := 'AmigaOS, MorphOS, AROS, Atari TOS';
end;

destructor TTagEditorFrame.Destroy;
begin
  fTags.Free;
  inherited Destroy;
end;

end.

