unit tis.ui.grid.copyspecial;

{$i tis.ui.defines.inc}

interface

uses
  Classes,
  SysUtils,
  Forms,
  Controls,
  Graphics,
  Dialogs,
  ExtCtrls,
  Buttons,
  DefaultTranslator,
  StdCtrls;

type
  TCopySpecialForm = class(TForm)
    BitBtn1: TBitBtn;
    BitBtn2: TBitBtn;
    PanBottom: TPanel;
    Panel3: TPanel;
    Panel1: TPanel;
    FormatLabel: TLabel;
    RowsLabel: TLabel;
    SelectionCombo: TComboBox;
    FormatCombo: TComboBox;
    ColumnsGroupBox: TGroupBox;
    ColumnsVisibleOnlyCheckBox: TCheckBox;
    TranslatedColumnsCheckBox: TCheckBox;
  end;

implementation

{$R *.lfm}

end.

