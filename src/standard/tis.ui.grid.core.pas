// -----------------------------------------------------------------
//    This file is part of Tranquil IT Software
//    Copyright (C) 2012 - 2022  Tranquil IT https://www.tranquil.it
//    All Rights Reserved.
// ------------------------------------------------------------------
unit tis.ui.grid.core;

{.$i mormot.defines.inc}
{$mode objfpc}{$H+}


interface

uses
  {$ifdef windows}
  Windows,
  {$endif}
  Classes,
  SysUtils,
  Controls,
  Math,
  Menus,
  Graphics,
  Clipbrd,
  LMessages,
  StdCtrls,
  Types,
  LCLIntf,
  LCLType,
  Forms,
  Dialogs,
  Buttons,
  EditBtn,
  DefaultTranslator,
  VirtualTrees,
  mormot.core.base,
  mormot.core.data, // for dvoNameCaseSensitive
  mormot.core.datetime,
  mormot.core.variants,
  mormot.core.unicode,
  mormot.core.text,
  mormot.core.buffers,
  mormot.core.rtti,
  tisstrings,
  tis.core.os,
  tis.core.utils,
  tis.ui.searchedit,
  tis.ui.grid.controls;

type
  ETisGrid = class(Exception);

  TTisGrid = class;

  /// column data type
  TTisColumnDataType = (
    cdtString,
    cdtDate,
    cdtTime,
    cdtDateTime,
    cdtInteger,
    cdtFloat,
    cdtBoolean,
    cdtMemo,
    /// it shows only "***" in Columns and Editor
    // - this type will not be exported
    cdtPassword
  );

  TTisColumnDataTypes = set of TTisColumnDataType;

  /// adapter for TTisColumnDataType
  TTisColumnDataTypeAdapter = object
    /// convert enum to caption
    function EnumToCaption(const aValue: TTisColumnDataType): string;
    /// convert enum to RawUtf8
    function EnumToRawUtf8(const aValue: TTisColumnDataType): RawUtf8;
    /// convert enum to index
    function EnumToIndex(const aValue: TTisColumnDataType): Integer;
    /// convert caption to enum
    // - if aValue not found, it will return the first element
    function CaptionToEnum(const aValue: string): TTisColumnDataType;
    /// convert RawUtf8 to enum
    // - if aValue not found, it will return the first element
    function RawUtf8ToEnum(const aValue: RawUtf8): TTisColumnDataType;
    /// convert caption to index
    // - if aValue not found, it will return the first element
    function CaptionToIndex(const aValue: string): Integer;
    /// convert all enums to strings
    // - you can customize elements using aCustom
    procedure EnumsToStrings(aDest: TStrings; const aCustom: TTisColumnDataTypes = [
      low(TTisColumnDataType)..high(TTisColumnDataType)]);
  end;

  TTisGridColumn = class;

  /// it implements a edit control link, for each data type
  TTisGridEditLink = class(TInterfacedObject, IVTEditLink)
  private
    fControl: TTisGridControl;
    fGrid: TTisGrid;
    fNode: PVirtualNode;
    fColumn: Integer;
  protected
    procedure EditKeyDown({%H-}aSender: TObject; var Key: Word; Shift: TShiftState); virtual;
    procedure EditExit({%H-}aSender: TObject); virtual;
    procedure SetupControlClasses; virtual;
    /// override this method if you want to change the default control for the column
    // - by default, first it will check Grid.OnCustomEditor event to get an instance
    // - if none instance was provided, it will use Grid.OnEditorLookup to get one
    // - again, if none was provided, it will use ControlClasses array, according to the aColumn.DataType
    function NewControl(aColumn: TTisGridColumn): TTisGridControl; virtual;
  public var
    /// use this array to change the default control for each data type
    ControlClasses: array[TTisColumnDataType] of TTisGridControlClass;
  public
    constructor Create;
    destructor Destroy; override;
    function BeginEdit: Boolean; stdcall;
    function CancelEdit: Boolean; stdcall;
    function EndEdit: Boolean; stdcall;
    function GetBounds: TRect; stdcall;
    function PrepareEdit(aTree: TBaseVirtualTree; aNode: PVirtualNode; aColumn: TColumnIndex): Boolean; stdcall;
    procedure ProcessMessage(var aMessage: TLMessage); stdcall;
    procedure SetBounds(R: TRect); stdcall;
  end;

  /// a custom implementation for Grid Column
  TTisGridColumn = class(TVirtualTreeColumn)
  private
    fPropertyName: RawUtf8;
    fDataType: TTisColumnDataType;
    fRequired: Boolean;
    fReadOnly: Boolean;
    function GetTitle: TCaption;
    procedure SetTitle(const aValue: TCaption);
    procedure SetPropertyName(const aValue: RawUtf8);
  protected const
    DefaultDataType = cdtString;
    DefaultRequired = False;
    DefaultReadOnly = False;
  public
    constructor Create(aCollection: TCollection); override;
    procedure Assign(aSource: TPersistent); override;
  published
    property Text: TCaption read GetTitle write SetTitle;
    property PropertyName: RawUtf8 read fPropertyName write SetPropertyName;
    property DataType: TTisColumnDataType read fDataType write fDataType default DefaultDataType;
    /// if TRUE, it will not allow user to set NULL/blank for this column using Editor
    // - if editor focus is lost, it will return the previous value before edition
    property Required: Boolean read fRequired write fRequired default DefaultRequired;
    property ReadOnly: Boolean read fReadOnly write fReadOnly default DefaultReadOnly;
  end;

  /// a custom implementation for Grid Columns
  TTisGridColumns = class(TVirtualTreeColumns)
  protected
    /// it implements a tri-state sorting click
    // - sort ascending, then descending, and finally no sort (showing no arrow on header)
    procedure HandleClick(P: TPoint; aButton: TMouseButton; aForce,
      aDblClick: Boolean); override;
  public
    // overriding for circumvent original Assign, which freezes Lazarus when having invisible columns
    procedure Assign(aSource: TPersistent); override;
  end;

  TTisGridHeaderPopupOption = (
    /// show menu items in original column order as they were added to the tree
    poOriginalOrder,
    /// allows to hide all columns, including the last one
    poAllowHideAll
  );

  TTisGridHeaderPopupOptions = set of TTisGridHeaderPopupOption;

  TTisGridHeaderPopupItem = (
    apNormal,
    apDisabled,
    apHidden
  );

  TTisGridExportFormatOption = (
    /// inherited types
    efoRtf,  // by ContentToRTF
    efoHtml, // by ContentToHTML
    efoText, // by ContentToText
    /// our custom types
    efoCsv,  // by ContentToCsv
    efoJson // by ContentToJson
  );

  TTisGridExportFormatOptions = set of TTisGridExportFormatOption;

  /// adapter for TTisGridExportFormatOption
  TTisGridExportFormatOptionAdapter = object
    /// convert enum to caption
    function EnumToCaption(const aValue: TTisGridExportFormatOption): string;
    /// convert caption to enum
    // - if aValue not found, it will return the first element
    function CaptionToEnum(const aValue: string): TTisGridExportFormatOption;
    /// convert all enums to strings
    // - you can customize elements using aCustom
    procedure EnumsToStrings(aDest: TStrings; const aCustom: TTisGridExportFormatOptions = [
      low(TTisGridExportFormatOption)..high(TTisGridExportFormatOption)]);
    /// convert file extension to enum
    // - if aValue not found, it will return the first element
    function ExtensionToEnum(const aValue: TFileName): TTisGridExportFormatOption;
    /// convert enum to save dialog filter
    function EnumToFilter(const aValue: TTisGridExportFormatOption): string;
  end;

  TTisGridTextSourceTypes = set of TVSTTextSourceType;

  /// adapter for TVSTTextSourceType
  TTisGridTextSourceTypeAdapter = object
    /// convert enum to caption
    function EnumToCaption(const aValue: TVSTTextSourceType): string;
    /// convert caption to enum
    // - if aValue not found, it will return the first element
    function CaptionToEnum(const aValue: string): TVSTTextSourceType;
    /// convert all enums to strings
    // - you can customize elements using aCustom
    procedure EnumsToStrings(aDest: TStrings; const aCustom: TTisGridTextSourceTypes = [tstAll, tstSelected]);
  end;

  TOnGridHeaderAddPopupItem = procedure(const aSender: TBaseVirtualTree; const aColumn: TColumnIndex;
    var aItem: TTisGridHeaderPopupItem) of object;

  TOnGridHeaderColumnChange = procedure(const aSender: TBaseVirtualTree; const aColumn: TColumnIndex; aVisible: Boolean) of object;

  TTisGridHeaderMenuItem = class(TMenuItem);

  TTisGridHeaderPopupMenu = class(TPopupMenu)
  private
    fOptions: TTisGridHeaderPopupOptions;
    fOnAddPopupItem: TOnGridHeaderAddPopupItem;
    fOnColumnChange: TOnGridHeaderColumnChange;
  protected
    procedure RemoveAutoItems; virtual;
    procedure DoAddHeaderPopupItem(const aColumn: TColumnIndex; out aItem: TTisGridHeaderPopupItem); virtual;
    procedure DoColumnChange(aColumn: TColumnIndex; aVisible: Boolean); virtual;
    procedure OnMenuItemClick(aSender: TObject);
    procedure OnMenuShowAllClick(aSender: TObject);
    procedure OnMenuHideAllClick(aSender: TObject);
    procedure OnMenuRestoreClick(aSender: TObject);
  public
    procedure Popup(x, y: Integer); override;
  published
    property Options: TTisGridHeaderPopupOptions read fOptions write fOptions default [];
    property OnAddPopupItem: TOnGridHeaderAddPopupItem read fOnAddPopupItem write fOnAddPopupItem;
    property OnColumnChange: TOnGridHeaderColumnChange read fOnColumnChange write fOnColumnChange;
  end;

  /// a custom implementation for Grid Header
  TTisGridHeader = class(TVTHeader)
  private
    function GetPopupMenu: TPopupMenu;
    procedure SetPopupMenu(aValue: TPopupMenu);
  protected
    function GetColumnsClass: TVirtualTreeColumnsClass; override;
    /// creates a default popup menu
    procedure NewDefaultPopupMenu;
  public
    constructor Create(aOwner: TBaseVirtualTree); override;
    // overriding the original Assign for do not assign PopupMenu from another grid header
    // - a Popup in design mode will not work when using Editor
    procedure Assign(aSource: TPersistent); override;
    property PopupMenu: TPopupMenu read GetPopupMenu write SetPopupMenu;
  end;

  /// a custom implementation for String Tree Options
  TTisStringTreeOptions = class(TCustomStringTreeOptions)
  protected
    const DefaultPaintOptions = VirtualTrees.DefaultPaintOptions -
      [toShowRoot] + [toAlwaysHideSelection, toShowHorzGridLines, toShowVertGridLines, toHideFocusRect];
    const DefaultSelectionOptions = VirtualTrees.DefaultSelectionOptions +
      [toExtendedFocus, toSimpleDrawSelection, toRightClickSelect];
    const DefaultMiscOptions = VirtualTrees.DefaultMiscOptions +
      [toGridExtensions, toFullRowDrag] - [toWheelPanning,toEditOnClick,toEditOnDblClick];
    const DefaultAutoOptions = VirtualTrees.DefaultAutoOptions +
      [toAutoSort, toAutoChangeScale];
  public
    constructor Create(aOwner: TBaseVirtualTree); override;
  published
    property AnimationOptions;
    property AutoOptions default DefaultAutoOptions;
    property ExportMode;
    property MiscOptions default DefaultMiscOptions;
    property PaintOptions default DefaultPaintOptions;
    property SelectionOptions default DefaultSelectionOptions;
    property StringOptions;
  end;

  TTisDataEvent = (
    deDataSetChange,
    deAddrecord,
    deDeleteRecord,
    deUpdateRecord,
    deUpdateState,
    deFieldListChange
  );

  /// popup menu options that will be added when it shows up
  // - some items depends of others grid properties combined to appear
  // - see DefaultPopupMenuOptions constant to know what is enabled/disabled by default in Grid
  TTisPopupMenuOption = (
    pmoShowFind,
    pmoShowFindNext,
    pmoShowCut,
    pmoShowCopy,
    pmoShowCopyCell,
    pmoShowCopySpecial,
    pmoShowPaste,
    pmoShowDelete,
    pmoShowSelectAll,
    pmoShowCustomizeColumns,
    pmoShowExport,
    pmoShowCustomizeGrid
  );

  TTisPopupMenuOptions = set of TTisPopupMenuOption;

  /// node options
  TTisNodeOptions = class(TPersistent)
  private
    fGrid: TTisGrid;
    fMultiLine: Boolean;
    fMultiLineHeight: Integer;
    procedure SetMultiLine(aValue: Boolean);
    procedure SetMultiLineHeight(aValue: Integer);
  protected
    const DefaultMultiLine = False;
    const DefaultMultiLineHeight = 4;
  public
    constructor Create(aGrid: TTisGrid); reintroduce;
    procedure AssignTo(aDest: TPersistent); override;
  published
    property MultiLine: Boolean read fMultiLine write SetMultiLine default DefaultMultiLine;
    property MultiLineHeight: Integer read fMultiLineHeight write SetMultiLineHeight default DefaultMultiLineHeight;
  end;

  TOnGridGetText = procedure(aSender: TBaseVirtualTree; aNode: PVirtualNode;
    const aCell: TDocVariantData; aColumn: TColumnIndex; aTextType: TVSTTextType;
    var aText: string) of object;

  TOnGridRows = procedure(aSender: TTisGrid; aRows: PDocVariantData) of object;

  /// event to manipulate rows before deleting them
  // - as used by TTisGrid.OnBeforeDeleteRows
  // - use it for change the rows or abort the process
  TOnGridBeforeDeleteRows = procedure (aSender: TTisGrid; aRows: PDocVariantData;
    var aAskUser, aAbort: Boolean) of object;

  /// event to manipulate data before change the internal Data
  // - as used by TTisGrid.OnBeforeDataChage
  // - use it for check/change the aData argument, before assign it, and/or abort the process
  TOnGridBeforeDataChange = procedure (aSender: TTisGrid; aData: PDocVariantData;
    var aAbort: Boolean) of object;

  /// event to manipulate data after Data changed
  // - as used by TTisGrid.OnAfterDataChage
  TOnGridAfterDataChange = type TNotifyEvent;

  /// event for comparing rows of objects
  // - as used by TTisGrid.OnCompareByRow
  TOnGridCompareByRow = function(aSender: TTisGrid; const aPropertyName: RawUtf8;
    const aRow1, aRow2: TDocVariantData; var aHandled: Boolean): PtrInt of object;

  /// event to manipulate rows using copy/paste on grid
  // - use it for check/change the aData argument, before assign it, and/or abort the process
  TOnGridPaste = procedure(aSender: TTisGrid; aData: PDocVariantData; var aAbort: Boolean) of object;

  /// event that allows users customize the control instance, creating a new one, replacing the default
  TOnGridCustomEditor = procedure(aSender: TObject; aColumn: TTisGridColumn;
    out aControl: TTisGridControl) of object;

  /// event that simplifies the use of a TisSearchEdit as Edit Control
  TOnGridEditorLookup = procedure(aSender: TTisGrid; aColumn: TTisGridColumn;
    aSearchEdit: TTisSearchEdit; var aHandled: Boolean) of object;

  /// event that allows users to change some edit control properties, before it shows up
  TOnGridPrepareEditor = procedure(aSender: TTisGrid; aColumn: TTisGridColumn;
    aControl: TTisGridControl) of object;

  /// event that allow to validate the new value from user input
  // - aCurValue is the current value for the aColumn
  // - use it for check/change the aNewValue argument, before assign it, and/or abort the process
  TOnGridEditValidated = procedure(aSender: TTisGrid; aColumn: TTisGridColumn;
    const aCurValue: Variant; var aNewValue: Variant; var aAbort: Boolean) of object;

  /// export a custom format
  // - use it to pass a custom buffer to the grid when call ExportData, if you use a non-default format
  TOnGridExportCustomContent = procedure(aSender: TTisGrid; aSource: TVSTTextSourceType;
    var aBuffer: RawUtf8) of object;

  /// this component is based on TVirtualStringTree, using mORMot TDocVariantData type
  // as the protocol for receiving and sending data
  TTisGrid = class(TCustomVirtualStringTree)
  private type
    TInternalData = object
      Offset: Cardinal;
      procedure Init(aGrid: TTisGrid);
      function Data(aNode: PVirtualNode): Pointer;
    end;
  private
    // ------------------------------- new fields ----------------------------------
    fInternalData: TInternalData;
    fKeyFieldsList, fParentKeyFieldsList: array of string;
    fSelectedAndTotalLabel: TLabel;
    fTextFound: boolean;
    fFindDlg: TFindDialog;
    fZebraColor: TColor;
    fZebraPaint: Boolean;
    fReplaceDialog: TReplaceDialog;
    fColumnToFind: integer;
    fStartSearchNode: PVirtualNode;
    fTextToFind: string;
    fData: TDocVariantData;
    fSelectedData: TDocVariantData;
    fNodeOptions: TTisNodeOptions;
    fPopupMenuOptions: TTisPopupMenuOptions;
    fPopupOrigEvent: TNotifyEvent; // it saves the original OnPopup event, if an external Popup instance was setted
    fExportFormatOptions: TTisGridExportFormatOptions;
    fDefaultSettings: Variant; // all default settings after load component
    // ------------------------------- new events ----------------------------------
    fOnGetText: TOnGridGetText;
    fOnCutToClipboard: TNotifyEvent;
    fOnBeforeDataChange: TOnGridBeforeDataChange;
    fOnAfterDataChange: TOnGridAfterDataChange;
    fOnBeforePaste: TOnGridPaste;
    fOnBeforeDeleteRows: TOnGridBeforeDeleteRows;
    fOnCompareByRow: TOnGridCompareByRow;
    fOnAfterFillPopupMenu: TNotifyEvent;
    fOnCustomEditor: TOnGridCustomEditor;
    fOnEditorLookup: TOnGridEditorLookup;
    fOnPrepareEditor: TOnGridPrepareEditor;
    fOnEditValidated: TOnGridEditValidated;
    fOnGridExportCustomContent: TOnGridExportCustomContent;
    // ------------------------------- HMENU ---------------------------------------
    HMUndo, HMRevert: HMENU;
    HMFind, HMFindNext, HMReplace: HMENU;
    HMCut, HMCopy, HMCopyCell, HMCopySpecial, HMPaste, HMFindReplace: HMENU;
    HMInsert, HMDelete, HMSelAll: HMENU;
    HMExport, HMPrint: HMENU;
    HMCollAll, HMExpAll: HMENU;
    HMCustomize: HMENU;
    HMAdvancedCustomize: HMENU;
    // ------------------------------- new methods ---------------------------------
    function FocusedPropertyName: string;
    function GetFocusedColumnObject: TTisGridColumn;
    function GetFocusedRow: PDocVariantData;
    function GetKeyFieldsNames: string;
    procedure SetKeyFieldsNames(const aValue: string);
    function GetParentKeyFieldsNames: string;
    procedure SetParentKeyFieldsNames(const aValue: string);
    function GetSettings: Variant;
    procedure SetSettings(const aValue: Variant);
    function GetGridSettings: string;
    procedure SetGridSettings(const aValue: string);
    procedure SetColumnToFind(aValue: integer);
    procedure SetData(const aValue: TDocVariantData);
    function GetMetaData: RawUtf8;
    procedure SetMetaData(const aValue: RawUtf8);
    procedure SetFocusedColumnObject(aValue: TTisGridColumn);
    procedure SetFocusedRow(aValue: PDocVariantData);
    procedure SetOnCutToClipboard(aValue: TNotifyEvent);
    function GetOptions: TStringTreeOptions;
    procedure SetOptions(const aValue: TStringTreeOptions);
    function GetSelectedRows: TDocVariantData;
    /// select all the nodes matching the aValue array list of TDocVariantData
    procedure SetSelectedRows(const aValue: TDocVariantData);
    function GetSelectedObjects: PDocVariantDataDynArray;
    function GetSelectedRow: TDocVariantData;
    procedure SetSelectedRow(aValue: TDocVariantData);
    procedure SetSelectedAndTotalLabel(aValue: TLabel);
    procedure WMKeyDown(var Message: TLMKeyDown); message LM_KEYDOWN;
  protected
    // ------------------------------- new constants -------------------------------
    const DefaultPopupMenuOptions = [
      pmoShowFind, pmoShowFindNext, pmoShowCut, pmoShowCopy, pmoShowCopyCell,
      pmoShowPaste, pmoShowDelete, pmoShowSelectAll, pmoShowCustomizeColumns];
    const DefaultExportFormatOptions = [efoCsv, efoJson];
    const DefaultWantTabs = True;
    // ------------------------------- new fields ----------------------------------
  protected
    DefaultCsvSeparator: string;
    // ------------------------------- inherited methods ---------------------------
    procedure Loaded; override;
    function GetPopupMenu: TPopupMenu; override;
    procedure SetPopupMenu(aValue: TPopupMenu); // it should be virtual and protected on TControl class
    procedure WndProc(var Message: TLMessage); override;
    /// after cell editing to set Data
    procedure DoNewText(aNode: PVirtualNode; aColumn: TColumnIndex;
      const aText: string); override;
    procedure DoGetText(aNode: PVirtualNode; aColumn: TColumnIndex;
      aTextType: TVSTTextType; var aText: string); override;
    procedure DoInitNode(aParentNode, aNode: PVirtualNode;
      var aInitStates: TVirtualNodeInitStates); override;
    procedure DoMeasureItem(aTargetCanvas: TCanvas; aNode: PVirtualNode; var aNodeHeight: Integer); override;
    function DoCompare(aNode1, aNode2: PVirtualNode; aColumn: TColumnIndex): Integer; override;
    function GetColumnClass: TVirtualTreeColumnClass; override;
    function GetOptionsClass: TTreeOptionsClass; override;
    /// it will check if OnCreateEditor was implemented first
    // - if user did not implement this event, a default implementation will be used
    function DoCreateEditor(aNode: PVirtualNode; aColumn: TColumnIndex): IVTEditLink; override;
    procedure PrepareCell(var PaintInfo: TVTPaintInfo;
      WindowOrgX, MaxWidth: integer); override;
    /// multiselection display management
    procedure DoBeforeCellPaint(ACanvas: TCanvas; Node: PVirtualNode;
      Column: TColumnIndex; CellPaintMode: TVTCellPaintMode; CellRect: TRect;
      var ContentRect: TRect); override;
    procedure DoTextDrawing(var PaintInfo: TVTPaintInfo; const AText: string;
      CellRect: TRect; DrawFormat: cardinal); override;
    procedure DoBeforeItemErase(aCanvas: TCanvas; aNode: PVirtualNode;
      const aItemRect: TRect; var aColor: TColor;
      var EraseAction: TItemEraseAction); override;
    function DoKeyAction(var CharCode: Word; var Shift: TShiftState): Boolean; override;
    procedure Notification(aComponent: TComponent; aOperation: TOperation); override;
    procedure DoAutoAdjustLayout(const aMode: TLayoutAdjustmentPolicy;
      const aXProportion, {%H-}aYProportion: Double); override;
    procedure DoChange(Node: PVirtualNode); override;
    function GetHeaderClass: TVTHeaderClass; override;
    property RootNodeCount stored False;
    // ----------------------------------- new methods -----------------------------
    /// standard menu management
    procedure FillPopupMenu({%H-}aSender: TObject);
    function FindText(const aText: string): PVirtualNode;
    procedure FindDlgFind(aSender: TObject);
    /// add aData into Data property
    // - will test if it is an array or object
    // - returns TRUE if something has been added
    function Add(aData: PDocVariantData): Boolean;
    function DoCompareByRow(const aPropertyName: RawUtf8; const aRow1,
      aRow2: PDocVariantData): PtrInt; virtual;
    procedure DoFindText({%H-}aSender: TObject);
    procedure DoFindNext({%H-}aSender: TObject);
    procedure DoFindReplace({%H-}aSender: TObject);
    procedure DoUndoLastUpdate({%H-}aSender: TObject); virtual;
    procedure DoRevertRecord({%H-}aSender: TObject); virtual;
    procedure DoExport({%H-}aSender: TObject); virtual;
    procedure DoExportCustomContent(aSource: TVSTTextSourceType; var aBuffer: RawUtf8); virtual;
    procedure DoCopyToClipboard({%H-}aSender: TObject); virtual;
    procedure DoCopyCellToClipboard({%H-}aSender: TObject); virtual;
    procedure DoCopySpecialToClipboard({%H-}aSender: TObject); virtual;
    procedure DoCutToClipboard(aSender: TObject); virtual;
    procedure DoDeleteRows({%H-}aSender: TObject); virtual;
    procedure DoPaste({%H-}aSender: TObject); virtual;
    procedure DoSelectAllRows({%H-}aSender: TObject); virtual;
    procedure DoPrint({%H-}aSender: TObject); virtual;
    procedure DoCustomizeColumns({%H-}aSender: TObject); virtual;
    procedure DoAdvancedCustomizeColumns({%H-}aSender: TObject); virtual;
    procedure DoExpandAll({%H-}aSender: TObject); virtual;
    procedure DoCollapseAll({%H-}aSender: TObject); virtual;
    /// performs OnCustonEditor event, if it was assigned
    procedure DoCustomEditor(const aColumn: TTisGridColumn; out aControl: TTisGridControl); virtual;
    /// performs OnEditorLookup event, if it was assigned
    procedure DoEditorLookup(const aColumn: TTisGridColumn; out
      aControl: TTisGridControl; var aHandled: Boolean); virtual;
    /// performs OnPrepareEditor event, if it was assigned
    procedure DoPrepareEditor(const aColumn: TTisGridColumn; aControl: TTisGridControl); virtual;
    procedure DoEditValidated(const aColumn: TTisGridColumn; const aCurValue: Variant;
      var aNewValue: Variant; var aAbort: Boolean); virtual;
    /// it returns the filter for the Save Dialog, when user wants to export data
    // - it will add file filters based on ExportFormatOptions property values
    // - you can override this method to customize default filters
    function GetExportDialogFilter: string; virtual;
    /// it restore original settings from original design
    procedure RestoreSettings;
    // ------------------------------- new properties ------------------------------
    property ColumnToFind: integer read fColumnToFind write SetColumnToFind;
    property TextToFind: string read fTextToFind write fTextToFind;
    property TextFound: boolean read fTextFound write fTextFound;
  public
    const TREEMODE_OPTIONS = [toShowRoot, toShowButtons, toShowTreeLines];
  public
    /// primary construtor
    constructor Create(AOwner: TComponent); override;
    /// destructor
    destructor Destroy; override;
    // ------------------------------- inherited methods ---------------------------
    procedure FixDesignFontsPPI(const ADesignTimePPI: Integer); override;
    procedure ScaleFontsPPI(const AToPPI: Integer; const AProportion: Double); override;
    /// it will clear Data and everything else related
    procedure Clear; override;
    // ----------------------------------- new methods -----------------------------
    /// cast aNode in PDocVariantData
    // - will get the same aNode.Index in Data
    // - if aNode is nil, it will use FocusedNode as default
    function GetNodeDataAsDocVariant(aNode: PVirtualNode = nil): PDocVariantData;
    /// refresh the grid using Data content
    // - call LoadData, if you change Data content directly
    procedure LoadData;
    /// it will try load aJson into Data and create Columns from it
    // - it will not clean previous columns, if they exists
    // - return TRUE if success, otherwise FALSE, with a Dialog error if aShowError is TRUE
    function TryLoadAllFrom(const aJson: string; aShowError: Boolean = True): Boolean;
    /// export data
    // - it will export using the format that matchs to aFileName extension
    // - if extension do not exist in ExportFormatOptions, it will use OnExportCustomContent event to get the content
    // - use aSelection as tstAll to export all nodes - default
    // - use aSelection as tstSelected to export only selected nodes
    procedure ExportData(const aFileName: TFileName; const aSelection: TVSTTextSourceType = tstAll);
    /// get all checked rows
    function CheckedRows: TDocVariantData;
    procedure SetFocusedRowNoClearSelection(aValue: PDocVariantData);
    /// returns the cell value
    // - return the supplied default if aColName is not found
    function GetCellData(aNode: PVirtualNode; const aColName: RawUtf8;
      aDefault: PDocVariantData = nil): PDocVariantData;
    /// returns the cell value as string
    // - return the supplied default if aColName is not found
    function GetCellDataAsString(aNode: PVirtualNode; const aColName: RawUtf8;
      const aDefault: string = ''): string;
    /// returns a list of nodes which is matching exactly to aData
    // - use aUseKeyFieldsList for search only into fields from KeyFieldsList
    // but if KeyFieldsList is empty, it will be the same as passing FALSE
    function GetNodesBy(aData: PDocVariantData; aUseKeyFieldsList: Boolean = False): TNodeArray; overload;
    /// returns the first node which is matching to aData
    // - use aUseKeyFieldsList for search only into fields from KeyFieldsList
    // but if KeyFieldsList is empty, it will be the same as passing FALSE
    // - use aRowPosition to get a specific row, in case of returning more than one
    function GetNodeBy(aData: PDocVariantData; aUseKeyFieldsList: Boolean = False;
      aRowPosition: PtrInt = 0): PVirtualNode; overload;
    /// returns a list of nodes which is matching to key and value
    function GetNodesBy(const aKey, aValue: RawUtf8): TNodeArray; overload;
    function FindColumnByPropertyName(const aPropertyName: RawUtf8): TTisGridColumn;
    function FindColumnByIndex(const aIndex: TColumnIndex): TTisGridColumn;
    /// append a list of rows to the Grid
    // - use aAllowDuplicates=TRUE for allow duplicate rows
    // - use aCreateColumns=TRUE for create columns if they not exists yet
    procedure AddRows(aData: PDocVariantData; aAllowDuplicates: Boolean = True; aCreateColumns: Boolean = True);
    /// append rows, calling OnBeforePaste
    procedure PasteRows(aRows: PDocVariantData);
    /// delete a list of rows that match with aRows
    procedure DeleteRows(aRows: PDocVariantData);
    /// ask to delete selected rows
    procedure DeleteSelectedRows;
    /// redraw the rows matching this record
    procedure InvalidateNodeByDocVariant(const aData: PDocVariantData);
    /// it will call Clear and also clear everything else as Columns, etc
    procedure ClearAll; virtual;
    /// sort columns headers collection based on manual positioning
    procedure ReorderColumns;
    /// shows grid editor
    procedure Customize;
    /// source events handling
    procedure NotifyChange(aEventType: TTisDataEvent; aRow: PDocVariantData;
      const {%H-}aOldValues, {%H-}aNewValues: TDocVariantData);
    /// add columns based on Data content
    procedure CreateColumnsFromData(aAutoFitColumns, aAppendMissingAsHidden: Boolean);
    /// export Data to CSV format
    function ContentToCsv(aSource: TVSTTextSourceType; const aSeparator: string = ',';
      aColumnsVisibleOnly: Boolean = True; aColumnsTranslated: Boolean = True): RawUtf8;
    /// export Data to JSON format
    function ContentToJson(aSource: TVSTTextSourceType; aColumnsVisibleOnly: Boolean = True): RawUtf8;
    /// force refresh the "Selected / Total : %d/%d" label
    procedure UpdateSelectedAndTotalLabel;
    /// save Settings to an IniFile
    procedure SaveSettingsToIni(const aFileName: TFileName);
    /// load Settings from an IniFile
    procedure LoadSettingsFromIni(const aFileName: TFileName);
    /// it returns TRUE if tree mode options were settled
    function IsTreeMode: Boolean;
    /// if using TreeMode, it will expand all
    procedure ExpandAllNodes;
    /// if using TreeMode, it will collapse all
    procedure CollapseAllNodes;
    /// it will search aText in all columns
    // - if found, focus will go to the grid
    function Search(const aText: string): Boolean;
    // ------------------------------- inherited events ----------------------------
    property OnCompareNodes; // hiding from Object Inspector, use OnCompareByRow event instead
    // ------------------------------- new properties ------------------------------
    /// direct access to the low-level internal data
    // - if you change its content directly, you should call Invalidate or LoadData for VirtualTree be aware about it
    property Data: TDocVariantData
      read fData write SetData;
    property MetaData: RawUtf8 read GetMetaData write SetMetaData;
    /// returns a copy of the object from the main selected row
    // - do not use this to edit Data values, instead use SelectedObjects
    property SelectedRow: TDocVariantData
      read GetSelectedRow write SetSelectedRow;
    /// returns a copy of objects from selected rows
    // - do not use this to edit Data values, instead use SelectedObjects
    property SelectedRows: TDocVariantData
      read GetSelectedRows write SetSelectedRows;
    /// returns objects from selected rows
    property SelectedObjects: PDocVariantDataDynArray
      read GetSelectedObjects;
    property FocusedRow: PDocVariantData
      read GetFocusedRow write SetFocusedRow;
    property FocusedColumnObject: TTisGridColumn
      read GetFocusedColumnObject write SetFocusedColumnObject;
    /// saving and restoring user customizations
    property Settings: Variant read GetSettings write SetSettings;
  published
    // ------------------------------- inherited properties ------------------------
    property Action;
    property Align;
    property Alignment;
    property Anchors;
    property AnimationDuration;
    property AutoExpandDelay;
    property AutoScrollDelay;
    property AutoScrollInterval;
    property Background;
    property BackgroundOffsetX;
    property BackgroundOffsetY;
    property BiDiMode;
    property BorderSpacing;
    property BorderStyle default bsSingle;
    property BottomSpace;
    property ButtonFillMode;
    property ButtonStyle;
    property BorderWidth;
    property ChangeDelay;
    property CheckImageKind;
    property ClipboardFormats;
    property Color;
    property Colors;
    property Constraints;
    property DefaultNodeHeight;
    property DefaultPasteMode;
    property DragCursor;
    property DragHeight;
    property DragKind;
    property DragImageKind;
    property DragMode;
    property DragOperations;
    property DragType;
    property DragWidth;
    property DrawSelectionMode;
    property EditDelay;
    property Enabled;
    property Font;
    property Header;
    property HintMode;
    property HotCursor;
    property Images;
    property IncrementalSearch;
    property IncrementalSearchDirection;
    property IncrementalSearchStart;
    property IncrementalSearchTimeout;
    property Indent;
    property LineMode;
    property LineStyle;
    property Margin;
    property NodeAlignment;
    property ParentBiDiMode;
    property ParentColor default False;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu read GetPopupMenu write SetPopupMenu;
    property ScrollBarOptions;
    property SelectionBlendFactor;
    property SelectionCurveRadius;
    property ShowHint;
    property StateImages;
    property TabOrder;
    property TabStop default True;
    property TextMargin;
    property Visible;
    property WantTabs default DefaultWantTabs;
    // ------------------------------- new properties ------------------------------
    property SelectedAndTotalLabel: TLabel
      read fSelectedAndTotalLabel write SetSelectedAndTotalLabel;
    property TreeOptions: TStringTreeOptions
      read GetOptions write SetOptions;
    property KeyFieldsList: TStringDynArray
      read fKeyFieldsList;
    property KeyFieldsNames: string
      read GetKeyFieldsNames write SetKeyFieldsNames;
    property ParentKeyFieldsNames: string
      read GetParentKeyFieldsNames write SetParentKeyFieldsNames;
    property GridSettings: string
      read GetGridSettings write SetGridSettings stored False;
    property ZebraColor: TColor
      read fZebraColor write fZebraColor;
    property ZebraPaint: Boolean
      read fZebraPaint write fZebraPaint stored True default False;
    property NodeOptions: TTisNodeOptions
      read fNodeOptions write fNodeOptions;
    property PopupMenuOptions: TTisPopupMenuOptions
      read fPopupMenuOptions write fPopupMenuOptions default DefaultPopupMenuOptions;
    property ExportFormatOptions: TTisGridExportFormatOptions
      read fExportFormatOptions write fExportFormatOptions default DefaultExportFormatOptions;
    // ------------------------------- inherited events ----------------------------
    property OnAdvancedHeaderDraw;
    property OnAfterAutoFitColumns;
    property OnAfterCellPaint;
    property OnAfterGetMaxColumnWidth;
    property OnAfterItemErase;
    property OnAfterItemPaint;
    property OnAfterPaint;
    property OnBeforeAutoFitColumns;
    property OnBeforeCellPaint;
    property OnBeforeGetMaxColumnWidth;
    property OnBeforeItemErase;
    property OnBeforeItemPaint;
    property OnBeforePaint;
    property OnCanSplitterResizeColumn;
    property OnChange;
    property OnChecked;
    property OnChecking;
    property OnClick;
    property OnCollapsed;
    property OnCollapsing;
    property OnColumnClick;
    property OnColumnDblClick;
    property OnColumnResize;
    property OnContextPopup;
    property OnCreateDataObject;
    property OnCreateDragManager;
    property OnCreateEditor;
    property OnDblClick;
    property OnDragAllowed;
    property OnDragOver;
    property OnDragDrop;
    property OnDrawText;
    property OnEditCancelled;
    property OnEdited;
    property OnEditing;
    property OnEndDock;
    property OnEndDrag;
    property OnEnter;
    property OnExit;
    property OnExpanded;
    property OnExpanding;
    property OnFocusChanged;
    property OnFocusChanging;
    property OnFreeNode;
    property OnGetCellIsEmpty;
    property OnGetCursor;
    property OnGetHeaderCursor;
    property OnPaintText;
    property OnGetHelpContext;
    property OnGetImageIndex;
    property OnGetImageIndexEx;
    property OnGetImageText;
    property OnGetHint;
    property OnGetLineStyle;
    property OnGetPopupMenu;
    property OnGetUserClipboardFormats;
    property OnHeaderClick;
    property OnHeaderDblClick;
    property OnHeaderDragged;
    property OnHeaderDraggedOut;
    property OnHeaderDragging;
    property OnHeaderDraw;
    property OnHeaderDrawQueryElements;
    property OnHeaderMouseDown;
    property OnHeaderMouseMove;
    property OnHeaderMouseUp;
    property OnHotChange;
    property OnIncrementalSearch;
    property OnInitChildren;
    property OnInitNode;
    property OnKeyAction;
    property OnKeyDown;
    property OnKeyPress;
    property OnKeyUp;
    property OnLoadNode;
    property OnMeasureItem;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    property OnMouseWheel;
    property OnNewText;
    property OnNodeCopied;
    property OnNodeCopying;
    property OnNodeMoved;
    property OnNodeMoving;
    property OnPaintBackground;
    property OnRenderOLEData;
    property OnResetNode;
    property OnResize;
    property OnSaveNode;
    property OnScroll;
    property OnShortenString;
    property OnShowScrollbar;
    property OnStartDock;
    property OnStartDrag;
    property OnStateChange;
    property OnStructureChange;
    property OnUpdating;
    // ------------------------------- new events ----------------------------------
    property OnGetText: TOnGridGetText
      read fOnGetText write fOnGetText;
    property OnCutToClipboard: TNotifyEvent
      read fOnCutToClipboard write SetOnCutToClipboard;
    /// event to manipulate data before change the internal Data
    // - use it for check/change the aData argument, before assign it, and/or abort the process
    property OnBeforeDataChange: TOnGridBeforeDataChange
      read fOnBeforeDataChange write fOnBeforeDataChange;
    /// event to manipulate data after the internal Data changed
    // - as used by TTisGrid.OnAfterDataChage
    property OnAfterDataChange: TOnGridAfterDataChange
      read fOnAfterDataChange write fOnAfterDataChange;
    property OnBeforePaste: TOnGridPaste
      read fOnBeforePaste write fOnBeforePaste;
    /// event to manipulate rows before deleting them
    // - use it for change the rows or abort the process by assign True to aAbort
    // - if you do not use this event, by default it will ask user about deletion
    // and, if the answer was yes, selected rows will be deleted
    // - you can also supress the user dialog, asking it should continue, by assign False to aAskUser
    property OnBeforeDeleteRows: TOnGridBeforeDeleteRows
      read fOnBeforeDeleteRows write fOnBeforeDeleteRows;
    /// comparing rows of objects
    // - aPropertyName is the header column that was clicked
    // - aRow1, aRow2 are the whole lines that should be compared
    // - assign aHandle to True if you did some comparison, otherwise assign to False or do nothing
    // to let grid do the job itself
    // - on user callback, the compararison could be like this:
    // !aRow1.CompareObject([aPropertyName], aRow2); // you can use other columns together too
    // !aHandled := True;
    property OnCompareByRow: TOnGridCompareByRow
      read fOnCompareByRow write fOnCompareByRow;
    property OnAfterFillPopupMenu: TNotifyEvent
      read fOnAfterFillPopupMenu write fOnAfterFillPopupMenu;
    /// event that allows users customize the edit control instance, creating a new one,
    // - you should return an instance in aControl
    // - you can use OnPrepareEditor to customize some properties
    property OnCustomEditor: TOnGridCustomEditor
      read fOnCustomEditor write fOnCustomEditor;
    /// event that simplifies the use of a TisSearchEdit as Edit Control
    // - you may want to use this event instead OnCustomEditor for TisSearchEdit
    property OnEditorLookup: TOnGridEditorLookup
      read fOnEditorLookup write fOnEditorLookup;
    /// event that allows users to change some edit control properties, before it shows up
    property OnPrepareEditor: TOnGridPrepareEditor
      read fOnPrepareEditor write fOnPrepareEditor;
    property OnEditValidated: TOnGridEditValidated
      read fOnEditValidated write fOnEditValidated;
    property OnGridExportCustomContent: TOnGridExportCustomContent
      read fOnGridExportCustomContent write fOnGridExportCustomContent;
  end;

resourcestring
  rsNoRecordFind = 'No more record found for "%s"';
  rsPrintOn = 'Printed on';
  rsPage = 'Page';
  rsConfirmation = 'Confirm';
  rsUndoLastUpdate = 'Undo last change';
  rsRevertRecord = 'Revert to initial record';
  rsFind = 'Search...';
  rsFindNext = 'Find next';
  rsFindReplace = 'Find and replace...';
  rsCopy = 'Copy';
  rsCopyCell = 'Copy cell';
  rsCopySpecial = 'Copy special...';
  rsCut = 'Cut';
  rsPaste = 'Paste';
  rsInsert = 'Insert';
  rsDelete = 'Delete';
  rsDeleteRows = 'Delete selected rows';
  rsConfDeleteRow = 'Confirm the deletion of the %d selected rows ?';
  rsSelectAll = 'Select all rows';
  rsExportSelected = 'Export selected rows to file...';
  rsExportAll = 'Export all rows to file...';
  rsPrint = 'Print...';
  rsExpandAll = 'Expand all';
  rsCollapseAll = 'Collapse all';
  rsCustomizeColumns = 'Customize columns...';
  rsAdvancedCustomizeColumns = 'Advanced customize of table...';
  rsShowAllColumns = 'Show all columns';
  rsHideAllColumns = 'Hide all columns';
  rsRestoreDefaultColumns = 'Restore default columns';

implementation

uses
  IniFiles,
  Variants,
  tis.ui.grid.editor,
  tis.ui.grid.copyspecial;

{ TTisColumnDataTypeAdapter }

const
  COLUMN_DATA_TYPES: array[TTisColumnDataType] of record
    Caption: string;
  end = (
    (Caption: 'String'),
    (Caption: 'Date'),
    (Caption: 'Time'),
    (Caption: 'DateTime'),
    (Caption: 'Integer'),
    (Caption: 'Float'),
    (Caption: 'Boolean'),
    (Caption: 'Memo'),
    (Caption: 'Password')
  );

{ TTisColumnDataTypeAdapter }

function TTisColumnDataTypeAdapter.EnumToCaption(const aValue: TTisColumnDataType): string;
begin
  result := COLUMN_DATA_TYPES[aValue].Caption;
end;

function TTisColumnDataTypeAdapter.EnumToRawUtf8(
  const aValue: TTisColumnDataType): RawUtf8;
begin
  result := StringToUtf8(EnumToCaption(aValue));
end;

function TTisColumnDataTypeAdapter.EnumToIndex(const aValue: TTisColumnDataType): Integer;
begin
  result := ord(aValue);
end;

function TTisColumnDataTypeAdapter.CaptionToEnum(const aValue: string): TTisColumnDataType;
var
  i: TTisColumnDataType;
begin
  result := low(TTisColumnDataType);
  for i := low(COLUMN_DATA_TYPES) to high(COLUMN_DATA_TYPES) do
    if COLUMN_DATA_TYPES[i].Caption = aValue then
    begin
      result := i;
      exit;
    end;
end;

function TTisColumnDataTypeAdapter.RawUtf8ToEnum(const aValue: RawUtf8): TTisColumnDataType;
begin
  result := CaptionToEnum(Utf8ToString(aValue));
end;

function TTisColumnDataTypeAdapter.CaptionToIndex(const aValue: string): Integer;
begin
  result := ord(CaptionToEnum(aValue));
end;

procedure TTisColumnDataTypeAdapter.EnumsToStrings(aDest: TStrings;
  const aCustom: TTisColumnDataTypes);
var
  i: TTisColumnDataType;
begin
  for i := low(TTisColumnDataType) to high(TTisColumnDataType) do
    if i in aCustom then
      aDest.Append(EnumToCaption(i));
end;

{ TTisGridEditLink }

procedure TTisGridEditLink.EditKeyDown(aSender: TObject; var Key: Word; Shift: TShiftState);
var
  vCanAdvance: Boolean;
begin
  vCanAdvance := True;
  case Key of
    VK_ESCAPE:
      if vCanAdvance then
      begin
        fControl.Internal.OnExit := nil; // prevents an Access Violation
        fGrid.SetFocusSafe; // needed if grid.parent is a Frame
        fGrid.CancelEditNode;
        Key := 0;
      end;
    VK_RETURN:
      // consider special cases before finishing edit mode
      if not (ssShift in Shift) then
      begin
        fGrid.EndEditNode;
        Key := 0;
      end;
    VK_UP,
    VK_DOWN:
      begin  { TODO -omsantos : should be refactored, as these checks should came from the instance }
        // consider special cases before finishing edit mode
        vCanAdvance := Shift = [];
        if fControl.Internal is TCustomComboBox then
          vCanAdvance := vCanAdvance and not TCustomComboBox(fControl.Internal).DroppedDown;
        if fControl.Internal is TCustomMemo then
          vCanAdvance := False;
        if vCanAdvance then
        begin
          // forward the keypress to the tree
          // it will asynchronously change the focused node
          PostMessage(fGrid.Handle, LM_KEYDOWN, Key, 0);
          Key := 0;
        end;
      end;
  end;
end;

procedure TTisGridEditLink.EditExit(aSender: TObject);
begin
  if Assigned(fControl) then
  begin
    if (toAutoAcceptEditChange in fGrid.TreeOptions.StringOptions) then
      fGrid.EndEditNode
    else
      fGrid.CancelEditNode;
  end;
end;

procedure TTisGridEditLink.SetupControlClasses;
begin
  ControlClasses[cdtString] := TTisGridEditControl;
  ControlClasses[cdtDate] := TTisGridDateEditControl;
  ControlClasses[cdtTime] := TTisGridTimeEditControl;
  ControlClasses[cdtDateTime] := TTisGridDateTimeEditControl;
  ControlClasses[cdtInteger] := TTisGridIntegerEditControl;
  ControlClasses[cdtFloat] := TTisGridFloatEditControl;
  ControlClasses[cdtBoolean] := TTisGridBooleanEditControl;
  ControlClasses[cdtMemo] := TTisGridMemoControl;
  ControlClasses[cdtPassword] := TTisGridPasswordEditControl;
end;

function TTisGridEditLink.NewControl(aColumn: TTisGridColumn): TTisGridControl;
var
  vHandled: Boolean;
begin
  fGrid.DoCustomEditor(aColumn, result);
  if result = nil then
  begin
    vHandled := False;
    fGrid.DoEditorLookup(aColumn, result, vHandled);
    if not vHandled then
      result := ControlClasses[aColumn.DataType].Create;
  end;
  if Assigned(result) then
  begin
    result.SetOnKeyDown(@EditKeyDown);
    result.SetOnExit(@EditExit);
    result.Internal.Visible := False;
    result.Internal.Parent := fGrid;
  end;
end;

constructor TTisGridEditLink.Create;
begin
  inherited Create;
  SetupControlClasses;
end;

destructor TTisGridEditLink.Destroy;
begin
  fControl.Free;
  inherited Destroy;
end;

function TTisGridEditLink.BeginEdit: Boolean; stdcall;
begin
  result := True;
  fControl.Internal.Show;
  fControl.Internal.SetFocus;
end;

function TTisGridEditLink.CancelEdit: Boolean; stdcall;
begin
  result := True;
  fControl.Internal.Hide;
end;

function TTisGridEditLink.EndEdit: Boolean; stdcall;
var
  vDoc: PDocVariantData;
  vCol: TTisGridColumn;
  vAborted: Boolean;
  vCur, vNew: Variant;
begin
  result := True;
  vDoc := fGrid.GetNodeDataAsDocVariant(fNode);
  vCol := fGrid.FindColumnByIndex(fColumn);
  vAborted := False;
  vCur := vDoc^.GetValueOrNull(vCol.PropertyName);
  vNew := fControl.GetValue;
  fGrid.DoEditValidated(vCol, vCur, vNew, vAborted);
  try
    if vAborted then
      exit;
    if VarIsNull(vNew) then
    begin
      if not vCol.Required then
        vDoc^.Value[vCol.PropertyName] := NULL;
    end
    else
      case vCol.DataType of
        cdtString, cdtMemo:
          vDoc^.S[vCol.PropertyName] := VarToStr(vNew);
        cdtDate, cdtTime, cdtDateTime:
          vDoc^.U[vCol.PropertyName] := DateTimeToIso8601Text(vNew);
        cdtInteger:
          vDoc^.I[vCol.PropertyName] := vNew;
        cdtFloat:
          vDoc^.D[vCol.PropertyName] := vNew;
        cdtBoolean:
          vDoc^.B[vCol.PropertyName] := vNew;
      else
        vDoc^.S[vCol.PropertyName] := VarToStr(vNew);
      end;
  finally
    FreeAndNil(fControl); // for do not perform any event from it
    fGrid.InvalidateNode(fNode);
    fGrid.SetFocusSafe;
  end;
end;

function TTisGridEditLink.GetBounds: TRect; stdcall;
begin
  result := fControl.Internal.BoundsRect;
end;

function TTisGridEditLink.PrepareEdit(aTree: TBaseVirtualTree; aNode: PVirtualNode;
  aColumn: TColumnIndex): Boolean; stdcall;
var
  vDoc: PDocVariantData;
  vCol: TTisGridColumn;
begin
  result := True;
  fGrid := aTree as TTisGrid;
  fNode := aNode;
  fColumn := aColumn;
  FreeAndNil(fControl);
  vDoc := fGrid.GetNodeDataAsDocVariant(fNode);
  vCol := fGrid.FindColumnByIndex(fColumn);
  fControl := NewControl(vCol);
  fControl.ReadOnly := vCol.ReadOnly;
  case vCol.DataType of
    cdtString, cdtMemo:
      fControl.SetValue(vDoc^.S[vCol.PropertyName]);
    cdtDate, cdtTime, cdtDateTime:
      fControl.SetValue(Iso8601ToDateTime(vDoc^.U[vCol.PropertyName]));
    cdtInteger:
      fControl.SetValue(vDoc^.I[vCol.PropertyName]);
    cdtFloat:
      fControl.SetValue(vDoc^.D[vCol.PropertyName]);
    cdtBoolean:
      fControl.SetValue(vDoc^.B[vCol.PropertyName]);
  else
    fControl.SetValue(vDoc^.S[vCol.PropertyName]);
  end;
  fGrid.DoPrepareEditor(vCol, fControl);
end;

procedure TTisGridEditLink.ProcessMessage(var aMessage: TLMessage); stdcall;
begin
  PostMessage(fControl.Internal.Handle, aMessage.Msg, aMessage.wParam, aMessage.lParam);
end;

procedure TTisGridEditLink.SetBounds(R: TRect); stdcall;
var
  vDummy: Integer;
begin
  // Since we don't want to activate grid extensions in the tree (this would
  // influence how the selection is drawn)
  // we have to set the edit's width explicitly to the width of the column.
  fGrid.Header.Columns.GetColumnBounds(fColumn, vDummy, R.Right);
  fControl.Internal.BoundsRect := R;
end;

{ TTisGridColumn }

function TTisGridColumn.GetTitle: TCaption;
begin
  result := GetText;
end;

procedure TTisGridColumn.SetTitle(const aValue: TCaption);
begin
  SetText(aValue);
end;

procedure TTisGridColumn.SetPropertyName(const aValue: RawUtf8);
begin
  if fPropertyName = aValue then
    exit;
  if (Text = '') or (Text = fPropertyName) then
    Text := aValue;
  fPropertyName := aValue;
end;

constructor TTisGridColumn.Create(aCollection: TCollection);
begin
  inherited Create(aCollection);
  Options := Options + [coWrapCaption];
  fDataType := DefaultDataType;
  fRequired := DefaultRequired;
  fReadOnly := DefaultReadOnly;
end;

procedure TTisGridColumn.Assign(aSource: TPersistent);
var
  c: TTisGridColumn;
begin
  inherited Assign(aSource);
  if aSource is TTisGridColumn then
  begin
    c := TTisGridColumn(aSource);
    PropertyName := c.PropertyName;
    DataType := c.DataType;
    Required := c.Required;
    ReadOnly := c.ReadOnly;
  end;
end;

{ TTisGridColumns }

procedure TTisGridColumns.HandleClick(P: TPoint; aButton: TMouseButton; aForce,
  aDblClick: Boolean);
var
  idx: Integer;
begin
  if (csDesigning in Header.Treeview.ComponentState) then
    exit;
  idx := ColumnFromPosition(P);
  if (hoHeaderClickAutoSort in Header.Options) and (aButton = mbLeft) and (idx >= 0) then
  begin
    if (idx = Header.SortColumn) and (Header.SortDirection = sdDescending) then
      Header.SortColumn := -1
    else
      inherited HandleClick(P, aButton, aForce, aDblClick);
  end;
end;

procedure TTisGridColumns.Assign(aSource: TPersistent);
var
  i: Integer;
  vCol: TTisGridColumn;
  vSource: TTisGridColumns;
begin
  if aSource is TTisGridColumns then
  begin
    Header.Columns.Clear;
    vSource := aSource as TTisGridColumns;
    for i := 0 to vSource.Header.Columns.Count-1 do
    begin
      vCol := Header.Columns.Add as TTisGridColumn;
      vCol.Assign(vSource.Header.Columns[i]);
      vCol.Options := vSource.Header.Columns[i].Options;
    end;
  end
  else
    inherited Assign(aSource);
end;

{ TTisGridExportFormatOptionAdapter }

const
  GRID_EXPORT_FORMAT_OPTIONS: array[TTisGridExportFormatOption] of record
    Caption: string;
    Extension: string;
    Filter: string;
  end = (
    (Caption: 'RTF'; Extension: '.rtf'; Filter: 'RTF (*.rtf)|*.rtf'),
    (Caption: 'HTML'; Extension: '.html'; Filter: 'HTML (*.html)|*.html'),
    (Caption: 'Text'; Extension: '.text'; Filter: 'Text (*.txt)|*.txt'),
    (Caption: 'CSV'; Extension: '.csv'; Filter: 'CSV (*.csv)|*.csv'),
    (Caption: 'JSON'; Extension: '.json'; Filter: 'JSON (*.json)|*.json')
  );

function TTisGridExportFormatOptionAdapter.EnumToCaption(
  const aValue: TTisGridExportFormatOption): string;
begin
  result := GRID_EXPORT_FORMAT_OPTIONS[aValue].Caption;
end;

function TTisGridExportFormatOptionAdapter.CaptionToEnum(const aValue: string): TTisGridExportFormatOption;
var
  i: TTisGridExportFormatOption;
begin
  result := low(TTisGridExportFormatOption);
  for i := low(GRID_EXPORT_FORMAT_OPTIONS) to high(GRID_EXPORT_FORMAT_OPTIONS) do
    if GRID_EXPORT_FORMAT_OPTIONS[i].Caption = aValue then
    begin
      result := i;
      exit;
    end;
end;

procedure TTisGridExportFormatOptionAdapter.EnumsToStrings(aDest: TStrings;
  const aCustom: TTisGridExportFormatOptions);
var
  i: TTisGridExportFormatOption;
begin
  for i := low(TTisGridExportFormatOption) to high(TTisGridExportFormatOption) do
    if i in aCustom then
      aDest.Append(EnumToCaption(i));
end;

function TTisGridExportFormatOptionAdapter.ExtensionToEnum(
  const aValue: TFileName): TTisGridExportFormatOption;
var
  i: TTisGridExportFormatOption;
begin
  result := low(TTisGridExportFormatOption);
  for i := low(GRID_EXPORT_FORMAT_OPTIONS) to high(GRID_EXPORT_FORMAT_OPTIONS) do
    if GRID_EXPORT_FORMAT_OPTIONS[i].Extension = aValue then
    begin
      result := i;
      exit;
    end;
end;

function TTisGridExportFormatOptionAdapter.EnumToFilter(
  const aValue: TTisGridExportFormatOption): string;
begin
  result := GRID_EXPORT_FORMAT_OPTIONS[aValue].Filter;
end;

{ TTisGridTextSourceTypeAdapter }

const
  GRID_TEXT_SOURCE_TYPES: array[TVSTTextSourceType] of record
    Caption: string;
  end = (
    (Caption: 'All'),
    (Caption: 'Initialized'),
    (Caption: 'Selected'),
    (Caption: 'CutCopySet'),
    (Caption: 'Visible'),
    (Caption: 'Checked')
  );

function TTisGridTextSourceTypeAdapter.EnumToCaption(
  const aValue: TVSTTextSourceType): string;
begin
  result := GRID_TEXT_SOURCE_TYPES[aValue].Caption;
end;

function TTisGridTextSourceTypeAdapter.CaptionToEnum(const aValue: string): TVSTTextSourceType;
var
  i: TVSTTextSourceType;
begin
  result := low(TVSTTextSourceType);
  for i := low(GRID_TEXT_SOURCE_TYPES) to high(GRID_TEXT_SOURCE_TYPES) do
    if GRID_TEXT_SOURCE_TYPES[i].Caption = aValue then
    begin
      result := i;
      exit;
    end;
end;

procedure TTisGridTextSourceTypeAdapter.EnumsToStrings(aDest: TStrings;
  const aCustom: TTisGridTextSourceTypes);
var
  i: TVSTTextSourceType;
begin
  for i := low(TVSTTextSourceType) to high(TVSTTextSourceType) do
    if i in aCustom then
      aDest.Append(EnumToCaption(i));
end;

{ TTisGridHeaderPopupMenu }

type
  TVirtualTreeCast = class(TBaseVirtualTree); // necessary to make the header accessible

procedure TTisGridHeaderPopupMenu.RemoveAutoItems;
var
  i: Integer;
begin
  i := Items.Count;
  while i > 0 do
  begin
    Dec(i);
    if Items[i] is TTisGridHeaderMenuItem then
      Items[i].Free;
  end;
end;

procedure TTisGridHeaderPopupMenu.DoAddHeaderPopupItem(const aColumn: TColumnIndex;
  out aItem: TTisGridHeaderPopupItem);
begin
  aItem := apNormal;
  if Assigned(fOnAddPopupItem) then
    fOnAddPopupItem(TVirtualTreeCast(PopupComponent), aColumn, aItem);
end;

procedure TTisGridHeaderPopupMenu.DoColumnChange(aColumn: TColumnIndex;
  aVisible: Boolean);
begin
  if Assigned(fOnColumnChange) then
    fOnColumnChange(TVirtualTreeCast(PopupComponent), aColumn, aVisible);
end;

procedure TTisGridHeaderPopupMenu.OnMenuItemClick(aSender: TObject);
begin
  if Assigned(PopupComponent) and (PopupComponent is TBaseVirtualTree) then
  begin
    with TMenuItem(aSender), TVirtualTreeCast(PopupComponent).Header.Columns.Items[Tag] do
    begin
      if Checked then
        Options := Options - [coVisible]
      else
        Options := Options + [coVisible];
       DoColumnChange(TMenuItem(aSender).Tag, not Checked);
    end;
  end;
end;

procedure TTisGridHeaderPopupMenu.OnMenuShowAllClick(aSender: TObject);
var
  i: Integer;
begin
  if Assigned(PopupComponent) and (PopupComponent is TBaseVirtualTree) then
  begin
    with TVirtualTreeCast(PopupComponent).Header.Columns do
    begin
      for i := 0 to Count-1 do
      if not (coVisible in Items[i].Options) then
      begin
        Items[i].Options := Items[i].Options + [coVisible];
        DoColumnChange(i, True);
      end;
    end;
  end;
end;

procedure TTisGridHeaderPopupMenu.OnMenuHideAllClick(aSender: TObject);
var
  i: Integer;
begin
  if Assigned(PopupComponent) and (PopupComponent is TBaseVirtualTree) then
  begin
    with TVirtualTreeCast(PopupComponent).Header.Columns do
    begin
      for i := 0 to Count-1 do
      if coVisible in Items[i].Options then
      begin
        Items[i].Options := Items[i].Options - [coVisible];
        DoColumnChange(i, False);
      end;
    end;
  end;
end;

procedure TTisGridHeaderPopupMenu.OnMenuRestoreClick(aSender: TObject);
begin
   TTisGrid(PopupComponent).RestoreSettings;
end;

procedure TTisGridHeaderPopupMenu.Popup(x, y: Integer);
var
  vColPos: TColumnPosition;
  vColIdx: TColumnIndex;
  vNewMenuItem: TMenuItem;
  vHpi: TTisGridHeaderPopupItem;
  vVisibleCounter: Cardinal;
  vVisibleItem: TMenuItem;
begin
  if Assigned(PopupComponent) and (PopupComponent is TBaseVirtualTree) then
  begin
    // delete existing menu items
    RemoveAutoItems;
    // add column menu items
    with TVirtualTreeCast(PopupComponent).Header do
    begin
      if hoShowImages in Options then
        self.Images := Images
      else
        // remove a possible reference to image list of another tree previously assigned
        self.Images := nil;
      vVisibleItem := nil;
      vVisibleCounter := 0;
      for vColPos := 0 to Columns.Count - 1 do
      begin
        if poOriginalOrder in fOptions then
          vColIdx := vColPos
        else
          vColIdx := Columns.ColumnFromPosition(vColPos);
        if vColIdx = NoColumn then
          break;
        with Columns[vColIdx] as TTisGridColumn do
        begin
          if coVisible in Options then
            Inc(vVisibleCounter);
          DoAddHeaderPopupItem(vColIdx, vHpi);
          if vHpi <> apHidden then
          begin
            vNewMenuItem := TTisGridHeaderMenuItem.Create(self);
            vNewMenuItem.Tag := vColIdx;
            vNewMenuItem.Caption := Text + ' (' + Utf8ToString(PropertyName) + ')';
            vNewMenuItem.Hint := Hint;
            vNewMenuItem.ImageIndex := ImageIndex;
            vNewMenuItem.Checked := coVisible in Options;
            vNewMenuItem.OnClick := @OnMenuItemClick;
            if vHpi = apDisabled then
              vNewMenuItem.Enabled := False
            else
              if coVisible in Options then
                vVisibleItem := vNewMenuItem;
            Items.Add(vNewMenuItem);
          end;
        end;
      end;
      vNewMenuItem := TTisGridHeaderMenuItem.Create(Self);
      vNewMenuItem.Caption := '-';
      Items.Add(vNewMenuItem);
      // show all columns
      vNewMenuItem := TTisGridHeaderMenuItem.Create(Self);
      vNewMenuItem.Tag := -1;
      vNewMenuItem.Caption := rsShowAllColumns;
      vNewMenuItem.OnClick := @OnMenuShowAllClick;
      Items.Add(vNewMenuItem);
      // hide all columns
      vNewMenuItem := TTisGridHeaderMenuItem.Create(Self);
      vNewMenuItem.Tag := -2;
      vNewMenuItem.Caption := rsHideAllColumns;
      vNewMenuItem.OnClick := @OnMenuHideAllClick;
      Items.Add(vNewMenuItem);
      // restore default columns
      vNewMenuItem := TTisGridHeaderMenuItem.Create(Self);
      vNewMenuItem.Tag := -3;
      vNewMenuItem.Caption := rsRestoreDefaultColumns;
      vNewMenuItem.OnClick := @OnMenuRestoreClick;
      Items.Add(vNewMenuItem);
      // conditionally disable menu item of last enabled column
      if (vVisibleCounter = 1) and (vVisibleItem <> nil) and not (poAllowHideAll in fOptions) then
        vVisibleItem.Enabled := False;
    end;
  end;
  inherited Popup(x, y);
end;

{ TTisGridHeader }

function TTisGridHeader.GetPopupMenu: TPopupMenu;
begin
  result := inherited PopupMenu;
end;

procedure TTisGridHeader.SetPopupMenu(aValue: TPopupMenu);
begin
  if Assigned(aValue) then
    inherited PopupMenu := aValue
  else
    NewDefaultPopupMenu;
end;

function TTisGridHeader.GetColumnsClass: TVirtualTreeColumnsClass;
begin
  result := TTisGridColumns;
end;

procedure TTisGridHeader.NewDefaultPopupMenu;
begin
  inherited PopupMenu := TTisGridHeaderPopupMenu.Create(Treeview);
  inherited PopupMenu.PopupComponent := Treeview;
end;

constructor TTisGridHeader.Create(aOwner: TBaseVirtualTree);
begin
  inherited Create(aOwner);
  NewDefaultPopupMenu;
end;

procedure TTisGridHeader.Assign(aSource: TPersistent);
begin
  if aSource is TTisGridHeader then
    with TTisGridHeader(aSource) do
    begin
      self.AutoSizeIndex := AutoSizeIndex;
      self.Background := Background;
      self.Columns := Columns;
      self.Font := Font;
      self.FixedAreaConstraints.Assign(FixedAreaConstraints);
      self.Height := Height;
      self.Images := Images;
      self.MainColumn := MainColumn;
      self.Options := Options;
      self.ParentFont := ParentFont;
      self.SortColumn := SortColumn;
      self.SortDirection := SortDirection;
      self.Style := Style;
      self.RescaleHeader;
    end
  else
    inherited Assign(aSource);
end;

{ TTisStringTreeOptions }

constructor TTisStringTreeOptions.Create(aOwner: TBaseVirtualTree);
begin
  inherited Create(aOwner);
  AutoOptions := DefaultAutoOptions;
  MiscOptions := DefaultMiscOptions;
  PaintOptions := DefaultPaintOptions;
  SelectionOptions := DefaultSelectionOptions;
end;

{ TTisNodeOptions }

procedure TTisNodeOptions.SetMultiLine(aValue: Boolean);
begin
  if fMultiLine = aValue then
    exit;
  fMultiLine := aValue;
  fGrid.LoadData;
end;

procedure TTisNodeOptions.SetMultiLineHeight(aValue: Integer);
begin
  if fMultiLineHeight = aValue then
    exit;
  fMultiLineHeight := aValue;
  fGrid.LoadData;
end;

constructor TTisNodeOptions.Create(aGrid: TTisGrid);
begin
  inherited Create;
  fGrid := aGrid;
  fMultiLine := DefaultMultiLine;
  fMultiLineHeight := DefaultMultiLineHeight;
end;

procedure TTisNodeOptions.AssignTo(aDest: TPersistent);
begin
  if aDest is TTisNodeOptions then
  begin
    with TTisNodeOptions(aDest) do
    begin
      MultiLine := self.MultiLine;
      MultiLineHeight := self.MultiLineHeight;
    end;
  end
  else
    inherited AssignTo(aDest);
end;

{ TTisGrid.TInternalData }

procedure TTisGrid.TInternalData.Init(aGrid: TTisGrid);
begin
  Offset := aGrid.AllocateInternalDataArea(SizeOf(Cardinal));
end;

function TTisGrid.TInternalData.Data(aNode: PVirtualNode): Pointer;
begin
  if (aNode = nil) or (Offset <= 0) then
    result := nil
  else
    result := PByte(aNode) + Offset;
end;

{ TTisGrid }

function TTisGrid.FocusedPropertyName: string;
begin
  result := TTisGridColumn(Header.Columns[FocusedColumn]).PropertyName;
end;

function TTisGrid.GetFocusedColumnObject: TTisGridColumn;
begin
  if (FocusedColumn >= 0) and Header.Columns.IsValidColumn(FocusedColumn) then
    result := TTisGridColumn(Header.Columns[FocusedColumn])
  else
    result := nil;
end;

function TTisGrid.GetFocusedRow: PDocVariantData;
var
  n: PVirtualNode;
begin
  n := FocusedNode;
  if n <> nil then
    result := GetNodeDataAsDocVariant(n)
  else
    result := nil;
end;

function TTisGrid.GetKeyFieldsNames: string;
begin
  result := StrJoin(';', fKeyFieldsList);
end;

procedure TTisGrid.SetKeyFieldsNames(const aValue: string);
begin
  fKeyFieldsList := StrSplit(aValue, ';', True);
end;

function TTisGrid.GetParentKeyFieldsNames: string;
begin
  result := StrJoin(';', fParentKeyFieldsList);
end;

procedure TTisGrid.SetParentKeyFieldsNames(const aValue: string);
var
  vKeys: TRawUtf8DynArray;
  vParents: TRawUtf8DynArray;
begin
  if aValue <> '' then
  begin
    StringDynArrayToRawUtf8DynArray(fKeyFieldsList, vKeys);
    StringDynArrayToRawUtf8DynArray(fParentKeyFieldsList, vParents);
    if high(vParents) > high(vKeys) then
      raise ETisGrid.Create('ParentKeyFieldsList should not have more fields than KeyFieldsList.');
  end;
  fParentKeyFieldsList := StrSplit(aValue, ';', True);
end;

function TTisGrid.GetSettings: Variant;
var
  i: integer;
  vCol: TTisGridColumn;
  r, vCols: PDocVariantData;
begin
  TDocVariant.NewFast(result);
  r := @result;
  r^.I['sortcolumn'] := Header.SortColumn;
  r^.I['sortdirection'] := ord(Header.SortDirection);
  vCols := r^.A_['columns'];
  for i := 0 to Header.Columns.Count-1 do
  begin
    vCol := Header.Columns[i] as TTisGridColumn;
    vCols^.AddItem(
      _ObjFast([
        'propertyname', vCol.PropertyName,
        'text', StringToUtf8(vCol.Text),
        'position', vCol.Position,
        'width', vCol.Width,
        'visible', (coVisible in vCol.Options)
      ])
    );
  end;
end;

procedure TTisGrid.SetSettings(const aValue: Variant);
var
  vObj, vSettings: PDocVariantData;
  vIntValue: Integer;
  vPropName: RawUtf8;
  vCol: TTisGridColumn;
begin
  vSettings := _Safe(aValue);
  if not vSettings^.IsVoid then
  begin
    for vObj in vSettings^.A_['columns']^.Objects do
    begin
      vPropName := vObj^.U['propertyname'];
      vCol := FindColumnByPropertyName(vPropName);
      if vCol = nil then
      begin
        vCol := Header.Columns.Add as TTisGridColumn;
        vCol.Text := vObj^.S['text'];
        vCol.PropertyName := vPropName;
        vCol.Width := 100;
      end
      else
      begin
        vCol.Position := vObj^.I['position'];
        vCol.Width := vObj^.I['width'];
        if vObj^.B['visible'] then
          vCol.Options := vCol.Options + [coVisible]
        else
          vCol.Options := vCol.Options - [coVisible];
      end;
    end;
    if vSettings^.GetAsInteger('sortcolumn', vIntValue) then
      Header.SortColumn := vIntValue;
    if vSettings^.GetAsInteger('sortdirection', vIntValue) then
      Header.SortDirection := TSortDirection(vIntValue);
  end;
end;

function TTisGrid.GetGridSettings: string;
begin
  result := BinToBase64(Utf8ToString(_Safe(GetSettings)^.ToJson));
end;

procedure TTisGrid.SetGridSettings(const aValue: string);
begin
  if aValue <> '' then
    SetSettings(_Json(Base64ToBin(StringToUtf8(aValue))));
end;

procedure TTisGrid.SetColumnToFind(aValue: integer);
begin
  if fColumnToFind = aValue then
    exit;
  fColumnToFind := aValue;
end;

procedure TTisGrid.SetData(const aValue: TDocVariantData);
var
  vAborted: Boolean;
begin
  if fData.Equals(aValue) then
    exit;
  vAborted := False;
  if Assigned(fOnBeforeDataChange) then
    fOnBeforeDataChange(self, @aValue, vAborted);
  if vAborted then
    exit;
  fData := aValue;
  LoadData;
  UpdateSelectedAndTotalLabel;
  if Assigned(fOnAfterDataChange) then
    fOnAfterDataChange(self);
end;

function TTisGrid.GetMetaData: RawUtf8;
var
  i: Integer;
  vDoc: TDocVariantData;
  vCol: record
    cur: TTisGridColumn;
    items: PDocVariantData;
    adapt: TTisColumnDataTypeAdapter;
  end;
begin
  vDoc.InitFast;
  vCol.items := vDoc.A_['columns'];
  for i := 0 to Header.Columns.Count -1 do
  begin
    vCol.cur := Header.Columns[i] as TTisGridColumn;
    vCol.items^.AddItem(
      _ObjFast([
        'propertyname', vCol.cur.PropertyName,
        'datatype', vCol.adapt.EnumToRawUtf8(vCol.cur.DataType),
        'required', vCol.cur.Required,
        'readonly', vCol.cur.ReadOnly,
        'width',vCol.cur.Width
      ])
    );
  end;
  result := vDoc.ToJson;
end;

procedure TTisGrid.SetMetaData(const aValue: RawUtf8);
var
  vDoc: TDocVariantData;
  vCol: record
    cur: PDocVariantData;
    adapt: TTisColumnDataTypeAdapter;
    gc: TTisGridColumn;
  end;
begin
  vDoc.InitJson(aValue, JSON_FAST_FLOAT);
  if vDoc.IsVoid then
    exit;
  for vCol.cur in vDoc.A_['columns']^.Objects do
  begin
    vCol.gc := FindColumnByPropertyName(vCol.cur^.U['propertyname']);
    if vCol.gc <> nil then
    begin
      vCol.gc.DataType := vCol.adapt.RawUtf8ToEnum(vCol.cur^.U['datatype']);
      vCol.gc.Required := vCol.cur^.B['required'];
      vCol.gc.ReadOnly := vCol.cur^.B['readonly'];
      if vCol.cur^.GetValueIndex('width')>=0 then
        vCol.gc.Width := vCol.cur^.I['width'];
    end
  end;
end;

procedure TTisGrid.SetFocusedColumnObject(aValue: TTisGridColumn);
begin
  if (aValue <> nil) and Header.Columns.IsValidColumn(aValue.Index) then
    FocusedColumn := aValue.Index;
end;

procedure TTisGrid.SetFocusedRow(aValue: PDocVariantData);
begin
  ClearSelection;
  SetFocusedRowNoClearSelection(aValue);
end;

procedure TTisGrid.SetOnCutToClipboard(aValue: TNotifyEvent);
begin
  fOnCutToClipboard := aValue;
end;

function TTisGrid.GetOptions: TStringTreeOptions;
begin
  result := TStringTreeOptions(inherited TreeOptions);
end;

procedure TTisGrid.SetOptions(const aValue: TStringTreeOptions);
begin
  TreeOptions.Assign(aValue);
end;

function TTisGrid.GetSelectedRows: TDocVariantData;
var
  vNode: PVirtualNode;
  vDoc: PDocVariantData;
begin
  vNode := GetFirstSelected;
  result.InitArray([], JSON_FAST);
  while vNode <> nil do
  begin
    vDoc := GetNodeDataAsDocVariant(vNode);
    result.AddItem(variant(vDoc^));
    vNode := GetNextSelected(vNode, True);
  end;
end;

procedure TTisGrid.SetSelectedRows(const aValue: TDocVariantData);
var
  vNodesToSelect: TNodeArray;
  vNode, vNodeToFocus: PVirtualNode;
  vRow, vPreviousFocusedRow: PDocVariantData;
begin
  if aValue.IsVoid then
  begin
    ClearSelection;
    FocusedNode := nil;
  end
  else
  begin
    vPreviousFocusedRow := FocusedRow;
    ClearSelection;
    vNode := nil;
    vNodeToFocus := nil;
    BeginUpdate;
    try
      for vRow in aValue.Objects do
      begin
        if Length(KeyFieldsList) = 1 then
          vNodesToSelect := GetNodesBy(StringToUtf8(KeyFieldsList[0]), vRow^.U[StringToUtf8(KeyFieldsList[0])])
        else
          vNodesToSelect := GetNodesBy(vRow, Length(KeyFieldsList) > 0);
        for vNode in vNodesToSelect do
        begin
          if (vPreviousFocusedRow <> nil) and vRow^.Equals(vPreviousFocusedRow^) then
            vNodeToFocus := vNode;
          Selected[vNode] := True;
        end;
      end;
    finally
      EndUpdate;
    end;
    // focused the last selected node
    if vNodeToFocus <> nil then
      FocusedNode := vNodeToFocus
    else if vNode <> nil then
      SetFocusedRowNoClearSelection(vPreviousFocusedRow);
  end;
end;

function TTisGrid.GetSelectedObjects: PDocVariantDataDynArray;
var
  n: integer;
  vNode: PVirtualNode;
begin
  vNode := GetFirstSelected;
  n := 0;
  result := nil;
  while vNode <> nil do
  begin
    PtrArrayAdd(result, GetNodeDataAsDocVariant(vNode), n);
    vNode := GetNextSelected(vNode, True);
  end;
  SetLength(result, n);
end;

function TTisGrid.GetSelectedRow: TDocVariantData;
var
  r: PDocVariantData;
begin
  result.InitFast;
  r := FocusedRow;
  if r <> nil then
    result.AddFrom(variant(r^));
end;

procedure TTisGrid.SetSelectedRow(aValue: TDocVariantData);
var
  a: TDocVariantData;
begin
  a.InitArray([]);
  a.AddItem(variant(aValue));
  SetSelectedRows(a);
end;

procedure TTisGrid.SetSelectedAndTotalLabel(aValue: TLabel);
begin
  fSelectedAndTotalLabel := aValue;
  if fSelectedAndTotalLabel <> nil then
  begin
    fSelectedAndTotalLabel.FreeNotification(self);
    UpdateSelectedAndTotalLabel;
  end;
end;

procedure TTisGrid.WMKeyDown(var Message: TLMKeyDown);

{$IFNDEF windows}
  procedure GetKeyboardState(ks: TKeyBoardState);
  var
    i : integer;
  begin
    for i := 0 to 255 do
      ks[i] := GetKeyState(i);
  end;

  function ToASCII(vk: integer; san_code: integer; const key_state: TKeyboardState;
    output_buffer: PChar; flags: integer): integer;
  begin
    if( (vk >= VK_NUMPAD0) and (vk <= VK_NUMPAD9) ) then
    begin
      output_buffer^ := char(vk - 48) ;
      result := 1;
      exit;
    end;
    if (vk >= VK_0) and (vk <= VK_9 ) then
    begin
      output_buffer^ := char(vk);
      result := 1;
      exit;
    end;
    if( (vk >= VK_A) and (vk <= VK_Z) ) then
    begin
      output_buffer^ := char(vk + 32);
      result := 1;
      exit;
    end;
    result := 0;
  end;
{$ENDIF}

var
  m: TLMessage;
  vShift: TShiftState;
  vKeyState: TKeyboardState;
  vBuffer: array[0..1] of Char;
begin
  // manage immediate editor
  with Message do
  begin
    vShift := KeyDataToShiftState(KeyData);
    vKeyState := Default(TKeyboardState);
    GetKeyboardState(vKeyState);
    // avoid conversion to control characters
    // - we have captured the control key state already in vShift
    vKeyState[VK_CONTROL] := 0;
    if (
      (ToASCII(Message.CharCode, (Message.KeyData shr 16) and 7, vKeyState, @vBuffer, 0) > 0) or
      (Message.CharCode = VK_F2)
      )
      and (vShift * [ssCtrl, ssAlt] = []) and (CharCode >= 32) then
    begin
      EditColumn := FocusedColumn;
      if EditColumn = NoColumn then
        exit;
      // send first key which triggered the editor to newly created editor
      if CanEdit(FocusedNode, EditColumn) then
      begin
        DoEdit;
        m.msg := LM_CHAR;
        m.wParam := ord(vBuffer[0]);
        m.lParam := 0;
        if Message.CharCode <> VK_F2 then
          EditLink.ProcessMessage(m);
      end;
    end
    else
      inherited WMKeyDown(Message);
  end;
end;

procedure TTisGrid.Loaded;
begin
  inherited Loaded;
  if not (csDesigning in ComponentState) then
    FillPopupMenu(PopupMenu);
  fDefaultSettings := GetSettings;
end;

function TTisGrid.GetPopupMenu: TPopupMenu;
begin
  result := inherited GetPopupMenu;
end;

procedure TTisGrid.SetPopupMenu(aValue: TPopupMenu);
begin
  inherited PopupMenu := aValue;
  if Assigned(PopupMenu) then
  begin
    if not (csDesigning in ComponentState) then
    begin
      fPopupOrigEvent := PopupMenu.OnPopup;
      PopupMenu.OnPopup := @FillPopupMenu;
    end;
  end;
end;

// hack to allow right click menu on header popup menu  and different popup menu on rows
// set message.msg to 0 if handled to stop message processing.
type
  TVTHeaderHack = class(TVTHeader);

//Bugfix :
procedure TTisGrid.WndProc(var Message: TLMessage);
var
  vHandled: Boolean;
begin
  vHandled := False;
  // try the header whether it needs to take this message
  if Assigned(Header) and (Header.States <> []) then
    vHandled := TVTHeaderHack(Header).HandleMessage(Message);
  if not vHandled then
  begin
    // for auto drag mode, let tree handle itself, instead of TControl
    if not (csDesigning in ComponentState) and
       ((Message.Msg = LM_LBUTTONDOWN) or (Message.Msg = LM_LBUTTONDBLCLK)) then
    begin
      //lclheader
      //when FHeader.FStates = [] it comes until here unlike Delphi (uses NC messages)
      //skip this code when is clicked inside the header
      if (DragMode = dmAutomatic) and (DragKind = dkDrag) and
        not Header.InHeader(SmallPointToPoint(TLMMouse(Message).Pos)) then
      begin
        if IsControlMouseMsg(TLMMouse(Message)) then
          vHandled := True;
        if not vHandled then
        begin
          ControlState := ControlState + [csLButtonDown];
          Dispatch(Message);  // overrides TControl's BeginDrag
          vHandled := True;
        end;
      end;
    end;
    if not vHandled and Assigned(Header) then
      vHandled := TVTHeaderHack(Header).HandleMessage(Message);
    if not vHandled then
    begin
      //lcl: probably  necessary
      {$IFNDEF UNIX}
      if (Message.Msg in [WM_NCLBUTTONDOWN, WM_NCRBUTTONDOWN, WM_NCMBUTTONDOWN]) and not Focused and CanFocus then
        SetFocus;
      {$ENDIF}
      inherited;
    end
    //// BUGFIX Tranquil IT Systems.
    else
       Message.Msg := 0;
    //// end BUGFIX
  end;
end;

procedure TTisGrid.DoNewText(aNode: PVirtualNode; aColumn: TColumnIndex;
  const aText: string);
var
  vRowData: PDocVariantData;
  vPropertyName: RawUtf8;
begin
  if aNode = nil then
    exit;
  vRowData := GetNodeDataAsDocVariant(aNode);
  if vRowData <> nil then
  begin
    if aColumn >= 0 then
      vPropertyName := TTisGridColumn(Header.Columns.Items[aColumn]).PropertyName
    else
      vPropertyName := StringToUtf8(DefaultText);
    { TODO -omsantos : we should test for more cases }
    case VarType(vRowData^.Value[vPropertyName]) of
      varDouble, varCurrency:
        vRowData^.D[vPropertyName] := StrToFloatDef(aText, 0);
      varInteger:
        vRowData^.I[vPropertyName] := StrToIntDef(aText, 0);
      else
        vRowData^.S[vPropertyName] := aText;
    end;
    inherited DoNewText(aNode, aColumn, vRowData^.S[vPropertyName]);
  end;
end;

procedure TTisGrid.DoGetText(aNode: PVirtualNode; aColumn: TColumnIndex;
  aTextType: TVSTTextType; var aText: string);
var
  vDoc: PDocVariantData;
  vCol: TTisGridColumn;
begin
  vDoc := nil;
  if aNode <> nil then
  begin
    vDoc := GetNodeDataAsDocVariant(aNode);
    if vDoc <> nil then
    begin
      if (aColumn >= 0) and Header.Columns.IsValidColumn(aColumn) then
        aText := vDoc^.S[TTisGridColumn(Header.Columns.Items[aColumn]).PropertyName]
      else if DefaultText <> '' then
        aText := vDoc^.S[DefaultText];
      if aText = '' then
        aText := DefaultText;
    end
    else
      aText := 'uninitialized';
  end
  else
    aText := '';
  if Assigned(fOnGetText) and (aColumn >= 0) and Header.Columns.IsValidColumn(aColumn) then
    fOnGetText(self, aNode, vDoc^, aColumn, aTextType, aText);
  vCol := FindColumnByIndex(aColumn);
  if vCol <> nil then
  begin
    if vCol.DataType = cdtPassword then
      aText := StrRepeatChar('*', Length(aText));
  end;
end;

procedure TTisGrid.DoInitNode(aParentNode, aNode: PVirtualNode;
  var aInitStates: TVirtualNodeInitStates);
var
  vData: PCardinal;
begin
  vData := fInternalData.Data(aNode);
  if (vData <> nil ) and (not fData.IsVoid) and (aNode^.Index < fData.Count) then
  begin
    vData^ := aNode^.Index;
    aNode^.CheckType := ctCheckBox;
    if fNodeOptions.MultiLine then
      aNode^.States := aNode^.States + [vsMultiline];
  end;
  inherited DoInitNode(aParentNode, aNode, aInitStates);
end;

procedure TTisGrid.DoMeasureItem(aTargetCanvas: TCanvas; aNode: PVirtualNode;
  var aNodeHeight: Integer);
var
  i, vCellHeight, vMaxHeight: Integer;
begin
  if Assigned(OnMeasureItem) then
    inherited DoMeasureItem(aTargetCanvas, aNode, aNodeHeight)
  else
  begin
    vMaxHeight := DefaultNodeHeight;
    if MultiLine[aNode] then
    begin
      for i := 0 to Header.Columns.Count -1 do
      begin
        if (coVisible in Header.Columns[i].Options) then
        begin
          vCellHeight := ComputeNodeHeight(aTargetCanvas, aNode, i);
          if vCellHeight > vMaxHeight then
            vMaxHeight := vCellHeight;
        end;
      end;
    end;
    if vMaxHeight > 6 * DefaultNodeHeight then
      vMaxHeight := 6 * DefaultNodeHeight;
    aNodeHeight := vMaxHeight + fNodeOptions.MultiLineHeight;
  end;
end;

function TTisGrid.DoCompare(aNode1, aNode2: PVirtualNode; aColumn: TColumnIndex): Integer;
begin
  result := inherited DoCompare(aNode1, aNode2, aColumn);
  if aColumn = NoColumn then
    exit;
  result := DoCompareByRow(
    TTisGridColumn(Header.Columns[aColumn]).PropertyName,
    GetNodeDataAsDocVariant(aNode1), GetNodeDataAsDocVariant(aNode2));
end;

function TTisGrid.GetColumnClass: TVirtualTreeColumnClass;
begin
  result := TTisGridColumn;
end;

function TTisGrid.GetOptionsClass: TTreeOptionsClass;
begin
  result := TTisStringTreeOptions;
end;

function TTisGrid.DoCreateEditor(aNode: PVirtualNode; aColumn: TColumnIndex): IVTEditLink;
begin
  result := nil;
  if Assigned(OnCreateEditor) then
    OnCreateEditor(Self, aNode, aColumn, result);
  if result = nil then
    result := TTisGridEditLink.Create;
end;

procedure TTisGrid.PrepareCell(var PaintInfo: TVTPaintInfo;
  WindowOrgX, MaxWidth: integer);
begin
  inherited PrepareCell(PaintInfo, WindowOrgX, MaxWidth);
end;

procedure TTisGrid.DoBeforeCellPaint(ACanvas: TCanvas; Node: PVirtualNode;
  Column: TColumnIndex; CellPaintMode: TVTCellPaintMode; CellRect: TRect;
  var ContentRect: TRect);
begin
  if CellPaintMode = cpmPaint then
  begin
    if focused or not (toHideSelection in TreeOptions.PaintOptions) or (toPopupMode in TreeOptions.PaintOptions) then
    begin
      if (vsSelected in Node^.States) then
      begin
        if (column <> FocusedColumn) or (Node <> FocusedNode) then
        begin
          ACanvas.Brush.Color := Colors.UnfocusedSelectionColor;
          ACanvas.FillRect(CellRect);
        end
        else
        if (Column = FocusedColumn) and (Node = FocusedNode)  then
        begin
          ACanvas.Brush.Color := Colors.FocusedSelectionColor;
          ACanvas.FillRect(CellRect);
        end;
      end;
    end;
  end;
  inherited DoBeforeCellPaint(ACanvas, Node, Column, CellPaintMode, CellRect, ContentRect);
end;

procedure TTisGrid.DoTextDrawing(var PaintInfo: TVTPaintInfo;
  const AText: string; CellRect: TRect; DrawFormat: cardinal);
begin
  // pour affichage lignes multiselect en gris clair avec cellule focused en bleu
  if (focused or not (toHideSelection in TreeOptions.PaintOptions) or (toPopupMode in TreeOptions.PaintOptions))  and
    (vsSelected in PaintInfo.Node^.States) and (PaintInfo.Node = FocusedNode) and
    (PaintInfo.column = FocusedColumn) then
    PaintInfo.Canvas.Font.Color := Colors.SelectionTextColor;
  inherited;
end;

procedure TTisGrid.DoBeforeItemErase(aCanvas: TCanvas; aNode: PVirtualNode;
  const aItemRect: TRect; var aColor: TColor; var EraseAction: TItemEraseAction);
begin
  inherited DoBeforeItemErase(aCanvas, aNode, aItemRect, aColor, EraseAction);
  if fZebraPaint and (aNode <> nil) and Odd(aNode^.Index) then
  begin
    aColor := fZebraColor;
    EraseAction := eaColor;
  end;
end;

function TTisGrid.DoKeyAction(var CharCode: Word; var Shift: TShiftState): Boolean;
begin
  result := inherited DoKeyAction(CharCode, Shift);
end;

procedure TTisGrid.Notification(aComponent: TComponent; aOperation: TOperation);
begin
  inherited Notification(aComponent, aOperation);
  if (aOperation = opRemove) and (aComponent = fSelectedAndTotalLabel) then
    fSelectedAndTotalLabel := nil;
end;

procedure TTisGrid.DoAutoAdjustLayout(const aMode: TLayoutAdjustmentPolicy;
  const aXProportion, aYProportion: Double);
var
  i: Integer;
begin
  if (aMode in [lapAutoAdjustForDPI]) then
  begin
    Header.MinHeight := max(18,round(Header.MinHeight * aXProportion)+1);
    Header.MaxHeight := max(18,round(Header.MaxHeight * aXProportion)+1);
    Header.DefaultHeight := max(18,round(Header.DefaultHeight * aXProportion)+1);
    Header.Height := max(18,round(Header.Height * aXProportion)+1);
    for i := 0 to header.Columns.Count-1 do
    begin
      header.Columns[i].MaxWidth:=round(header.Columns[i].MaxWidth * aXProportion);
      header.Columns[i].Width:=round(header.Columns[i].Width * aXProportion);
      header.Columns[i].MinWidth:=round(header.Columns[i].MinWidth * aXProportion);
    end;
  end;
end;

procedure TTisGrid.DoChange(Node: PVirtualNode);
begin
  inherited DoChange(Node);
  fSelectedData := SelectedRows;
  UpdateSelectedAndTotalLabel;
end;

function TTisGrid.GetHeaderClass: TVTHeaderClass;
begin
  result := TTisGridHeader;
end;

procedure TTisGrid.FillPopupMenu(aSender: TObject);

  procedure _RemoveAutoItems;
  var
    i: Integer;
  begin
    for i := PopupMenu.Items.Count-1 downto 0 do
      if PopupMenu.Items[i].Tag = 250 then
        PopupMenu.Items.Delete(i);
  end;

  function _AddItem(const aCaption: string; aShortcut: TShortCut; aEvent: TNotifyEvent;
    aEnabled: Boolean = True): HMENU;
  var
    vMenuItem: TMenuItem;
  begin
    vMenuItem := PopupMenu.Items.Find(aCaption);
    if vMenuItem = nil then
    begin
      vMenuItem := TMenuItem.Create(PopupMenu);
      with vMenuItem do
      begin
        Caption := aCaption;
        ShortCut := aShortcut;
        OnClick := aEvent;
        Enabled := aEnabled;
        // to delete them
        Tag := 250; { TODO -omsantos : we might create a new property for custom this number }
      end;
      PopupMenu.Items.Add(vMenuItem);
    end;
    result := vMenuItem.Handle;
  end;

begin
  if PopupMenu = nil then
    exit;
  _RemoveAutoItems;
  if Assigned(fPopupOrigEvent) then
    fPopupOrigEvent(self);
  if (PopupMenu.Items.Count > 0) then
    _AddItem('-', 0, nil);
  if pmoShowFind in fPopupMenuOptions then
    HMFind := _AddItem(rsFind, ShortCut(Ord('F'), [ssCtrl]), @DoFindText, not fData.IsVoid);
  if pmoShowFindNext in fPopupMenuOptions then
    HMFindNext := _AddItem(rsFindNext, VK_F3, @DoFindNext, not fData.IsVoid);
  {HMFindReplace := _AddItem(rsFindReplace, ShortCut(Ord('H'), [ssCtrl]),
    @DoFindReplace);}
  _AddItem('-', 0, nil);
  if (pmoShowCut in fPopupMenuOptions) and (not (toReadOnly in TreeOptions.MiscOptions)) and Assigned(fOnCutToClipboard) then
    HMCut := _AddItem(rsCut, ShortCut(Ord('X'), [ssCtrl]), @DoCutToClipboard, not fData.IsVoid);
  if pmoShowCopy in fPopupMenuOptions then
    HMCopy := _AddItem(rsCopy, ShortCut(Ord('C'), [ssCtrl]), @DoCopyToClipboard, not fData.IsVoid);
  if pmoShowCopyCell in fPopupMenuOptions then
    HMCopyCell := _AddItem(rsCopyCell, ShortCut(Ord('C'), [ssCtrl,ssShift]), @DoCopyCellToClipboard, not fData.IsVoid);
  if pmoShowCopySpecial in fPopupMenuOptions then
    HMCopySpecial := _AddItem(rsCopySpecial, ShortCut(Ord('S'), [ssCtrl,ssShift]), @DoCopySpecialToClipboard, not fData.IsVoid);
  if (pmoShowPaste in fPopupMenuOptions) and (not (toReadOnly in TreeOptions.MiscOptions)) and
    ((toEditable in TreeOptions.MiscOptions) or Assigned(fOnBeforePaste))  then
    HMPaste := _AddItem(rsPaste, ShortCut(Ord('V'), [ssCtrl]), @DoPaste, Header.UseColumns);
  _AddItem('-', 0, nil);
  if (pmoShowDelete in fPopupMenuOptions) and ((not (toReadOnly in TreeOptions.MiscOptions)) or Assigned(fOnBeforeDeleteRows)) then
    HMDelete := _AddItem(rsDeleteRows, ShortCut(VK_DELETE, [ssCtrl]), @DoDeleteRows, not fData.IsVoid);
  if (pmoShowSelectAll in fPopupMenuOptions) and (toMultiSelect in TreeOptions.SelectionOptions) then
    HMSelAll := _AddItem(rsSelectAll, ShortCut(Ord('A'), [ssCtrl]), @DoSelectAllRows, not fData.IsVoid);
  _AddItem('-', 0, nil);
  if pmoShowExport in fPopupMenuOptions then
  begin
    if toMultiSelect in TreeOptions.SelectionOptions then
      HMExport := _AddItem(rsExportSelected, 0, @DoExport, not fData.IsVoid)
    else
      HMExport := _AddItem(rsExportAll, 0, @DoExport, not fData.IsVoid);
  end;
  {if (HMPrint = 0) then
    HMPrint := _AddItem(rsPrint, ShortCut(Ord('P'), [ssCtrl]), @DoPrint);
  _AddItem('-', 0, nil);
  HMExpAll := _AddItem(rsExpandAll, Shortcut(Ord('E'), [ssCtrl, ssShift]),
    @DoExpandAll);
  HMCollAll := _AddItem(rsCollapseAll, Shortcut(Ord('R'), [ssCtrl, ssShift]),
    @DoCollapseAll);}
  _AddItem('-', 0, nil);
  if (pmoShowCustomizeColumns in fPopupMenuOptions) and Assigned(Header.PopupMenu) then
    HMCustomize := _AddItem(rsCustomizeColumns, 0, @DoCustomizeColumns);
  if (csDesigning in ComponentState) or (pmoShowCustomizeGrid in fPopupMenuOptions) then
    HMAdvancedCustomize := _AddItem(rsAdvancedCustomizeColumns, 0, @DoAdvancedCustomizeColumns);
  if Assigned(fOnAfterFillPopupMenu) then
    fOnAfterFillPopupMenu(self);
end;

function TTisGrid.FindText(const aText: string): PVirtualNode;
begin
  TextToFind := aText;
  fStartSearchNode := nil;
  TextFound := False;
  DoFindNext(self);
  if TextFound then
    result := FocusedNode
  else
    result := nil;
end;

function TTisGrid.FindColumnByPropertyName(const aPropertyName: RawUtf8): TTisGridColumn;
var
  i: integer;
begin
  result := nil;
  for i := 0 to Header.Columns.Count - 1 do
  begin
    if TTisGridColumn(Header.Columns[i]).PropertyName = aPropertyName then
    begin
      result := TTisGridColumn(Header.Columns[i]);
      break;
    end;
  end;
end;

function TTisGrid.FindColumnByIndex(const aIndex: TColumnIndex): TTisGridColumn;
begin
  if aIndex = NoColumn then
    result := nil
  else
    result := TTisGridColumn(Header.Columns[aIndex]);
end;

procedure TTisGrid.FindDlgFind(aSender: TObject);
begin
  if (fFindDlg.FindText <> TextToFind) then
  begin
    if toFullRowSelect in TreeOptions.SelectionOptions then
      ColumnToFind := -1
    else
      ColumnToFind := FocusedColumn;
    TextToFind := fFindDlg.FindText;
    //if frDown in fFindDlg.Options then
    begin
      DoFindNext(aSender);
      if TextFound then
        fFindDlg.CloseDialog;
    end;
    //else
    //  if DoFindLast then
    //    fFindDlg.CloseDialog;
  end
  else
    //if frDown in fFindDlg.Options then
    DoFindNext(aSender);
  //else
  //DoFindPrior(aSender);
end;

function TTisGrid.Add(aData: PDocVariantData): Boolean;
var
  vObj: PDocVariantData;
begin
  result := True;
  case aData^.Kind of
    dvArray:
      for vObj in aData^.Objects do
        fData.AddItem(variant(vObj^));
    dvObject:
      fData.AddItem(variant(aData^));
    else
      result := False;
  end;
  if result then
    LoadData;
end;

function TTisGrid.DoCompareByRow(const aPropertyName: RawUtf8; const aRow1,
  aRow2: PDocVariantData): PtrInt;
var
  vHandled: Boolean;
begin
  vHandled := False;
  if not Assigned(aRow1) or not Assigned(aRow2) then
    exit(0);
  if Assigned(OnCompareByRow) then
    result := OnCompareByRow(self, aPropertyName, aRow1^, aRow2^, vHandled)
  else
    result := 0;
  if not vHandled then // use default comparison
    result := aRow1^.CompareObject([aPropertyName], aRow2^);
end;

procedure TTisGrid.DoFindText(aSender: TObject);
begin
  fStartSearchNode := nil;
  TextFound := False;
  TextToFind := '';
  DoFindNext(aSender);
end;

procedure TTisGrid.DoFindNext(aSender: TObject);
var
  col: integer;
  p: PVirtualNode;

  procedure _Focus(aColumnIndex: TColumnIndex; aNode: PVirtualNode);
  begin
    SelectNodes(aNode,aNode,False);
    FocusedNode := aNode;
    FocusedColumn := aColumnIndex;
    SetFocusSafe;
  end;

  function _Match(aNode: PVirtualNode; var aTxt: string): integer;
  var
    i: integer;
    vTxt: string;
  begin
    vTxt := '';
    aTxt := LowerCase(aTxt);
    result := -1;
    if fColumnToFind >= 0 then
    begin
      DoGetText(aNode, fColumnToFind, ttNormal, vTxt);
      if (not (frWholeWord in fFindDlg.Options) and (Pos(aTxt, LowerCase(vTxt)) > 0)) or
        (aTxt = LowerCase(vTxt)) then
      begin
        result := fColumnToFind;
        TextFound := True;
        exit;
      end;
    end
    else
      for i := 0 to Header.Columns.Count - 1 do
      begin
        DoGetText(aNode, i, ttNormal, vTxt);
        if not (frWholeWord in fFindDlg.Options) and (Pos(aTxt, LowerCase(vTxt)) > 0) or
          (aTxt = LowerCase(vTxt)) then
        begin
          TextFound := True;
          result := i;
          exit;
        end;
      end;
  end;

begin
  // on part de la ligne en cours
  if (TextToFind = '') then
    fFindDlg.Execute
  else
  begin
    if IsTreeMode then
     // necessary to show the node for the user, otherwise it gets in an infinity loop
      ExpandAllNodes;
    p := FocusedNode;
    TextFound := False;
    if p <> nil then
    begin
      // depart de recherche. teste la ligne en cours
      if (fStartSearchNode = nil) then
      begin
        fStartSearchNode := p;
        col := _Match(p, fTextToFind);
        if col >= 0 then
        begin
          _Focus(col, p);
          exit;
        end;
      end;
      //on teste a partir du suivant
      if (fFindDlg <> nil) and not (frDown in fFindDlg.Options) then
        p := GetPrevious(P)
      else
        p := GetNext(P);
      while (p <> nil) and (p <> fStartSearchNode) do
      begin
        col := _Match(p, fTextToFind);
        if col >= 0 then
        begin
          _Focus(col, p);
          exit;
        end;
        // on teste a partir du suivant
        if (fFindDlg <> nil) and not (frDown in fFindDlg.Options) then
          p := GetPrevious(p)
        else
          p := GetNext(p);
        // on reboucle sur le debut
        if p = nil then
          if (fFindDlg <> nil) and not (frDown in fFindDlg.Options) then
            p := GetLast(nil)
          else
            p := GetFirst(False);
      end;
    end;
    fStartSearchNode := nil;
    ShowMessageFmt(RsNoRecordFind, [TextToFind]);
    SetFocusSafe;
  end;
end;

procedure TTisGrid.DoFindReplace(aSender: TObject);
begin
  if fReplaceDialog = nil then
    fReplaceDialog := TReplaceDialog.Create(self);
  try
    fReplaceDialog.Options := [frDown, frDisableUpDown, frReplace, frReplaceAll,frEntireScope];
    //fReplaceDialog.OnReplace := ReplaceDialog1Replace;
    if EditLink <> nil  then
      fReplaceDialog.FindText := TStringEditLink(EditLink).Edit.Text
    else
      fReplaceDialog.FindText := GetCellDataAsString(FocusedNode, FocusedPropertyName);
    fReplaceDialog.Execute;
  finally
  end;
end;

procedure TTisGrid.DoUndoLastUpdate(aSender: TObject);
begin
end;

procedure TTisGrid.DoRevertRecord(aSender: TObject);
begin
end;

procedure TTisGrid.DoExport(aSender: TObject);

  function _GetSelectionType: TVSTTextSourceType;
  begin
    if (toMultiSelect in TreeOptions.SelectionOptions) then
      result := tstSelected
    else
      result := tstAll;
  end;

var
  vDialog: TSaveDialog;
begin
  vDialog := TSaveDialog.Create(nil);
  try
    vDialog.Title := Application.Title;
    vDialog.Filter := GetExportDialogFilter;
    vDialog.FileName := 'data';
    vDialog.Options := vDialog.Options + [ofOverwritePrompt];
    if vDialog.Execute then
      ExportData(vDialog.FileName, _GetSelectionType);
  finally
    vDialog.Free;
  end;
end;

procedure TTisGrid.DoExportCustomContent(aSource: TVSTTextSourceType;
  var aBuffer: RawUtf8);
begin
  if Assigned(fOnGridExportCustomContent) then
    fOnGridExportCustomContent(self, aSource, aBuffer);
end;

procedure TTisGrid.DoCopyToClipboard(aSender: TObject);
var
  s: RawByteString;
  c: TClipboardAdapter;
begin
  c.Open;
  try
    c.Clear;
    s := ContentToUTF8(tstSelected, ';');
    c.Add(cbkText, s[1], Length(s)+1);
    s := SelectedRows.ToJson;
    c.Add(cbkJson, s[1], Length(s));
  finally
    c.Close;
  end;
end;

procedure TTisGrid.DoCopyCellToClipboard(aSender: TObject);
var
  c: TClipboardAdapter;
  r: TDocVariantData;
  s: RawByteString;
begin
  if (FocusedColumnObject <> nil) and (FocusedRow <> nil) then
  begin
    c.Open;
    try
      c.Clear;
      SelectedRows.Reduce(FocusedColumnObject.PropertyName, False, r);
      s := VariantToUtf8(GetNodeDataAsDocVariant(FocusedNode)^.GetValueOrDefault(FocusedColumnObject.PropertyName, ''));
      c.Add(cbkText, s[1], Length(s)+1);
      s := r.ToJson;
      c.Add(cbkJson, s[1], Length(s));
    finally
      c.Close;
    end;
  end;
end;

procedure TTisGrid.DoCopySpecialToClipboard(aSender: TObject);
var
  vBuf: RawByteString;
  vClipAdapter: TClipboardAdapter;
  vExportAdapter: TTisGridExportFormatOptionAdapter;
  vSourceAdapter: TTisGridTextSourceTypeAdapter;
  vParams: record
    Selection: TVSTTextSourceType;
    Format: TTisGridExportFormatOption;
    Columns: record
      VisibleOnly: Boolean;
      Translated: Boolean;
    end;
  end;
  vCopyForm: TCopySpecialForm;
begin
  vCopyForm := TCopySpecialForm.Create(self.Owner);
  try
    vClipAdapter.Open;
    vExportAdapter.EnumsToStrings(vCopyForm.FormatCombo.Items, [efoCsv, efoJson]);
    vCopyForm.FormatCombo.ItemIndex := 0;
    vSourceAdapter.EnumsToStrings(vCopyForm.SelectionCombo.Items);
    vCopyForm.SelectionCombo.ItemIndex := 0;
    if vCopyForm.ShowModal <> mrOK then
      exit;
    vClipAdapter.Clear;
    vParams.Selection := vSourceAdapter.CaptionToEnum(vCopyForm.SelectionCombo.Text);
    vParams.Format := vExportAdapter.CaptionToEnum(vCopyForm.FormatCombo.Text);
    vParams.Columns.VisibleOnly := vCopyForm.ColumnsVisibleOnlyCheckBox.Checked;
    vParams.Columns.Translated := vCopyForm.TranslatedColumnsCheckBox.Checked;
    case vParams.Format of
      efoCsv:
      begin
        vBuf := ContentToCsv(vParams.Selection,
          DefaultCsvSeparator, vParams.Columns.VisibleOnly, vParams.Columns.Translated);
        vClipAdapter.Add(cbkText, vBuf[1], Length(vBuf));
      end;
      efoJson:
      begin
        vBuf := ContentToJson(vParams.Selection, vParams.Columns.VisibleOnly);
        vClipAdapter.Add(cbkText, vBuf[1], Length(vBuf));
        vClipAdapter.Add(cbkJson, vBuf[1], Length(vBuf));
      end;
    else
      raise ETisGrid.Create('Format not enabled to copy from it.');
    end;
  finally
    vClipAdapter.Close;
    vCopyForm.Free;
  end;
end;

procedure TTisGrid.DoCutToClipboard(aSender: TObject);
begin
  if Assigned(fOnCutToClipboard) then
    fOnCutToClipboard(aSender);
end;

procedure TTisGrid.DoDeleteRows(aSender: TObject);
var
  vDoc: TDocVariantData;
  vAsk, vAborted: Boolean;

  function UserConfirmed: Boolean;
  begin
    result := Dialogs.MessageDlg(
      RsConfirmation, Format(RsConfDeleteRow, [SelectedCount]),
      mtConfirmation, mbYesNoCancel, 0) = mrYes;
  end;

begin
  vDoc := SelectedRows;
  if vDoc.IsVoid then
    exit;
  if Assigned(fOnBeforeDeleteRows) then
  begin
    vAsk := True;
    vAborted := False;
    fOnBeforeDeleteRows(self, @vDoc, vAsk, vAborted);
    if vAborted then
      exit;
    if vAsk and (not UserConfirmed) then
      exit;
    DeleteRows(@vDoc);
  end
  else
    if UserConfirmed then
      DeleteRows(@vDoc);
end;

procedure TTisGrid.DoPaste(aSender: TObject);
var
  vClipAdapter: TClipboardAdapter;
  vDoc: PDocVariantData;
begin
  if vClipAdapter.IsValidFor(cbkJson) or vClipAdapter.IsValidFor(cbkText) then
  begin
    vDoc := _Safe(_Json(vClipAdapter.AsJson));
    PasteRows(vDoc);
  end;
end;

procedure TTisGrid.DoSelectAllRows(aSender: TObject);
begin
  SelectAll(False);
end;

procedure TTisGrid.DoPrint(aSender: TObject);
begin
  raise Exception.Create('Not implemented');
end;

procedure TTisGrid.DoCustomizeColumns(aSender: TObject);
begin
  Header.PopupMenu.PopUp;
end;

procedure TTisGrid.DoAdvancedCustomizeColumns(aSender: TObject);
begin
  Customize;
end;

procedure TTisGrid.DoExpandAll(aSender: TObject);
begin
  FullExpand;
end;

procedure TTisGrid.DoCollapseAll(aSender: TObject);
begin
  FullCollapse;
end;

procedure TTisGrid.DoCustomEditor(const aColumn: TTisGridColumn; out aControl: TTisGridControl);
begin
  aControl := nil;
  if Assigned(fOnCustomEditor) then
    fOnCustomEditor(self, aColumn, aControl);
end;

procedure TTisGrid.DoEditorLookup(const aColumn: TTisGridColumn; out
  aControl: TTisGridControl; var aHandled: Boolean);
begin
  aControl := nil;
  aHandled := False;
  if Assigned(fOnEditorLookup) then
  begin
    aControl := TTisGridSearchEditControl.Create;
    fOnEditorLookup(self, aColumn, (aControl as TTisGridSearchEditControl).Edit, aHandled);
    if not aHandled then
      FreeAndNilSafe(aControl);
  end;
end;

procedure TTisGrid.DoPrepareEditor(const aColumn: TTisGridColumn;
  aControl: TTisGridControl);
begin
  if Assigned(fOnPrepareEditor) then
    fOnPrepareEditor(self, aColumn, aControl);
end;

procedure TTisGrid.DoEditValidated(const aColumn: TTisGridColumn;
  const aCurValue: Variant; var aNewValue: Variant; var aAbort: Boolean);
begin
  if Assigned(fOnEditValidated) then
    fOnEditValidated(self, aColumn, aCurValue, aNewValue, aAbort);
end;

function TTisGrid.GetExportDialogFilter: string;
var
  vExportAdapter: TTisGridExportFormatOptionAdapter;
  i: TTisGridExportFormatOption;
begin
  result := '';
  for i := high(TTisGridExportFormatOption) downto low(TTisGridExportFormatOption) do
  begin
    if i in fExportFormatOptions then
    begin
      if result <> '' then
        result += '|';
      result += vExportAdapter.EnumToFilter(i);
    end;
  end;
end;

procedure TTisGrid.RestoreSettings;
begin
  Settings := fDefaultSettings;
end;

constructor TTisGrid.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  fInternalData.Init(self);
  Clear;
  fSelectedData.Clear;
  fSelectedData.InitArray([], JSON_FAST_FLOAT);
  DefaultText := '';
  DefaultCsvSeparator := ',';
  fZebraColor := $00EDF0F1;
  SetLength(fKeyFieldsList, 0);
  fNodeOptions := TTisNodeOptions.Create(self);
  fPopupMenuOptions := DefaultPopupMenuOptions;
  fExportFormatOptions := DefaultExportFormatOptions;
  WantTabs := DefaultWantTabs;
  TabStop := True;
  with TreeOptions do
  begin
    PaintOptions := PaintOptions - [toShowRoot] +
      [toAlwaysHideSelection, toShowHorzGridLines, toShowVertGridLines, toHideFocusRect];
    SelectionOptions := SelectionOptions + [toExtendedFocus, toSimpleDrawSelection, toRightClickSelect];
    MiscOptions := MiscOptions + [toGridExtensions, toFullRowDrag] -
      [toWheelPanning,toEditOnClick,toEditOnDblClick];
    AutoOptions := AutoOptions + [toAutoSort, toAutoChangeScale];
  end;
  Header.Options := [hoColumnResize, hoDblClickResize, hoDrag,
    hoShowSortGlyphs, hoVisible,hoHeaderClickAutoSort];
  Header.Style := hsFlatButtons;
  Header.DefaultHeight := 18;
  Header.MinHeight := 18;
  Header.Height := 18;
  Header.MaxHeight := 100;
  DefaultNodeHeight := 18;
  // initialisation de la boite de dialogue de recherche
  fFindDlg := TFindDialog.Create(self);
  fFindDlg.OnFind := @FindDlgFind;
  fFindDlg.Options := fFindDlg.Options + [frHideMatchCase, frHideEntireScope, frEntireScope, frHideUpDown];
  PopupMenu := TPopupMenu.Create(self);
end;

destructor TTisGrid.Destroy;
begin
  fData.Clear;
  if Assigned(fFindDlg) then
    FreeAndNil(fFindDlg);
  fNodeOptions.Free;
  inherited Destroy;
end;

procedure TTisGrid.FixDesignFontsPPI(const ADesignTimePPI: Integer);
begin
  inherited FixDesignFontsPPI(ADesignTimePPI);
  DoFixDesignFontPPI(Header.Font, ADesignTimePPI);
end;

procedure TTisGrid.ScaleFontsPPI(const AToPPI: Integer; const AProportion: Double);
begin
  inherited ScaleFontsPPI(AToPPI, AProportion);
  DoScaleFontPPI(Header.Font, AToPPI, AProportion);
end;

procedure TTisGrid.Clear;
var
  vPrevReadOnly: Boolean;
begin
  vPrevReadOnly := toReadOnly in TreeOptions.MiscOptions;
  TreeOptions.MiscOptions := TreeOptions.MiscOptions - [toReadOnly];
  inherited Clear;
  if vPrevReadOnly then
    TreeOptions.MiscOptions := TreeOptions.MiscOptions + [toReadOnly];
  fData.Clear;
  fData.InitArray([], JSON_FAST_FLOAT);
end;

function TTisGrid.GetNodeDataAsDocVariant(aNode: PVirtualNode): PDocVariantData;
var
  vData: PCardinal;
begin
  result := nil;
  if aNode = nil then
    aNode := FocusedNode;
  if aNode <> nil then
  begin
    if aNode^.Index < Cardinal(fData.Count) then
    begin
      vData := fInternalData.Data(aNode);
      if vData <> nil then
        result := _Safe(fData.Values[vData^]);
    end;
  end;
end;

procedure TTisGrid.LoadData;

  procedure _ViewInTreeMode;
  var
    i: PtrInt;
    vDoc, vObj: PDocVariantData;
    vNode: PVirtualNode;
    vKeys: TRawUtf8DynArray;
    vParents: TRawUtf8DynArray;
    vEqual: Boolean;
  begin
    StringDynArrayToRawUtf8DynArray(fKeyFieldsList, vKeys);
    StringDynArrayToRawUtf8DynArray(fParentKeyFieldsList, vParents);
    vNode := GetFirst(True);
    while vNode <> nil do
    begin
      vDoc := GetNodeDataAsDocVariant(vNode);
      if vDoc <> nil then
      begin
        for vObj in fData.Objects do
        begin
          vEqual := True;
          for i := low(vParents) to high(vParents) do
          begin
            if vDoc^.U[vParents[i]] <> vObj^.U[vKeys[i]] then
            begin
              vEqual := False;
              break;
            end;
          end;
          if vEqual then
            MoveTo(vNode, GetNodeBy(vObj, True), amAddChildLast, False);
        end;
      end;
      vNode := GetNext(vNode, True);
    end;
  end;

var
  f, t: PDocVariantData;
  a: TNodeArray;
  n: PVirtualNode;
  r: Boolean;
begin
  if fData.IsVoid then
  begin
    r := toReadOnly in TreeOptions.MiscOptions;
    TreeOptions.MiscOptions := TreeOptions.MiscOptions - [toReadOnly];
    inherited Clear;
    if r then
      TreeOptions.MiscOptions := TreeOptions.MiscOptions + [toReadOnly];
  end
  else
  begin
    // stores previous focused and selected rows
    BeginUpdate;
    try
      f := FocusedRow;
      t := GetNodeDataAsDocVariant(TopNode);
      SetLength(a, 0);
      r := toReadOnly in TreeOptions.MiscOptions;
      TreeOptions.MiscOptions := TreeOptions.MiscOptions - [toReadOnly];
      try
        inherited Clear;
        RootNodeCount := fData.Count;
        if IsTreeMode and (KeyFieldsNames <> '') and (ParentKeyFieldsNames <> '') then
          _ViewInTreeMode;
      finally
        if r then
          TreeOptions.MiscOptions := TreeOptions.MiscOptions + [toReadOnly];
      end;
    finally
      try
        // restore selected nodes
        SelectedRows := fSelectedData;
        // restore focused node
        if f <> nil then
          SetFocusedRowNoClearSelection(f);
        // restore top visible node
        if (t <> nil) and not (tsScrolling in TreeStates) then
          a := GetNodesBy(t, KeyFieldsNames <> '');
      finally
        EndUpdate;
        for n in a do
        begin
          TopNode := n;
          break;
        end;
        // restore visible focused column
        ScrollIntoView(FocusedColumn,False);
      end;
    end;
  end;
end;

function TTisGrid.TryLoadAllFrom(const aJson: string; aShowError: Boolean): Boolean;

  procedure _ShowError;
  begin
    if aShowError then
      Dialogs.MessageDlg('Invalid JSON array of records', mtError, [mbOK], 0);
  end;

var
  doc: TDocVariantData;
  d: PDocVariantData;
  i, r: PVariant;
begin
  result := False;
  try
    if doc.InitJson(StringToUtf8(aJson), JSON_FAST_FLOAT) then
    begin
      if doc.Kind = dvArray then
      begin
        for i in doc.Items do // using .Items to get all kind of data, eg: [1,2,3]
        begin
          d := PDocVariantData(i);
          case d^.Kind of
            dvArray:
              for r in d^.Items do
                Data.AddItem(r^);
            dvObject:
              Data.AddItem(i^);
          else
            Data.AddItem(_Json('{"unknown":"' + VariantToUtf8(i^) + '"}'));
          end;
        end;
      end
      else if doc.Kind = dvObject then
      begin
        Data.AddItem(variant(doc));
      end;
      LoadData;
      CreateColumnsFromData(True, False);
      result := True;
    end
    else
      _ShowError;
  except
    _ShowError;
  end;
end;

procedure TTisGrid.ExportData(const aFileName: TFileName;
  const aSelection: TVSTTextSourceType);

  procedure _SaveToFile(const aBuffer: RawUtf8);
  var
    vBuf: PUtf8Char;
    l: LongInt;
    vFile: File;
  begin
    AssignFile(vFile, aFileName);
    Rewrite(vFile,1);
    try
      vBuf := PUtf8Char(aBuffer + #0);
      l := StrLen(vBuf);
      BlockWrite(vFile, vBuf^, l);
    finally
      CloseFile(vFile);
    end;
  end;

var
  vBuf: RawUtf8;
  vExportAdapter: TTisGridExportFormatOptionAdapter;
begin
  vBuf := '';
  case vExportAdapter.ExtensionToEnum(SysUtils.LowerCase(ExtractFileExt(aFileName))) of
    efoCsv:
      vBuf := ContentToCsv(aSelection, DefaultCsvSeparator);
    efoJson:
      vBuf := ContentToJson(aSelection);
    efoHtml:
      vBuf := StringToUtf8(ContentToHTML(aSelection));
    efoRtf:
      vBuf := StringToUtf8(ContentToRTF(aSelection));
    efoText:
      vBuf := StringToUtf8(ContentToText(aSelection, ','));
  else
    DoExportCustomContent(aSelection, vBuf);
  end;
  _SaveToFile(vBuf);
end;

function TTisGrid.CheckedRows: TDocVariantData;
var
  n: PVirtualNode;
begin
  n := GetFirstChecked;
  result.InitArray([]);
  while n <> nil do
  begin
    result.AddFrom(variant(GetNodeDataAsDocVariant(n)^));
    n := GetNextChecked(n, csCheckedNormal, True);
  end;
end;

procedure TTisGrid.SetFocusedRowNoClearSelection(aValue: PDocVariantData);
var
  a: TNodeArray;
begin
  if (aValue = nil) or (aValue^.IsVoid) then
    FocusedNode := nil
  else
  begin
    a := GetNodesBy(aValue, True);
    if Length(a) > 0 then
    begin
      FocusedNode := a[0];
      Selected[a[0]] := True;
      ScrollIntoView(FocusedNode, False);
    end;
  end;
end;

function TTisGrid.GetCellData(aNode: PVirtualNode; const aColName: RawUtf8;
  aDefault: PDocVariantData): PDocVariantData;
begin
  result := aDefault;
  if aNode <> nil then
  begin
    result := GetNodeDataAsDocVariant(aNode);
    if result <> nil then
    begin
      if result^.GetValueIndex(aColName) = -1 then
        result := aDefault;
    end;
  end;
end;

function TTisGrid.GetCellDataAsString(aNode: PVirtualNode;
  const aColName: RawUtf8; const aDefault: string): string;
var
  d: PDocVariantData;
begin
  d := GetCellData(aNode, aColName);
  if d = nil then
    result := aDefault
  else if d^.Kind = dvArray then
    result := Utf8ToString(d^.ToCsv(','))
  else
    result := d^.S[aColName];
end;

function TTisGrid.GetNodesBy(aData: PDocVariantData;
  aUseKeyFieldsList: Boolean): TNodeArray;

  procedure _Add(var aArray: TNodeArray; aNode: PVirtualNode);
  begin
    SetLength(aArray, length(aArray) + 1);
    aArray[Length(aArray) - 1] := aNode;
  end;

var
  d: PDocVariantData;
  p: PVirtualNode;
  a, b: TDocVariantData;
  vArray: TRawUtf8DynArray;
  vUseArray: Boolean;
begin
  SetLength(result, 0);
  if not assigned(aData) or aData^.IsVoid then
    exit;
  vUseArray := aUseKeyFieldsList and (Length(fKeyFieldsList) > 0);
  if vUseArray then
    StringDynArrayToRawUtf8DynArray(fKeyFieldsList, vArray);
  p := GetFirst(True);
  while p <> nil do
  begin
    d := GetNodeDataAsDocVariant(p);
    if d <> nil then
    begin
      if vUseArray then
      begin
        a.Clear;
        d^.Reduce(vArray, False, a);
        b.Clear;
        aData^.Reduce(vArray, False, b);
        if a.Equals(b) then
          _Add(result, p);
      end
      else if d^.Equals(aData^) then
        _Add(result, p);
    end;
    p := GetNext(p, True);
  end;
end;

function TTisGrid.GetNodeBy(aData: PDocVariantData; aUseKeyFieldsList: Boolean;
  aRowPosition: PtrInt): PVirtualNode;
var
  a: TNodeArray;
begin
  a := GetNodesBy(aData, aUseKeyFieldsList);
  if a = nil then
    result := nil
  else
    result := a[aRowPosition];
end;

function TTisGrid.GetNodesBy(const aKey, aValue: RawUtf8): TNodeArray;
var
  d: PDocVariantData;
  p: PVirtualNode;
begin
  SetLength(result, 0);
  p := GetFirst(True);
  while p <> nil do
  begin
    d := GetNodeDataAsDocVariant(p);
    if (d <> nil) and (not d^.IsVoid) and (d^.U[aKey] = aValue) then
    begin
      SetLength(result, Length(result) + 1);
      result[Length(result)-1] := p;
    end;
    p := GetNext(p, True);
  end;
end;

procedure TTisGrid.AddRows(aData: PDocVariantData; aAllowDuplicates: Boolean;
  aCreateColumns: Boolean);
begin
  // don't add if already in grid...
  if aAllowDuplicates or
    ((Length(KeyFieldsList) = 0) and (Length(GetNodesBy(aData)) = 0)) or
    (Length(GetNodesBy(aData, True)) = 0) then
  begin
    Add(aData);
    if aCreateColumns and (Header.Columns.Count = 0) then
      CreateColumnsFromData(True, False);
  end;
end;

procedure TTisGrid.PasteRows(aRows: PDocVariantData);
var
  vAborted: Boolean;
begin
  vAborted := False;
  if Assigned(fOnBeforePaste) then
    fOnBeforePaste(self, aRows, vAborted);
  if not vAborted then
    Add(aRows);
end;

procedure TTisGrid.DeleteRows(aRows: PDocVariantData);
var
  o: PDocVariantData;
  i: Integer;
begin
  if aRows = nil then
    exit;
  for o in aRows^.Objects do
  begin
    // remove from Data
    for i := fData.Count - 1 downto 0 do
      if _Safe(fData.Values[i])^.Equals(o^) then
        fData.Delete(i);
    LoadData;
    // go to the last (new) row, if user has deleted latest ones
    if FocusedRow = nil then
      FocusedRow := GetNodeDataAsDocVariant(GetLast);
  end;
end;

procedure TTisGrid.DeleteSelectedRows;
begin
  DoDeleteRows(self);
end;

procedure TTisGrid.InvalidateNodeByDocVariant(const aData: PDocVariantData);
var
  p: PVirtualNode;
begin
  if aData = nil then
    exit;
  p := GetFirst(True);
  while p <> nil do
  begin
    if GetNodeDataAsDocVariant(p) = aData then
      InvalidateNode(p);
    p := GetNext(p, True);
  end;
end;

procedure TTisGrid.ClearAll;
begin
  Clear;
  Header.Columns.Clear;
end;

function SortColumnsPosition(c1, c2: TCollectionItem): integer;
begin
  result := 0;
  if (c1 = nil) or (c2 = nil) then
  begin
    if c1 = nil then
      result := -1;
    if c2 = nil then
      result := 1;
  end
  else
  begin
    if TTisGridColumn(c1).Tag < TTisGridColumn(c2).Tag then
      result := -1
    else
      if TTisGridColumn(c1).Tag > TTisGridColumn(c2).Tag then
        result := 1
  end;
end;

procedure TTisGrid.ReorderColumns;
var
  i: TColumnIndex;
  vFocusedColumn: TTisGridColumn;
begin
  try
    vFocusedColumn := FocusedColumnObject;
    for i := 0 to Header.Columns.Count-1 do
      Header.Columns[i].Tag := Header.Columns[i].Position;
    Header.Columns.Sort(@SortColumnsPosition);
    for i := 0 to Header.Columns.Count-1 do
      Header.Columns[i].Position := TTisGridColumn(Header.Columns[i]).Index;
  finally
    FocusedColumnObject := vFocusedColumn;
  end;
end;

procedure TTisGrid.Customize;
var
  vTarget: TTisGrid;
begin
  BeginUpdate;
  try
    with TTisGridEditor.Create(Application) do
    try
      vTarget := self;
      Grid.ClearAll;
      Grid.Header.Assign(vTarget.Header);
      Grid.TreeOptions.Assign(vTarget.TreeOptions);
      Grid.NodeOptions.Assign(vTarget.NodeOptions);
      Grid.Settings := vTarget.Settings;
      Grid.KeyFieldsNames := vTarget.KeyFieldsNames;
      Grid.ParentKeyFieldsNames := vTarget.ParentKeyFieldsNames;
      if ShowModal = mrOK then
      begin
        vTarget.ClearAll;
        vTarget.Header.Assign(Grid.Header);
        vTarget.TreeOptions.Assign(Grid.TreeOptions);
        vTarget.NodeOptions.Assign(Grid.NodeOptions);
        vTarget.KeyFieldsNames := KeyNamesEdit.Text;
        vTarget.ParentKeyFieldsNames := ParentNamesEdit.Text;
        if KeepDataCheckBox.Checked then
          vTarget.Data := Grid.Data;
      end;
    finally
      Free;
    end;
  finally
    EndUpdate;
  end;
end;

procedure TTisGrid.NotifyChange(aEventType: TTisDataEvent;
  aRow: PDocVariantData; const aOldValues, aNewValues: TDocVariantData);
begin
  if not (csDestroying in ComponentState) then
  begin
    if (aEventType in [deUpdateRecord]) and (aRow <> nil) then
      InvalidateNodeByDocVariant(aRow)
    else if aEventType in [deUpdateState, deDataSetChange, deAddrecord, deDeleteRecord] then
      LoadData
    else
      Invalidate;
  end;
end;

procedure TTisGrid.CreateColumnsFromData(aAutoFitColumns,
  aAppendMissingAsHidden: Boolean);
var
  n: PRawUtf8;
  o: PDocVariantData;
  c: TTisGridColumn;
  x: Integer;
begin
  if fData.IsVoid then
    exit;
  x := NoColumn;
  BeginUpdate;
  try
    for o in fData.Objects do
    begin
      for n in o^.FieldNames do
      begin
        c := FindColumnByPropertyName(n^);
        if c = nil then
        begin
          c := Header.Columns.Add as TTisGridColumn;
          x := c.Index;
          c.Text := Utf8ToString(n^);
          c.PropertyName := n^;
          c.Width := 100;
          if aAppendMissingAsHidden then
            c.Options := c.Options - [coVisible];
          if VarType(o^.Value[n^]) in [varDouble, varCurrency, varInteger] then
            c.Alignment := taRightJustify;
        end;
      end;
    end;
  finally
    if aAutoFitColumns and (x <> NoColumn) then
      Header.AutoFitColumns(False, smaUseColumnOption, x);
    EndUpdate;
  end;
end;

function TTisGrid.ContentToCsv(aSource: TVSTTextSourceType;
  const aSeparator: string; aColumnsVisibleOnly: Boolean;
  aColumnsTranslated: Boolean): RawUtf8;
var
  vTmp, vCols, vRows: TDocVariantData;
  vCol: TTisGridColumn;
  c: Integer;
  s: RawUtf8;
  o: PDocVariantData;
begin
  if aSource in [tstAll, tstInitialized, tstVisible] then
    vRows := fData
  else
    vRows := SelectedRows;
  vCols.InitArray([], JSON_FAST_FLOAT);
  for c := 0 to Header.Columns.Count-1 do
  begin
    vCol := TTisGridColumn(Header.Columns[c]);
    if ((coVisible in vCol.Options) or not aColumnsVisibleOnly) and (vCol.DataType <> cdtPassword) then
    begin
      if aColumnsTranslated then
        vCols.AddItemText(StringToUtf8('"' + vCol.Text + '"'))
      else
        vCols.AddItemText(StringToUtf8('"' + vCol.PropertyName + '"'));
    end;
  end;
  result := vCols.ToCsv(aSeparator) + LineEnding;
  vTmp.InitArray([], JSON_FAST_FLOAT);
  for o in vRows.Objects do
  begin
    vTmp.Reset;
    for c := 0 to Header.Columns.Count-1 do
    begin
      vCol := TTisGridColumn(Header.Columns[c]);
      if ((coVisible in vCol.Options) or not aColumnsVisibleOnly) and (vCol.DataType <> cdtPassword) then
      begin
        s := o^.U[vCol.PropertyName];
        if s <> '' then
        begin
          if VarType(o^.Value[s]) in [varDouble, varCurrency, varInteger] then
            vTmp.AddItemText(s)
          else
            vTmp.AddItemText(QuotedStr(s, '"'));
        end
        else
          vCols.AddItemText('""');
      end;
    end;
    result := result + vTmp.ToCsv(aSeparator) + LineEnding;
  end;
end;

function TTisGrid.ContentToJson(aSource: TVSTTextSourceType;
  aColumnsVisibleOnly: Boolean): RawUtf8;
var
  vCols, vRows, vRes: TDocVariantData;
  vCol: TTisGridColumn;
  c: Integer;
begin
  if aSource in [tstAll, tstInitialized, tstVisible] then
    vRows := fData
  else
    vRows := SelectedRows;
  vCols.InitArray([], JSON_FAST_FLOAT);
  for c := 0 to Header.Columns.Count-1 do
  begin
    vCol := TTisGridColumn(Header.Columns[c]);
    if ((coVisible in vCol.Options) or not aColumnsVisibleOnly) and (vCol.DataType <> cdtPassword) then
      vCols.AddItemText(vCol.PropertyName);
  end;
  vRes.Clear;
  vRows.Reduce(vCols.ToRawUtf8DynArray, False, vRes);
  result := vRes.ToJson;
end;

procedure TTisGrid.UpdateSelectedAndTotalLabel;
var
  t, s: Integer;
begin
  if not Assigned(fSelectedAndTotalLabel) then
    exit;
  if not fData.IsVoid then
    t := fData.Count
  else
    t := 0;
  s := SelectedCount;
  if s > 0 then
    fSelectedAndTotalLabel.Caption := Format('Selected / Total : %d / %d', [s, t])
  else
    fSelectedAndTotalLabel.Caption := Format('Total : %d elements', [t]);
end;

procedure TTisGrid.SaveSettingsToIni(const aFileName: TFileName);
var
  vJson: RawUtf8;
  vB64: string;
  vIni: TIniFile;
begin
  vJson := _Safe(Settings)^.ToJson;
  vB64 := BinToBase64(Utf8ToString(vJson));
  vIni := TIniFile.Create(aFileName);
  try
    vIni.WriteString(Owner.Name, Name, vB64);
  finally
    FreeAndNil(vIni);
  end;
end;

procedure TTisGrid.LoadSettingsFromIni(const aFileName: TFileName);
var
  vB64: string;
  vIni: TIniFile;
begin
  vIni := TIniFile.Create(aFileName);
  try
    vB64 := vIni.ReadString(Owner.Name, Name, '');
    if vB64 <> '' then
    begin
      SetSettings(_Json(Base64ToBin(StringToUtf8(vB64))));
    end;
  finally
    FreeAndNil(vIni);
  end;
end;

function TTisGrid.IsTreeMode: Boolean;
begin
  result := TREEMODE_OPTIONS <= TreeOptions.PaintOptions;
end;

procedure TTisGrid.ExpandAllNodes;
var
  n: PVirtualNode;
begin
  n := GetFirst;
  BeginUpdate;
  while Assigned(n) do
  begin
    Expanded[n] := True;
    n := GetNext(n);
  end;
  EndUpdate;
end;

procedure TTisGrid.CollapseAllNodes;
var
  n: PVirtualNode;
begin
  n := GetFirst;
  BeginUpdate;
  while Assigned(n) do
  begin
    Expanded[n] := False;
    n := GetNext(n);
  end;
  EndUpdate;
end;

function TTisGrid.Search(const aText: string): Boolean;
begin
  result := False;
  if (Trim(aText) = '') or fData.IsVoid then
    exit;
  FocusedNode := GetFirstVisible;
  fStartSearchNode := nil;
  ColumnToFind := NoColumn;
  TextFound := False;
  TextToFind := aText;
  DoFindNext(self);
  result := TextFound;
end;

end.
