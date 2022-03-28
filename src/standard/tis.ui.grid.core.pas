// -----------------------------------------------------------------
//    This file is part of Tranquil IT Software
//    Copyright (C) 2012 - 2021  Tranquil IT https://www.tranquil.it
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
    cdtMemo
  );

  TTisColumnDataTypes = set of TTisColumnDataType;

  /// adapter for TTisColumnDataType
  TTisColumnDataTypeAdapter = object
    /// convert enum to caption
    function EnumToCaption(const aValue: TTisColumnDataType): string;
    /// convert enum to index
    function EnumToIndex(const aValue: TTisColumnDataType): Integer;
    /// convert caption to enum
    // - if aValue not found, it will return the first element
    function CaptionToEnum(const aValue: string): TTisColumnDataType;
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
    procedure EditKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState); virtual;
    procedure EditExit(Sender: TObject); virtual;
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
    function GetTitle: TCaption;
    procedure SetTitle(const aValue: TCaption);
    procedure SetPropertyName(const aValue: RawUtf8);
  protected const
    DefaultDataType = cdtString;
    DefaultRequired = False;
  public
    constructor Create(aCollection: TCollection); override;
    procedure Assign(aSource: TPersistent); override;
  published
    property Text: TCaption read GetTitle write SetTitle;
    property PropertyName: RawUtf8 read fPropertyName write SetPropertyName;
    property DataType: TTisColumnDataType read fDataType write fDataType default DefaultDataType;
    /// if TRUE, it will not allow user to set NULL for this column using Editor
    property Required: Boolean read fRequired write fRequired default DefaultRequired;
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

  TOnGridHeaderAddPopupItem = procedure(const sender: TBaseVirtualTree; const aColumn: TColumnIndex;
    var aItem: TTisGridHeaderPopupItem) of object;

  TOnGridHeaderColumnChange = procedure(const sender: TBaseVirtualTree; const aColumn: TColumnIndex; aVisible: Boolean) of object;

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
    procedure OnMenuItemClick(sender: TObject);
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
    /// creates a default popup menu to inherited PopupMenu
    procedure NewDefaultPopupMenu;
  public
    constructor Create(aOwner: TBaseVirtualTree); override;
    // overriding the original Assign for do not assign PopupMenu from another grid header
    // - a Popup in design mode will not work when using Editor
    procedure Assign(aSource: TPersistent); override;
    property PopupMenu: TPopupMenu read GetPopupMenu write SetPopupMenu;
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
  TTisPopupMenuOption = (
    pmoShowFind,
    pmoShowFindNext,
    pmoShowCut,
    pmoShowCopy,
    pmoShowCopyCell,
    pmoShowPaste,
    pmoShowDelete,
    pmoShowSelectAll,
    pmoShowCustomizeColumns,
    // below here, False by default
    pmoShowExportExcel,
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

  TOnGridGetText = procedure(sender: TBaseVirtualTree; aNode: PVirtualNode;
    const aCell: TDocVariantData; aColumn: TColumnIndex; aTextType: TVSTTextType;
    var aText: string) of object;

  TOnGridRows = procedure(sender: TTisGrid; aRows: PDocVariantData) of object;

  /// event to manipulate rows before deleting them
  // - as used by TTisGrid.OnBeforeDeleteRows
  // - use it for change the rows or abort the process
  TOnGridBeforeDeleteRows = procedure (sender: TTisGrid; aRows: PDocVariantData;
    var aAskUser, aAbort: Boolean) of object;

  /// event to manipulate data before change the internal Data
  // - as used by TTisGrid.OnBeforeDataChage
  // - use it for check/change the aData argument, before assign it, and/or abort the process
  TOnGridBeforeDataChange = procedure (sender: TTisGrid; aData: PDocVariantData;
    var aAbort: Boolean) of object;

  /// event to manipulate data after Data changed
  // - as used by TTisGrid.OnAfterDataChage
  TOnGridAfterDataChange = type TNotifyEvent;

  /// event for comparing rows of objects
  // - as used by TTisGrid.OnCompareByRow
  TOnGridCompareByRow = function(sender: TTisGrid; const aPropertyName: RawUtf8;
    const aRow1, aRow2: TDocVariantData; var aHandled: Boolean): PtrInt of object;

  /// event to manipulate rows using copy/paste on grid
  // - use it for check/change the aData argument, before assign it, and/or abort the process
  TOnGridPaste = procedure(sender: TTisGrid; aData: PDocVariantData; var aAbort: Boolean) of object;

  /// event that allows users customize the control instance, creating a new one, replacing the default
  TOnGridCustomEditor = procedure(sender: TObject; aColumn: TTisGridColumn;
    out aControl: TTisGridControl) of object;

  /// event that simplifies the use of a TisSearchEdit as Edit Control
  TOnGridEditorLookup = procedure(sender: TTisGrid; aColumn: TTisGridColumn;
    aSearchEdit: TTisSearchEdit; var aHandled: Boolean) of object;

  /// event that allows users to change some edit control properties, before it shows up
  TOnGridPrepareEditor = procedure(sender: TTisGrid; aColumn: TTisGridColumn;
    aControl: TWinControl) of object;

  /// event that allow to validate the new value from user input
  // - aCurValue is the current value for the aColumn
  // - use it for check/change the aNewValue argument, before assign it, and/or abort the process
  TOnGridEditValidated = procedure(sender: TTisGrid; aColumn: TTisGridColumn;
    const aCurValue: Variant; var aNewValue: Variant; var aAbort: Boolean) of object;

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
    fKeyFieldsList: array of string;
    fParentProperty: string;
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
    // ------------------------------- new events ----------------------------------
    fOnGetText: TOnGridGetText;
    fOnCutToClipBoard: TNotifyEvent;
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
    // ------------------------------- HMENU ----------------------------------
    HMUndo, HMRevert: HMENU;
    HMFind, HMFindNext, HMReplace: HMENU;
    HMCut, HMCopy, HMCopyCell, HMPaste, HMFindReplace: HMENU;
    HMInsert, HMDelete, HMSelAll: HMENU;
    HMExcel, HMPrint: HMENU;
    HMCollAll, HMExpAll: HMENU;
    HMCustomize: HMENU;
    HMAdvancedCustomize: HMENU;
    // ------------------------------- new methods ----------------------------------
    function FocusedPropertyName: string;
    function GetFocusedColumnObject: TTisGridColumn;
    function GetFocusedRow: PDocVariantData;
    function GetKeyFieldsNames: string;
    procedure SetKeyFieldsNames(const aValue: string);
    function GetSettings: Variant;
    procedure SetSettings(const aValue: Variant);
    function GetGridSettings: string;
    procedure SetGridSettings(const aValue: string);
    procedure SetColumnToFind(aValue: integer);
    procedure SetData(const aValue: TDocVariantData);
    procedure SetFocusedColumnObject(aValue: TTisGridColumn);
    procedure SetFocusedRow(aValue: PDocVariantData);
    procedure SetOnCutToClipBoard(aValue: TNotifyEvent);
    function GetOptions: TStringTreeOptions;
    procedure SetOptions(const aValue: TStringTreeOptions);
    procedure SetParentProperty(const aValue: string);
    function GetSelectedRows: TDocVariantData;
    /// select all the nodes matching the aValue array list of TDocVariantData
    procedure SetSelectedRows(const aValue: TDocVariantData);
    function GetSelectedObjects: PDocVariantDataDynArray;
    function GetSelectedRow: TDocVariantData;
    procedure SetSelectedRow(aValue: TDocVariantData);
    procedure SetSelectedAndTotalLabel(aValue: TLabel);
    procedure WMKeyDown(var Message: TLMKeyDown); message LM_KEYDOWN;
  protected
    // ------------------------------- inherited methods ----------------------------------
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
    procedure DoAutoAdjustLayout(const AMode: TLayoutAdjustmentPolicy;
      const AXProportion, AYProportion: Double); override;
    procedure DoChange(Node: PVirtualNode); override;
    function GetHeaderClass: TVTHeaderClass; override;
    property RootNodeCount stored False;
    // ----------------------------------- new methods --------------------------------------
    /// standard menu management
    procedure FillPopupMenu(sender: TObject);
    function FindText(const aText: string): PVirtualNode;
    procedure FindDlgFind(Sender: TObject);
    /// add aData into Data property
    // - will test if it is an array or object
    // - returns TRUE if something has been added
    function Add(aData: PDocVariantData): Boolean;
    function DoCompareByRow(const aPropertyName: RawUtf8; const aRow1,
      aRow2: PDocVariantData): PtrInt; virtual;
    procedure DoFindText(Sender: TObject);
    procedure DoFindNext(Sender: TObject);
    procedure DoFindReplace(Sender: TObject);
    procedure DoUndoLastUpdate(Sender: TObject); virtual;
    procedure DoRevertRecord(Sender: TObject); virtual;
    procedure DoExportExcel(Sender: TObject); virtual;
    procedure DoCopyToClipBoard(Sender: TObject); virtual;
    procedure DoCopyCellToClipBoard(Sender: TObject); virtual;
    procedure DoCutToClipBoard(Sender: TObject); virtual;
    procedure DoDeleteRows(Sender: TObject); virtual;
    procedure DoPaste(Sender: TObject); virtual;
    procedure DoSelectAllRows(Sender: TObject); virtual;
    procedure DoPrint(Sender: TObject); virtual;
    procedure DoCustomizeColumns(Sender: TObject); virtual;
    procedure DoAdvancedCustomizeColumns(Sender: TObject); virtual;
    procedure DoExpandAll(Sender: TObject); virtual;
    procedure DoCollapseAll(Sender: TObject); virtual;
    /// performs OnCustonEditor event, if it was assigned
    procedure DoCustomEditor(const aColumn: TTisGridColumn; out aControl: TTisGridControl); virtual;
    /// performs OnEditorLookup event, if it was assigned
    procedure DoEditorLookup(const aColumn: TTisGridColumn; out
      aControl: TTisGridControl; var aHandled: Boolean); virtual;
    /// performs OnPrepareEditor event, if it was assigned
    procedure DoPrepareEditor(const aColumn: TTisGridColumn; aControl: TWinControl); virtual;
    procedure DoEditValidated(const aColumn: TTisGridColumn; const aCurValue: Variant;
      var aNewValue: Variant; var aAbort: Boolean); virtual;
    property ColumnToFind: integer read fColumnToFind write SetColumnToFind;
    property TextToFind: string read fTextToFind write fTextToFind;
    property TextFound: boolean read fTextFound write fTextFound;
  public
    /// primary construtor
    constructor Create(AOwner: TComponent); override;
    /// destructor
    destructor Destroy; override;
    // ------------------------------- inherited methods ----------------------------------
    procedure FixDesignFontsPPI(const ADesignTimePPI: Integer); override;
    procedure ScaleFontsPPI(const AToPPI: Integer; const AProportion: Double); override;
    /// it will clear Data and everything else related
    procedure Clear; override;
    // ----------------------------------- new methods --------------------------------------
    /// cast aNode in PDocVariantData
    // - will get the same aNode.Index in Data
    // - if aNode is nil, it will use FocusedNode as default
    function GetNodeDataAsDocVariant(aNode: PVirtualNode = nil): PDocVariantData;
    /// refresh the grid using Data content
    // - call LoadData, if you change Data content directly
    procedure LoadData;
    /// it will try load aJson into Data and create Columns from it
    // - return TRUE if success, otherwise FALSE, with a Dialog error if aShowError is TRUE
    function TryLoadAllFrom(const aJson: string; aShowError: Boolean = True): Boolean;
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
    /// returns a list of nodes matching exactly this record
    // - if fKeyFieldsList is not empty, only fields from fKeyFieldsList are taken in account
    function GetNodesBy(aData: PDocVariantData; aUseKeyFieldsList: boolean = False): TNodeArray; overload;
    /// returns a list of nodes that matching with key and value
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
    /// it will call Clear, plus it will clear everything else as Columns, etc
    procedure ClearAll; virtual;
    /// sort columns headers collection based on manual positioning
    procedure ReorderColumns;
    /// shows grid editor
    procedure Customize;
    /// source events handling
    procedure NotifyChange(aEventType: TTisDataEvent; aRow: PDocVariantData;
      const aOldValues, aNewValues: TDocVariantData);
    /// add columns based on Data content
    procedure CreateColumnsFromData(aAutoFitColumns, aAppendMissingAsHidden: Boolean);
    function ContentAsCSV(aSource: TVSTTextSourceType; const aSeparator: string): RawUtf8;
    /// creates a temporary CSV file and open it in the default app
    procedure ExportExcel(const aPrefix: string; aSelection: TVSTTextSourceType;
      aSeparator: Char);
    /// force refresh the "Selected / Total : %d/%d" label
    procedure UpdateSelectedAndTotalLabel;
    /// save Settings to an IniFile
    procedure SaveSettingsToIni(const aFileName: TFileName);
    /// load Settings from an IniFile
    procedure LoadSettingsFromIni(const aFileName: TFileName);
    // ------------------------------- inherited events ----------------------------------
    property OnCompareNodes; // hiding from Object Inspector, use OnCompareByRow event instead
    // ------------------------------- new properties ----------------------------------
    /// direct access to the low-level internal data
    // - if you change its content directly, you should call Invalidate or LoadData for VirtualTree be aware about it
    property Data: TDocVariantData
      read fData write SetData;
    property ParentProperty: string
      read fParentProperty write SetParentProperty;
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
    // ------------------------------- inherited properties ----------------------------------
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
    property WantTabs;
    // ------------------------------- new properties ----------------------------------
    property SelectedAndTotalLabel: TLabel
      read fSelectedAndTotalLabel write SetSelectedAndTotalLabel;
    property TreeOptions: TStringTreeOptions
      read GetOptions write SetOptions;
    property KeyFieldsList: TStringDynArray
      read fKeyFieldsList;
    property KeyFieldsNames: string
      read GetKeyFieldsNames write SetKeyFieldsNames;
    property GridSettings: string
      read GetGridSettings write SetGridSettings stored False;
    property ZebraColor: TColor
      read fZebraColor write fZebraColor;
    property ZebraPaint: Boolean
      read fZebraPaint write fZebraPaint stored True default False;
    property NodeOptions: TTisNodeOptions
      read fNodeOptions write fNodeOptions;
    property PopupMenuOptions: TTisPopupMenuOptions
      read fPopupMenuOptions write fPopupMenuOptions;
    // ------------------------------- inherited events ----------------------------------
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
    property OnCutToClipBoard: TNotifyEvent
      read fOnCutToClipBoard write SetOnCutToClipBoard;
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
  end;

resourcestring
  RsNoRecordFind = 'No more record found for "%s"';
  RsPrintOn = 'Printed on';
  RsPage = 'Page';
  RsConfirmation = 'Confirm';
  RsUndoLastUpdate = 'Undo last change';
  RsRevertRecord = 'Revert to initial record';
  RsFind = 'Search...';
  RsFindNext = 'Find next';
  RsFindReplace = 'Find and replace...';
  RsCopy = 'Copy';
  RsCopyCell = 'Copy cell';
  RsCut = 'Cut';
  RsPaste = 'Paste';
  RsInsert = 'Insert';
  RsDelete = 'Delete';
  RsDeleteRows = 'Delete selected rows';
  RsConfDeleteRow = 'Confirm the deletion of the %d selected rows ?';
  RsSelectAll = 'Select all rows';
  RsExportSelectedExcel = 'Export selected rows to CSV file...';
  RsExportAllExcel = 'Export all rows to CSV file...';
  RsPrint = 'Print...';
  RsExpandAll = 'Expand all';
  RsCollapseAll = 'Collapse all';
  RsCustomizeColumns = 'Customize columns...';
  RsAdvancedCustomizeColumns = 'Advanced customize of table...';

implementation

uses
  IniFiles,
  Variants,
  tis.ui.grid.editor;

{ TTisColumnDataTypeAdapter }

var
  COLUMNDATATYPES: array [TTisColumnDataType] of record
    Caption: string;
  end = (
    (Caption: 'String'),
    (Caption: 'Date'),
    (Caption: 'Time'),
    (Caption: 'DateTime'),
    (Caption: 'Integer'),
    (Caption: 'Float'),
    (Caption: 'Boolean'),
    (Caption: 'Memo')
  );

function TTisColumnDataTypeAdapter.EnumToCaption(const aValue: TTisColumnDataType): string;
begin
  result := COLUMNDATATYPES[aValue].Caption;
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
  for i := low(COLUMNDATATYPES) to high(COLUMNDATATYPES) do
    if COLUMNDATATYPES[i].Caption = aValue then
    begin
      result := i;
      exit;
    end;
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

procedure TTisGridEditLink.EditKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
var
  CanAdvance: Boolean;
begin
  CanAdvance := True;
  case Key of
    VK_ESCAPE:
      if CanAdvance then
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
        CanAdvance := Shift = [];
        if fControl.Internal is TCustomComboBox then
          CanAdvance := CanAdvance and not TCustomComboBox(fControl.Internal).DroppedDown;
        if fControl.Internal is TCustomMemo then
          CanAdvance := False;
        if CanAdvance then
        begin
          // forward the keypress to the tree
          // it will asynchronously change the focused node
          PostMessage(fGrid.Handle, LM_KEYDOWN, Key, 0);
          Key := 0;
        end;
      end;
  end;
end;

procedure TTisGridEditLink.EditExit(Sender: TObject);
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
end;

function TTisGridEditLink.NewControl(aColumn: TTisGridColumn): TTisGridControl;
var
  handled: Boolean;
begin
  fGrid.DoCustomEditor(aColumn, result);
  if result = nil then
  begin
    handled := False;
    fGrid.DoEditorLookup(aColumn, result, handled);
    if not handled then
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
  d: PDocVariantData;
  c: TTisGridColumn;
  aborted: Boolean;
  cur, new: Variant;
begin
  result := True;
  d := fGrid.GetNodeDataAsDocVariant(fNode);
  c := fGrid.FindColumnByIndex(fColumn);
  aborted := False;
  cur := d^.GetValueOrNull(c.PropertyName);
  new := fControl.GetValue;
  fGrid.DoEditValidated(c, cur, new, aborted);
  try
    if aborted then
      exit;
    if VarIsNull(new) then
    begin
      if not c.Required then
        d^.Value[c.PropertyName] := NULL;
    end
    else
      case c.DataType of
        cdtString, cdtMemo:
          d^.S[c.PropertyName] := VarToStr(new);
        cdtDate, cdtTime, cdtDateTime:
          d^.U[c.PropertyName] := DateTimeToIso8601Text(new);
        cdtInteger:
          d^.I[c.PropertyName] := new;
        cdtFloat:
          d^.D[c.PropertyName] := new;
        cdtBoolean:
          d^.B[c.PropertyName] := new;
      else
        d^.S[c.PropertyName] := VarToStr(new);
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
  d: PDocVariantData;
  c: TTisGridColumn;
begin
  result := True;
  fGrid := aTree as TTisGrid;
  fNode := aNode;
  fColumn := aColumn;
  FreeAndNil(fControl);
  d := fGrid.GetNodeDataAsDocVariant(fNode);
  c := fGrid.FindColumnByIndex(fColumn);
  fControl := NewControl(c);
  case c.DataType of
    cdtString, cdtMemo:
      fControl.SetValue(d^.S[c.PropertyName]);
    cdtDate, cdtTime, cdtDateTime:
      fControl.SetValue(Iso8601ToDateTime(d^.U[c.PropertyName]));
    cdtInteger:
      fControl.SetValue(d^.I[c.PropertyName]);
    cdtFloat:
      fControl.SetValue(d^.D[c.PropertyName]);
    cdtBoolean:
      fControl.SetValue(d^.B[c.PropertyName]);
  else
    fControl.SetValue(d^.S[c.PropertyName]);
  end;
  fGrid.DoPrepareEditor(c, fControl.Internal);
end;

procedure TTisGridEditLink.ProcessMessage(var aMessage: TLMessage); stdcall;
begin
  PostMessage(fControl.Internal.Handle, aMessage.Msg, aMessage.wParam, aMessage.lParam);
end;

procedure TTisGridEditLink.SetBounds(R: TRect); stdcall;
var
  dummy: Integer;
begin
  // Since we don't want to activate grid extensions in the tree (this would
  // influence how the selection is drawn)
  // we have to set the edit's width explicitly to the width of the column.
  fGrid.Header.Columns.GetColumnBounds(fColumn, dummy, R.Right);
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
  fDataType := DefaultDataType;
  fRequired := DefaultRequired;
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
  c: TTisGridColumn;
  src: TTisGridColumns;
begin
  if aSource is TTisGridColumns then
  begin
    Header.Columns.Clear;
    src := aSource as TTisGridColumns;
    for i := 0 to src.Header.Columns.Count-1 do
    begin
      c := Header.Columns.Add as TTisGridColumn;
      c.Assign(src.Header.Columns[i]);
      c.Options := src.Header.Columns[i].Options;
    end;
  end
  else
    inherited Assign(aSource);
end;

type
  TVirtualTreeCast = class(TBaseVirtualTree); // necessary to make the header accessible

{ TTisGridHeaderPopupMenu }

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

procedure TTisGridHeaderPopupMenu.OnMenuItemClick(sender: TObject);
begin
  if Assigned(PopupComponent) and (PopupComponent is TBaseVirtualTree) then
  begin
    with TMenuItem(Sender), TVirtualTreeCast(PopupComponent).Header.Columns.Items[Tag] do
    begin
      if Checked then
        Options := Options - [coVisible]
      else
        Options := Options + [coVisible];
       DoColumnChange(TMenuItem(Sender).Tag, not Checked);
    end;
  end;
end;

procedure TTisGridHeaderPopupMenu.Popup(x, y: Integer);
var
  ColPos: TColumnPosition;
  ColIdx: TColumnIndex;
  NewMenuItem: TMenuItem;
  hpi: TTisGridHeaderPopupItem;
  VisibleCounter: Cardinal;
  VisibleItem: TMenuItem;
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
      VisibleItem := nil;
      VisibleCounter := 0;
      for ColPos := 0 to Columns.Count - 1 do
      begin
        if poOriginalOrder in fOptions then
          ColIdx := ColPos
        else
          ColIdx := Columns.ColumnFromPosition(ColPos);
        if ColIdx = NoColumn then
          break;
        with Columns[ColIdx] as TTisGridColumn do
        begin
          if coVisible in Options then
            Inc(VisibleCounter);
          DoAddHeaderPopupItem(ColIdx, hpi);
          if hpi <> apHidden then
          begin
            NewMenuItem := TTisGridHeaderMenuItem.Create(self);
            NewMenuItem.Tag := ColIdx;
            NewMenuItem.Caption := Text + ' (' + Utf8ToString(PropertyName) + ')';
            NewMenuItem.Hint := Hint;
            NewMenuItem.ImageIndex := ImageIndex;
            NewMenuItem.Checked := coVisible in Options;
            NewMenuItem.OnClick := @OnMenuItemClick;
            if hpi = apDisabled then
              NewMenuItem.Enabled := False
            else
              if coVisible in Options then
                VisibleItem := NewMenuItem;
            Items.Add(NewMenuItem);
          end;
        end;
      end;
      // conditionally disable menu item of last enabled column
      if (VisibleCounter = 1) and (VisibleItem <> nil) and not (poAllowHideAll in fOptions) then
        VisibleItem.Enabled := False;
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

function TTisGrid.GetSettings: Variant;
var
  i: integer;
  col: TTisGridColumn;
  r, cols: PDocVariantData;
begin
  TDocVariant.NewFast(result);
  r := @result;
  r^.I['sortcolumn'] := Header.SortColumn;
  r^.I['sortdirection'] := ord(Header.SortDirection);
  r^.I['headerheight'] := Header.Height;
  r^.I['defaultnodeheight'] := DefaultNodeHeight;
  cols := r^.A_['columns'];
  for i := 0 to Header.Columns.Count-1 do
  begin
    col := Header.Columns[i] as TTisGridColumn;
    cols^.AddItem(
      _ObjFast([
        'propertyname', StringToUtf8(col.PropertyName),
        'text', StringToUtf8(col.Text),
        'position', col.Position,
        'width', col.Width,
        'visible', (coVisible in col.Options)
      ])
    );
  end;
end;

procedure TTisGrid.SetSettings(const aValue: Variant);
var
  c, v: PDocVariantData;
  int: Integer;
  n: RawUtf8;
  gc: TTisGridColumn;
begin
  v := _Safe(aValue);
  if not v^.IsVoid then
  begin
    for c in v^.A_['columns']^.Objects do
    begin
      n := c^.U['propertyname'];
      gc := FindColumnByPropertyName(n);
      if gc = nil then
      begin
        gc := Header.Columns.Add as TTisGridColumn;
        gc.Text := c^.S['text'];
        gc.PropertyName := n;
        gc.Width := 100;
      end
      else
      begin
        gc.Position := c^.I['position'];
        gc.Width := c^.I['width'];
        if c^.B['visible'] then
          gc.Options := gc.Options + [coVisible]
        else
          gc.Options := gc.Options - [coVisible];
      end;
    end;
    if v^.GetAsInteger('sortcolumn', int) then
      Header.SortColumn := int;
    if v^.GetAsInteger('sortdirection', int) then
      Header.SortDirection := TSortDirection(int);
    if v^.GetAsInteger('headerheight', int) then
      Header.Height := int;
    if v^.GetAsInteger('defaultnodeheight', int) then
      DefaultNodeHeight := int;
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
  aborted: Boolean;
begin
  if fData.Equals(aValue) then
    exit;
  aborted := False;
  if Assigned(fOnBeforeDataChange) then
    fOnBeforeDataChange(self, @aValue, aborted);
  if aborted then
    exit;
  fData := aValue;
  LoadData;
  UpdateSelectedAndTotalLabel;
  if Assigned(fOnAfterDataChange) then
    fOnAfterDataChange(self);
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

procedure TTisGrid.SetOnCutToClipBoard(aValue: TNotifyEvent);
begin
  fOnCutToClipBoard := aValue;
end;

function TTisGrid.GetOptions: TStringTreeOptions;
begin
  result := TStringTreeOptions(inherited TreeOptions);
end;

procedure TTisGrid.SetOptions(const aValue: TStringTreeOptions);
begin
  TreeOptions.Assign(aValue);
end;

procedure TTisGrid.SetParentProperty(const aValue: string);
begin
  if fParentProperty = aValue then
    exit;
  fParentProperty := aValue;
  LoadData;
end;

function TTisGrid.GetSelectedRows: TDocVariantData;
var
  n: PVirtualNode;
  d: PDocVariantData;
begin
  n := GetFirstSelected;
  result.InitArray([], JSON_FAST);
  while n <> nil do
  begin
    d := GetNodeDataAsDocVariant(n);
    result.AddItem(variant(d^));
    n := GetNextSelected(n, True);
  end;
end;

procedure TTisGrid.SetSelectedRows(const aValue: TDocVariantData);
var
  a: TNodeArray;
  n, m: PVirtualNode;
  o, f: PDocVariantData;
begin
  if aValue.IsVoid then
  begin
    ClearSelection;
    FocusedNode := nil;
  end
  else
  begin
    f := FocusedRow;
    ClearSelection;
    n := nil;
    m := nil;
    BeginUpdate;
    try
      for o in aValue.Objects do
      begin
        if Length(KeyFieldsList) = 1 then
          a := GetNodesBy(StringToUtf8(KeyFieldsList[0]), o^.U[StringToUtf8(KeyFieldsList[0])])
        else
          a := GetNodesBy(o, Length(KeyFieldsList) > 0);
        for n in a do
        begin
          if (f <> nil) and o^.Equals(f^) then
            m := n;
          Selected[n] := True;
        end;
      end;
    finally
      EndUpdate;
    end;
    // focused the last selected node
    if m <> nil then
      FocusedNode := m
    else if n <> nil then
      FocusedNode := n;
  end;
end;

function TTisGrid.GetSelectedObjects: PDocVariantDataDynArray;
var
  node: PVirtualNode;
  n: integer;
begin
  node := GetFirstSelected;
  n := 0;
  result := nil;
  while node <> nil do
  begin
    PtrArrayAdd(result, GetNodeDataAsDocVariant(node), n);
    node := GetNextSelected(node, True);
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
  Shift: TShiftState;
  KeyState: TKeyboardState;
  Buffer: array[0..1] of Char;
  m: TLMessage;
begin
  // manage immediate editor
  with Message do
  begin
    Shift := KeyDataToShiftState(KeyData);
    KeyState := Default(TKeyboardState);
    GetKeyboardState(KeyState);
    // avoid conversion to control characters
    // - we have captured the control key state already in Shift
    KeyState[VK_CONTROL] := 0;
    if (
      (ToASCII(Message.CharCode, (Message.KeyData shr 16) and 7, KeyState, @Buffer, 0) > 0) or
      (Message.CharCode = VK_F2)
      )
      and (Shift * [ssCtrl, ssAlt] = []) and (CharCode >= 32) then
    begin
      EditColumn := FocusedColumn;
      if EditColumn = NoColumn then
        exit;
      // send first key which triggered the editor to newly created editor
      if CanEdit(FocusedNode, EditColumn) then
      begin
        DoEdit;
        m.msg := LM_CHAR;
        m.wParam := ord(Buffer[0]);
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
  Handled: Boolean;
begin
  Handled := False;
  // try the header whether it needs to take this message
  if Assigned(Header) and (Header.States <> []) then
    Handled := TVTHeaderHack(Header).HandleMessage(Message);
  if not Handled then
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
          Handled := True;
        if not Handled then
        begin
          ControlState := ControlState + [csLButtonDown];
          Dispatch(Message);  // overrides TControl's BeginDrag
          Handled := True;
        end;
      end;
    end;
    if not Handled and Assigned(Header) then
      Handled := TVTHeaderHack(Header).HandleMessage(Message);
    if not Handled then
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
  RowData: PDocVariantData;
  PropertyName: RawUtf8;
begin
  if aNode = nil then
    exit;
  RowData := GetNodeDataAsDocVariant(aNode);
  if RowData <> nil then
  begin
    if aColumn >= 0 then
      PropertyName := TTisGridColumn(Header.Columns.Items[aColumn]).PropertyName
    else
      PropertyName := StringToUtf8(DefaultText);
    { TODO -omsantos : we should test for more cases }
    case VarType(RowData^.Value[PropertyName]) of
      varDouble, varCurrency:
        RowData^.D[PropertyName] := StrToFloatDef(aText, 0);
      varInteger:
        RowData^.I[PropertyName] := StrToIntDef(aText, 0);
      else
        RowData^.S[PropertyName] := aText;
    end;
    inherited DoNewText(aNode, aColumn, RowData^.S[PropertyName]);
  end;
end;

procedure TTisGrid.DoGetText(aNode: PVirtualNode; aColumn: TColumnIndex;
  aTextType: TVSTTextType; var aText: string);
var
  r: PDocVariantData;
begin
  r := nil;
  if aNode <> nil then
  begin
    r := GetNodeDataAsDocVariant(aNode);
    if r <> nil then
    begin
      if (aColumn >= 0) and Header.Columns.IsValidColumn(aColumn) then
        aText := r^.S[TTisGridColumn(Header.Columns.Items[aColumn]).PropertyName]
        //aText := VariantToString(r^.GetValueByPath(TTisGridColumn(Header.Columns.Items[aColumn]).PropertyName))
      else if DefaultText <> '' then
        aText := r^.S[DefaultText];
      if aText = '' then
        aText := DefaultText;
    end
    else
      aText := 'uninitialized';
  end
  else
    aText := '';
  if Assigned(fOnGetText) and (aColumn >= 0) and Header.Columns.IsValidColumn(aColumn) then
    fOnGetText(self, aNode, r^, aColumn, aTextType, aText);
end;

procedure TTisGrid.DoInitNode(aParentNode, aNode: PVirtualNode;
  var aInitStates: TVirtualNodeInitStates);
var
  AData: PCardinal;
begin
  AData := fInternalData.Data(aNode);
  if (AData <> nil ) and (not fData.IsVoid) and (aNode^.Index < fData.Count) then
  begin
    AData^ := aNode^.Index;
    aNode^.CheckType := ctCheckBox;
    if fNodeOptions.MultiLine then
      aNode^.States := aNode^.States + [vsMultiline];
  end;
  inherited DoInitNode(aParentNode, aNode, aInitStates);
end;

procedure TTisGrid.DoMeasureItem(aTargetCanvas: TCanvas; aNode: PVirtualNode;
  var aNodeHeight: Integer);
var
  i, cellheight, maxheight: Integer;
begin
  if Assigned(OnMeasureItem) then
    inherited DoMeasureItem(aTargetCanvas, aNode, aNodeHeight)
  else
  begin
    maxheight := DefaultNodeHeight;
    if MultiLine[aNode] then
    begin
      for i := 0 to Header.Columns.Count -1 do
      begin
        if (coVisible in Header.Columns[i].Options) then
        begin
          cellheight := ComputeNodeHeight(aTargetCanvas, aNode, i);
          if cellheight > maxheight then
            maxheight := cellheight;
        end;
      end;
    end;
    if maxheight > 6 * DefaultNodeHeight then
      maxheight := 6 * DefaultNodeHeight;
    aNodeHeight := maxheight + fNodeOptions.MultiLineHeight;
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
  result := TStringTreeOptions;
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
    if vsSelected in Node^.States then
    begin
      if not Focused or (column <> FocusedColumn) or (Node <> FocusedNode) then
      begin
        ACanvas.Brush.Color := clLtGray;
        ACanvas.FillRect(CellRect);
      end
      else
      if (Column = FocusedColumn) and (Node = FocusedNode) and Focused then
      begin
        ACanvas.Brush.Color := Colors.SelectionRectangleBlendColor;
        ACanvas.FillRect(CellRect);
      end;
    end;
  end;
  inherited DoBeforeCellPaint(ACanvas, Node, Column, CellPaintMode, CellRect, ContentRect);
end;

procedure TTisGrid.DoTextDrawing(var PaintInfo: TVTPaintInfo;
  const AText: string; CellRect: TRect; DrawFormat: cardinal);
begin
  // pour affichage lignes multiselect en gris clair avec cellule focused en bleu
  if Focused and
    (vsSelected in PaintInfo.Node^.States) and (PaintInfo.Node = FocusedNode) and
    (PaintInfo.column = FocusedColumn) then
    PaintInfo.Canvas.Font.Color := clWhite;
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

procedure TTisGrid.DoAutoAdjustLayout(const AMode: TLayoutAdjustmentPolicy;
  const AXProportion, AYProportion: Double);
var
  i: Integer;
begin
  if (AMode in [lapAutoAdjustForDPI]) then
  begin
    Header.MinHeight := max(18,round(Header.MinHeight * AXProportion)+1);
    Header.MaxHeight := max(18,round(Header.MaxHeight * AXProportion)+1);
    Header.DefaultHeight := max(18,round(Header.DefaultHeight * AXProportion)+1);
    Header.Height := max(18,round(Header.Height * AXProportion)+1);
    for i := 0 to header.Columns.Count-1 do
    begin
      header.Columns[i].MaxWidth:=round(header.Columns[i].MaxWidth * AXProportion);
      header.Columns[i].Width:=round(header.Columns[i].Width * AXProportion);
      header.Columns[i].MinWidth:=round(header.Columns[i].MinWidth * AXProportion);
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

procedure TTisGrid.FillPopupMenu(sender: TObject);

  procedure RemoveAutoItems;
  var
    i: Integer;
  begin
    for i := PopupMenu.Items.Count-1 downto 0 do
      if PopupMenu.Items[i].Tag = 250 then
        PopupMenu.Items.Delete(i);
  end;

  function AddItem(const aCaption: string; aShortcut: TShortCut; aEvent: TNotifyEvent): HMENU;
  var
    mi: TMenuItem;
  begin
    mi := PopupMenu.Items.Find(aCaption);
    if mi = nil then
    begin
      mi := TMenuItem.Create(PopupMenu);
      with mi do
      begin
        Caption := aCaption;
        ShortCut := aShortcut;
        OnClick := aEvent;
        // to delete them
        Tag := 250; { TODO -omsantos : we might create a new property for custom this number }
      end;
      PopupMenu.Items.Add(mi);
    end;
    result := mi.Handle;
  end;

begin
  if PopupMenu = nil then
    exit;
  RemoveAutoItems;
  if Assigned(fPopupOrigEvent) then
    fPopupOrigEvent(self);
  if (PopupMenu.Items.Count > 0) then
    AddItem('-', 0, nil);
  if pmoShowFind in fPopupMenuOptions then
    HMFind := AddItem(RsFind, ShortCut(Ord('F'), [ssCtrl]), @DoFindText);
  if pmoShowFindNext in fPopupMenuOptions then
    HMFindNext := AddItem(RsFindNext, VK_F3, @DoFindNext);
  {HMFindReplace := AddItem(RsFindReplace, ShortCut(Ord('H'), [ssCtrl]),
    @DoFindReplace);}
  AddItem('-', 0, nil);
  if (pmoShowCut in fPopupMenuOptions) and (not (toReadOnly in TreeOptions.MiscOptions)) and Assigned(fOnCutToClipBoard) then
    HMCut := AddItem(RsCut, ShortCut(Ord('X'), [ssCtrl]), @DoCutToClipBoard);
  if pmoShowCopy in fPopupMenuOptions then
    HMCopy := AddItem(RsCopy, ShortCut(Ord('C'), [ssCtrl]), @DoCopyToClipBoard);
  if pmoShowCopyCell in fPopupMenuOptions then
    HMCopyCell := AddItem(RsCopyCell, ShortCut(Ord('C'), [ssCtrl,ssShift]), @DoCopyCellToClipBoard);
  if (pmoShowPaste in fPopupMenuOptions) and (not (toReadOnly in TreeOptions.MiscOptions)) and
    ((toEditable in TreeOptions.MiscOptions) or Assigned(fOnBeforePaste))  then
    HMPaste := AddItem(RsPaste, ShortCut(Ord('V'), [ssCtrl]), @DoPaste);
  AddItem('-', 0, nil);
  if (pmoShowDelete in fPopupMenuOptions) and ((not (toReadOnly in TreeOptions.MiscOptions)) or Assigned(fOnBeforeDeleteRows)) then
    HMDelete := AddItem(RsDeleteRows, ShortCut(VK_DELETE, [ssCtrl]), @DoDeleteRows);
  if (pmoShowSelectAll in fPopupMenuOptions) and (toMultiSelect in TreeOptions.SelectionOptions) then
    HMSelAll := AddItem(RsSelectAll, ShortCut(Ord('A'), [ssCtrl]), @DoSelectAllRows);
  AddItem('-', 0, nil);
  if (pmoShowExportExcel in fPopupMenuOptions) and (toMultiSelect in TreeOptions.SelectionOptions) then
    HMExcel := AddItem(RsExportSelectedExcel, 0, @DoExportExcel)
  else
    HMExcel := AddItem(RsExportAllExcel, 0, @DoExportExcel);
  {if (HMPrint = 0) then
    HMPrint := AddItem(RsPrint, ShortCut(Ord('P'), [ssCtrl]), @DoPrint);
  AddItem('-', 0, nil);
  HMExpAll := AddItem(RsExpandAll, Shortcut(Ord('E'), [ssCtrl, ssShift]),
    @DoExpandAll);
  HMCollAll := AddItem(RsCollapseAll, Shortcut(Ord('R'), [ssCtrl, ssShift]),
    @DoCollapseAll);}
  AddItem('-', 0, nil);
  if (pmoShowCustomizeColumns in fPopupMenuOptions) and Assigned(Header.PopupMenu) then
    HMCustomize := AddItem(RsCustomizeColumns, 0, @DoCustomizeColumns);
  if (csDesigning in ComponentState) or (pmoShowCustomizeGrid in fPopupMenuOptions) then
    HMAdvancedCustomize := AddItem(RsAdvancedCustomizeColumns, 0, @DoAdvancedCustomizeColumns);
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
  result := TTisGridColumn(Header.Columns[aIndex]);
end;

procedure TTisGrid.FindDlgFind(Sender: TObject);
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
      DoFindNext(Sender);
      if TextFound then
        fFindDlg.CloseDialog;
    end;
    //else
    //  if DoFindLast then
    //    fFindDlg.CloseDialog;
  end
  else
    //if frDown in fFindDlg.Options then
    DoFindNext(Sender);
  //else
  //DoFindPrior(Sender);
end;

function TTisGrid.Add(aData: PDocVariantData): Boolean;
var
  o: PDocVariantData;
begin
  result := True;
  case aData^.Kind of
    dvArray:
      for o in aData^.Objects do
        fData.AddItem(variant(o^));
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
  handled: Boolean;
begin
  handled := False;
  if not Assigned(aRow1) or not Assigned(aRow2) then
    exit(0);
  if Assigned(OnCompareByRow) then
    result := OnCompareByRow(self, aPropertyName, aRow1^, aRow2^, handled)
  else
    result := 0;
  if not handled then // use default comparison
    result := aRow1^.CompareObject([aPropertyName], aRow2^);
end;

procedure TTisGrid.DoFindText(Sender: TObject);
begin
  fStartSearchNode := nil;
  TextFound := False;
  TextToFind := '';
  DoFindNext(Sender);
end;

procedure TTisGrid.DoFindNext(Sender: TObject);
var
  col: integer;
  p: PVirtualNode;

  function match(node: PVirtualNode; Txt: string): integer;
  var
    i: integer;
    cellTxt: string;
  begin
    txt := LowerCase(txt);
    result := -1;
    if fColumnToFind >= 0 then
    begin
      DoGetText(node, fColumnToFind, ttNormal, cellTxt);
      if (not (frWholeWord in fFindDlg.Options) and (pos(txt, LowerCase(cellTxt)) > 0)) or
        (txt = LowerCase(cellTxt)) then
      begin
        result := fColumnToFind;
        TextFound := True;
        exit;
      end;
    end
    else
      for i := 0 to Header.Columns.Count - 1 do
      begin
        DoGetText(node, i, ttNormal, cellTxt);
        if not (frWholeWord in fFindDlg.Options) and (pos(txt, LowerCase(cellTxt)) > 0) or
          (txt = LowerCase(cellTxt)) then
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
  try
   p := FocusedNode;
   TextFound := False;
   if p <> nil then
   begin
     // depart de recherche. teste la ligne en cours
     if (fStartSearchNode = nil) then
     begin
       fStartSearchNode := P;
       col := match(p, fTextToFind);
       if col >= 0 then
       begin
         FocusedColumn := col;
         exit;
       end;
     end;
     //on teste a partir du suivant
     if (fFindDlg <> nil) and not (frDown in fFindDlg.Options) then
       P := GetPrevious(P)
     else
       P := GetNext(P);
     while (p <> nil) and (p <> fStartSearchNode) do
     begin
       col := match(p, fTextToFind);
       if col >= 0 then
       begin
         SelectNodes(p,p,False);
         FocusedNode := p;
         FocusedColumn := col;
         SetFocusSafe;
         exit;
       end;
       // on teste a partir du suivant
       if (fFindDlg <> nil) and not (frDown in fFindDlg.Options) then
         P := GetPrevious(P)
       else
         P := GetNext(P);
       // on reboucle sur le debut
       if p = nil then
         if (fFindDlg <> nil) and not (frDown in fFindDlg.Options) then
           P := GetLast(nil)
         else
           P := GetFirst(False);
     end;
   end;
   fStartSearchNode := nil;
   ShowMessageFmt(RsNoRecordFind,[TextToFind]);
   SetFocusSafe;
  finally
  end;
end;

procedure TTisGrid.DoFindReplace(Sender: TObject);
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

procedure TTisGrid.DoUndoLastUpdate(Sender: TObject);
begin
end;

procedure TTisGrid.DoRevertRecord(Sender: TObject);
begin
end;

procedure TTisGrid.DoExportExcel(Sender: TObject);
begin
  if (toMultiSelect in TreeOptions.SelectionOptions) then
    ExportExcel(Name, tstSelected,',')
  else
    ExportExcel(Name, tstAll,',');
end;

procedure TTisGrid.DoCopyToClipBoard(Sender: TObject);
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

procedure TTisGrid.DoCopyCellToClipBoard(Sender: TObject);
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

procedure TTisGrid.DoCutToClipBoard(Sender: TObject);
begin
  if Assigned(fOnCutToClipBoard) then
    fOnCutToClipBoard(Sender);
end;

procedure TTisGrid.DoDeleteRows(Sender: TObject);
var
  d: TDocVariantData;
  ask, aborted: Boolean;

  function UserConfirmed: Boolean;
  begin
    result := Dialogs.MessageDlg(
      RsConfirmation, Format(RsConfDeleteRow, [SelectedCount]),
      mtConfirmation, mbYesNoCancel, 0) = mrYes;
  end;

begin
  d := SelectedRows;
  if d.IsVoid then
    exit;
  if Assigned(fOnBeforeDeleteRows) then
  begin
    ask := True;
    aborted := False;
    fOnBeforeDeleteRows(self, @d, ask, aborted);
    if aborted then
      exit;
    if ask and (not UserConfirmed) then
      exit;
    DeleteRows(@d);
  end
  else
    if UserConfirmed then
      DeleteRows(@d);
end;

procedure TTisGrid.DoPaste(Sender: TObject);
var
  c: TClipboardAdapter;
  d: PDocVariantData;
begin
  if c.IsValidFor(cbkJson) or c.IsValidFor(cbkText) then
  begin
    d := _Safe(_Json(c.AsJson));
    PasteRows(d);
  end;
end;

procedure TTisGrid.DoSelectAllRows(Sender: TObject);
begin
  SelectAll(False);
end;

procedure TTisGrid.DoPrint(Sender: TObject);
begin
  raise Exception.Create('Not implemented');
end;

procedure TTisGrid.DoCustomizeColumns(Sender: TObject);
begin
  Header.PopupMenu.PopUp;
end;

procedure TTisGrid.DoAdvancedCustomizeColumns(Sender: TObject);
begin
  Customize;
end;

procedure TTisGrid.DoExpandAll(Sender: TObject);
begin
  FullExpand;
end;

procedure TTisGrid.DoCollapseAll(Sender: TObject);
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
  aControl: TWinControl);
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

constructor TTisGrid.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  fInternalData.Init(self);
  Clear;
  fSelectedData.Clear;
  fSelectedData.InitArray([], JSON_FAST_FLOAT);
  DefaultText := '';
  fZebraColor := $00EDF0F1;
  SetLength(fKeyFieldsList, 0);
  fNodeOptions := TTisNodeOptions.Create(self);
  fPopupMenuOptions := [pmoShowFind..pmoShowCustomizeColumns];
  WantTabs := True;
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
  PrevReadOnly: Boolean;
begin
  PrevReadOnly := toReadOnly in TreeOptions.MiscOptions;
  TreeOptions.MiscOptions := TreeOptions.MiscOptions - [toReadOnly];
  inherited Clear;
  if PrevReadOnly then
    TreeOptions.MiscOptions := TreeOptions.MiscOptions + [toReadOnly];
  fData.Clear;
  fData.InitArray([], JSON_FAST_FLOAT);
end;

function TTisGrid.GetNodeDataAsDocVariant(aNode: PVirtualNode): PDocVariantData;
var
  d: PCardinal;
begin
  result := nil;
  if aNode = nil then
    aNode := FocusedNode;
  if aNode <> nil then
  begin
    if aNode^.Index < Cardinal(fData.Count) then
    begin
      d := fInternalData.Data(aNode);
      if d <> nil then
        result := _Safe(fData.Values[d^])
    end;
  end;
end;

procedure TTisGrid.LoadData;
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
        if ParentProperty = '' then
          RootNodeCount := fData.Count
        else
        begin
          // find root nodes (Parent value is nil or not found in current data array)
          // RootData := GetSORootNodes(Data,ParentRow);
          // RootNodeCount := RootData.AsArray.Length;
          // For each root node, set SOChildren recursively
        end;
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
        begin
          if KeyFieldsNames <> '' then
            a := GetNodesBy(t, True)
          else
            a := GetNodesBy(t);
        end;
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
    if doc.InitJson(aJson, JSON_FAST_FLOAT) then
    begin
      ClearAll;
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
    a := GetNodesBy(aValue{, True});
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

function TTisGrid.GetNodesBy(aData: PDocVariantData; aUseKeyFieldsList: boolean): TNodeArray;

  procedure _Add(var aArray: TNodeArray; aNode: PVirtualNode);
  begin
    SetLength(aArray, length(aArray) + 1);
    aArray[Length(aArray) - 1] := aNode;
  end;

var
  d: PDocVariantData;
  p: PVirtualNode;
  a, b: TDocVariantData;
  ar: TRawUtf8DynArray;
  usearray: Boolean;
begin
  SetLength(result, 0);
  if not assigned(aData) or aData^.IsVoid then
    exit;
  usearray := aUseKeyFieldsList and (Length(fKeyFieldsList) > 0);
  if usearray then
    StringDynArrayToRawUtf8DynArray(fKeyFieldsList, ar);
  p := GetFirst(True);
  while p <> nil do
  begin
    d := GetNodeDataAsDocVariant(p);
    if d <> nil then
    begin
      if usearray then
      begin
        d^.Reduce(ar, False, a);
        aData^.Reduce(ar, False, b);
        if a.Equals(b) then
          _Add(result, p);
      end
      else if d^.Equals(aData^) then
        _Add(result, p);
    end;
    p := GetNext(p, True);
  end;
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
    if (d <> nil) and (not d^.IsVoid) and (d^.U[aKey] = aValue ) then
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
  aborted: Boolean;
begin
  aborted := False;
  if Assigned(fOnBeforePaste) then
    fOnBeforePaste(self, aRows, aborted);
  if not aborted then
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
  FocColumn: TTisGridColumn;
begin
  try
    FocColumn := FocusedColumnObject;
    for i := 0 to Header.Columns.Count-1 do
      Header.Columns[i].Tag := Header.Columns[i].Position;
    Header.Columns.Sort(@SortColumnsPosition);
    for i := 0 to Header.Columns.Count-1 do
      Header.Columns[i].Position := TTisGridColumn(Header.Columns[i]).Index;
  finally
    FocusedColumnObject := FocColumn;
  end;
end;

procedure TTisGrid.Customize;
var
  target: TTisGrid;
begin
  BeginUpdate;
  try
    with TTisGridEditor.Create(Application) do
    try
      target := self;
      Grid.ClearAll;
      Grid.Header.Assign(target.Header);
      Grid.TreeOptions.Assign(target.TreeOptions);
      Grid.NodeOptions.Assign(target.NodeOptions);
      Grid.Settings := target.Settings;
      if ShowModal = mrOK then
      begin
        target.ClearAll;
        target.Header.Assign(Grid.Header);
        target.TreeOptions.Assign(Grid.TreeOptions);
        target.NodeOptions.Assign(Grid.NodeOptions);
        if KeepDataCheckBox.Checked then
          target.Data := Grid.Data;
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

function TTisGrid.ContentAsCSV(aSource: TVSTTextSourceType;
  const aSeparator: string): RawUtf8;
var
  tmp, cols, rows: TDocVariantData;
  c: Integer;
  s: RawUtf8;
  o: PDocVariantData;
begin
  if aSource in [tstAll, tstInitialized, tstVisible] then
    rows := fData
  else
    rows := SelectedRows;
  cols.InitArray([], JSON_FAST_FLOAT);
  for c := 0 to Header.Columns.Count-1 do
  begin
    if coVisible in Header.Columns[c].Options then
      cols.AddItemText(StringToUtf8('"' + TTisGridColumn(Header.Columns[c]).Text + '"'));
  end;
  result := cols.ToCsv(aSeparator) + LineEnding;
  tmp.InitArray([], JSON_FAST_FLOAT);
  for o in rows.Objects do
  begin
    tmp.Clear;
    for c := 0 to Header.Columns.Count-1 do
    begin
      if coVisible in Header.Columns[c].Options then
      begin
        s := o^.U[TTisGridColumn(Header.Columns[c]).PropertyName];
        if s <> '' then
        begin
          if VarType(o^.Value[s]) in [varDouble, varCurrency, varInteger] then
            tmp.AddItemText(s)
          else
            tmp.AddItemText(QuotedStr(s, '"'));
        end
        else
          cols.AddItemText('""');
      end;
    end;
    result := result + tmp.ToCsv(aSeparator) + LineEnding;
  end;
end;

procedure TTisGrid.ExportExcel(const aPrefix: string; aSelection: TVSTTextSourceType; aSeparator: Char);

  function GetTempFileName(const Prefix, ext: string): string;
  var
    I: Integer;
    Start: string;
    Disc: string;
  begin
    Start := GetTempDir;
    I := 0;
    Disc := '';
    repeat
      result := Format('%s%s%s%s', [Start,Prefix,Disc,ext]);
      Disc := Format('%.5d', [i]);
      Inc(I);
    until not FileExists(result);
  end;

var
  tempfn: Utf8String;
  txt: Utf8String;
  txtbuf: PChar;
  l: LongInt;
  st: File;
begin
  tempfn := GetTempFileName(aPrefix,'.csv');
  AssignFile(st,tempfn);
  Rewrite(st,1);
  try
    txt := ContentAsCSV(aSelection, aSeparator)+#0;
    txtbuf := PChar(txt);
    l := strlen(txtbuf);
    BlockWrite(st, txtbuf^, l);
  finally
    CloseFile(st);
    OpenDocument(tempfn);
  end;
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
  json: RawUtf8;
  b64: string;
  ini: TIniFile;
begin
  json := _Safe(Settings)^.ToJson;
  b64 := BinToBase64(Utf8ToString(json));
  ini := TIniFile.Create(aFileName);
  try
    ini.WriteString(Owner.Name, Name, b64);
  finally
    FreeAndNil(ini);
  end;
end;

procedure TTisGrid.LoadSettingsFromIni(const aFileName: TFileName);
var
  b64: string;
  ini: TIniFile;
begin
  ini := TIniFile.Create(aFileName);
  try
    b64 := ini.ReadString(Owner.Name, Name, '');
    if b64 <> '' then
    begin
      SetSettings(_Json(Base64ToBin(StringToUtf8(b64))));
    end;
  finally
    FreeAndNil(ini);
  end;
end;

end.
