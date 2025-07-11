// -----------------------------------------------------------------
//    This file is part of Tranquil IT Software
//    Copyright (C) 2012 - 2024  Tranquil IT https://www.tranquil.it
//    All Rights Reserved.
// ------------------------------------------------------------------
unit tis.ui.grid.core;

{$i tis.ui.defines.inc}

interface

uses
  {$ifdef WINDOWS}
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
  TextStrings,
  TAGraph,
  TASources,
  TAChartUtils,
  VirtualTrees,
  mormot.core.base,
  mormot.core.data,
  mormot.core.os,
  mormot.core.datetime,
  mormot.core.variants,
  mormot.core.unicode,
  mormot.core.text,
  mormot.core.buffers,
  mormot.core.rtti,
  mormot.core.mustache,
  mormot.core.search,
  tisstrings,
  tis.core.os,
  tis.core.utils,
  tis.ui.resourcestrings,
  tis.ui.searchedit,
  tis.ui.grid.controls,
  tis.ui.grid.chart,
  tis.frameviewer;

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
    cdtJson,
    cdtHtml,
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
    fValueIsString: Boolean;
    fAbortAll: Boolean;
  protected
    /// it will initialize ControlClasses array using default values for each data type
    // - it is called once, in initialization section
    class procedure SetupControlClasses;
    /// disable control events, especially OnExit, to prevents a GPF
    procedure DisableControlEvents;
    procedure EditKeyDown(aSender: TObject; var Key: Word; Shift: TShiftState); virtual;
    procedure EditExit(aSender: TObject); virtual;
    /// override this method if you want to change the default control for aNode/aColumn
    // - by default, first it will check Grid.OnCustomEditor event to get an instance
    // - if none instance was provided, it will use Grid.OnEditorLookup to get one
    // - again, if none was provided, it will use ControlClasses array, according to the aColumn.DataType
    function NewControl(aNode: PVirtualNode; aColumn: TTisGridColumn): TTisGridControl; virtual;
  public
    /// use this array to change the default control for each data type on all grids
    // - initialized by SetupControlClasses
    class var ControlClasses: array[TTisColumnDataType] of TTisGridControlClass;
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

  /// datetime options for a column
  TTisGridColumnDateTimeOptions = class(TPersistent)
  private
    fSaveAsUtc: Boolean;
    fShowAsDateTime: Boolean;
    fShowAsLocal: Boolean;
    fCustomFormat: string;
  protected const
    DefaultSaveAsUtc = True;
    DefaultShowAsDateTime = True;
    DefaultShowAsLocal = True;
  public
    constructor Create; reintroduce;
    procedure AssignTo(aDest: TPersistent); override;
    /// convert a UTC time to local time
    function UtcToLocal(const aValue: TDateTime): TDateTime;
    /// convert a local time to UTC time
    function LocalToUtc(const aValue: TDateTime): TDateTime;
  published
    /// it will save date/time value as UTC
    property SaveAsUtc: Boolean read fSaveAsUtc write fSaveAsUtc default DefaultSaveAsUtc;
    /// it will show Iso8601 value as TDateTime
    property ShowAsDateTime: Boolean read fShowAsDateTime write fShowAsDateTime default DefaultShowAsDateTime;
    /// it will show date/time value as local time
    property ShowAsLocal: Boolean read fShowAsLocal write fShowAsLocal default DefaultShowAsLocal;
    /// a custom format for date/time
    // - if blank, the format will be the same as OS
    property CustomFormat: string read fCustomFormat write fCustomFormat;
  end;

  /// html options for a column
  TTisGridColumnHtmlOptions = class(TPersistent)
  private
    fMustacheTemplate: TStrings;
    procedure SetMustacheTemplate(aValue: TStrings);
  public
    constructor Create; reintroduce;
    destructor Destroy; override;
    procedure AssignTo(aDest: TPersistent); override;
  published
    property MustacheTemplate: TStrings read fMustacheTemplate write SetMustacheTemplate;
  end;

  /// data type options for a column
  TTisGridColumnDataTypeOptions = class(TPersistent)
  private
    fDateTimeOptions: TTisGridColumnDateTimeOptions;
    fHtmlOptions: TTisGridColumnHtmlOptions;
  public
    constructor Create; reintroduce;
    destructor Destroy; override;
    procedure AssignTo(aDest: TPersistent); override;
  published
    property DateTimeOptions: TTisGridColumnDateTimeOptions read fDateTimeOptions write fDateTimeOptions;
    property HtmlOptions: TTisGridColumnHtmlOptions read fHtmlOptions write fHtmlOptions;
  end;

  /// custom Grid Column implementation
  TTisGridColumn = class(TVirtualTreeColumn)
  private
    fAllowChart: Boolean;
    fAllowFilter: Boolean;
    fDataType: TTisColumnDataType;
    fDataTypeOptions: TTisGridColumnDataTypeOptions;
    fPropertyName: RawUtf8;
    fRequired: Boolean;
    fReadOnly: Boolean;
    fChartSettings: RawUtf8;
    function GetTitle: TCaption;
    procedure SetTitle(const aValue: TCaption);
    procedure SetPropertyName(const aValue: RawUtf8);
  protected const
    DefaultAllowChart = True;
    DefaultAllowFilter = True;
    DefaultDataType = cdtString;
    DefaultRequired = False;
    DefaultReadOnly = False;
  protected
    fOriginal: Boolean; // from designtime
    property ChartSettings: RawUtf8 read fChartSettings write fChartSettings;
  public
    constructor Create(aCollection: TCollection); override;
    destructor Destroy; override;
    procedure Assign(aSource: TPersistent); override;
  published
    /// allow to show a chart for this column
    property AllowChart: Boolean read fAllowChart write fAllowChart default DefaultAllowChart;
    /// allow use filter for this column, if Grid.FilterOptions.Enable is TRUE
    property AllowFilter: Boolean read fAllowFilter write fAllowFilter default DefaultAllowFilter;
    property DataType: TTisColumnDataType read fDataType write fDataType default DefaultDataType;
    property DataTypeOptions: TTisGridColumnDataTypeOptions read fDataTypeOptions write fDataTypeOptions;
    property PropertyName: RawUtf8 read fPropertyName write SetPropertyName;
    /// if TRUE, it will not allow user to set NULL/blank for this column using Editor
    // - if editor focus is lost, it will return the previous value before edition
    property Required: Boolean read fRequired write fRequired default DefaultRequired;
    property ReadOnly: Boolean read fReadOnly write fReadOnly default DefaultReadOnly;
    property Text: TCaption read GetTitle write SetTitle;
  end;

  /// a custom implementation for Grid Columns
  TTisGridColumns = class(TVirtualTreeColumns)
  protected
    /// it implements a tri-state sorting click
    // - sort ascending, then descending, and finally no sort (showing no arrow on header)
    procedure HandleClick(P: TPoint; aButton: TMouseButton; aForce,
      aDblClick: Boolean); override;
  public
    /// overriding for circumvent original Assign, which freezes Lazarus when having invisible columns
    procedure Assign(aSource: TPersistent); override;
    /// return the column index by property name
    // - if not found, it will return NoColumn value
    function GetColumnIndexByPropertyName(const aPropertyName: RawUtf8): Integer;
    /// delete the column by property name
    // - return the columns count
    function DeleteColumnByPropertyName(const aPropertyName: RawUtf8): Integer;
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
    procedure OnMenuRemoveCustomColumnClick(aSender: TObject);
    procedure OnMenuFilterEnableClick(aSender: TObject);
    procedure OnMenuFilterClick(aSender: TObject);
    procedure OnMenuFilterClearClick(aSender: TObject);
    procedure OnMenuFilterCustomClick(aSender: TObject);
    procedure OnMenuFilterRemoveCustomClick(aSender: TObject);
  public
    constructor Create(aOwner: TComponent); override;
    procedure AssignTo(aDest: TPersistent); override;
    procedure FillPopupMenu;
  published
    property Options: TTisGridHeaderPopupOptions read fOptions write fOptions default [];
    property OnAddPopupItem: TOnGridHeaderAddPopupItem read fOnAddPopupItem write fOnAddPopupItem;
    property OnColumnChange: TOnGridHeaderColumnChange read fOnColumnChange write fOnColumnChange;
  end;

  /// a custom implementation for Grid Header
  TTisGridHeader = class(TVTHeader)
  protected
    function GetColumnsClass: TVirtualTreeColumnsClass; override;
  public
    // overriding the original Assign for do not assign PopupMenu from another grid header
    // - a Popup in design mode will not work when using Editor
    procedure Assign(aSource: TPersistent); override;
  end;

  /// a custom implementation for String Tree Options
  TTisStringTreeOptions = class(TStringTreeOptions)
  protected const
    DefaultPaintOptions = VirtualTrees.DefaultPaintOptions -
      [toShowRoot] + [toAlwaysHideSelection, toShowHorzGridLines, toShowVertGridLines, toHideFocusRect];
    DefaultSelectionOptions = VirtualTrees.DefaultSelectionOptions +
      [toExtendedFocus, toSimpleDrawSelection, toRightClickSelect, toDisableDrawSelection];
    DefaultMiscOptions = VirtualTrees.DefaultMiscOptions +
      [toGridExtensions, toFullRowDrag] - [toWheelPanning, toEditOnClick, toEditOnDblClick, toToggleOnDblClick, toAcceptOLEDrop];
    DefaultAutoOptions = VirtualTrees.DefaultAutoOptions +
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

  /// most used values on a chart
  TTisGridChartMostUsedValues = class(TPersistent)
  private
    fCount: Integer;
    fEnabled: Boolean;
  protected const
    DefaultCount = 10;
    DefaultEnabled = True;
  public
    constructor Create; reintroduce;
  published
    /// how many values will be used as the most used ones
    property Count: Integer read fCount write fCount default DefaultCount;
    /// enable/disable the use of it
    property Enabled: Boolean read fEnabled write fEnabled default DefaultEnabled;
  end;

  /// grid chart options for column chart
  TTisGridChartOptions = class(TPersistent)
  private
    fGrid: TTisGrid;
    fMaxLabelLength: Integer;
    fMostUsedValues: TTisGridChartMostUsedValues;
  protected const
    DefaultMaxLabelLength = 30;
  public
    constructor Create(aGrid: TTisGrid); reintroduce;
    destructor Destroy; override;
  published
    /// it determines the max length that a label can have
    property MaxLabelLength: Integer read fMaxLabelLength write fMaxLabelLength default DefaultMaxLabelLength;
    /// most used values on a chart source
    property MostUsedValues: TTisGridChartMostUsedValues read fMostUsedValues write fMostUsedValues;
  end;

  /// types of sorting a filter
  TTisGridFilterSort = (
    gfsMostUsedValues,
    gfsFirstValues
  );

  /// filter options for grid header popup menu
  TTisGridFilterOptions = class(TPersistent)
  private
    fGrid: TTisGrid;
    fFilters: TDocVariantData;
    fCaseInsensitive: Boolean;
    fClearAfterLoadingData: Boolean;
    fDisplayedCount: Integer;
    fEnabled: Boolean;
    fMaxCaptionLength: Integer;
    fSort: TTisGridFilterSort;
    fShowAutoFilters: Boolean;
  protected const
    DefaultDisplayedCount = 10;
    DefaultEnabled = False;
    DefaultCaseInsensitive = False;
    DefaultClearAfterLoadingData = False;
    DefaultMaxCaptionLength = 45;
    DefaultSort = gfsMostUsedValues;
    protected const
    DownArrow = ' ↓';
  protected
    class var fMruFilters: TDocVariantData;
    class procedure InitClass;
    /// clear DownArrow mark of all header columns
    procedure ClearHeaderArrows;
  public
    constructor Create(aGrid: TTisGrid); reintroduce;
    procedure AssignTo(aDest: TPersistent); override;
    /// it will add a new filter to the list of filters values and trigger it,
    // filtering the grid nodes by calling ApplyFilters
    // - use aCustom to add to the MRU list filters as well
    // - it returns the new item added
    function AddFilter(const aFieldName, aValue: RawUtf8; aCustom: Boolean = False): Variant;
    /// check if a filter exists
    function FilterExists(const aFieldName: RawUtf8; const aValue: string): Boolean;
    /// apply all filters
    // - it is allowed more than one filter for the first column that user started filtering;
    // the filter system will use an "OR" clause for all values chosen by the user
    // - for the second column filtered onwards, it will use an "AND" clause, combining to values from the first column
    procedure ApplyFilters;
    /// clear all filters
    procedure ClearFilters;
    procedure AddMruFilter(const aFieldName, aValue: RawUtf8);
    procedure RemoveMruFilter(const aFieldName, aValue: RawUtf8);
    function GetMruFilters: PDocVariantData;
    function GetMruFiltersAsArrayOfString(const aFieldName: RawUtf8): TStringArray;
    /// filter table
    property Filters: TDocVariantData read fFilters;
    /// enable and show auto filters for user
    property ShowAutoFilters: Boolean read fShowAutoFilters write fShowAutoFilters;
  published
    property CaseInsensitive: Boolean read fCaseInsensitive write fCaseInsensitive default DefaultCaseInsensitive;
    /// used after call Grid.LoadData
    // - if it is TRUE, it will call ClearFilters, otherwise it will call ApplyFilters
    property ClearAfterLoadingData: Boolean read fClearAfterLoadingData write fClearAfterLoadingData default DefaultClearAfterLoadingData;
    /// how many menu items will be used to show filters
    property DisplayedCount: Integer read fDisplayedCount write fDisplayedCount default DefaultDisplayedCount;
    /// enable/disable the use of it
    property Enabled: Boolean read fEnabled write fEnabled default DefaultEnabled;
    /// it determines the max length a menu item caption can have
    property MaxCaptionLength: Integer read fMaxCaptionLength write fMaxCaptionLength default DefaultMaxCaptionLength;
    /// which order it will show the menu items
    property Sort: TTisGridFilterSort read fSort write fSort default DefaultSort;
  end;

  /// node options
  TTisNodeOptions = class(TPersistent)
  private
    fGrid: TTisGrid;
    fMultiEdit: Boolean;
    fMultiLine: Boolean;
    fMultiLineHeight: Integer;
    fShowChildren: Boolean;
    procedure SetMultiLine(aValue: Boolean);
    procedure SetMultiLineHeight(aValue: Integer);
  protected const
    DefaultMultiEdit = False;
    DefaultMultiLine = False;
    DefaultMultiLineHeight = 4;
    DefaultShowChildren = False;
  public
    constructor Create(aGrid: TTisGrid); reintroduce;
    procedure AssignTo(aDest: TPersistent); override;
  published
    /// allow users edit/set multi nodes values at the same time
    property MultiEdit: Boolean read fMultiEdit write fMultiEdit default DefaultMultiEdit;
    property MultiLine: Boolean read fMultiLine write SetMultiLine default DefaultMultiLine;
    property MultiLineHeight: Integer read fMultiLineHeight write SetMultiLineHeight default DefaultMultiLineHeight;
    property ShowChildren: Boolean read fShowChildren write fShowChildren default DefaultShowChildren;
  end;

  TTisNodeColumnCache = packed object
    Column: TColumnIndex;
    Image: TBitmap;
    Hash: Cardinal;
    // call it before use an object instance
    procedure Init;
    /// call it to free and reset all fields
    procedure Clear;
  end;

  PTisNodeColumnCache = ^TTisNodeColumnCache;

  TTisNodeColumnCacheDynArray = array of TTisNodeColumnCache;

  PTisNodeColumnCacheDynArray = ^TTisNodeColumnCacheDynArray;

  TTisNodeCache = packed object
    Hash: Cardinal;
    Height: Integer;
    Columns: TTisNodeColumnCacheDynArray;
    // call it before use an object instance
    procedure Init;
    /// call it to free and reset all fields
    procedure Clear;
    function GetColumnCache(aColumn: Integer): PTisNodeColumnCache;
    /// set a node column cache
    // - if the cache already exists, it will be replaced
    // otherwise it will create a new item on array of columns
    procedure SetColumnCache(aColumn: Integer; const aValue: TTisNodeColumnCache);
  end;

  PTisNodeCache = ^TTisNodeCache;

  TTisNodeCacheDynArray = array of TTisNodeCache;

  /// as known as "user data" for the Grid
  // - each node has its own NodeData
  TTisNodeData = record
    /// it will be TRUE only if NodeOptions.ShowChildren is TRUE
    IsChild: Boolean;
    /// it points to original name
    // - it can be NIL, if the original object/array is nameless
    Name: PRawUtf8;
    /// it points to orinal value
    // - it cannot be NIL
    Value: PVariant;
    /// it points to original row data
    Data: PDocVariantData;
    /// node cache values
    Cache: TTisNodeCache;
  end;

  PTisNodeData = ^TTisNodeData;

  /// adapter for a node
  TTisNodeAdapter = object
    Offset: Cardinal;
    Grid: TTisGrid;
    procedure Init(aGrid: TTisGrid);
    /// return aNode Data pointer
    // - use this method if you need a PTisNodeData local variable,
    // as it will be a little faster then GetData
    function GetDataPointer(aNode: PVirtualNode): Pointer;
    /// return aNode Data pointer
    // - convenient way to access Data but without using a local variable
    function GetData(aNode: PVirtualNode): PTisNodeData;
    /// return aNode Data as string
    // - convenient way to access Data but without using a local variable
    function GetDataAsString(aNode: PVirtualNode): string;
    /// return aNode name
    // - if aNode represents an object, it will return its Name
    // - if aNode represents an object nameless, it will return "{}"
    // - if aNode represents an array, it will return its value
    // - you could use it to read Child nodes values in DoGetText
    function GetName(aNode: PVirtualNode): string;
    /// returns the value corresponding to the aNode
    // - if it is not a child node, it will (try to) return the value corresponding to
    // the column instance using aColumn as the index - it will return NIL, if no column was found
    function GetValue(aNode: PVirtualNode; aColumn: TColumnIndex = NoColumn): PVariant;
    /// returns the value corresponding to the aNode
    // - it will return only simple values (no object/array), otherwise it will return NIL
    function GetValueAsSimple(aNode: PVirtualNode; aColumn: TColumnIndex = NoColumn): PVariant;
    /// returns the value corresponding to the aNode
    // - just a wrapper around GetValueAsSimple
    // - if it is NIL, aDefault will be used
    function GetValueAsSimpleString(aNode: PVirtualNode; aColumn: TColumnIndex = NoColumn; const aDefault: string = ''): string;
    /// returns the value corresponding to the aNode
    // - it will try to get the value from node Data, if not found, it will try to get from Grid OnCalcAttributes event
    // otherwise it will return NULL
    function GetValueAsVariant(aNode: PVirtualNode; aColumn: TColumnIndex = NoColumn): Variant;
    /// set a value to aNode
    // - if it is not a child node, it will use PropertyName from its TTisGridColumn instance getting it by aColumn,
    // trying to update the original object data, but if the object.name was not found, it will do nothing
    // - if aValueIsString is FALSE, it will try to detect JSON numbers or constants to use the correct date type
    procedure SetValue(aNode: PVirtualNode; const aValue: Variant; aColumn: TColumnIndex = NoColumn;
      aValueIsString: Boolean = False);
    /// return TRUE if aNode is child
    function IsChild(aNode: PVirtualNode): Boolean;
    /// returns the node cache
    function GetCache(aNode: PVirtualNode): PTisNodeCache;
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
    pmoShowClearCell,
    pmoShowCopySpecial,
    pmoShowPaste,
    pmoShowInsert,
    pmoShowDelete,
    pmoShowSelectAll,
    pmoShowCustomizeColumns,
    pmoShowChart,
    pmoShowExport,
    pmoShowCustomizeGrid
  );

  TTisPopupMenuOptions = set of TTisPopupMenuOption;

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
    const aRow1, aRow2: PDocVariantData; var aHandled: Boolean): PtrInt of object;

  /// event to manipulate aData before use it
  // - use it for check/change/assign default values on aData argument or abort the process
  TOnGridInsert = procedure(aSender: TTisGrid; aData: PDocVariantData; var aAbort: Boolean) of object;

  /// event to manipulate rows using copy/paste on grid
  // - use it for check/change the aData argument, before assign it, and/or abort the process
  TOnGridPaste = procedure(aSender: TTisGrid; aData: PDocVariantData; var aAbort: Boolean) of object;
  /// event that allows users customize the control instance, creating a new one, replacing the default
  TOnGridCustomEditor = procedure(aSender: TObject; aNode: PVirtualNode; aColumn: TTisGridColumn;
    out aControl: TTisGridControl) of object;

  /// event that simplifies the use of a TisSearchEdit as Edit Control
  TOnGridEditorLookup = procedure(aSender: TTisGrid; aNode: PVirtualNode; aColumn: TTisGridColumn;
    aSearchEdit: TTisSearchEdit; var aHandled: Boolean) of object;

  /// event that allows users to change some edit control properties, before it shows up
  TOnGridPrepareEditor = procedure(aSender: TTisGrid; aNode: PVirtualNode; aColumn: TTisGridColumn;
    aControl: TTisGridControl) of object;

  /// event that allows to validate the new user input value
  // - aCurValue is the current value for the aNode + aColumn
  // - use it for check/change the aNewValue argument, before assign it, and/or abort the process
  TOnGridEditValidated = procedure(aSender: TTisGrid; aNode: PVirtualNode; aColumn: TTisGridColumn;
    const aCurValue: Variant; var aNewValue: Variant; var aAbort: Boolean) of object;

  /// export a custom format
  // - use it to pass a custom buffer to the grid when call ExportData, if you use a non-default format
  TOnGridExportCustomContent = procedure(aSender: TTisGrid; aSource: TVSTTextSourceType; var aBuffer: RawUtf8) of object;

  /// event that allows customizing the metadata
  TOnGridGetMetaData = procedure(aSender: TTisGrid; var aMetaData: RawUtf8) of object;

  /// event that allows change aNode.States after it was changed
  // - use it to force showing (or not) some node by setting aHandled=TRUE
  TOnGridNodeFiltering = procedure(aSender: TTisGrid; aNode: PVirtualNode; var aHandled: Boolean) of object;

  /// event that allows overriden the default HTML rendering
  // - set aHandled=TRUE, if you overriden aHtmlResult
  TOnGridBeforeHtmlRendering = procedure(aSender: TTisGrid; aRowData: PDocVariantData; aColumn: TTisGridColumn;
    var aHtmlResult: string; var aHandled: Boolean) of object;

  /// event that provides a value for a "virtual column" (when the column PropertyName does not exist in Data)
  // - you could set aValue using a simple value or even a DocVariantData
  // - set aHandled=TRUE, if you filled aValue
  TOnGridCalcAttributes = procedure(aSender: TTisGrid; aNode: PVirtualNode; aColumn: TTisGridColumn;
    out aValue: Variant; var aHandled: Boolean) of object;

  /// event that allows changing the chart's source values before sending to it
  TOnGridBeforeAddingChartSource = procedure(aSender: TTisGrid; aColumn: TTisGridColumn;
    var aX, aY: Double; var aLabel: string; var aColor: TColor) of object;

  /// event that allows naming the chart's title
  TOnGridChartTitle = procedure(aSender: TTisGrid; aChart: TChart;
    aColumn: TTisGridColumn; var aFlags: TTisChartChangeFlags; var aChartTitle: string) of object;

  /// event that will fired if user changes something on the chart
  TOnGridChartChange = procedure(aSender: TTisGrid; aChart: TChart;
    aColumn: TTisGridColumn; var aFlags: TTisChartChangeFlags) of object;

  /// this component is based on TVirtualStringTree, using mORMot TDocVariantData type
  // as the protocol for receiving and sending data

  { TTisGrid }

  TTisGrid = class(TCustomVirtualStringTree)
  private
    // ------------------------------- new fields ----------------------------------
    fNodeAdapter: TTisNodeAdapter;
    fKeyFieldsList, fParentKeyFieldsList: array of string;
    fSelectedAndTotalLabel: TLabel;
    fTextFound: boolean;
    fFindDlg: TFindDialog;
    fZebraColor: TColor;
    fZebraPaint: Boolean;
    fZebraLightness: Byte;
    fReplaceDialog: TReplaceDialog;
    fColumnToFind: integer;
    fStartSearchNode: PVirtualNode;
    fTextToFind: string;
    fData: TDocVariantData;
    fNodeOptions: TTisNodeOptions;
    fPopupMenuOptions: TTisPopupMenuOptions;
    fExportFormatOptions: TTisGridExportFormatOptions;
    fChartOptions: TTisGridChartOptions;
    fFilterOptions: TTisGridFilterOptions;
    fDefaultSettings: Variant; // all default settings after load component
    // ------------------------------- new events ----------------------------------
    fOnGetText: TOnGridGetText;
    fOnCutToClipboard: TNotifyEvent;
    fOnBeforeDataChange: TOnGridBeforeDataChange;
    fOnAfterDataChange: TOnGridAfterDataChange;
    fOnInsert: TOnGridInsert;
    fOnBeforePaste: TOnGridPaste;
    fOnBeforeDeleteRows: TOnGridBeforeDeleteRows;
    fOnCompareByRow: TOnGridCompareByRow;
    fOnAfterFillPopupMenu: TNotifyEvent;
    fOnCustomEditor: TOnGridCustomEditor;
    fOnEditorLookup: TOnGridEditorLookup;
    fOnPrepareEditor: TOnGridPrepareEditor;
    fOnEditValidated: TOnGridEditValidated;
    fOnExportCustomContent: TOnGridExportCustomContent;
    fOnGetMetaData: TOnGridGetMetaData;
    fOnNodeFiltering: TOnGridNodeFiltering;
    fOnBeforeHtmlRendering: TOnGridBeforeHtmlRendering;
    fOnCalcAttributes: TOnGridCalcAttributes;
    fOnBeforeAddingChartSource: TOnGridBeforeAddingChartSource;
    fOnChartTitle: TOnGridChartTitle;
    fOnChartChange: TOnGridChartChange;
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
    procedure SetSelectedAndTotalLabel(aValue: TLabel);
    procedure SetZebraLightness(aValue: Byte);
  protected const
    // ------------------------------- new constants -------------------------------
    DefaultPopupMenuOptions = [
      pmoShowFind, pmoShowFindNext, pmoShowCut, pmoShowCopy, pmoShowCopyCell,
      pmoShowPaste, pmoShowDelete, pmoShowSelectAll, pmoShowCustomizeColumns, pmoShowChart];
    DefaultExportFormatOptions = [efoCsv, efoJson];
    DefaultWantTabs = True;
    DefaultZebraLightness = 250;
  protected
    DefaultCsvSeparator: string;
    // ------------------------------- inherited methods ---------------------------
    procedure WMKeyDown(var Message: TLMKeyDown); message LM_KEYDOWN;
    procedure Loaded; override;
    procedure WndProc(var Message: TLMessage); override;
    /// after cell editing to set Data
    procedure DoNewText(aNode: PVirtualNode; aColumn: TColumnIndex;
      const aText: string); override;
    procedure DoGetText(aNode: PVirtualNode; aColumn: TColumnIndex;
      aTextType: TVSTTextType; var aText: string); override;
    procedure DoInitNode(aParentNode, aNode: PVirtualNode;
      var aInitStates: TVirtualNodeInitStates); override;
    procedure DoFreeNode(aNode: PVirtualNode); override;
    procedure DoMeasureItem(aTargetCanvas: TCanvas; aNode: PVirtualNode; var aNodeHeight: Integer); override;
    function DoCompare(aNode1, aNode2: PVirtualNode; aColumn: TColumnIndex): Integer; override;
    procedure DoCanEdit(aNode: PVirtualNode; aColumn: TColumnIndex; var aAllowed: Boolean); override;
    function GetColumnClass: TVirtualTreeColumnClass; override;
    function GetOptionsClass: TTreeOptionsClass; override;
    /// it will check if OnCreateEditor was implemented first
    // - if user did not implement this event, a default implementation will be used
    function DoCreateEditor(aNode: PVirtualNode; aColumn: TColumnIndex): IVTEditLink; override;
    procedure PrepareCell(var PaintInfo: TVTPaintInfo;
      WindowOrgX, MaxWidth: integer); override;
    /// multiselection display management
    procedure DoBeforeCellPaint(aCanvas: TCanvas; aNode: PVirtualNode;
      aColumn: TColumnIndex; aCellPaintMode: TVTCellPaintMode; aCellRect: TRect;
      var aContentRect: TRect); override;
    procedure DoTextDrawing(var aPaintInfo: TVTPaintInfo; const aText: string;
      aCellRect: TRect; aDrawFormat: cardinal); override;
    procedure DoBeforeItemErase(aCanvas: TCanvas; aNode: PVirtualNode;
      const aItemRect: TRect; var aColor: TColor; var aEraseAction: TItemEraseAction); override;
    function DoKeyAction(var CharCode: Word; var Shift: TShiftState): Boolean; override;
    procedure Notification(aComponent: TComponent; aOperation: TOperation); override;
    procedure DoAutoAdjustLayout(const aMode: TLayoutAdjustmentPolicy;
      const aXProportion, aYProportion: Double); override;
    procedure DoChange(aNode: PVirtualNode); override;
    /// called before open a context menu
    // - it will call Clean/FillPopupMenu, as some Captions translation should be done before show up
    procedure DoContextPopup(aMousePos: TPoint; var aHandled: Boolean); override;
    procedure DoHeaderMouseDown(aButton: TMouseButton; aShift: TShiftState; aX, aY: Integer); override;
    procedure DoEnter; override;
    procedure DoExit; override;
    function GetHeaderClass: TVTHeaderClass; override;

    function GetTreeOptions: TTisStringTreeOptions;
    procedure SetTreeOptions(const aValue: TTisStringTreeOptions);

    // ----------------------------------- new methods -----------------------------
    /// fill the popup menu with items in runtime when grid gets focus, for deal with the data and grid itself
    // - each item will use the POPUP_ITEM_TAG public constant as its Tag value
    // - it is called in DoEnter and DoContextPopup
    procedure FillPopupMenu; virtual;
    /// clean the popup when lost focus, removing items inserted before
    // - each item that has the POPUP_ITEM_TAG public constant as its Tag value, will be removed
    // - it is called in DoExit and DoContextPopup
    procedure CleanPopupMenu; virtual;
    function FindText(const aText: string): PVirtualNode;
    procedure FindDlgFind(aSender: TObject);
    /// add aData into Data property
    // - will test if it is an array or object
    // - returns the index of the corresponding newly added item
    function Add(aData: PDocVariantData): Integer;
    function DoCompareByRow(const aPropertyName: RawUtf8; const aRow1, aRow2: PDocVariantData): PtrInt; virtual;
    procedure DoFindText(aSender: TObject);
    procedure DoFindNext(aSender: TObject);
    procedure DoFindReplace(aSender: TObject);
    procedure DoUndoLastUpdate(aSender: TObject); virtual;
    procedure DoRevertRecord(aSender: TObject); virtual;
    procedure DoExport(aSender: TObject); virtual;
    procedure DoExportCustomContent(aSource: TVSTTextSourceType; var aBuffer: RawUtf8); virtual;
    procedure DoCopyToClipboard(aSender: TObject); virtual;
    procedure DoCopyCellToClipboard(aSender: TObject); virtual;
    /// it will clear selected cell(s) by setting NULL
    // - if NodeOptions.MultiEdit is TRUE, it will perform in all selected ones
    // - for each cell, DoEditValidated() event will be called
    procedure DoClearCell(aSender: TObject); virtual;
    procedure DoCopySpecialToClipboard(aSender: TObject); virtual;
    procedure DoCutToClipboard(aSender: TObject); virtual;
    procedure DoInsert(aSender: TObject); virtual;
    procedure DoDeleteRows(aSender: TObject); virtual;
    procedure DoPaste(aSender: TObject); virtual;
    procedure DoSelectAllRows(aSender: TObject); virtual;
    procedure DoPrint(aSender: TObject); virtual;
    procedure DoCustomizeColumns(aSender: TObject); virtual;
    procedure DoAdvancedCustomizeColumns(aSender: TObject); virtual;
    procedure DoExpandAll(aSender: TObject); virtual;
    procedure DoCollapseAll(aSender: TObject); virtual;
    procedure DoShowChart(aSender: TObject); virtual;
    /// performs OnCustonEditor event, if it was assigned
    procedure DoCustomEditor(aNode: PVirtualNode; const aColumn: TTisGridColumn;
      out aControl: TTisGridControl); virtual;
    /// performs OnEditorLookup event, if it was assigned
    procedure DoEditorLookup(aNode: PVirtualNode; const aColumn: TTisGridColumn;
      out aControl: TTisGridControl; var aHandled: Boolean); virtual;
    /// performs OnPrepareEditor event, if it was assigned
    procedure DoPrepareEditor(aNode: PVirtualNode; const aColumn: TTisGridColumn; aControl: TTisGridControl); virtual;
    procedure DoEditValidated(aNode: PVirtualNode; const aColumn: TTisGridColumn;
      const aCurValue: Variant; var aNewValue: Variant; var aAbort: Boolean); virtual;
    procedure DoBeforeDataChange(aData: PDocVariantData; var aAbort: Boolean); virtual;
    procedure DoAfterDataChange; virtual;
    procedure DoGetMetaData(var aMetaData: RawUtf8); virtual;
    function DoNodeFiltering(aNode: PVirtualNode): Boolean; virtual;
    function DoBeforeHtmlRendering(aNode: PVirtualNode; aColumn: TTisGridColumn): string; virtual;
    function DoCalcAttributes(aNode: PVirtualNode; aColumn: TTisGridColumn;
      out aValue: Variant): Boolean; virtual;
    procedure DoBeforeAddingChartSource(aColumn: TTisGridColumn; var aX,
      aY: Double; var aLabel: string; var aColor: TColor); virtual;
    procedure DoChartTitle(aChart: TChart; aColumn: TTisGridColumn; var aFlags: TTisChartChangeFlags); virtual;
    procedure DoChartFillSource(aChart: TChart; aSource: TListChartSource; var aFlags: TTisChartFillSourceFlags); virtual;
    procedure DoChartChange(aChart: TChart; var aFlags: TTisChartChangeFlags); virtual;
    /// it returns the filter for the Save Dialog, when user wants to export data
    // - it will add file filters based on ExportFormatOptions property values
    // - you can override this method to customize default filters
    function GetExportDialogFilter: string; virtual;
    /// it restore original settings from original design
    procedure RestoreSettings;
    procedure InitData;
    procedure FillDataFromJsonObject(const aObject: TDocVariantData);
    /// return all selected rows (by copy)
    function GetSelectedRows: TDocVariantData;
    /// select all rows which are matching aValue in Data
    procedure SetSelectedRows(const aValue: TDocVariantData);
    // ------------------------------- inherited properties ------------------------
    property RootNodeCount stored False;
    // ------------------------------- new properties ------------------------------
    property ColumnToFind: integer read fColumnToFind write SetColumnToFind;
    property TextToFind: string read fTextToFind write fTextToFind;
    property TextFound: boolean read fTextFound write fTextFound;
  public const
    TREEMODE_OPTIONS = [toShowRoot, toShowButtons, toShowTreeLines];
    POPUP_ITEM_TAG = 250;
  public
    /// default callback for naming all charts' title
    // - use this callback to set a global algorithm for all grids and its charts
    class var OnDefaultChartTitle: TOnGridChartTitle;
    /// primary construtor
    constructor Create(aOwner: TComponent); override;
    /// destructor
    destructor Destroy; override;
    // ------------------------------- inherited methods ---------------------------
    procedure Assign(aSource: TPersistent); override;
    procedure FixDesignFontsPPI(const ADesignTimePPI: Integer); override;
    procedure ScaleFontsPPI(const AToPPI: Integer; const AProportion: Double); override;
    function AddChild(aParent: PVirtualNode; aUserData: Pointer = nil): PVirtualNode; override;
    /// it will clear Data and everything else related
    procedure Clear; override;
    function ComputeNodeHeight(aCanvas: TCanvas; aNode: PVirtualNode; aColumn: TColumnIndex; S: string = ''): Integer; override;
    // ----------------------------------- new methods -----------------------------
    /// refresh the grid using Data content
    // - call LoadData, if you change Data content directly
    procedure LoadData(const NewGridData: PDocVariantdata=Nil);
    /// it will try load aJson into Data and create Columns from it
    // - it will not clean previous columns or data, if they exists
    // - return TRUE if success, otherwise FALSE with a Dialog error if aShowError is TRUE
    // - if NodeOptions.ShowChildren is TRUE and aJson is an object, it will
    // use FillDataFromJsonObject method for split and convert its fields in Data, which is an array
    function TryLoadAllFrom(const aJson: string; aShowError: Boolean = True): Boolean;
    /// export data
    // - it will export using the format that matchs to aFileName extension
    // - if extension do not exist in ExportFormatOptions, it will use OnExportCustomContent event to get the content
    // - use aSelection as tstAll to export all nodes - default
    // - use aSelection as tstSelected to export only selected nodes
    procedure ExportData(const aFileName: TFileName; const aSelection: TVSTTextSourceType = tstAll);
    // Creates a temporary CSV file and open it in the default app
    procedure ExportExcel(Prefix:String='';Selection: TVSTTextSourceType=tstAll; Separator:Char=';');
    /// get all checked rows
    function CheckedRows: TDocVariantData;
    /// it will focus in a row it which is matching exactly to aValue,
    // but it will not clear the current selection
    procedure SetFocusedRowNoClearSelection(aValue: PDocVariantData; EnsureScrollIntoView: Boolean = False);
    /// returns Data property as an object instead an array (default)
    // - it will convert Data array to a only single JSON object
    // - each object in Data array will be its fields splitted into the result
    // - you may use this method when NodeOptions.ShowChildren is TRUE, to export
    // all data in just one JSON object, after user has finished his edition
    function GetDataAsJsonObject: TDocVariantData;
    /// returns the cell value
    // - it will return the supplied default, if aPropertyName is not found
    // - you should better not use it when NodeOptions.ShowChildren is TRUE
    function GetCellData(aNode: PVirtualNode; const aPropertyName: RawUtf8;
      aDefault: PVariant = nil): PVariant;
    /// returns the cell value as string
    // - it will return the supplied default, if aPropertyName is not found
    // - you should better not use it when NodeOptions.ShowChildren is TRUE
    function GetCellDataAsString(aNode: PVirtualNode; const aPropertyName: RawUtf8;
      const aDefault: string = ''): string;
    /// it will return aNode as PDocVariantData
    // - if aNode is NIL and aUseFocusedNodeAsDefault is TRUE, it will use the node from FocusedNode property
    // - if aNode is child type, it will lookup and return its Parent non-child with the original PDocVariantData
    function GetNodeAsPDocVariantData(aNode: PVirtualNode; aUseFocusedNodeAsDefault: Boolean = True): PDocVariantData;
    /// it will return aNode as PDocVariantData
    // - see GetNodeAsPDocVariantData
    function GetNodeDataAsDocVariant(aNode: PVirtualNode; aUseFocusedNodeAsDefault: Boolean = True): PDocVariantData;
      deprecated 'Use GetNodeAsPDocVariantData instead';
    /// returns a list of nodes which is matching exactly to aData
    // - use aUseKeyFieldsList for search only into fields from KeyFieldsList
    // - if KeyFieldsList is empty, it will be the same as passing aUseKeyFieldsList = FALSE
    function GetNodesBy(aData: PDocVariantData; aUseKeyFieldsList: Boolean = False): TNodeArray; overload;
    /// returns the first node which is matching to aData
    // - use aUseKeyFieldsList for search only into fields from KeyFieldsList
    // - if KeyFieldsList is empty, it will be the same as passing aUseKeyFieldsList = FALSE
    // - use aRowPosition to get a specific row, if you know that it will return more than one
    function GetNodeBy(aData: PDocVariantData; aUseKeyFieldsList: Boolean = False;
      aRowPosition: PtrInt = 0): PVirtualNode; overload;
    /// returns a list of nodes which is matching to key and value
    function GetNodesBy(const aKey, aValue: RawUtf8): TNodeArray; overload;
    /// find a column by its property name
    // - if not found, it will return NIL
    function FindColumnByPropertyName(const aPropertyName: RawUtf8): TTisGridColumn;
    /// find a column by its index
    // - if not found, it will return NIL
    function FindColumnByIndex(const aIndex: TColumnIndex): TTisGridColumn;
    /// retuns TRUE if the column spicified by property name is visible
    // - if not found or not visible, it will return FALSE
    function IsVisibleColumnByPropertyName(const aPropertyName: RawUtf8): Boolean;
    /// append a list of rows to the Grid
    // - use aAllowDuplicates=TRUE for allow duplicate rows
    // - use aCreateColumns=TRUE for create columns if they not exists yet
    function AddRows(aData: PDocVariantData; aAllowDuplicates: Boolean = True; aCreateColumns: Boolean = True): TIntegerDynArray;
    /// delete a list of rows that match aRows
    // - it will not call OnBeforeDeleteRows event by design - use DeleteSelectedRows instead
    procedure DeleteRows(aRows: PDocVariantData);
    /// delete selected rows
    // - if OnBeforeDeleteRows was not assigned, it will ask user to confirm
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
      const aOldValues, aNewValues: TDocVariantData);
    /// add columns based on Data content
    procedure CreateColumnsFromData(aAutoFitColumns, aAppendMissingAsHidden: Boolean);
    /// export Data to CSV format
    function ContentToCsv(aSource: TVSTTextSourceType; const aSeparator: string = ';';
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
    /// it returns TRUE if tree mode options were settled
    // - also KeyFieldsNames and ParentKeyFieldsNames should not be empty
    function IsTreeModeKeyParent: Boolean;
    /// it will search aText in all columns
    // - if found, focus will go to the grid
    function Search(const aText: string): Boolean;
    /// return selected objects by reference
    function SelectedObjects: PDocVariantDataDynArray;
    // ------------------------------- inherited events ----------------------------
    property OnCompareNodes; // hiding from Object Inspector, use OnCompareByRow event instead
    // ------------------------------- new properties ------------------------------
    /// direct access to the low-level internal data
    // - if you change its content directly, you must call LoadData
    // - Data is initialized as an array of objects
    // - you can change Data.Kind to dvObject, but after you call LoadData, Data will
    // be converted to array again
    // - if you want a final result as a single JSON object, see GetDataAsJsonObject method
    property Data: TDocVariantData read fData write SetData;
    /// columns metadata as JSON
    property MetaData: RawUtf8 read GetMetaData write SetMetaData;
    /// direct access to the node adapter object
    // - you can use it to get more information about a node, especially when
    // it is working in a tree mode with children nodes
    property NodeAdapter: TTisNodeAdapter read fNodeAdapter;
    /// return selected rows by copy
    // - do not use this to edit Data by code, instead use SelectedObjects
    property SelectedRows: TDocVariantData read GetSelectedRows write SetSelectedRows;
    /// returns the focused row
    // - be careful using it in tree mode - see NodeOptions.ShowChildren
    property FocusedRow: PDocVariantData read GetFocusedRow write SetFocusedRow;
    /// returns the focused column as TTisGridColumn
    property FocusedColumnObject: TTisGridColumn read GetFocusedColumnObject write SetFocusedColumnObject;
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
    property DragType default dtVCL;
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
    property PopupMenu;
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
    property SelectedAndTotalLabel: TLabel read fSelectedAndTotalLabel write SetSelectedAndTotalLabel;
    property TreeOptions: TTisStringTreeOptions read GetTreeOptions write SetTreeOptions;
    property KeyFieldsList: TStringDynArray read fKeyFieldsList;
    property KeyFieldsNames: string read GetKeyFieldsNames write SetKeyFieldsNames;
    property ParentKeyFieldsNames: string read GetParentKeyFieldsNames write SetParentKeyFieldsNames;
    property GridSettings: string read GetGridSettings write SetGridSettings stored False;
    property ZebraLightness: Byte read fZebraLightness write SetZebraLightness default DefaultZebraLightness;
    property ZebraColor: TColor read fZebraColor write fZebraColor;
    property ZebraPaint: Boolean read fZebraPaint write fZebraPaint stored True default False;
    property NodeOptions: TTisNodeOptions read fNodeOptions write fNodeOptions;
    property PopupMenuOptions: TTisPopupMenuOptions read fPopupMenuOptions write fPopupMenuOptions default DefaultPopupMenuOptions;
    property ExportFormatOptions: TTisGridExportFormatOptions read fExportFormatOptions write fExportFormatOptions default DefaultExportFormatOptions;
    property ChartOptions: TTisGridChartOptions read fChartOptions write fChartOptions;
    property FilterOptions: TTisGridFilterOptions read fFilterOptions write fFilterOptions;
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
    property OnGetText: TOnGridGetText read fOnGetText write fOnGetText;
    property OnCutToClipboard: TNotifyEvent read fOnCutToClipboard write SetOnCutToClipboard;
    /// event to manipulate data before change the internal Data
    // - use it for check/change the aData argument, before assign it, and/or abort the process
    property OnBeforeDataChange: TOnGridBeforeDataChange read fOnBeforeDataChange write fOnBeforeDataChange;
    /// event to manipulate data after the internal Data changed
    // - as used by TTisGrid.OnAfterDataChage
    property OnAfterDataChange: TOnGridAfterDataChange read fOnAfterDataChange write fOnAfterDataChange;
    property OnInsert: TOnGridInsert read fOnInsert write fOnInsert;
    property OnBeforePaste: TOnGridPaste read fOnBeforePaste write fOnBeforePaste;
    /// event to manipulate rows before deleting them
    // - use it for change the rows or abort the process by assign True to aAbort
    // - if you do not use this event, by default it will ask user about deletion
    // and, if the answer was yes, selected rows will be deleted
    // - you can also supress the user dialog, asking it should continue, by assign False to aAskUser
    property OnBeforeDeleteRows: TOnGridBeforeDeleteRows read fOnBeforeDeleteRows write fOnBeforeDeleteRows;
    /// comparing rows of objects
    // - aPropertyName is the header column that was clicked
    // - aRow1, aRow2 are the whole lines that should be compared
    // - assign aHandle to True if you did some comparison, otherwise assign to False or do nothing
    // to let grid do the job itself
    // - on user callback, the compararison could be like this:
    // !aRow1.CompareObject([aPropertyName], aRow2); // you can use other columns together too
    // !aHandled := True;
    property OnCompareByRow: TOnGridCompareByRow read fOnCompareByRow write fOnCompareByRow;
    /// event to manipulate Popup items after they being inserted
    // - see FillPopupMenu
    property OnAfterFillPopupMenu: TNotifyEvent read fOnAfterFillPopupMenu write fOnAfterFillPopupMenu;
    /// event that allows users customize the edit control instance, creating a new one,
    // - you should return an instance in aControl
    // - you can use OnPrepareEditor to customize some properties
    property OnCustomEditor: TOnGridCustomEditor read fOnCustomEditor write fOnCustomEditor;
    /// event that simplifies the use of a TisSearchEdit as Edit Control
    // - you may want to use this event instead OnCustomEditor for TisSearchEdit
    property OnEditorLookup: TOnGridEditorLookup read fOnEditorLookup write fOnEditorLookup;
    /// event that allows users to change some edit control properties, before it shows up
    property OnPrepareEditor: TOnGridPrepareEditor read fOnPrepareEditor write fOnPrepareEditor;
    /// event that allows to validate the new user input value
    // - if the new value is coming from a calculated attribute (see OnCalcAttributes), you can use it
    // elsewhere (eg: another DocVariantData, change another attribute in Grid Data, etc) you want to
    property OnEditValidated: TOnGridEditValidated read fOnEditValidated write fOnEditValidated;
    /// event when export a custom format
    // - use it to pass a custom buffer to the grid when call ExportData, if you use a format that
    // is not included in TTisGridExportFormatOption
    property OnExportCustomContent: TOnGridExportCustomContent read fOnExportCustomContent write fOnExportCustomContent;
    /// event that allows customizing the metadata, before give it to the caller
    property OnGetMetaData: TOnGridGetMetaData read fOnGetMetaData write fOnGetMetaData;
    /// event that allows change aNode.States after it was changed
    // - use it to force showing (or not) some node
    property OnNodeFiltering: TOnGridNodeFiltering read fOnNodeFiltering write fOnNodeFiltering;
    /// event that allows apply a HTML template for the data
    property OnBeforeHtmlRendering: TOnGridBeforeHtmlRendering read fOnBeforeHtmlRendering write fOnBeforeHtmlRendering;
    /// event that provides a value for a "virtual column" (when the column PropertyName does not exist in Data)
    // - you could set aValue using a simple value or even a DocVariantData
    // - set aHandled=TRUE, if you filled aValue
    // - when editing it, you can use this new value in OnEditValidated event
    property OnCalcAttributes: TOnGridCalcAttributes read fOnCalcAttributes write fOnCalcAttributes;
    /// event that allows changing the chart's source values before sending to it
    property OnBeforeAddingChartSource: TOnGridBeforeAddingChartSource read fOnBeforeAddingChartSource write fOnBeforeAddingChartSource;
    /// event that allows naming the chart's title
    property OnChartTitle: TOnGridChartTitle read fOnChartTitle write fOnChartTitle;
    /// event that will fired if user changes something on the chart
    property OnChartChange: TOnGridChartChange read fOnChartChange write fOnChartChange;
  end;

implementation

uses
  GraphUtil,
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
    (Caption: 'JSON'),
    (Caption: 'HTML'),
    (Caption: 'Password')
  );

  CHILD_COLUMN_NAME_INDEX = 0;
  CHILD_COLUMN_VALUE_INDEX = 1;

{ TTisGridColumnHtmlOptions }

procedure TTisGridColumnHtmlOptions.SetMustacheTemplate(aValue: TStrings);
begin
  if Assigned(aValue) then
    fMustacheTemplate.Assign(aValue);
end;

constructor TTisGridColumnHtmlOptions.Create;
begin
  inherited Create;
  fMustacheTemplate := TTextStrings.Create;
end;

destructor TTisGridColumnHtmlOptions.Destroy;
begin
  fMustacheTemplate.Free;
  inherited Destroy;
end;

procedure TTisGridColumnHtmlOptions.AssignTo(aDest: TPersistent);
begin
  if aDest is TTisGridColumnHtmlOptions then
  begin
    with TTisGridColumnHtmlOptions(aDest) do
    begin
      MustacheTemplate.Assign(self.MustacheTemplate);
    end;
  end
  else
    inherited AssignTo(aDest);
end;

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
  v1: TTisColumnDataType;
begin
  result := low(TTisColumnDataType);
  for v1 := low(COLUMN_DATA_TYPES) to high(COLUMN_DATA_TYPES) do
    if COLUMN_DATA_TYPES[v1].Caption = aValue then
    begin
      result := v1;
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
  v1: TTisColumnDataType;
begin
  for v1 := low(TTisColumnDataType) to high(TTisColumnDataType) do
    if v1 in aCustom then
      aDest.Append(EnumToCaption(v1));
end;

{ TTisGridEditLink }

procedure TTisGridEditLink.DisableControlEvents;
begin
  if Assigned(fControl) then
    fControl.SetOnExit(nil);
end;

procedure TTisGridEditLink.EditKeyDown(aSender: TObject; var Key: Word; Shift: TShiftState);
var
  vCanAdvance: Boolean;
begin
  if fAbortAll then
    exit;
  vCanAdvance := True;
  case Key of
    VK_ESCAPE:
      if vCanAdvance then
      begin
        DisableControlEvents;
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
  if fAbortAll then
    exit;
  if Assigned(fControl) then
  begin
    if (toAutoAcceptEditChange in fGrid.TreeOptions.StringOptions) then
      fGrid.EndEditNode
    else
      fGrid.CancelEditNode;
  end;
end;

class procedure TTisGridEditLink.SetupControlClasses;
begin
  ControlClasses[cdtString] := TTisGridEditControl;
  ControlClasses[cdtDate] := TTisGridDateEditControl;
  ControlClasses[cdtTime] := TTisGridTimeEditControl;
  ControlClasses[cdtDateTime] := TTisGridDateTimeEditControl;
  ControlClasses[cdtInteger] := TTisGridIntegerEditControl;
  ControlClasses[cdtFloat] := TTisGridFloatEditControl;
  ControlClasses[cdtBoolean] := TTisGridBooleanEditControl;
  ControlClasses[cdtMemo] := TTisGridMemoControl;
  ControlClasses[cdtJson] := TTisGridEditControl;
  ControlClasses[cdtHtml] := TTisGridHtmlControl;
  ControlClasses[cdtPassword] := TTisGridPasswordEditControl;
end;

function TTisGridEditLink.NewControl(aNode: PVirtualNode;
  aColumn: TTisGridColumn): TTisGridControl;
var
  vHandled: Boolean;
begin
  fGrid.DoCustomEditor(aNode, aColumn, result);
  if result = nil then
  begin
    vHandled := False;
    fGrid.DoEditorLookup(aNode, aColumn, result, vHandled);
    if not vHandled then
      result := ControlClasses[aColumn.DataType].Create(fGrid);
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
end;

destructor TTisGridEditLink.Destroy;
begin
  FreeAndNil(fControl);
  inherited Destroy;
end;

function TTisGridEditLink.BeginEdit: Boolean; stdcall;
begin
  result := not fAbortAll;
  if result then
  begin
    fControl.Internal.Show;
    fControl.Internal.SetFocus;
  end;
end;

function TTisGridEditLink.CancelEdit: Boolean; stdcall;
begin
  result := True;
  DisableControlEvents;
  fControl.Internal.Hide;
end;

function TTisGridEditLink.EndEdit: Boolean; stdcall;
var
  vCol: TTisGridColumn;
  vAborted: Boolean;
  vCur, vNew: Variant;
  vNode: PVirtualNode;
begin
  result := True;
  if fAbortAll then
  begin
    result := False;
    exit;
  end;
  DisableControlEvents;
  try
    vCur := fGrid.fNodeAdapter.GetValueAsVariant(fNode, fColumn);
    vNew := fControl.GetValue;
    vCol := fGrid.FindColumnByIndex(fColumn);
    vAborted := False;
    fGrid.DoEditValidated(fNode, vCol, vCur, vNew, vAborted);
    if vAborted then
      exit;
    if fGrid.NodeOptions.MultiEdit then
      for vNode in fGrid.SelectedNodes do
        fGrid.fNodeAdapter.SetValue(vNode, vNew, fColumn, fValueIsString)
    else
      fGrid.fNodeAdapter.SetValue(fNode, vNew, fColumn, fValueIsString);
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
  vCol: TTisGridColumn;
  vValue: Variant;
  vDateTime: TDateTime;
  vCanEdit: Boolean;
begin
  result := True;
  fAbortAll := False;
  fGrid := aTree as TTisGrid;
  fNode := aNode;
  fColumn := aColumn;
  FreeAndNil(fControl);
  vCol := fGrid.FindColumnByIndex(fColumn);
  fControl := NewControl(fNode, vCol);
  fControl.ReadOnly := vCol.ReadOnly;
  // it is allowed to edit:
  vCanEdit :=
    // if it is HTML - it will be a "fake editing", just to able to select and copy
    (vCol.DataType = cdtHtml) or (
      // if it is not read only and has no children - see NodeOptions.ShowChildren property
      not fControl.ReadOnly and not (vsHasChildren in fNode^.States) and (
        // if it has a simple value
        Assigned(fGrid.fNodeAdapter.GetValueAsSimple(fNode, fColumn)) or
        // if it is a calculated node column - see OnCalcAttributes event
        not Assigned(fGrid.fNodeAdapter.GetValue(fNode, fColumn)) and
        fGrid.DoCalcAttributes(aNode, vCol, vValue)
      )
    );
  if not vCanEdit then
  begin
    result := False;
    fAbortAll := True;
    DisableControlEvents;
    exit;
  end;
  vValue := fGrid.fNodeAdapter.GetValueAsVariant(aNode, aColumn);
  fValueIsString := VarIsStr(vValue);
  // format date/time, if needed
  if (vCol.DataType in [cdtDate, cdtTime, cdtDateTime]) and
    vCol.DataTypeOptions.DateTimeOptions.SaveAsUtc and
    vCol.DataTypeOptions.DateTimeOptions.ShowAsLocal then
  begin
    vDateTime := vCol.DataTypeOptions.DateTimeOptions.UtcToLocal(Iso8601ToDateTime(VariantToUtf8(vValue)));
    fControl.SetValue(DateTimeToIso8601(vDateTime, True));
  end
  // HTML rendering
  else if vCol.DataType = cdtHtml then
    fControl.SetValue(fGrid.DoBeforeHtmlRendering(fNode, vCol))
  else
    fControl.SetValue(vValue);
  fGrid.DoPrepareEditor(fNode, vCol, fControl);
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

{ TTisGridColumnDateTimeOptions }

constructor TTisGridColumnDateTimeOptions.Create;
begin
  inherited Create;
  fSaveAsUtc := DefaultSaveAsUtc;
  fShowAsDateTime := DefaultShowAsDateTime;
  fShowAsLocal := DefaultShowAsLocal;
end;

procedure TTisGridColumnDateTimeOptions.AssignTo(aDest: TPersistent);
begin
  if aDest is TTisGridColumnDateTimeOptions then
  begin
    with TTisGridColumnDateTimeOptions(aDest) do
    begin
      SaveAsUtc := self.SaveAsUtc;
      ShowAsDateTime := self.ShowAsDateTime;
      ShowAsLocal := self.ShowAsLocal;
    end;
  end
  else
    inherited AssignTo(aDest);
end;

function TTisGridColumnDateTimeOptions.UtcToLocal(const aValue: TDateTime): TDateTime;
begin
  result := aValue + TimeZoneLocalBias / 24 / 60;
end;

function TTisGridColumnDateTimeOptions.LocalToUtc(const aValue: TDateTime): TDateTime;
begin
  result := aValue - TimeZoneLocalBias / 24 / 60;
end;

{ TTisGridColumnDataTypeOptions }

constructor TTisGridColumnDataTypeOptions.Create;
begin
  inherited Create;
  fDateTimeOptions := TTisGridColumnDateTimeOptions.Create;
  fHtmlOptions := TTisGridColumnHtmlOptions.Create;
end;

destructor TTisGridColumnDataTypeOptions.Destroy;
begin
  fDateTimeOptions.Free;
  fHtmlOptions.Free;
  inherited Destroy;
end;

procedure TTisGridColumnDataTypeOptions.AssignTo(aDest: TPersistent);
begin
  if aDest is TTisGridColumnDataTypeOptions then
  begin
    with TTisGridColumnDataTypeOptions(aDest) do
    begin
      DateTimeOptions.Assign(self.DateTimeOptions);
      HtmlOptions.Assign(self.HtmlOptions);
    end;
  end
  else
    inherited AssignTo(aDest);
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
  fAllowChart := DefaultAllowChart;
  fAllowFilter := DefaultAllowFilter;
  fDataType := DefaultDataType;
  fDataTypeOptions := TTisGridColumnDataTypeOptions.Create;
  fRequired := DefaultRequired;
  fReadOnly := DefaultReadOnly;
end;

destructor TTisGridColumn.Destroy;
begin
  fDataTypeOptions.Free;
  inherited Destroy;
end;

procedure TTisGridColumn.Assign(aSource: TPersistent);
var
  vColumn: TTisGridColumn;
begin
  inherited Assign(aSource);
  if aSource is TTisGridColumn then
  begin
    vColumn := TTisGridColumn(aSource);
    AllowFilter := vColumn.AllowFilter;
    DataType := vColumn.DataType;
    DataTypeOptions.Assign(vColumn.DataTypeOptions);
    PropertyName := vColumn.PropertyName;
    Required := vColumn.Required;
    ReadOnly := vColumn.ReadOnly;
  end;
end;

{ TTisGridColumns }

function GetShiftState: TShiftState;

begin
  Result := [];
  if GetKeyState(VK_SHIFT) < 0 then
    Include(Result, ssShift);
  if GetKeyState(VK_LWIN) < 0 then      // Mac OS X substitute of ssCtrl
    Include(Result, ssMeta);
  if GetKeyState(VK_CONTROL) < 0 then
    Include(Result, ssCtrl);
  if GetKeyState(VK_MENU) < 0 then
    Include(Result, ssAlt);
end;


procedure TTisGridColumns.HandleClick(P: TPoint; aButton: TMouseButton; aForce,
  aDblClick: Boolean);
var
  vColumnIndex: Integer;
  HitInfo: TVTHeaderHitInfo;
begin
  if (csDesigning in Header.Treeview.ComponentState) then
    exit;
  vColumnIndex := ColumnFromPosition(P);
  if (hoHeaderClickAutoSort in Header.Options) and (aButton = mbLeft) and (vColumnIndex >= 0) then
  begin
    if (vColumnIndex = Header.SortColumn) and (Header.SortDirection = sdDescending) then
    begin
      Header.SortColumn := -1;
      if (csDesigning in Header.Treeview.ComponentState) then
        exit;
      if Assigned(TTisGrid(Header.Treeview).OnHeaderClick) then
      begin
        with HitInfo do
        begin
          X := P.X;
          Y := P.Y;
          Shift := GetShiftState;
          if aDblClick then
            Shift := Shift + [ssDouble];
        end;
        HitInfo.Button := aButton;
        if aDblClick then
          TTisGrid(Header.Treeview).DoHeaderDblClick(HitInfo)
        else
          TTisGrid(Header.Treeview).DoHeaderClick(HitInfo);
      end;
    end
    else
      inherited HandleClick(P, aButton, aForce, aDblClick);
  end;
end;

procedure TTisGridColumns.Assign(aSource: TPersistent);
var
  v1: Integer;
  vCol: TTisGridColumn;
  vSource: TTisGridColumns;
begin
  if aSource is TTisGridColumns then
  begin
    Header.Columns.Clear;
    vSource := aSource as TTisGridColumns;
    for v1 := 0 to vSource.Header.Columns.Count-1 do
    begin
      vCol := Header.Columns.Add as TTisGridColumn;
      vCol.Assign(vSource.Header.Columns[v1]);
      vCol.Options := vSource.Header.Columns[v1].Options;
    end;
  end
  else
    inherited Assign(aSource);
end;

function TTisGridColumns.GetColumnIndexByPropertyName(
  const aPropertyName: RawUtf8): Integer;
var
  v1: Integer;
begin
  result := NoColumn;
  for v1 := 0 to Header.Columns.Count-1 do
    if (Header.Columns[v1] as TTisGridColumn).PropertyName = aPropertyName then
    begin
      result := v1;
      break;
    end;
end;

function TTisGridColumns.DeleteColumnByPropertyName(const aPropertyName: RawUtf8): Integer;
var
  vIdx : Integer;
begin
  vIdx := Self.GetColumnIndexByPropertyName(aPropertyName);
  if vIdx > NoColumn then
    Header.Columns.Delete(vIdx);
  result := Header.Columns.Count;
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
  v1: TTisGridExportFormatOption;
begin
  result := low(TTisGridExportFormatOption);
  for v1 := low(GRID_EXPORT_FORMAT_OPTIONS) to high(GRID_EXPORT_FORMAT_OPTIONS) do
    if GRID_EXPORT_FORMAT_OPTIONS[v1].Caption = aValue then
    begin
      result := v1;
      exit;
    end;
end;

procedure TTisGridExportFormatOptionAdapter.EnumsToStrings(aDest: TStrings;
  const aCustom: TTisGridExportFormatOptions);
var
  v1: TTisGridExportFormatOption;
begin
  for v1 := low(TTisGridExportFormatOption) to high(TTisGridExportFormatOption) do
    if v1 in aCustom then
      aDest.Append(EnumToCaption(v1));
end;

function TTisGridExportFormatOptionAdapter.ExtensionToEnum(
  const aValue: TFileName): TTisGridExportFormatOption;
var
  v1: TTisGridExportFormatOption;
begin
  result := low(TTisGridExportFormatOption);
  for v1 := low(GRID_EXPORT_FORMAT_OPTIONS) to high(GRID_EXPORT_FORMAT_OPTIONS) do
    if GRID_EXPORT_FORMAT_OPTIONS[v1].Extension = aValue then
    begin
      result := v1;
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
  v1: TVSTTextSourceType;
begin
  result := low(TVSTTextSourceType);
  for v1 := low(GRID_TEXT_SOURCE_TYPES) to high(GRID_TEXT_SOURCE_TYPES) do
    if GRID_TEXT_SOURCE_TYPES[v1].Caption = aValue then
    begin
      result := v1;
      exit;
    end;
end;

procedure TTisGridTextSourceTypeAdapter.EnumsToStrings(aDest: TStrings;
  const aCustom: TTisGridTextSourceTypes);
var
  v1: TVSTTextSourceType;
begin
  for v1 := low(TVSTTextSourceType) to high(TVSTTextSourceType) do
    if v1 in aCustom then
      aDest.Append(EnumToCaption(v1));
end;

{ TTisGridHeaderPopupMenu }

type
  TVirtualTreeCast = class(TBaseVirtualTree); // necessary to make the header accessible

procedure TTisGridHeaderPopupMenu.RemoveAutoItems;
var
  v1: Integer;
begin
  v1 := Items.Count;
  while v1 > 0 do
  begin
    Dec(v1);
    if Items[v1] is TTisGridHeaderMenuItem then
      Items[v1].Free;
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
  v1: Integer;
  vGrid: TTisGrid;
begin
  if Assigned(PopupComponent) and (PopupComponent is TBaseVirtualTree) then
  begin
    vGrid := PopupComponent as TTisGrid;
    with vGrid.Header.Columns do
    begin
      for v1 := 0 to Count-1 do
      if not (coVisible in Items[v1].Options) then
      begin
        Items[v1].Options := Items[v1].Options + [coVisible];
        DoColumnChange(v1, True);
      end;
    end;
    vGrid.Invalidate; // needed it, at least for MacOS
  end;
end;

procedure TTisGridHeaderPopupMenu.OnMenuHideAllClick(aSender: TObject);
var
  v1: Integer;
  vGrid: TTisGrid;
begin
  if Assigned(PopupComponent) and (PopupComponent is TBaseVirtualTree) then
  begin
    if PopupComponent is TTisGrid then
    begin
      vGrid := PopupComponent as TTisGrid;
      with vGrid.Header.Columns do
      begin
        for v1 := 0 to Count-1 do
        if coVisible in Items[v1].Options then
        begin
          Items[v1].Options := Items[v1].Options - [coVisible];
          DoColumnChange(v1, False);
        end;
      end;
      vGrid.Invalidate; // needed it, at least for MacOS
    end;
  end;
end;

procedure TTisGridHeaderPopupMenu.OnMenuRestoreClick(aSender: TObject);
begin
  TTisGrid(PopupComponent).RestoreSettings;
end;

procedure TTisGridHeaderPopupMenu.OnMenuRemoveCustomColumnClick(aSender: TObject);
var
  vGrid: TTisGrid;
  vItem: TMenuItem;
begin
  if Assigned(PopupComponent) and (PopupComponent is TBaseVirtualTree) then
  begin
    if PopupComponent is TTisGrid then
    begin
      vGrid := PopupComponent as TTisGrid;
      vItem := aSender as TMenuItem;
      vGrid.Header.Columns.Delete(vItem.Tag);
      if vGrid.Header.Columns.IsValidColumn(vItem.Tag) then
        vGrid.FocusedColumn := vItem.Tag
      else if vGrid.Header.Columns.GetLastVisibleColumn >= 0 then
        vGrid.FocusedColumn := vGrid.Header.Columns.GetLastVisibleColumn;
    end;
  end;
end;

procedure TTisGridHeaderPopupMenu.OnMenuFilterEnableClick(aSender: TObject);
var
  vGrid: TTisGrid;
begin
  if Assigned(PopupComponent) and (PopupComponent is TBaseVirtualTree) then
  begin
    if PopupComponent is TTisGrid then
    begin
      vGrid := PopupComponent as TTisGrid;
      vGrid.FilterOptions.ShowAutoFilters := not vGrid.FilterOptions.ShowAutoFilters;
      if not vGrid.FilterOptions.ShowAutoFilters then
        vGrid.FilterOptions.ClearFilters;
    end;
  end;
end;

procedure TTisGridHeaderPopupMenu.OnMenuFilterClick(aSender: TObject);
var
  vGrid: TTisGrid;
  vItem: TMenuItem;
  vColumn: TTisGridColumn;
  vObj: Variant;
begin
  if Assigned(PopupComponent) and (PopupComponent is TBaseVirtualTree) then
  begin
    if PopupComponent is TTisGrid then
    begin
      vItem := aSender as TMenuItem;
      vItem.Checked := not vItem.Checked;
      vGrid := PopupComponent as TTisGrid;
      vColumn := vGrid.FindColumnByIndex(vItem.Tag);
      vObj := _ObjFast([
        'field', vColumn.PropertyName,
        'value', StringToUtf8(vItem.Caption)
      ]);
      if vItem.Checked then
        vGrid.FilterOptions.Filters.AddItem(vObj)
      else
        vGrid.FilterOptions.Filters.DeleteByValue(vObj, vGrid.FilterOptions.CaseInsensitive);
      vGrid.FilterOptions.ApplyFilters;
    end;
  end;
end;

procedure TTisGridHeaderPopupMenu.OnMenuFilterClearClick(aSender: TObject);
var
  vGrid: TTisGrid;
  vItem: TMenuItem;
  vColumn: TTisGridColumn;
begin
  if Assigned(PopupComponent) and (PopupComponent is TBaseVirtualTree) then
  begin
    if PopupComponent is TTisGrid then
    begin
      vGrid := PopupComponent as TTisGrid;
      vItem := aSender as TMenuItem;
      vColumn := vGrid.FindColumnByIndex(vItem.Tag);
      if Assigned(vColumn) then
      begin
        // clear all filters for the same propertyname
        with vGrid.FilterOptions do
          while Filters.DeleteByProp('field', vColumn.PropertyName, not CaseInsensitive) do ;
        vGrid.FilterOptions.ApplyFilters;
      end
      else
        // if not found vColumn, it should clear all filters in the grid
        vGrid.FilterOptions.ClearFilters;
    end;
  end;
end;

procedure TTisGridHeaderPopupMenu.OnMenuFilterCustomClick(aSender: TObject);
var
  vGrid: TTisGrid;
  vItem: TMenuItem;
  vColumn: TTisGridColumn;
  vObj: Variant;
  vValue: string;
begin
  if Assigned(PopupComponent) and (PopupComponent is TBaseVirtualTree) then
  begin
    if PopupComponent is TTisGrid then
    begin
      vItem := aSender as TMenuItem;
      vItem.Checked := not vItem.Checked;
      vGrid := PopupComponent as TTisGrid;
      vColumn := vGrid.FindColumnByIndex(vItem.Tag);
      vValue := Dialogs.InputComboEx(
        rsGridFilterCustomExpression, rsGridFilterCustomExpressionCaption,
        vGrid.FilterOptions.GetMruFiltersAsArrayOfString(vColumn.PropertyName), True);
      if Trim(vValue) <> '' then
      begin
        vGrid.FilterOptions.AddMruFilter(vColumn.PropertyName, vValue);
        vObj := _ObjFast(['field', vColumn.PropertyName, 'value', StringToUtf8(vValue)]);
        if vItem.Checked then
          vGrid.FilterOptions.Filters.AddItem(vObj)
        else
          vGrid.FilterOptions.Filters.DeleteByValue(vObj, vGrid.FilterOptions.CaseInsensitive);
        vGrid.FilterOptions.ApplyFilters;
      end;
    end;
  end;
end;

procedure TTisGridHeaderPopupMenu.OnMenuFilterRemoveCustomClick(aSender: TObject);
var
  vGrid: TTisGrid;
  vItem: TMenuItem;
  vColumn: TTisGridColumn;
begin
  if Assigned(PopupComponent) and (PopupComponent is TBaseVirtualTree) then
  begin
    if PopupComponent is TTisGrid then
    begin
      vItem := aSender as TMenuItem;
      vItem.Checked := not vItem.Checked;
      vGrid := PopupComponent as TTisGrid;
      vColumn := vGrid.FindColumnByIndex(vItem.Tag);
      vGrid.FilterOptions.RemoveMruFilter(vColumn.PropertyName, StringToUtf8(vItem.Caption));
      OnMenuFilterClick(aSender); // should remove it from filters too
    end;
  end;
end;

constructor TTisGridHeaderPopupMenu.Create(aOwner: TComponent);
begin
  inherited Create(aOwner);
end;

procedure TTisGridHeaderPopupMenu.AssignTo(aDest: TPersistent);
begin
  if aDest is TTisGridHeaderPopupMenu then
  begin
    with TTisGridHeaderPopupMenu(aDest) do
    begin
      Options := self.Options;
    end;
  end
  else
    inherited AssignTo(aDest);
end;

procedure TTisGridHeaderPopupMenu.FillPopupMenu;

  procedure AddAutoFiltersItems(aGrid: TTisGrid; aColIdx: TColumnIndex);
  var
    vNewMenuItem: TTisGridHeaderMenuItem;
    vCount, vIndex: Integer;
    vNode: PVirtualNode;
    vData, vObj: PDocVariantData;
    vValue: RawUtf8;
    vColumn: TTisGridColumn;
    vJson, vFilters: TDocVariantData;
    vItem: PVariant;
  begin
    vCount := 0;
    vColumn := aGrid.FindColumnByIndex(aColIdx);
    vNode := aGrid.GetFirst;
    vFilters.InitFast;
    vJson.InitFast;
    while vNode <> nil do
    begin
      vData := aGrid.GetNodeAsPDocVariantData(vNode, False);
      if Assigned(vData) then
      begin
        vValue := vData^.U[vColumn.PropertyName];
        vJson.Clear;
        if vJson.InitJson(vValue, JSON_FAST_FLOAT) and (vJson.Kind = dvArray) then
        begin
          for vItem in vJson.Items do
          begin
            vValue := VariantToUtf8(vItem^);
            vIndex := vFilters.SearchItemByProp(vColumn.PropertyName, vValue, not aGrid.FilterOptions.CaseInsensitive);
            if vIndex >= 0 then
              with _Safe(vFilters.Value[vIndex])^ do
                I['count'] := I['count'] + 1
            else
              vFilters.AddItem(_ObjFast([vColumn.PropertyName, vValue, 'count', 1, 'is_array', True]));
          end;
        end
        else
        begin
          if Length(vValue) > aGrid.FilterOptions.MaxCaptionLength then
            vValue := Copy(vValue, 1, aGrid.FilterOptions.MaxCaptionLength) + '*';
          vIndex := vFilters.SearchItemByProp(vColumn.PropertyName, vValue, not aGrid.FilterOptions.CaseInsensitive);
          if vIndex >= 0 then
            with _Safe(vFilters.Value[vIndex])^ do
              I['count'] := I['count'] + 1
          else
            vFilters.AddItem(_ObjFast([vColumn.PropertyName, vValue, 'count', 1]));
        end;
      end;
      vNode := aGrid.GetNext(vNode);
    end;
    if aGrid.FilterOptions.Sort = gfsMostUsedValues then
      vFilters.SortArrayByFields(['is_array', 'count', vColumn.PropertyName], nil, nil, True)
    else
      vFilters.SortArrayByFields(['is_array', vColumn.PropertyName], nil, nil, True);
    // add non-custom filters
    for vObj in vFilters.Objects do
    begin
      vNewMenuItem := TTisGridHeaderMenuItem.Create(self);
      vNewMenuItem.Tag := aColIdx; // it will be use to locate the column by its index
      vNewMenuItem.Caption := vObj^.S[vColumn.PropertyName];
      vNewMenuItem.OnClick := @OnMenuFilterClick;
      vNewMenuItem.Checked := aGrid.FilterOptions.FilterExists(vColumn.PropertyName, vObj^.S[vColumn.PropertyName]);
      Items.Add(vNewMenuItem);
      Inc(vCount);
      if vCount >= aGrid.FilterOptions.DisplayedCount then
        Break;
    end;
    // add custom filters
    for vObj in aGrid.FilterOptions.fMruFilters.Objects do
    begin
      if vObj^.U['field'] <> vColumn.PropertyName then
        Continue;
      vNewMenuItem := TTisGridHeaderMenuItem.Create(self);
      vNewMenuItem.Tag := aColIdx; // it will be use to locate the column by its index
      vNewMenuItem.Caption := vObj^.U['value'];
      vNewMenuItem.OnClick := @OnMenuFilterClick;
      vNewMenuItem.Checked := aGrid.FilterOptions.FilterExists(vColumn.PropertyName, vObj^.U['value']);
      Items.Add(vNewMenuItem);
    end;
  end;

  procedure AddCustomExpressionsToRemoveItems(aGrid: TTisGrid; aColIdx: TColumnIndex);
  var
    vColumn: TTisGridColumn;
    vObj: PDocVariantData;
    vParentMenuItem, vNewMenuItem: TTisGridHeaderMenuItem;
  begin
    if aGrid.FilterOptions.fMruFilters.IsVoid then
      Exit;
    vParentMenuItem := nil;
    vColumn := aGrid.FindColumnByIndex(aColIdx);
    for vObj in aGrid.FilterOptions.fMruFilters.Objects do
    begin
      if vObj^.U['field'] = vColumn.PropertyName then
      begin
        if not Assigned(vParentMenuItem) then
        begin
          vParentMenuItem := TTisGridHeaderMenuItem.Create(Self);
          vParentMenuItem.Tag := NoColumn;
          vParentMenuItem.Caption := rsGridFilterCustomExpressionRemove;
          Items.Add(vParentMenuItem);
        end;
        vNewMenuItem := TTisGridHeaderMenuItem.Create(self);
        vNewMenuItem.Tag := aColIdx; // it will be use to locate the column by its index
        vNewMenuItem.Caption := vObj^.U['value'];
        vNewMenuItem.OnClick := @OnMenuFilterRemoveCustomClick;
        vParentMenuItem.Add(vNewMenuItem);
      end;
    end;
  end;

  procedure AddCustomColumns(aGrid: TTisGrid);
  var
    vColumn: TTisGridColumn;
    vParentMenuItem, vNewMenuItem: TTisGridHeaderMenuItem;
    v1: Integer;
  begin
    vParentMenuItem := nil;
    for v1 := 0 to aGrid.Header.Columns.Count-1 do
    begin
      vColumn := aGrid.Header.Columns[v1] as TTisGridColumn;
      if not vColumn.fOriginal then
      begin
        if not Assigned(vParentMenuItem) then
        begin
          vParentMenuItem := TTisGridHeaderMenuItem.Create(Self);
          vParentMenuItem.Tag := NoColumn;
          vParentMenuItem.Caption := rsGridRemoveCustomColumn;
          Items.Add(vParentMenuItem);
        end;
        vNewMenuItem := TTisGridHeaderMenuItem.Create(self);
        vNewMenuItem.Tag := vColumn.Index; // it will be use to locate the column by its index
        vNewMenuItem.Caption := vColumn.Text + ' (' + Utf8ToString(vColumn.PropertyName) + ')';
        vNewMenuItem.OnClick := @OnMenuRemoveCustomColumnClick;
        vParentMenuItem.Add(vNewMenuItem);
      end;
    end;
  end;

var
  vColPos: TColumnPosition;
  vColIdx: TColumnIndex;
  vNewMenuItem, vParentMenuItem: TMenuItem;
  vHpi: TTisGridHeaderPopupItem;
  vVisibleCounter: Cardinal;
  vVisibleItem: TMenuItem;
  vMousePos: TPoint;
  vGrid: TTisGrid;
  vColumn: TTisGridColumn;
begin
  if Assigned(PopupComponent) and (PopupComponent is TBaseVirtualTree) then
  begin
    // delete existing menu items
    RemoveAutoItems;
    with TVirtualTreeCast(PopupComponent).Header do
    begin
      // enable/disable filters
      if PopupComponent is TTisGrid then
      begin
        vGrid := PopupComponent as TTisGrid;
        if vGrid.FilterOptions.Enabled then
        begin
          // add a divisor
          vNewMenuItem := TTisGridHeaderMenuItem.Create(Self);
          vNewMenuItem.Caption := rsGridFilterEnabled;
          vNewMenuItem.OnClick := @OnMenuFilterEnableClick;
          vNewMenuItem.Checked := vGrid.FilterOptions.ShowAutoFilters;
          Items.Add(vNewMenuItem);
          // add a divisor
          vNewMenuItem := TTisGridHeaderMenuItem.Create(Self);
          vNewMenuItem.Caption := '-';
          Items.Add(vNewMenuItem);
        end;
        RecordZero(@vMousePos, TypeInfo(TPoint));
        GetCursorPos(vMousePos);
        vColIdx := Columns.ColumnFromPosition(vGrid.ScreenToClient(vMousePos));
        vColumn := vGrid.FindColumnByIndex(vColIdx);
        if Assigned(vColumn)
          and not vGrid.Data.IsVoid
          and vGrid.FilterOptions.Enabled
          and vGrid.FilterOptions.ShowAutoFilters
          and vColumn.AllowFilter
          and not vGrid.NodeOptions.ShowChildren then
        begin
          // add a item for delete filters for the column, if it has some filter(s) already
          if Pos(vGrid.FilterOptions.DownArrow, vColumn.Text) > 0 then
          begin
            vNewMenuItem := TTisGridHeaderMenuItem.Create(Self);
            vNewMenuItem.Tag := vColIdx; // it will be use to locate the column by its index
            vNewMenuItem.Caption := rsGridFilterClear;
            vNewMenuItem.OnClick := @OnMenuFilterClearClick;
            Items.Add(vNewMenuItem);
            vNewMenuItem := TTisGridHeaderMenuItem.Create(Self);
            vNewMenuItem.Caption := '-';
            Items.Add(vNewMenuItem);
          end;
          AddAutoFiltersItems(vGrid, vColIdx);
          // add a divisor
          vNewMenuItem := TTisGridHeaderMenuItem.Create(Self);
          vNewMenuItem.Caption := '-';
          Items.Add(vNewMenuItem);
          // add the custom expression menu item
          vNewMenuItem := TTisGridHeaderMenuItem.Create(Self);
          vNewMenuItem.Tag := vColIdx; // it will be use to locate the column by its index
          vNewMenuItem.Caption := rsGridFilterCustomExpression + '...';
          vNewMenuItem.OnClick := @OnMenuFilterCustomClick;
          Items.Add(vNewMenuItem);
          // add a remove custom expression menu items
          AddCustomExpressionsToRemoveItems(vGrid, vColIdx);
          // add an item to delete all filters
          vNewMenuItem := TTisGridHeaderMenuItem.Create(Self);
          vNewMenuItem.Tag := NoColumn;
          vNewMenuItem.Caption := rsGridFilterClearAll;
          vNewMenuItem.OnClick := @OnMenuFilterClearClick;
          Items.Add(vNewMenuItem);
          // add a divisor
          vNewMenuItem := TTisGridHeaderMenuItem.Create(Self);
          vNewMenuItem.Caption := '-';
          Items.Add(vNewMenuItem);
        end;
        // add subitem "show/hide columns"
        vParentMenuItem := TTisGridHeaderMenuItem.Create(Self);
        vParentMenuItem.Caption := rsGridShowHideColumns;
        Items.Add(vParentMenuItem);
        if hoShowImages in Options then
          self.Images := Images
        else
          // remove a possible reference to image list of another tree previously assigned
          self.Images := nil;
        vVisibleItem := nil;
        vVisibleCounter := 0;
        // add column menu items
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
              vNewMenuItem := TTisGridHeaderMenuItem.Create(Self);
              vNewMenuItem.Tag := vColIdx; // it will be use to locate the column by its index
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
              vParentMenuItem.Add(vNewMenuItem);
            end;
          end;
        end;
        // show all columns
        vNewMenuItem := TTisGridHeaderMenuItem.Create(Self);
        vNewMenuItem.Tag := -1;
        vNewMenuItem.Caption := rsGridShowAllColumns;
        vNewMenuItem.OnClick := @OnMenuShowAllClick;
        Items.Add(vNewMenuItem);
        // hide all columns
        vNewMenuItem := TTisGridHeaderMenuItem.Create(Self);
        vNewMenuItem.Tag := -2;
        vNewMenuItem.Caption := rsGridHideAllColumns;
        vNewMenuItem.OnClick := @OnMenuHideAllClick;
        Items.Add(vNewMenuItem);
        // restore default columns
        vNewMenuItem := TTisGridHeaderMenuItem.Create(Self);
        vNewMenuItem.Tag := -3;
        vNewMenuItem.Caption := rsGridRestoreDefaultColumns;
        vNewMenuItem.OnClick := @OnMenuRestoreClick;
        Items.Add(vNewMenuItem);
        // custom columns
        AddCustomColumns(vGrid);
        // conditionally disable menu item of last enabled column
        if (vVisibleCounter = 1) and (vVisibleItem <> nil) and not (poAllowHideAll in fOptions) then
          vVisibleItem.Enabled := False;
      end;
    end;
  end;
end;

{ TTisGridHeader }

function TTisGridHeader.GetColumnsClass: TVirtualTreeColumnsClass;
begin
  result := TTisGridColumns;
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

{ TTisGridChartMostUsedValues }

constructor TTisGridChartMostUsedValues.Create;
begin
  inherited Create;
  fCount := DefaultCount;
  fEnabled := DefaultEnabled;
end;

{ TTisGridChartOptions }

constructor TTisGridChartOptions.Create(aGrid: TTisGrid);
begin
  inherited Create;
  fGrid := aGrid;
  fMaxLabelLength := DefaultMaxLabelLength;
  fMostUsedValues := TTisGridChartMostUsedValues.Create;
end;

destructor TTisGridChartOptions.Destroy;
begin
  fMostUsedValues.Free;
  inherited Destroy;
end;

{ TTisGridFilterOptions }

class procedure TTisGridFilterOptions.InitClass;
begin
  fMruFilters.InitArray([], JSON_FAST_FLOAT);
end;

procedure TTisGridFilterOptions.ClearHeaderArrows;
var
  v1: Integer;
  vColumn: TVirtualTreeColumn;
begin
  for v1 := 0 to fGrid.Header.Columns.Count-1 do
  begin
    vColumn := fGrid.Header.Columns[v1];
    vColumn.Text := StringReplace(vColumn.Text, DownArrow, '', [rfReplaceAll]);
  end;
end;

constructor TTisGridFilterOptions.Create(aGrid: TTisGrid);
begin
  inherited Create;
  fGrid := aGrid;
  fFilters.InitArray([], JSON_FAST_FLOAT);
  fCaseInsensitive := DefaultCaseInsensitive;
  fDisplayedCount := DefaultDisplayedCount;
  fEnabled := DefaultEnabled;
  fMaxCaptionLength := DefaultMaxCaptionLength;
  fSort := DefaultSort;
end;

procedure TTisGridFilterOptions.AssignTo(aDest: TPersistent);
begin
  if aDest is TTisGridFilterOptions then
  begin
    with TTisGridFilterOptions(aDest) do
    begin
      fFilters.Reset;
      if not self.fFilters.IsVoid then
        fFilters.InitCopy(Variant(self.fFilters), JSON_[mDefault]);
      CaseInsensitive := self.CaseInsensitive;
      DisplayedCount := self.DisplayedCount;
      Enabled := self.Enabled;
      Sort := self.Sort;
    end;
  end
  else
    inherited AssignTo(aDest);
end;

function TTisGridFilterOptions.AddFilter(const aFieldName, aValue: RawUtf8; aCustom: Boolean): Variant;
begin
  result := _ObjFast(['field', aFieldName, 'value', aValue]);
  fFilters.AddItem(result);
  if aCustom then
    fGrid.FilterOptions.AddMruFilter(aFieldName, aValue);
  fGrid.FilterOptions.ApplyFilters;
end;

function TTisGridFilterOptions.FilterExists(const aFieldName: RawUtf8;
  const aValue: string): Boolean;
var
  vObj: PDocVariantData;
  vTest: TDocVariantData;
begin
  result := False;
  vTest.InitFast(dvObject);
  vTest.U['field'] := aFieldName;
  vTest.S['value'] := aValue;
  for vObj in fFilters.Objects do
  begin
    if vObj^.Equals(vTest) then
    begin
      result := True;
      break;
    end;
  end;
end;

procedure TTisGridFilterOptions.ApplyFilters;

  procedure SetNodeVisible(aNode: PVirtualNode; aInclude: Boolean);
  begin
    if not fGrid.DoNodeFiltering(aNode) then
    begin
      if aInclude then
        Include(aNode^.States, vsVisible)
      else
        Exclude(aNode^.States, vsVisible);
    end;
  end;

  function IsMatching(const aPattern, aText: RawUtf8): Boolean;
  var
    vJson: TDocVariantData;
    vItem: PVariant;
  begin
    result := False;
    if vJson.InitJson(aText, JSON_FAST_FLOAT) and (vJson.Kind = dvArray) then
    begin
      for vItem in vJson.Items do
      begin
        if IsMatch(aPattern, VariantToUtf8(vItem^), fGrid.FilterOptions.CaseInsensitive) then
        begin
          result := True;
          Break;
        end;
      end;
    end
    else if IsMatch(aPattern, aText, fGrid.FilterOptions.CaseInsensitive) then
      result := True;
  end;

var
  vData, vObj: PDocVariantData;
  vNode: PVirtualNode;
  vColumn: TTisGridColumn;
  vPropertyName: RawUtf8;
begin
  ClearHeaderArrows;
  vNode := fGrid.GetFirst;
  while vNode <> nil do
  begin
    vData := fGrid.GetNodeAsPDocVariantData(vNode, False);
    if Assigned(vData) then
    begin
      if fFilters.Count > 0 then
      begin
        SetNodeVisible(vNode, False);
        vPropertyName := '';
        for vObj in fFilters.Objects do
        begin
          vColumn := fGrid.FindColumnByPropertyName(vObj^.U['field']);
          if vColumn = nil then
            Continue;
          if vPropertyName = '' then
            vPropertyName := vColumn.PropertyName;
          if vPropertyName = vColumn.PropertyName then
          begin
            if IsMatching(vObj^.U['value'], vData^.U[vObj^.U['field']]) then
              SetNodeVisible(vNode, True)
          end
          else
          begin
            if vsVisible in vNode^.States then
            begin
              if IsMatching(vObj^.U['value'], vData^.U[vObj^.U['field']]) then
                SetNodeVisible(vNode, True)
              else
                SetNodeVisible(vNode, False);
            end;
          end;
          // add an DownArrow in header column text, if there are filters for this column
          if Pos(DownArrow, vColumn.Text) = 0 then
          begin
            vColumn.Text := vColumn.Text + DownArrow;
            fShowAutoFilters := True;
          end;
        end;
      end
      else
        SetNodeVisible(vNode, True);
    end;
    vNode := fGrid.GetNext(vNode);
  end;
  fGrid.Invalidate;
  if (fGrid.FocusedNode = nil) or not (vsVisible in fGrid.FocusedNode^.States) then
  begin
    fGrid.ClearSelection;
    fGrid.FocusedNode := fGrid.GetFirstVisible;
    fGrid.Selected[fGrid.FocusedNode] := True;
  end;
  //fGrid.ScrollIntoView(fGrid.FocusedNode, False);
end;

procedure TTisGridFilterOptions.ClearFilters;
begin
  fFilters.Clear;
  ApplyFilters;
end;

procedure TTisGridFilterOptions.AddMruFilter(const aFieldName, aValue: RawUtf8);
var
  vObj: PDocVariantData;
  vNewObj: Variant;
  vStr: string;
begin
  vStr := Utf8ToString(aValue);
  for vObj in fMruFilters.Objects do
  begin
    if vObj^.U['field'] = aFieldName then
    begin
      if (fCaseInsensitive and SameStr(vObj^.S['value'], vStr))
        or (not fCaseInsensitive and SameText(vObj^.S['value'], vStr)) then
        Exit;
    end;
  end;
  vNewObj := _ObjFast(['field', aFieldName, 'value', aValue]);
  fMruFilters.AddItem(vNewObj);
end;

procedure TTisGridFilterOptions.RemoveMruFilter(const aFieldName, aValue: RawUtf8);
begin
  fMruFilters.DeleteByValue(_ObjFast(['field', aFieldName, 'value', aValue]), fCaseInsensitive);
end;

function TTisGridFilterOptions.GetMruFilters: PDocVariantData;
begin
  result := @fMruFilters;
end;

function TTisGridFilterOptions.GetMruFiltersAsArrayOfString(
  const aFieldName: RawUtf8): TStringArray;
var
  vObj: PDocVariantData;
begin
  SetLength(result, 0);
  for vObj in fMruFilters.Objects do
  begin
    if vObj^.U['field'] = aFieldName then
    begin
      SetLength(result, Length(result) + 1);
      result[Length(result)-1] := vObj^.S['value'];
    end;
  end;
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
  fMultiEdit := DefaultMultiEdit;
  fMultiLine := DefaultMultiLine;
  fMultiLineHeight := DefaultMultiLineHeight;
  fShowChildren := DefaultShowChildren;
end;

procedure TTisNodeOptions.AssignTo(aDest: TPersistent);
begin
  if aDest is TTisNodeOptions then
  begin
    with TTisNodeOptions(aDest) do
    begin
      MultiEdit := self.MultiEdit;
      MultiLine := self.MultiLine;
      MultiLineHeight := self.MultiLineHeight;
      ShowChildren := self.ShowChildren;
    end;
  end
  else
    inherited AssignTo(aDest);
end;

{ TTisNodeColumnCache }

procedure TTisNodeColumnCache.Init;
begin
  RecordZero(@self, TypeInfo(TTisNodeColumnCache));
end;

procedure TTisNodeColumnCache.Clear;
begin
  FreeAndNil(Image);
  Init;
end;

{ TTisNodeCache }

procedure TTisNodeCache.Init;
var
  v1: Integer;
begin
  for v1 := 0 to High(Columns) do
    Columns[v1].Clear;
  RecordZero(@self, TypeInfo(TTisNodeCache));
end;

procedure TTisNodeCache.Clear;
begin
  Init;
end;

function TTisNodeCache.GetColumnCache(aColumn: Integer): PTisNodeColumnCache;
var
  v1: Integer;
begin
  result := nil;
  for v1 := 0 to High(Columns) do
    if Columns[v1].Column = aColumn then
    begin
      result := @Columns[v1];
      break;
    end;
end;

procedure TTisNodeCache.SetColumnCache(aColumn: Integer;
  const aValue: TTisNodeColumnCache);
var
  vCache: PTisNodeColumnCache;
begin
  vCache := GetColumnCache(aValue.Column);
  if not Assigned(vCache) then
  begin
    SetLength(Columns, Length(Columns)+1);
    vCache := @Columns[High(Columns)];
  end
  else
    vCache^.Clear;
  RecordCopy(vCache^, aValue, TypeInfo(TTisNodeColumnCache));
end;

{ TTisNodeAdapter }

procedure TTisNodeAdapter.Init(aGrid: TTisGrid);
begin
  Grid := aGrid;
  Offset := aGrid.AllocateInternalDataArea(SizeOf(TTisNodeData));
end;

function TTisNodeAdapter.GetDataPointer(aNode: PVirtualNode): Pointer;
begin
  if (aNode = nil) or (Offset <= 0) then
    result := nil
  else
    result := PByte(aNode) + Offset;
end;

function TTisNodeAdapter.GetData(aNode: PVirtualNode): PTisNodeData;
begin
  result := GetDataPointer(aNode);
end;

function TTisNodeAdapter.GetDataAsString(aNode: PVirtualNode): string;
begin
  result := Utf8ToString(GetData(aNode)^.Data^.ToJson);
end;

function TTisNodeAdapter.GetName(aNode: PVirtualNode): string;
var
  vNodeData: PTisNodeData;
begin
  result := '';
  vNodeData := GetData(aNode);
  if vNodeData^.IsChild then
  begin
    if Assigned(vNodeData^.Name) then
      result := Utf8ToString(vNodeData^.Name^)
    else
      result := aNode^.Index.ToString;
  end
  else
  begin
    if Length(vNodeData^.Data^.GetNames) = 1 then
      result := Utf8ToString(vNodeData^.Data^.Names[0])
    else
    begin
      if vNodeData^.Data^.Kind = dvObject then
        result := '{}'
      else
        result := '[]';
    end;
  end;
end;

function TTisNodeAdapter.GetValue(aNode: PVirtualNode;
  aColumn: TColumnIndex): PVariant;
var
  vNodeData: PTisNodeData;
  vCol: TTisGridColumn;
begin
  result := nil;
  vNodeData := GetData(aNode);
  if vNodeData^.IsChild then
  begin
    // ignoring aColumn, if it is a child node
    result := vNodeData^.Value
  end
  else
  begin
    vCol := Grid.FindColumnByIndex(aColumn);
    if Assigned(vCol) then
      vNodeData^.Data^.GetAsPVariant(vCol.PropertyName, result);
  end;
end;

function TTisNodeAdapter.GetValueAsSimple(aNode: PVirtualNode;
  aColumn: TColumnIndex): PVariant;
var
  vNodeData: PTisNodeData;
  vData: PDocVariantData;
  vCol: TTisGridColumn;
begin
  result := nil;
  vNodeData := GetData(aNode);
  if vNodeData^.IsChild then
  begin
    // ignoring aColumn, if it is a child node
    if not _Safe(vNodeData^.Value^, vData) then
      result := vNodeData^.Value
  end
  else
  begin
    vCol := Grid.FindColumnByIndex(aColumn);
    if Assigned(vCol) and vNodeData^.Data^.GetAsPVariant(vCol.PropertyName, result) then
    begin
      if _Safe(result^, vData) then
        result := nil;
    end;
  end;
end;

function TTisNodeAdapter.GetValueAsSimpleString(aNode: PVirtualNode;
  aColumn: TColumnIndex; const aDefault: string): string;

  function LJsonVarToStr(const aValue: Variant): string;
  begin
    if VarIsBool(aValue) then
      result := Utf8ToString(LowerCaseU(aValue))
    else
      result := Utf8ToString(VariantToUtf8(aValue));
  end;

var
  vValue: PVariant;
  vCol: TTisGridColumn;
begin
  result := aDefault;
  vValue := GetValueAsSimple(aNode, aColumn);
  if Assigned(vValue) then
  begin
    vCol := Grid.FindColumnByIndex(aColumn);
    if VarIsNull(vValue^) and Assigned(vCol) and (vCol.DataType <> cdtJson) then
      result := ''
    else
      result := LJsonVarToStr(vValue^);
  end;
end;

function TTisNodeAdapter.GetValueAsVariant(aNode: PVirtualNode;
  aColumn: TColumnIndex): Variant;
var
  vPVar: PVariant;
  vCol: TTisGridColumn;
begin
  result := NULL;
  vPVar := Grid.fNodeAdapter.GetValue(aNode, aColumn);
  if not Assigned(vPVar) then
  begin
    vCol := Grid.FindColumnByIndex(aColumn);
    // try to get the value
    if not Grid.DoCalcAttributes(aNode, vCol, result) then
    begin
      // if column data type is HTML, it will receiv the original row data by default
      if vCol.DataType = cdtHtml then
        result := Variant(Grid.GetNodeAsPDocVariantData(aNode)^);
    end;
  end
  else
    result := vPVar^;
end;

procedure TTisNodeAdapter.SetValue(aNode: PVirtualNode; const aValue: Variant;
  aColumn: TColumnIndex; aValueIsString: Boolean);
var
  vNodeData: PTisNodeData;
  vData: PDocVariantData;
  vCol: TTisGridColumn;
  vValue: Variant;
begin
  vCol := Grid.FindColumnByIndex(aColumn);
  if not Assigned(vCol) or vCol.ReadOnly then
    exit;
  if (vCol.DataType = cdtJson) and (not (VarIsNull(aValue) or aValueIsString)) then
    TextToVariant(aValue, True, vValue)
  else
    vValue := aValue;
  vNodeData := GetData(aNode);
  if vNodeData^.IsChild then
  begin
    if aColumn = CHILD_COLUMN_VALUE_INDEX then
      if VarIsNull(vValue) then
      begin
        if not vCol.Required then
          vNodeData^.Value^ := NULL;
      end
      else
        vNodeData^.Value^ := vValue;
  end
  else
  begin
    vData := vNodeData^.Data;
    // checking if "object.name" exists, otherwise it could raise an exception
    if Assigned(vData^.GetVarData(vCol.PropertyName)) then
    begin
      if VarIsNull(vValue) then
      begin
        if not vCol.Required then
          vData^.Value[vCol.PropertyName] := NULL;
      end
      else
        case vCol.DataType of
          cdtString, cdtMemo:
            vData^.S[vCol.PropertyName] := VarToStr(vValue);
          cdtDate, cdtTime, cdtDateTime:
            begin
              with vCol.DataTypeOptions.DateTimeOptions do
                if SaveAsUtc then
                  vData^.U[vCol.PropertyName] := DateTimeToIso8601Text(LocalToUtc(vValue))
                else
                  vData^.U[vCol.PropertyName] := DateTimeToIso8601Text(vValue);
            end;
          cdtInteger:
            vData^.I[vCol.PropertyName] := vValue;
          cdtFloat:
            vData^.D[vCol.PropertyName] := vValue;
          cdtBoolean:
            vData^.B[vCol.PropertyName] := vValue;
        else
          vData^.Value[vCol.PropertyName] := vValue;
        end;
    end;
  end;
end;

function TTisNodeAdapter.IsChild(aNode: PVirtualNode): Boolean;
begin
  if Assigned(aNode) then
    result := GetData(aNode)^.IsChild
  else
    result := False;
end;

function TTisNodeAdapter.GetCache(aNode: PVirtualNode): PTisNodeCache;
begin
  result := @GetData(aNode)^.Cache;
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
  vNode: PVirtualNode;
begin
  vNode := FocusedNode;
  if vNode <> nil then
    result := GetNodeAsPDocVariantData(vNode, False)
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
    DynArrayFakeLength(@vKeys, 1);
    DynArrayFakeLength(@vParents, 1);
    StringDynArrayToRawUtf8DynArray(fKeyFieldsList, vKeys);
    StringDynArrayToRawUtf8DynArray(fParentKeyFieldsList, vParents);
    if high(vParents) > high(vKeys) then
      raise ETisGrid.Create('ParentKeyFieldsList should not have more fields than KeyFieldsList.');
  end;
  fParentKeyFieldsList := StrSplit(aValue, ';', True);
end;

function TTisGrid.GetSettings: Variant;
var
  v1: integer;
  vCol: TTisGridColumn;
  vRes, vCols: PDocVariantData;
begin
  TDocVariant.NewFast(result);
  vRes := @result;
  vRes^.I['sortcolumn'] := Header.SortColumn;
  vRes^.I['sortdirection'] := ord(Header.SortDirection);
  vCols := vRes^.A_['columns'];
  for v1 := 0 to Header.Columns.Count-1 do
  begin
    vCol := Header.Columns[v1] as TTisGridColumn;
    vCols^.AddItem(
      _ObjFast([
        'propertyname', vCol.PropertyName,
        'text', StringToUtf8(vCol.Text),
        'position', vCol.Position,
        'width', vCol.Width,
        'visible', (coVisible in vCol.Options),
        'chartsettings', vCol.ChartSettings
      ])
    );
  end;
  if not FilterOptions.Filters.IsVoid then
    vRes^.A_['filters']^.InitCopy(Variant(FilterOptions.Filters), JSON_[mDefault]);
  if not FilterOptions.fMruFilters.IsVoid then
    vRes^.A_['mrufilters']^.InitCopy(Variant(FilterOptions.fMruFilters), JSON_[mDefault]);
end;

procedure TTisGrid.SetSettings(const aValue: Variant);
var
  vObj, vSettings: PDocVariantData;
  vIntValue: Integer;
  vPropName: RawUtf8;
  vCol: TTisGridColumn;
  vLock: TLightLock;
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
        vCol.ChartSettings := vObj^.U['chartsettings'];
      end;
    end;
    if not vSettings^.A_['filters']^.IsVoid then
    begin
      FilterOptions.Filters.Reset;
      FilterOptions.Filters.InitCopy(Variant(vSettings^.A_['filters']^), JSON_[mDefault]);
      FilterOptions.ApplyFilters;
    end;
    if not vSettings^.A_['mrufilters']^.IsVoid then
    begin
      vLock.Init;
      try
        vLock.Lock;
        FilterOptions.fMruFilters.Reset;
        FilterOptions.fMruFilters.InitCopy(Variant(vSettings^.A_['mrufilters']^), JSON_[mDefault]);
      finally
        vLock.UnLock;
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
  DoBeforeDataChange(@aValue, vAborted);
  if not vAborted then
  begin
    LoadData(@aValue);
    UpdateSelectedAndTotalLabel;
    DoAfterDataChange;
  end;
end;

function TTisGrid.GetMetaData: RawUtf8;
var
  v1: Integer;
  vDoc: TDocVariantData;
  vCol: TTisGridColumn;
  vColumns: PDocVariantData;
  vColAdapter: TTisColumnDataTypeAdapter;
begin
  vDoc.InitFast;
  vColumns := vDoc.A_['columns'];
  for v1 := 0 to Header.Columns.Count -1 do
  begin
    vCol := Header.Columns[v1] as TTisGridColumn;
    vColumns^.AddItem(
      _ObjFast([
        'propertyname', vCol.PropertyName,
        'datatype', vColAdapter.EnumToRawUtf8(vCol.DataType),
        'required', vCol.Required,
        'readonly', vCol.ReadOnly,
        'width',vCol.Width
      ])
    );
  end;
  result := vDoc.ToJson;
  DoGetMetaData(result);
end;

procedure TTisGrid.SetMetaData(const aValue: RawUtf8);
var
  vDoc: TDocVariantData;
  vCol: TTisGridColumn;
  vColAdapter: TTisColumnDataTypeAdapter;
  vObj: PDocVariantData;
begin
  vDoc.InitJson(aValue, JSON_FAST_FLOAT);
  if vDoc.IsVoid then
    exit;
  for vObj in vDoc.A_['columns']^.Objects do
  begin
    vCol := FindColumnByPropertyName(vObj^.U['propertyname']);
    if vCol <> nil then
    begin
      vCol.DataType := vColAdapter.RawUtf8ToEnum(vObj^.U['datatype']);
      vCol.Required := vObj^.B['required'];
      vCol.ReadOnly := vObj^.B['readonly'];
      if vObj^.GetValueIndex('width') >= 0 then
        vCol.Width := vObj^.I['width'];
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
  SetFocusedRowNoClearSelection(aValue,{EnsureScrollIntoView=}True);
end;

procedure TTisGrid.SetOnCutToClipboard(aValue: TNotifyEvent);
begin
  fOnCutToClipboard := aValue;
end;

function TTisGrid.GetTreeOptions: TTisStringTreeOptions;
begin
  result := TTisStringTreeOptions(inherited TreeOptions);
end;

procedure TTisGrid.SetTreeOptions(const aValue: TTisStringTreeOptions);
begin
  TreeOptions.Assign(aValue);
end;

function TTisGrid.GetSelectedRows: TDocVariantData;
var
  vNode: PVirtualNode;
  vDoc: PDocVariantData;
begin
  vNode := GetFirstSelected;
  result.InitArray([], []);
  while vNode <> nil do
  begin
    vDoc := GetNodeAsPDocVariantData(vNode);
    if Assigned(vDoc) then
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

function TTisGrid.SelectedObjects: PDocVariantDataDynArray;
var
  vCount: integer;
  vNode: PVirtualNode;
begin
  vNode := GetFirstSelected;
  vCount := 0;
  result := nil;
  while vNode <> nil do
  begin
    PtrArrayAdd(result, GetNodeAsPDocVariantData(vNode), vCount);
    vNode := GetNextSelected(vNode, True);
  end;
  SetLength(result, vCount);
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
  vMsg: TLMessage;
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
        vMsg.msg := LM_CHAR;
        vMsg.wParam := ord(vBuffer[0]);
        vMsg.lParam := 0;
        if (Message.CharCode <> VK_F2) and Assigned(EditLink) then
          EditLink.ProcessMessage(vMsg);
      end;
    end
    else
      inherited WMKeyDown(Message);
  end;
end;

procedure TTisGrid.SetZebraLightness(aValue: Byte);
begin
  if fZebraLightness = aValue then
    exit;
  fZebraLightness := aValue;
  Invalidate;
end;

procedure TTisGrid.Loaded;
var
  v1: Integer;
  vCol: TTisGridColumn;
begin
  inherited Loaded;
  fDefaultSettings := GetSettings;
  for v1 := 0 to Header.Columns.Count-1 do
  begin
    vCol := Header.Columns[v1] as TTisGridColumn;
    vCol.fOriginal := True; // set all original columns
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
  vValue: PVariant;
begin
  inherited DoNewText(aNode, aColumn, aText);
  if Assigned(aNode) then
    with fNodeAdapter do
    begin
      vValue := GetValueAsSimple(aNode);
      SetValue(aNode, StringToUtf8(aText), aColumn, Assigned(vValue) and VarIsStr(vValue^));
    end;
end;

procedure TTisGrid.DoGetText(aNode: PVirtualNode; aColumn: TColumnIndex;
  aTextType: TVSTTextType; var aText: string);
var
  vNodeData: PTisNodeData;
  vCol: TTisGridColumn;
  vDateTime: TDateTime;
  vVar: Variant;
begin
  Assert(Assigned(aNode), 'DoGetText: aNode must not be nil.');
  if fNodeOptions.ShowChildren then
  begin
    case aColumn of
      CHILD_COLUMN_NAME_INDEX:
        aText := fNodeAdapter.GetName(aNode);
      CHILD_COLUMN_VALUE_INDEX:
        aText := fNodeAdapter.GetValueAsSimpleString(aNode, aColumn);
    end;
  end
  else
  begin
    vNodeData := fNodeAdapter.GetData(aNode);
    vCol := FindColumnByIndex(aColumn);
    if Assigned(vNodeData^.Data) then
    begin
      if Assigned(vCol) then
      begin
        // it will be print an image and the original text should be hide
        if vCol.DataType = cdtHtml then
          exit;
        aText := fNodeAdapter.GetValueAsSimpleString(aNode, aColumn);
        // try to get the value
        if (aText = '') and DoCalcAttributes(aNode, vCol, vVar) then
          aText := VarToStr(vVar);
        if (aText <> '') and (vCol.DataType in [cdtDate, cdtTime, cdtDateTime]) then
        begin
          vDateTime := Iso8601ToDateTime(aText);
          with vCol.DataTypeOptions.DateTimeOptions do
          begin
            if SaveAsUtc and ShowAsLocal then
              vDateTime := UtcToLocal(vDateTime);
            if CustomFormat <> '' then
              aText := FormatDateTime(CustomFormat, vDateTime)
            else
              case vCol.DataType of
                cdtDate:
                  if ShowAsDateTime then
                    aText := DateToStr(vDateTime)
                  else
                    aText := DateToIso8601(vDateTime, True);
                cdtTime:
                  if ShowAsDateTime then
                    aText := TimeToStr(vDateTime)
                  else
                    aText := TimeToIso8601(vDateTime, True);
                cdtDateTime:
                  if ShowAsDateTime then
                    aText := DateTimeToStr(vDateTime)
                  else
                    aText := DateTimeToIso8601(vDateTime, True);
              end;
          end;
        end
      end
      else if DefaultText <> '' then
        aText := vNodeData^.Data^.S[DefaultText];
      if aText = '' then
        aText := DefaultText;
      // firing our customized OnGetText
      if Assigned(fOnGetText) then
        fOnGetText(self, aNode, vNodeData^.Data^, aColumn, aTextType, aText);
      // obfuscating after processing OnGetText event
      if Assigned(vCol) and (vCol.DataType = cdtPassword) then
        aText := StrRepeatChar('*', Length(aText));
    end
    else
      aText := 'uninitialized';
  end;
end;

procedure TTisGrid.DoInitNode(aParentNode, aNode: PVirtualNode;
  var aInitStates: TVirtualNodeInitStates);

  /// just a single place to setup a default configuration for all nodes
  procedure _SetNodeDefaults(aNode: PVirtualNode);
  begin
    if Assigned(aNode) then
    begin
      aNode^.CheckType := ctCheckBox;
      if fNodeOptions.MultiLine then
        Include(aNode^.States, vsMultiline);
    end;
  end;

  /// just a single place to setup a default configuration for all nodes, when in Tree Mode
  procedure _SetNodeDefaultsForTreeMode(aNode: PVirtualNode);
  begin
    if Assigned(aNode) then
    begin
      Include(aNode^.States, vsInitialized); // for do not call DoInitNode again
      Include(aNode^.States, vsExpanded);    // it's better show all at first
    end;
  end;

  /// it creates and returns a new child node for aParent
  function _CreateChild(aParent: PVirtualNode; aName: PRawUtf8; aValue: PVariant;
    aData: PDocVariantData): PVirtualNode;
  begin
    result := inherited AddChild(aParent);
    _SetNodeDefaults(result);
    _SetNodeDefaultsForTreeMode(result);
    with fNodeAdapter.GetData(result)^ do
    begin
      IsChild := True;
      Name := aName;
      Value := aValue;
      Data := aData;
    end;
  end;

  /// just a single place to create all children recursively
  // - we do not need to override/implement InitChildren or DoInitChildren methods
  procedure _CreateChildrenFor(aParent: PVirtualNode);
  var
    vDoc, vData, vTempData: PDocVariantData;
    vPair: TDocVariantFields;
  begin
    _SetNodeDefaultsForTreeMode(aParent);
    vData := fNodeAdapter.GetData(aParent)^.Data;
    if not fNodeAdapter.IsChild(aParent) then
    begin
      // need to check if the first "father" is a named object
      //   then, get only its fields instead of the whole object
      //   otherwise, the first node will be "{}"
      if Length(vData^.GetNames) = 1 then
        vDoc := _Safe(vData^.Values[0])
      else
        vDoc := vData;
    end
    else
      vDoc := vData;
    for vPair in vDoc^ do
    begin
      // is it be a valid array or object for the next child?
      if _Safe(vPair.Value^, vTempData) then
        _CreateChildrenFor(_CreateChild(aParent, vPair.Name, vPair.Value, vTempData))
      else
        _CreateChild(aParent, vPair.Name, vPair.Value, vDoc);
    end;
  end;

var
  vNodeData: PTisNodeData;
  vData: PDocVariantData;
begin
  if not fData.IsVoid then
  begin
    _SetNodeDefaults(aNode);
    vData := _Safe(fData.Values[aNode^.Index]);
    vNodeData := fNodeAdapter.GetDataPointer(aNode);
    with vNodeData^ do
    begin
      IsChild := aNode^.Parent <> RootNode;
      Name := nil;
      Value := PVariant(vData);
      Data := vData;
    end;
    _SetNodeDefaults(aNode);
    if fNodeOptions.ShowChildren then
      _CreateChildrenFor(aNode);
  end;
  inherited DoInitNode(aParentNode, aNode, aInitStates);
end;

procedure TTisGrid.DoFreeNode(aNode: PVirtualNode);
begin
  with fNodeAdapter.GetData(aNode)^ do
  begin
    Name := nil;
    Value := nil;
    Data := nil;
    Cache.Clear;
  end;
  inherited DoFreeNode(aNode);
end;

procedure TTisGrid.DoMeasureItem(aTargetCanvas: TCanvas; aNode: PVirtualNode;
  var aNodeHeight: Integer);
var
  v1, vCellHeight, vMaxHeight: Integer;
begin
  if Assigned(OnMeasureItem) then
    inherited DoMeasureItem(aTargetCanvas, aNode, aNodeHeight)
  else
  begin
    vMaxHeight := DefaultNodeHeight;
    if MultiLine[aNode] then
    begin
      for v1 := 0 to Header.Columns.Count -1 do
      begin
        if (coVisible in Header.Columns[v1].Options) then
        begin
          vCellHeight := inherited ComputeNodeHeight(aTargetCanvas, aNode, v1);
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
    GetNodeAsPDocVariantData(aNode1, False), GetNodeAsPDocVariantData(aNode2, False));
end;

procedure TTisGrid.DoCanEdit(aNode: PVirtualNode; aColumn: TColumnIndex;
  var aAllowed: Boolean);
begin
  if fNodeOptions.ShowChildren then
    aAllowed := aColumn = 1
  else
    inherited DoCanEdit(aNode, aColumn, aAllowed);
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

procedure TTisGrid.DoBeforeCellPaint(aCanvas: TCanvas; aNode: PVirtualNode;
  aColumn: TColumnIndex; aCellPaintMode: TVTCellPaintMode; aCellRect: TRect;
  var aContentRect: TRect);

  procedure PrintHtmlAsImage(aGrid: TTisGrid; const aHtml: string; aTargetCanvas: TCanvas;
    const aCellRect: TRect; var aContentRect: TRect);

    procedure HtmlToBitmap(const aHtml: string; aBitmap: TBitmap);
    var
      vHtml: TTisHtmlViewer;
    begin
      vHtml := TTisHtmlViewer.Create(self);
      try
        vHtml.Width := aBitmap.Width;
        vHtml.Height := aBitmap.Height;
        {$ifdef FRAMEVIEWER_ENABLED}
        vHtml.LoadFromString(aHtml);
        aBitmap.Canvas.Brush.Color := vHtml.DefBackground;
        vHtml.PaintHtml(aBitmap.Canvas, Rect(
          0, 0, aBitmap.Width, aBitmap.Height));
        {$endif FRAMEVIEWER_ENABLED}
      finally
        vHtml.Free;
      end;
    end;

  var
    vImage: TBitmap;
    vCache: PTisNodeCache;
    vColCache: PTisNodeColumnCache;
    vNewColCache: TTisNodeColumnCache;
    vHash: Cardinal;
  begin
    vHash := Hash32(aHtml + aContentRect.Width.ToString + aContentRect.Height.ToString);
    vCache := fNodeAdapter.GetCache(aNode);
    vColCache := vCache^.GetColumnCache(aColumn);
    if Assigned(vColCache) and (vColCache^.Hash = vHash) and Assigned(vColCache^.Image) then
      vImage := vColCache^.Image
    else
    begin
      vImage := TBitmap.Create;
      try
        vImage.Width := aCellRect.Width-10;
        vImage.Height := aCellRect.Height-4;
        HtmlToBitmap(aHtml, vImage);
        vNewColCache.Init;
        vNewColCache.Column := aColumn;
        vNewColCache.Image := vImage;
        vNewColCache.Hash := vHash;
        vCache^.SetColumnCache(aColumn, vNewColCache);
      except
        vImage.Free;
        raise;
      end;
    end;
    if Assigned(vImage) then
      aTargetCanvas.Draw(aContentRect.Left+2, aContentRect.Top+2, vImage);
  end;

  procedure CacheNode;
  var
    vHash: Cardinal;
    vCache: PTisNodeCache;
    vColumn: TTisGridColumn;
  begin
    vHash := Hash32(Text[aNode, aColumn] + aNode^.NodeHeight.ToString);
    vCache := fNodeAdapter.GetCache(aNode);
    if Assigned(vCache) and (vCache^.Hash <> vHash) then
    begin
      vCache^.Height := inherited ComputeNodeHeight(aCanvas, aNode, aColumn);
      vCache^.Hash := vHash;
    end;
    vColumn := FindColumnByIndex(aColumn);
    if vColumn.DataType = cdtHtml then
      PrintHtmlAsImage(self, DoBeforeHtmlRendering(aNode, vColumn), aCanvas, aCellRect, aContentRect);
  end;

begin
  //Pour affichage lignes multiselect en gris clair avec cellule focused en bleu
  if (aCellPaintMode = cpmPaint) and (toMultiSelect in TreeOptions.SelectionOptions) and
    (vsSelected in aNode^.States) then
  begin
    if not Focused or (aColumn <> FocusedColumn) or (aNode <> FocusedNode) then
    begin
      aCanvas.Brush.Color := clLtGray;
      aCanvas.FillRect(aCellRect);
    end
    else
    if (aColumn = FocusedColumn) and (aNode = FocusedNode) and Focused then
    begin
      aCanvas.Brush.Color := Colors.SelectionRectangleBlendColor;
      aCanvas.FillRect(aCellRect);
    end;
  end
  else
  if (aCellPaintMode = cpmPaint) and not (toMultiSelect in TreeOptions.SelectionOptions) and
     (aNode = FocusedNode) then
  begin
    if (aColumn <> FocusedColumn) then
    begin
      aCanvas.Brush.Color := clLtGray;
      aCanvas.FillRect(aCellRect);
    end
    else
    begin
      aCanvas.Brush.Color := Colors.SelectionRectangleBlendColor;
      aCanvas.FillRect(aCellRect);
    end;
  end;
  if (aCellPaintMode = cpmPaint) and Header.Columns.IsValidColumn(aColumn) then
    CacheNode;
  inherited DoBeforeCellPaint(aCanvas, aNode, aColumn, aCellPaintMode, aCellRect, aContentRect);
end;

procedure TTisGrid.DoTextDrawing(var aPaintInfo: TVTPaintInfo;
  const aText: string; aCellRect: TRect; aDrawFormat: cardinal);
var
  vHue, vSaturation, vLightness: Byte;
begin
  // to display multiselect rows in light gray with focused cell in blue
  if (Focused or not (toHideSelection in TreeOptions.PaintOptions) or (toPopupMode in TreeOptions.PaintOptions))  and
    (vsSelected in aPaintInfo.Node^.States) and
    (aPaintInfo.Node = FocusedNode) and
    (aPaintInfo.Column = FocusedColumn) then
    aPaintInfo.Canvas.Font.Color := Colors.SelectionTextColor
  else
  begin
    ColorToHLS(aPaintInfo.Canvas.Brush.Color, vHue, vLightness, vSaturation);
    if vLightness > 128 then
      aPaintInfo.Canvas.Font.Color := clBlack
    else
      aPaintInfo.Canvas.Font.Color := clWhite;
  end;
  inherited DoTextDrawing(aPaintInfo, aText, aCellRect, aDrawFormat);
end;

procedure TTisGrid.DoBeforeItemErase(aCanvas: TCanvas; aNode: PVirtualNode;
  const aItemRect: TRect; var aColor: TColor; var aEraseAction: TItemEraseAction);
var
  vHue, vSaturation, vLightness: Byte;
begin
  if fZebraPaint and (aNode <> nil) and Odd(aNode^.Index) then
  begin
    ColorToHLS(aColor, vHue, vLightness, vSaturation);
    if vLightness < fZebraLightness then
      aColor := HLStoColor(vHue, Byte((UInt16(vLightness) - UInt16(fZebraLightness)) and High(UInt8)), vSaturation)
    else
      aColor := HLStoColor(vHue, Byte((UInt16(vLightness) + UInt16(fZebraLightness)) and High(UInt8)), vSaturation);
    aEraseAction := eaColor;
  end;
  inherited DoBeforeItemErase(aCanvas, aNode, aItemRect, aColor, aEraseAction);
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
  v1: Integer;
begin
  if (aMode in [lapAutoAdjustForDPI]) then
  begin
    Header.MinHeight := max(18,round(Header.MinHeight * aXProportion)+1);
    Header.MaxHeight := max(18,round(Header.MaxHeight * aXProportion)+1);
    Header.DefaultHeight := max(18,round(Header.DefaultHeight * aXProportion)+1);
    Header.Height := max(18,round(Header.Height * aXProportion)+1);
    for v1 := 0 to header.Columns.Count-1 do
    begin
      header.Columns[v1].MaxWidth:=round(header.Columns[v1].MaxWidth * aXProportion);
      header.Columns[v1].Width:=round(header.Columns[v1].Width * aXProportion);
      header.Columns[v1].MinWidth:=round(header.Columns[v1].MinWidth * aXProportion);
    end;
  end;
end;

procedure TTisGrid.DoChange(aNode: PVirtualNode);
begin
  inherited DoChange(aNode);
  UpdateSelectedAndTotalLabel;
end;

procedure TTisGrid.DoContextPopup(aMousePos: TPoint; var aHandled: Boolean);
begin
  // on MacOS and Linux DoContextPopup is called even when user clicks on Header
  // - this will prevent not call PopupMenu from grid, instead using Header.PopupMenu
  if Header.InHeader(aMousePos) then
  begin
    aHandled := True;
    exit;
  end;
  CleanPopupMenu;
  FillPopupMenu;
  inherited DoContextPopup(aMousePos, aHandled);
end;

procedure TTisGrid.DoHeaderMouseDown(aButton: TMouseButton; aShift: TShiftState;
  aX, aY: Integer);
begin
  if not Assigned(Header.PopupMenu) then
    Header.PopupMenu := TTisGridHeaderPopupMenu.Create(self);
  if Header.PopupMenu is TTisGridHeaderPopupMenu then
    with Header.PopupMenu as TTisGridHeaderPopupMenu do
    begin
      Header.PopupMenu.PopupComponent := self;
      FillPopupMenu;
    end;
  inherited DoHeaderMouseUp(aButton, aShift, aX, aY);
end;

procedure TTisGrid.DoEnter;
begin
  inherited DoEnter;
  FillPopupMenu; // activate shortcuts
end;

procedure TTisGrid.DoExit;
begin
  CleanPopupMenu; // deactivate all shortcuts
  inherited DoExit;
end;

function TTisGrid.GetHeaderClass: TVTHeaderClass;
begin
  result := TTisGridHeader;
end;

procedure TTisGrid.FillPopupMenu;
const
  cDIVIDER = '-';

  procedure _AddItem(const aCaption: string; aShortcut: TShortCut; aEvent: TNotifyEvent;
    aEnabled: Boolean = True );
  var
    vMenuItem: TMenuItem;
  begin
    vMenuItem := PopupMenu.Items.Find(aCaption);
    if (vMenuItem = nil) or (aCaption = cDIVIDER) then
    begin
      vMenuItem := TMenuItem.Create(PopupMenu);
      with vMenuItem do
      begin
        Caption := aCaption;
        ShortCut := aShortcut;
        OnClick := aEvent;
        Enabled := aEnabled;
        Tag := POPUP_ITEM_TAG;
      end;
      PopupMenu.Items.Add(vMenuItem);
    end;
  end;

  procedure _AddDivider;
  begin
    _AddItem(cDIVIDER, 0, nil, True);
  end;

begin
  if not Assigned(PopupMenu) then
    PopupMenu := TPopupMenu.Create(self);
  // fire the original user event, if it exists, for customize its items
  if Assigned(PopupMenu.OnPopup) then
    PopupMenu.OnPopup(PopupMenu);
  if PopupMenu.Items.Count > 0 then
    _AddDivider;
  if pmoShowFind in fPopupMenuOptions then
    _AddItem(rsGridFind, ShortCut(Ord('F'), [ssCtrl]), @DoFindText, not fData.IsVoid);
  if pmoShowFindNext in fPopupMenuOptions then
    _AddItem(rsGridFindNext, VK_F3, @DoFindNext, not fData.IsVoid);
  {_AddItem(rsFindReplace, ShortCut(Ord('H'), [ssCtrl]), @DoFindReplace);}
  _AddDivider;
  if (pmoShowInsert in fPopupMenuOptions) and (not (toReadOnly in TreeOptions.MiscOptions)) and (not fNodeOptions.ShowChildren) then
    _AddItem(rsGridInsert, ShortCut(Ord('I'), [ssCtrl]), @DoInsert, Length(Header.Columns.GetVisibleColumns) > 0);
  if (pmoShowCut in fPopupMenuOptions) and (not (toReadOnly in TreeOptions.MiscOptions)) and Assigned(fOnCutToClipboard) then
    _AddItem(rsGridCut, ShortCut(Ord('X'), [ssCtrl]), @DoCutToClipboard, not fData.IsVoid);
  if (pmoShowCopy in fPopupMenuOptions) and (not fNodeOptions.ShowChildren) then
    _AddItem(rsGridCopy, ShortCut(Ord('C'), [ssCtrl]), @DoCopyToClipboard, not fData.IsVoid);
  if pmoShowCopyCell in fPopupMenuOptions then
    _AddItem(rsGridCopyCell, ShortCut(Ord('C'), [ssCtrl,ssShift]), @DoCopyCellToClipboard, not fData.IsVoid);
  if pmoShowClearCell in fPopupMenuOptions then
    _AddItem(rsGridClearCell, ShortCut(VK_DELETE, [ssCtrl,ssShift]), @DoClearCell, not fData.IsVoid);
  if (pmoShowCopySpecial in fPopupMenuOptions) and (not fNodeOptions.ShowChildren) then
    _AddItem(rsGridCopySpecial, ShortCut(Ord('S'), [ssCtrl,ssShift]), @DoCopySpecialToClipboard, not fData.IsVoid);
  if (pmoShowPaste in fPopupMenuOptions) and (not (toReadOnly in TreeOptions.MiscOptions)) and
    ((toEditable in TreeOptions.MiscOptions) or Assigned(fOnBeforePaste)) and (not fNodeOptions.ShowChildren)  then
    _AddItem(rsGridPaste, ShortCut(Ord('V'), [ssCtrl]), @DoPaste, Header.UseColumns);
  if (pmoShowDelete in fPopupMenuOptions) and
    ((not (toReadOnly in TreeOptions.MiscOptions)) or Assigned(fOnBeforeDeleteRows)) and
    (not fNodeOptions.ShowChildren) then
    _AddItem(rsGridDeleteRows, ShortCut(VK_DELETE, [ssCtrl]), @DoDeleteRows, not fData.IsVoid);
  if (pmoShowSelectAll in fPopupMenuOptions) and (toMultiSelect in TreeOptions.SelectionOptions) and
    (not fNodeOptions.ShowChildren) then
    _AddItem(rsGridSelectAll, ShortCut(Ord('A'), [ssCtrl]), @DoSelectAllRows, not fData.IsVoid);
  if (pmoShowChart in fPopupMenuOptions) and (not fNodeOptions.ShowChildren) then
  begin
    _AddDivider;
    _AddItem(rsGridChartShow, 0, @DoShowChart, not fData.IsVoid);
  end;
  if (pmoShowExport in fPopupMenuOptions) and (not fNodeOptions.ShowChildren) then
  begin
    _AddDivider;
    if toMultiSelect in TreeOptions.SelectionOptions then
      _AddItem(rsGridExportSelected, 0, @DoExport, not fData.IsVoid)
    else
      _AddItem(rsGridExportAll, 0, @DoExport, not fData.IsVoid);
  end;
  {if (HMPrint = 0) then
    _AddItem(rsPrint, ShortCut(Ord('P'), [ssCtrl]), @DoPrint);}
  if IsTreeMode then
  begin
    _AddDivider;
    _AddItem(rsGridExpandAll, Shortcut(Ord('E'), [ssCtrl, ssShift]), @DoExpandAll);
    _AddItem(rsGridCollapseAll, Shortcut(Ord('R'), [ssCtrl, ssShift]), @DoCollapseAll);
  end;
  if (pmoShowCustomizeColumns in fPopupMenuOptions) and Assigned(Header.PopupMenu) then
  begin
    _AddDivider;
    _AddItem(rsGridCustomizeColumns, 0, @DoCustomizeColumns);
  end;
  if (csDesigning in ComponentState) or (pmoShowCustomizeGrid in fPopupMenuOptions) then
  begin
    _AddDivider;
    _AddItem(rsGridAdvancedCustomizeColumns, 0, @DoAdvancedCustomizeColumns);
  end;
  if Assigned(fOnAfterFillPopupMenu) then
    fOnAfterFillPopupMenu(self);
end;

procedure TTisGrid.CleanPopupMenu;
var
  v1: Integer;
begin
  if Assigned(PopupMenu) then
    for v1 := PopupMenu.Items.Count-1 downto 0 do
      if PopupMenu.Items[v1].Tag = POPUP_ITEM_TAG then
        PopupMenu.Items.Delete(v1);
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
  v1: integer;
begin
  result := nil;
  for v1 := 0 to Header.Columns.Count - 1 do
  begin
    if TTisGridColumn(Header.Columns[v1]).PropertyName = aPropertyName then
    begin
      result := TTisGridColumn(Header.Columns[v1]);
      break;
    end;
  end;
end;

function TTisGrid.FindColumnByIndex(const aIndex: TColumnIndex): TTisGridColumn;
begin
  if Header.Columns.IsValidColumn(aIndex) then
    result := TTisGridColumn(Header.Columns[aIndex])
  else
    result := nil;
end;

function TTisGrid.IsVisibleColumnByPropertyName(const aPropertyName: RawUtf8): Boolean;
var
  Col: TVirtualTreeColumn;
begin
  Result := False;
  for Col in Header.Columns.GetVisibleColumns do
    if TTisGridColumn(Col).PropertyName = aPropertyName then
      Exit(True);
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

function TTisGrid.Add(aData: PDocVariantData): Integer;
var
  vObj: PDocVariantData;
  vAborted: Boolean;
begin
  result := -1;
  if Assigned(fOnInsert) then
  begin
    vAborted := False;
    fOnInsert(self, aData, vAborted);
    if vAborted then
      exit;
  end;
  case aData^.Kind of
    dvArray:
      for vObj in aData^.Objects do
        result := fData.AddItem(variant(vObj^));
    dvObject:
      result := fData.AddItem(variant(aData^));
  end;
  if result > -1 then
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
    result := OnCompareByRow(self, aPropertyName, aRow1, aRow2, vHandled)
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
  vColIndex: integer;
  vNode: PVirtualNode;

  procedure _Focus(aColumnIndex: TColumnIndex; aNode: PVirtualNode);
  begin
    SelectNodes(aNode,aNode,False);
    FocusedNode := aNode;
    FocusedColumn := aColumnIndex;
    SetFocusSafe;
  end;

  function _Match(aNode: PVirtualNode; var aTxt: string): integer;
  var
    v1: integer;
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
      for v1 := 0 to Header.Columns.Count - 1 do
      begin
        DoGetText(aNode, v1, ttNormal, vTxt);
        if not (frWholeWord in fFindDlg.Options) and (Pos(aTxt, LowerCase(vTxt)) > 0) or
          (aTxt = LowerCase(vTxt)) then
        begin
          TextFound := True;
          result := v1;
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
      FullExpand; // necessary to show the node for the user, otherwise it gets in an infinity loop
    vNode := FocusedNode;
    TextFound := False;
    if vNode <> nil then
    begin
      // depart de recherche. teste la ligne en cours
      if (fStartSearchNode = nil) then
      begin
        fStartSearchNode := vNode;
        vColIndex := _Match(vNode, fTextToFind);
        if vColIndex >= 0 then
        begin
          _Focus(vColIndex, vNode);
          exit;
        end;
      end;
      //on teste a partir du suivant
      if (fFindDlg <> nil) and not (frDown in fFindDlg.Options) then
        vNode := GetPrevious(vNode)
      else
        vNode := GetNext(vNode);
      while (vNode <> nil) and (vNode <> fStartSearchNode) do
      begin
        vColIndex := _Match(vNode, fTextToFind);
        if vColIndex >= 0 then
        begin
          _Focus(vColIndex, vNode);
          exit;
        end;
        // on teste a partir du suivant
        if (fFindDlg <> nil) and not (frDown in fFindDlg.Options) then
          vNode := GetPrevious(vNode)
        else
          vNode := GetNext(vNode);
        // on reboucle sur le debut
        if vNode = nil then
          if (fFindDlg <> nil) and not (frDown in fFindDlg.Options) then
            vNode := GetLast(nil)
          else
            vNode := GetFirst(False);
      end;
    end;
    fStartSearchNode := nil;
    ShowMessageFmt(rsGridNoRecordFind, [TextToFind]);
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
      fReplaceDialog.FindText := (EditLink as TStringEditLink).Edit.Text
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
  if Assigned(fOnExportCustomContent) then
    fOnExportCustomContent(self, aSource, aBuffer);
end;

procedure TTisGrid.DoCopyToClipboard(aSender: TObject);
var
  vStr: RawByteString;
  vAdapter: TClipboardAdapter;
begin
  vAdapter.Open;
  try
    vAdapter.Clear;
    vStr := ContentToUTF8(tstSelected, ';');
    vAdapter.Add(cbkText, vStr[1], Length(vStr)+1);
    vStr := ContentToJson(tstSelected,False);
    vAdapter.Add(cbkJson, vStr[1], Length(vStr));
  finally
    vAdapter.Close;
  end;
end;

procedure TTisGrid.DoCopyCellToClipboard(aSender: TObject);
var
  vAdapter: TClipboardAdapter;
  vRow: TDocVariantData;
  vStr: RawByteString;
begin
  if FocusedColumnObject <> nil then
  begin
    vAdapter.Open;
    try
      vAdapter.Clear;
      vStr := Text[FocusedNode, FocusedColumnObject.Index];
      if vStr <> '' then
      begin
        vAdapter.Add(cbkText, vStr[1], Length(vStr)+1);
        if not fNodeAdapter.IsChild(FocusedNode) then
        begin
          SelectedRows.Reduce(FocusedColumnObject.PropertyName, False, vRow);
          vStr := vRow.ToJson;
          vAdapter.Add(cbkJson, vStr[1], Length(vStr));
        end;
      end;
    finally
      vAdapter.Close;
    end;
  end;
end;

procedure TTisGrid.DoClearCell(aSender: TObject);
var
  vNode: PVirtualNode;
  vAborted: Boolean;
  vNewValue: Variant;
begin
  if Assigned(FocusedNode) then
  begin
    if Assigned(FocusedColumnObject) and FocusedColumnObject.ReadOnly then
      exit;
    vNewValue := NULL;
    if NodeOptions.MultiEdit then
      for vNode in SelectedNodes do
      begin
        vAborted := False;
        DoEditValidated(
          FocusedNode, FocusedColumnObject,
          fNodeAdapter.GetValueAsSimple(vNode, FocusedColumn)^, vNewValue, vAborted);
        if vAborted then
          Continue;
        fNodeAdapter.SetValue(vNode, vNewValue, FocusedColumn, VarIsStr(vNewValue));
        InvalidateNode(vNode);
      end
    else
    begin
      vAborted := False;
      DoEditValidated(
        FocusedNode, FocusedColumnObject,
        fNodeAdapter.GetValueAsSimple(vNode, FocusedColumn)^, vNewValue, vAborted);
      if not vAborted then
      begin
        fNodeAdapter.SetValue(FocusedNode, vNewValue, FocusedColumn, VarIsStr(vNewValue));
        InvalidateNode(FocusedNode);
      end;
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
        vClipAdapter.Add(cbkText, vBuf[1], Length(vBuf)+1);
      end;
      efoJson:
      begin
        vBuf := ContentToJson(vParams.Selection, vParams.Columns.VisibleOnly);
        vClipAdapter.Add(cbkText, vBuf[1], Length(vBuf)+1);
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

procedure TTisGrid.DoInsert(aSender: TObject);
var
  vNew: TDocVariantData;
  vCol: TTisGridColumn;
  v1: Integer;
begin
  vNew.InitObject([], JSON_FAST_FLOAT);
  for v1 := 0 to Header.Columns.Count -1 do
  begin
    vCol := Header.Columns[v1] as TTisGridColumn;
    vNew.AddValue(vCol.PropertyName, NULL);
  end;
  Add(@vNew);
  ClearSelection;
  FocusedNode := GetNodeBy(@vNew);
  Selected[FocusedNode] := True;
  ScrollIntoView(FocusedNode, False);
end;

procedure TTisGrid.DoDeleteRows(aSender: TObject);
var
  vDoc: TDocVariantData;
  vAsk, vAborted: Boolean;

  function _UserConfirmed: Boolean;
  begin
    result := Dialogs.MessageDlg(
      rsGridConfirmation, Format(rsGridConfDeleteRow, [SelectedCount]),
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
    if vAsk and (not _UserConfirmed) then
      exit;
    DeleteRows(@vDoc);
  end
  else
    if _UserConfirmed then
      DeleteRows(@vDoc);
end;

procedure TTisGrid.DoPaste(aSender: TObject);
var
  vClipAdapter: TClipboardAdapter;
  vDoc: PDocVariantData;
  vTempDoc: TDocVariantData;
  vAbort: Boolean;
begin
  vAbort := False;
  vDoc := Nil;
  if vClipAdapter.IsValidFor(cbkJson) or vClipAdapter.IsValidFor(cbkText) then
    vDoc := _Safe(_Json(vClipAdapter.AsJson))
  else
  begin
    vTempDoc.InitArray([],[dvoReturnNullForUnknownProperty]);
    vDoc := @vTempDoc;
  end;
  if Assigned(fOnBeforePaste) then
    fOnBeforePaste(Self,vDoc,vAbort);
  if vAbort then
    Exit;
  if Assigned(vDoc) then
    Add(vDoc);
end;

procedure TTisGrid.DoSelectAllRows(aSender: TObject);
begin
  SelectAll(True);
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

procedure TTisGrid.DoShowChart(aSender: TObject);
var
  vColumn: TTisGridColumn;
  vIndex: Integer;
  vChartForm: TTisChartForm;
  vFlags: TTisChartChangeFlags;
begin
  vColumn := FocusedColumnObject;
  if Assigned(vColumn) and vColumn.AllowChart then
  begin
    vChartForm := TTisChartForm.Create(Owner);
    try
      vFlags.Init;
      for vIndex := 0 to vChartForm.ComponentCount-1 do
        if vChartForm.Components[vIndex] is TChart then
          DoChartTitle(vChartForm.Components[vIndex] as TChart, vColumn, vFlags);
      vChartForm.OnChartChange := @DoChartChange;
      vChartForm.OnChartFillSource := @DoChartFillSource;
      // add columns
      for vIndex := 0 to Header.Columns.Count - 1 do
      begin
        with Header.Columns[vIndex] as TTisGridColumn do
        begin
          if coVisible in Options then
            vChartForm.PieValuesCombo.Items.Add(Text + ' (' + Utf8ToString(PropertyName) + ')');
        end;
      end;
      vChartForm.MostUsedCheckbox.Checked := ChartOptions.MostUsedValues.Enabled;
      vChartForm.TopMostUsedEdit.Value := ChartOptions.MostUsedValues.Count;
      vChartForm.Settings := vColumn.ChartSettings;
      vChartForm.ShowModal;
      vColumn.ChartSettings := vChartForm.Settings;
    finally
      vChartForm.Free;
    end;
  end;
end;

procedure TTisGrid.DoCustomEditor(aNode: PVirtualNode; const aColumn: TTisGridColumn;
  out aControl: TTisGridControl);
begin
  aControl := nil;
  if Assigned(fOnCustomEditor) then
    fOnCustomEditor(self, aNode, aColumn, aControl);
end;

procedure TTisGrid.DoEditorLookup(aNode: PVirtualNode; const aColumn: TTisGridColumn;
  out aControl: TTisGridControl; var aHandled: Boolean);
begin
  aControl := nil;
  aHandled := False;
  if Assigned(fOnEditorLookup) then
  begin
    aControl := TTisGridSearchEditControl.Create(self);
    fOnEditorLookup(self, aNode, aColumn, (aControl as TTisGridSearchEditControl).Edit, aHandled);
    if not aHandled then
      FreeAndNilSafe(aControl);
  end;
end;

procedure TTisGrid.DoPrepareEditor(aNode: PVirtualNode; const aColumn: TTisGridColumn;
  aControl: TTisGridControl);
begin
  if Assigned(fOnPrepareEditor) then
    fOnPrepareEditor(self, aNode, aColumn, aControl);
end;

procedure TTisGrid.DoEditValidated(aNode: PVirtualNode; const aColumn: TTisGridColumn;
  const aCurValue: Variant; var aNewValue: Variant; var aAbort: Boolean);
begin
  if Assigned(fOnEditValidated) then
    fOnEditValidated(self, aNode, aColumn, aCurValue, aNewValue, aAbort);
end;

procedure TTisGrid.DoBeforeDataChange(aData: PDocVariantData;
  var aAbort: Boolean);
begin
  if Assigned(fOnBeforeDataChange) then
    fOnBeforeDataChange(self, aData, aAbort);
end;

procedure TTisGrid.DoAfterDataChange;
begin
  if Assigned(fOnAfterDataChange) then
    fOnAfterDataChange(self);
end;

procedure TTisGrid.DoGetMetaData(var aMetaData: RawUtf8);
begin
  if Assigned(fOnGetMetaData) then
    fOnGetMetaData(self, aMetaData);
end;

function TTisGrid.DoNodeFiltering(aNode: PVirtualNode): Boolean;
begin
  result := False;
  if Assigned(fOnNodeFiltering) then
    fOnNodeFiltering(self, aNode, result);
end;

function TTisGrid.DoBeforeHtmlRendering(aNode: PVirtualNode; aColumn: TTisGridColumn): string;
var
  vHandled: Boolean;
  vMus: TSynMustache;
  vValue: Variant;
begin
  result := '';
  vHandled := False;
  vValue := fNodeAdapter.GetValueAsVariant(aNode, aColumn.Index);
  if Assigned(fOnBeforeHtmlRendering) then
    fOnBeforeHtmlRendering(self, _Safe(vValue), aColumn, result, vHandled);
  if not vHandled then
  begin
    // if not handled, and there is a template, use it
    result := aColumn.DataTypeOptions.HtmlOptions.MustacheTemplate.Text;
    // if there is no template, assume that the cell data already contains a plain HTML to be rendered
    if result = '' then
      result := VarToStr(vValue)
    else
    begin
      vMus := TSynMustache.Parse(result);
      result := vMus.Render(vValue);
    end;
  end;
end;

function TTisGrid.DoCalcAttributes(aNode: PVirtualNode; aColumn: TTisGridColumn;
  out aValue: Variant): Boolean;
begin
  result := False;
  if Assigned(fOnCalcAttributes) then
    fOnCalcAttributes(self, aNode, aColumn, aValue, result);
end;

procedure TTisGrid.DoBeforeAddingChartSource(aColumn: TTisGridColumn;
  var aX, aY: Double; var aLabel: string; var aColor: TColor);
begin
  if Assigned(fOnBeforeAddingChartSource) then
    fOnBeforeAddingChartSource(self, aColumn, aX, aY, aLabel, aColor);
end;

procedure TTisGrid.DoChartTitle(aChart: TChart; aColumn: TTisGridColumn;
  var aFlags: TTisChartChangeFlags);
var
  vTitle: string;
begin
  if Assigned(OnDefaultChartTitle) then
  begin
    vTitle := '';
    OnDefaultChartTitle(self, aChart, aColumn, aFlags, vTitle);
  end
  else
    vTitle := 'Chart per ' + aColumn.Text;
  if Assigned(fOnChartTitle) then
    fOnChartTitle(self, aChart, aColumn, aFlags, vTitle);
  aChart.Title.Text.Text := vTitle;
end;

procedure TTisGrid.DoChartFillSource(aChart: TChart; aSource: TListChartSource;
  var aFlags: TTisChartFillSourceFlags);

  function Darkened(aValue: TColor): TColor;
  var
    r, g, b: Byte;
  begin
    r := GetRValue(aValue);
    g := GetGValue(aValue);
    b := GetBValue(aValue);
    result := RGB(
      r - MulDiv(r, 15, 100),
      g - MulDiv(g, 15, 100),
      b - MulDiv(b, 15, 100)
    );
  end;

  function Compute(aObj: PDocVariantData): Double;
  var
    vValue: Double;
  begin
    result := 1;
    if Header.Columns.IsValidColumn(aFlags.ValueColumnIndex) and
      aObj^.GetAsDouble(FindColumnByIndex(aFlags.ValueColumnIndex).PropertyName, vValue) then
      result := vValue;
  end;

  procedure AddSource(aColumn: TTisGridColumn; aDefX, aDefY: Double; const aDefLabel: string);
  var
    vDefX, vDefY: Double;
    vDefLabel: string;
    vDefColor: TColor;
  begin
    vDefX := aDefX;
    vDefY := aDefY;
    vDefLabel := aDefLabel;
    vDefColor := Darkened(RGBToColor(Random(256), Random(256), Random(256)));
    DoBeforeAddingChartSource(aColumn, vDefX, vDefY, vDefLabel, vDefColor);
    if Length(vDefLabel) > ChartOptions.MaxLabelLength then
      vDefLabel := Copy(vDefLabel, 1, ChartOptions.MaxLabelLength) + '...';
    aSource.Add(vDefX, vDefY, vDefLabel, vDefColor);
  end;

var
  vColumn: TTisGridColumn;
  vObj: PDocVariantData;
  vLabels: TDocVariantData;
  vIndex, vMostUsedCount, vOthersCount: Integer;
  vValue: RawUtf8;
  vNode: PVirtualNode;
begin
  vLabels.InitFast;
  vColumn := FocusedColumnObject;
  for vNode in VisibleNodes do
  begin
    vObj := GetNodeAsPDocVariantData(vNode);
    // if selected rows is more than one, it will get only selected ones
    if (SelectedCount > 1) and not (vsSelected in vNode^.States) then
      Continue;
    vValue := vObj^.U[vColumn.PropertyName];
    vIndex := vLabels.SearchItemByProp('field', vValue, not FilterOptions.CaseInsensitive);
    if vIndex >= 0 then
    begin
      with _Safe(vLabels.Value[vIndex])^ do
        D['count'] := D['count'] + Compute(vObj);
    end
    else
      vLabels.AddItem(_ObjFast(['field', vValue, 'count', Compute(vObj)]));
  end;
  vMostUsedCount := 0;
  vOthersCount := 0;
  if aFlags.MostUsedValues.Enabled then
    vLabels.SortArrayByFields(['count', vColumn.PropertyName], nil, nil, True);
  Randomize;
  for vObj in vLabels.Objects do
  begin
    if aFlags.MostUsedValues.Enabled then
    begin
      Inc(vMostUsedCount);
      if vMostUsedCount <= aFlags.MostUsedValues.Count then
        AddSource(vColumn, 0, vObj^.D['count'], vObj^.S['field'])
      else
        Inc(vOthersCount);
    end
    else
      AddSource(vColumn, 0, vObj^.D['count'], vObj^.S['field']);
  end;
  if vOthersCount > 0 then
    AddSource(vColumn, 0, vOthersCount, rsGridChartOthersLabel);
end;

procedure TTisGrid.DoChartChange(aChart: TChart; var aFlags: TTisChartChangeFlags);
var
  vColumn: TTisGridColumn;
begin
  vColumn := FocusedColumnObject;
  if not aFlags.Title.Customized then
    DoChartTitle(aChart, vColumn, aFlags);
  if Assigned(fOnChartChange) then
    fOnChartChange(self, aChart, vColumn, aFlags);
end;

function TTisGrid.GetExportDialogFilter: string;
var
  vExportAdapter: TTisGridExportFormatOptionAdapter;
  v1: TTisGridExportFormatOption;
begin
  result := '';
  for v1 := high(TTisGridExportFormatOption) downto low(TTisGridExportFormatOption) do
  begin
    if v1 in fExportFormatOptions then
    begin
      if result <> '' then
        result += '|';
      result += vExportAdapter.EnumToFilter(v1);
    end;
  end;
end;

procedure TTisGrid.RestoreSettings;
begin
  Settings := fDefaultSettings;
end;

procedure TTisGrid.InitData;
begin
  fData.Clear;
  fData.InitArray([], JSON_FAST_FLOAT);
end;

procedure TTisGrid.FillDataFromJsonObject(const aObject: TDocVariantData);
var
  vData, vObj: TDocVariantData;
  vField: TDocVariantFields;
  vName: string;
begin
  // make a local copy, as fData could be used as argument too
  vData.InitJson(aObject.ToJson, JSON_FAST_FLOAT);
  vObj.InitObject([], JSON_FAST_FLOAT);
  InitData;
  if NodeOptions.ShowChildren then
    for vField in vData.Fields do
    begin
      vObj.Clear;
      if Assigned(vField.Name) then
        vName := vField.Name^
      else
        vName := '';
      vObj.InitObject([vName, vField.Value^], JSON_FAST_FLOAT);
      fData.AddItem(variant(vObj));
    end;
  // if found nothing, add the JSON as it came
  if fData.Count = 0 then
    fData.AddItem(variant(vData));
end;

constructor TTisGrid.Create(aOwner: TComponent);
begin
  inherited Create(AOwner);
  fNodeAdapter.Init(self);
  Clear;
  DefaultText := '';
  DefaultCsvSeparator := ';';
  fZebraColor := $00EDF0F1;
  fZebraLightness := DefaultZebraLightness;
  SetLength(fKeyFieldsList, 0);
  fNodeOptions := TTisNodeOptions.Create(self);
  fPopupMenuOptions := DefaultPopupMenuOptions;
  fExportFormatOptions := DefaultExportFormatOptions;
  fChartOptions := TTisGridChartOptions.Create(self);
  fFilterOptions := TTisGridFilterOptions.Create(self);
  fData.InitArray([]);
  DragType := dtVCL;

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
  fChartOptions.Free;
  fFilterOptions.Free;
  inherited Destroy;
end;

procedure TTisGrid.Assign(aSource: TPersistent);
begin
  inherited Assign(aSource);
  if aSource is TTisGrid then
    with TTisGrid(aSource) do
    begin
      self.ZebraColor := ZebraColor;
      self.ZebraLightness := ZebraLightness;
      self.ZebraPaint := ZebraPaint;
      self.OnGetMetaData := OnGetMetaData;
    end
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

function TTisGrid.AddChild(aParent: PVirtualNode; aUserData: Pointer): PVirtualNode;
begin
  raise ETisGrid.Create('Instead to call AddChild, use Data and LoadData to add a new child automatically.');
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
  InitData;
end;

function TTisGrid.ComputeNodeHeight(aCanvas: TCanvas; aNode: PVirtualNode;
  aColumn: TColumnIndex; S: string): Integer;
var
  vCache: PTisNodeCache;
begin
  vCache := fNodeAdapter.GetCache(aNode);
  if Assigned(vCache) then
    result := vCache^.Height;
end;

procedure TTisGrid.LoadData(const NewGridData: PDocVariantdata);

  procedure _ViewInTreeMode;
  var
    v1: PtrInt;
    vDoc, vObj: PDocVariantData;
    vNode: PVirtualNode;
    vKeys: TRawUtf8DynArray;
    vParents: TRawUtf8DynArray;
    vEqual: Boolean;
  begin
    DynArrayFakeLength(@vKeys, 1);
    DynArrayFakeLength(@vParents, 1);
    StringDynArrayToRawUtf8DynArray(fKeyFieldsList, vKeys);
    StringDynArrayToRawUtf8DynArray(fParentKeyFieldsList, vParents);
    vNode := GetFirst(True);
    while vNode <> nil do
    begin
      vDoc := GetNodeAsPDocVariantData(vNode);
      if vDoc <> nil then
      begin
        for vObj in fData.Objects do
        begin
          vEqual := True;
          for v1 := low(vParents) to high(vParents) do
          begin
            if vDoc^.U[vParents[v1]] <> vObj^.U[vKeys[v1]] then
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
  vFocusedRow, vTopRow, vSelectedRows: TDocVariantData;
  vNodeArray: TNodeArray;
  vNode: PVirtualNode;
  vIsReadOnly: Boolean;
begin
  if (csLoading in ComponentState) then
    Exit;

  if (Assigned(NewGridData) and NewGridData^.IsVoid) then
  begin
    if not fData.IsVoid then
      InitData;
    vIsReadOnly := toReadOnly in TreeOptions.MiscOptions;
    TreeOptions.MiscOptions := TreeOptions.MiscOptions - [toReadOnly];
    inherited Clear;
    if vIsReadOnly then
      TreeOptions.MiscOptions := TreeOptions.MiscOptions + [toReadOnly];
  end
  else
  begin
    // stores previous focused and selected rows
    BeginUpdate;
    try
      if Assigned(KeyFieldsList) then
      begin
        SelectedRows.Reduce(TRawUtf8DynArray(fKeyFieldsList), {aCaseSensitive=}True, vSelectedRows, True);
        if Assigned(FocusedRow) then
          FocusedRow^.Reduce(TRawUtf8DynArray(fKeyFieldsList), {aCaseSensitive=}True, vFocusedRow, True)
        else
          vFocusedRow.InitObject([]);
        if Assigned(TopNode) then
          GetNodeAsPDocVariantData(TopNode)^.Reduce(TRawUtf8DynArray(fKeyFieldsList), {aCaseSensitive=}True, vTopRow)
        else
          vTopRow.InitObject([]);
      end;
      if Assigned(NewGridData) then
        fData := NewGridData^;
      vIsReadOnly := toReadOnly in TreeOptions.MiscOptions;
      TreeOptions.MiscOptions := TreeOptions.MiscOptions - [toReadOnly];
      try
        inherited Clear;
        // user changed the default (array) type
        if fData.Kind = dvObject then
          FillDataFromJsonObject(fData);
        RootNodeCount := fData.Count;
        if IsTreeModeKeyParent then
          _ViewInTreeMode;
        if NodeOptions.ShowChildren then
          FullExpand;
      finally
        if vIsReadOnly then
          TreeOptions.MiscOptions := TreeOptions.MiscOptions + [toReadOnly];
      end;
    finally
      try
        if Assigned(KeyFieldsList) then
        begin
          // restore selected nodes
          SelectedRows := vSelectedRows;
          // restore focused node
          if not vFocusedRow.IsVoid then
            SetFocusedRowNoClearSelection(@vFocusedRow);
          // restore top visible node
          if not vTopRow.IsVoid and not (tsScrolling in TreeStates) then
            vNodeArray := GetNodesBy(@vTopRow, KeyFieldsNames <> '');
        end
        else
          SetLength(vNodeArray,0);
      finally
        EndUpdate;
        for vNode in vNodeArray do
        begin
          TopNode := vNode;
          break;
        end;
        // restore visible focused column
        ScrollIntoView(FocusedColumn, False);
      end;
      // should update the popup menu as some items depend on whether or not there is data
      CleanPopupMenu;
      FillPopupMenu;
      // clear all filters after loading
      if fFilterOptions.ClearAfterLoadingData then
        fFilterOptions.ClearFilters
      else
        fFilterOptions.ApplyFilters;
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
  vDoc: TDocVariantData;
  vData: PDocVariantData;
  vItem1, vItem2: PVariant;
begin
  result := False;
  try
    if vDoc.InitJson(StringToUtf8(aJson), JSON_FAST_FLOAT) then
    begin
      if vDoc.Kind = dvArray then
      begin
        for vItem1 in vDoc.Items do
        begin
          vData := PDocVariantData(vItem1);
          case vData^.Kind of
            dvArray:
              for vItem2 in vData^.Items do
                Data.AddItem(vItem2^);
            dvObject:
              Data.AddItem(vItem1^);
          else
            Data.AddItem(_Json('{"unknown":"' + VariantToUtf8(vItem1^) + '"}'));
          end;
        end;
      end
      else if vDoc.Kind = dvObject then
      begin
        FillDataFromJsonObject(vDoc);
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
    v1: LongInt;
    vFile: File;
  begin
    AssignFile(vFile, aFileName);
    Rewrite(vFile,1);
    try
      vBuf := PUtf8Char(aBuffer + #0);
      v1 := StrLen(vBuf);
      BlockWrite(vFile, vBuf^, v1);
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

procedure TTisGrid.ExportExcel(Prefix: String; Selection: TVSTTextSourceType;
  Separator: Char);
  function GetTempFileName(Const Prefix,ext : String) : String;
  var
    I: Integer;
    Start: String;
    Disc: String;
  begin
    Start := GetTempDir;
    I := 0;
    Disc := '';
    repeat
      Result := Format('%s%s%s%s', [Start, Prefix, Disc, ext]);
      Disc := Format('%.5d', [i]);
      Inc(I);
    until not FileExists(Result);
  end;
var
  tempfn:Utf8String;
  txt:Utf8String;
  txtbuf:PChar;
  l:LongInt;
  st:File;
begin
  tempfn:=GetTempFileName(Prefix,'.csv');
  AssignFile(st,tempfn);
  Rewrite(st,1);
  try
    txt := ContentToCsv(Selection,Separator)+#0;
    txtbuf := pchar(txt);
    l := strlen(txtbuf);
    BlockWrite(st,txtbuf^,l);
  finally
    CloseFile(st);
    OpenDocument(tempfn);
  end;
end;

function TTisGrid.CheckedRows: TDocVariantData;
var
  vNode: PVirtualNode;
begin
  vNode := GetFirstChecked;
  result.InitArray([]);
  while vNode <> nil do
  begin
    result.AddFrom(variant(GetNodeAsPDocVariantData(vNode)^));
    vNode := GetNextChecked(vNode, csCheckedNormal, True);
  end;
end;

procedure TTisGrid.SetFocusedRowNoClearSelection(aValue: PDocVariantData;
  EnsureScrollIntoView: Boolean);
var
  vArray: TNodeArray;
begin
  if (aValue = nil) or (aValue^.IsVoid) then
    FocusedNode := nil
  else
  begin
    vArray := GetNodesBy(aValue, True);
    if Length(vArray) > 0 then
    begin
      FocusedNode := vArray[0];
      Selected[vArray[0]] := True;
      // this is slow when there are many rows
      if EnsureScrollIntoView then
        ScrollIntoView(FocusedNode, False);
    end;
  end;
end;

function TTisGrid.GetDataAsJsonObject: TDocVariantData;
var
  vObj: PDocVariantData;
  vField: TDocVariantFields;
begin
  result.InitObject([], JSON_FAST_FLOAT);
  for vObj in fData.Objects do
  begin
    for vField in vObj^ do
    begin
      if Assigned(vField.Name) then
        result.AddValue(vField.Name^, vField.Value^);
    end;
  end;
end;

function TTisGrid.GetCellData(aNode: PVirtualNode; const aPropertyName: RawUtf8; aDefault: PVariant): PVariant;
var
  NodeData: PDocVariantData;
begin
  result := aDefault;
  if aNode <> nil then
  begin
    NodeData := GetNodeAsPDocVariantData(aNode);
    if NodeData <> nil then
      NodeData^.GetAsPVariant(aPropertyName, result);
  end;
end;

function TTisGrid.GetCellDataAsString(aNode: PVirtualNode;
  const aPropertyName: RawUtf8; const aDefault: string): string;
var
  vData: PVariant;
begin
  vData := GetCellData(aNode, aPropertyName);
  if vData = nil then
    result := aDefault
  {else if VarIs(vData,DocVariantVType) ; DocVariantType vData ^.Kind = dvArray then
    result := Utf8ToString(vData^.ToCsv(','))}
  else
    result := VariantToString(vData^);
end;

function TTisGrid.GetNodeAsPDocVariantData(aNode: PVirtualNode;
  aUseFocusedNodeAsDefault: Boolean): PDocVariantData;
var
  vNode: PVirtualNode;
begin
  result := nil;
  if (aNode = nil) and aUseFocusedNodeAsDefault then
    aNode := FocusedNode;
  if aNode <> nil then
  begin
    if fNodeAdapter.IsChild(aNode) then
    begin
      // get the node parent that is not child type, to return the original PDocVariantData
      vNode := aNode^.Parent;
      while not fNodeAdapter.IsChild(vNode) do
        vNode := vNode^.Parent;
      result := fNodeAdapter.GetData(vNode)^.Data;
    end
    else
      result := fNodeAdapter.GetData(aNode)^.Data;
  end;
end;

function TTisGrid.GetNodeDataAsDocVariant(aNode: PVirtualNode;
  aUseFocusedNodeAsDefault: Boolean): PDocVariantData;
begin
  result := GetNodeAsPDocVariantData(aNode, aUseFocusedNodeAsDefault);
end;

function TTisGrid.GetNodesBy(aData: PDocVariantData;
  aUseKeyFieldsList: Boolean): TNodeArray;

  procedure _Add(var aArray: TNodeArray; aNode: PVirtualNode);
  begin
    SetLength(aArray, length(aArray) + 1);
    aArray[Length(aArray) - 1] := aNode;
  end;

var
  vData: PDocVariantData;
  vNode: PVirtualNode;
  vA, vB: TDocVariantData;
  vArray: TRawUtf8DynArray;
  vUseArray: Boolean;
begin
  SetLength(result, 0);
  if not Assigned(aData) or aData^.IsVoid then
    exit;
  DynArrayFakeLength(@vArray, 1);
  vUseArray := aUseKeyFieldsList and (Length(fKeyFieldsList) > 0);
  if vUseArray then
    StringDynArrayToRawUtf8DynArray(fKeyFieldsList, vArray);
  vNode := GetFirst(True);
  vA.InitObject([],JSON_FAST);
  vB.InitObject([],JSON_FAST);
  while vNode <> nil do
  begin
    vData := GetNodeAsPDocVariantData(vNode, False);
    if vData <> nil then
    begin
      if vUseArray then
      begin
        vA.Clear;
        vData^.Reduce(vArray, False, vA);
        vB.Clear;
        aData^.Reduce(vArray, False, vB);
        if vA.Equals(vB) then
          _Add(result, vNode);
      end
      else if vData^.Equals(aData^) then
        _Add(result, vNode);
    end;
    vNode := GetNext(vNode, True);
  end;
end;

function TTisGrid.GetNodeBy(aData: PDocVariantData; aUseKeyFieldsList: Boolean;
  aRowPosition: PtrInt): PVirtualNode;
var
  vNode: TNodeArray;
begin
  vNode := GetNodesBy(aData, aUseKeyFieldsList);
  if vNode = nil then
    result := nil
  else
    result := vNode[aRowPosition];
end;

function TTisGrid.GetNodesBy(const aKey, aValue: RawUtf8): TNodeArray;
var
  vData: PDocVariantData;
  vNode: PVirtualNode;
begin
  SetLength(result, 0);
  vNode := GetFirst(True);
  while vNode <> nil do
  begin
    vData := GetNodeAsPDocVariantData(vNode, False);
    if (vData <> nil) and (not vData^.IsVoid) and (vData^.U[aKey] = aValue) then
    begin
      SetLength(result, Length(result) + 1);
      result[Length(result)-1] := vNode;
    end;
    vNode := GetNext(vNode, True);
  end;
end;

function TTisGrid.AddRows(aData: PDocVariantData; aAllowDuplicates: Boolean; aCreateColumns: Boolean): TIntegerDynArray;
var
  newRowId, i: Integer;
  vObj: PDocVariantData;
begin
  // don't add if already in grid...
  if aAllowDuplicates or
    ((Length(KeyFieldsList) = 0) and (Length(GetNodesBy(aData)) = 0)) or
    (Length(GetNodesBy(aData, True)) = 0) then
  begin
    if aData^.IsArray then
    begin
      SetLength(Result,aData^.Count);
      i := 0;
      for vObj in aData^.Objects do
      begin
        newRowId := Add(vObj);
        if newRowId >=0 then
        begin
          result[i] := newRowId;
          inc(i);
        end;
      end;
      SetLength(Result,i);
    end
    else
    begin
      newRowId := Add(aData);
      if newRowId>=0 then
      begin
        SetLength(Result,aData^.Count);
        Result := [newRowId];
      end;
    end;

    if (newRowId >= 0) and aCreateColumns and (Header.Columns.Count = 0) then
      CreateColumnsFromData(True, False);
  end;
end;

procedure TTisGrid.DeleteRows(aRows: PDocVariantData);
var
  vObj: PDocVariantData;
  v1: Integer;
begin
  if aRows = nil then
    exit;
  for vObj in aRows^.Objects do
  begin
    // remove from Data
    for v1 := fData.Count - 1 downto 0 do
      if _Safe(fData.Values[v1])^.Equals(vObj^) then
        fData.Delete(v1);
    LoadData;
    // go to the last (new) row, if user has deleted latest ones
    if FocusedRow = nil then
      FocusedRow := GetNodeAsPDocVariantData(GetLast);
  end;
end;

procedure TTisGrid.DeleteSelectedRows;
begin
  DoDeleteRows(self);
end;

procedure TTisGrid.InvalidateNodeByDocVariant(const aData: PDocVariantData);
var
  vNode: PVirtualNode;
begin
  if aData = nil then
    exit;
  vNode := GetFirst(True);
  while vNode <> nil do
  begin
    if GetNodeAsPDocVariantData(vNode) = aData then
      InvalidateNode(vNode);
    vNode := GetNext(vNode, True);
  end;
end;

procedure TTisGrid.ClearAll;
begin
  Clear;
  Header.Columns.Clear;
  FilterOptions.ClearFilters;
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
  v1: TColumnIndex;
  vFocusedColumn: TTisGridColumn;
begin
  try
    vFocusedColumn := FocusedColumnObject;
    for v1 := 0 to Header.Columns.Count-1 do
      Header.Columns[v1].Tag := Header.Columns[v1].Position;
    Header.Columns.Sort(@SortColumnsPosition);
    for v1 := 0 to Header.Columns.Count-1 do
      Header.Columns[v1].Position := TTisGridColumn(Header.Columns[v1]).Index;
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
      Grid.Assign(vTarget);
      Grid.PopupMenu := GridPopupMenu; // special for the Editor
      Grid.Header.Assign(vTarget.Header);
      Grid.TreeOptions.Assign(vTarget.TreeOptions);
      Grid.NodeOptions.Assign(vTarget.NodeOptions);
      Grid.FilterOptions.Assign(vTarget.FilterOptions);
      Grid.Settings := vTarget.Settings;
      Grid.KeyFieldsNames := vTarget.KeyFieldsNames;
      Grid.ParentKeyFieldsNames := vTarget.ParentKeyFieldsNames;
      if ShowModal = mrOK then
      begin
        vTarget.ClearAll;
        vTarget.Header.Assign(Grid.Header);
        vTarget.TreeOptions.Assign(Grid.TreeOptions);
        vTarget.NodeOptions.Assign(Grid.NodeOptions);
        vTarget.FilterOptions.Assign(Grid.FilterOptions);
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

procedure TTisGrid.CreateColumnsFromData(aAutoFitColumns, aAppendMissingAsHidden: Boolean);
const
  cColWidth = 100;
var
  vFieldName: PRawUtf8;
  vObj: PDocVariantData;
  vCol: TTisGridColumn;
  vColIndex: Integer;
  vNewCol: Boolean;
begin
  if fData.IsVoid then
    exit;
  vColIndex := NoColumn;
  BeginUpdate;
  try
    vNewCol := False;
    for vObj in fData.Objects do
    begin
      for vFieldName in vObj^.FieldNames do
      begin
        vCol := FindColumnByPropertyName(vFieldName^);
        if vCol = nil then
        begin
          vNewCol := True;
          vCol := Header.Columns.Add as TTisGridColumn;
          vColIndex := vCol.Index;
          vCol.Text := Utf8ToString(vFieldName^);
          vCol.PropertyName := vFieldName^;
          vCol.Width := cColWidth;
          if aAppendMissingAsHidden then
            vCol.Options := vCol.Options - [coVisible];
          if VarType(vObj^.Value[vFieldName^]) in [varDouble, varCurrency, varInteger] then
            vCol.Alignment := taRightJustify;
        end;
      end;
    end;
  finally
    if aAutoFitColumns and (vColIndex <> NoColumn) then
      Header.AutoFitColumns(False, smaUseColumnOption, vColIndex);
    if vNewCol and (Header.Columns.Count = 1) then
      Header.Columns[0].Width := cColWidth;
    EndUpdate;
  end;
end;

function TTisGrid.ContentToCsv(aSource: TVSTTextSourceType;
  const aSeparator: string; aColumnsVisibleOnly: Boolean;
  aColumnsTranslated: Boolean): RawUtf8;
var
  vTmp, vCols, vRows: TDocVariantData;
  vCol: TTisGridColumn;
  vColIndex: Integer;
  vPropName: RawUtf8;
  vObj: PDocVariantData;
begin
  if aSource in [tstAll, tstInitialized, tstVisible] then
    vRows := fData
  else
    vRows := SelectedRows;
  vCols.InitArray([], JSON_FAST_FLOAT);
  for vColIndex := 0 to Header.Columns.Count-1 do
  begin
    vCol := TTisGridColumn(Header.Columns[vColIndex]);
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
  for vObj in vRows.Objects do
  begin
    vTmp.Reset;
    for vColIndex := 0 to Header.Columns.Count-1 do
    begin
      vCol := TTisGridColumn(Header.Columns[vColIndex]);
      if ((coVisible in vCol.Options) or not aColumnsVisibleOnly) and (vCol.DataType <> cdtPassword) then
      begin
        vPropName := vObj^.U[vCol.PropertyName];
        if vPropName <> '' then
        begin
          if VarType(vObj^.Value[vCol.PropertyName]) in [varDouble, varCurrency, varInteger] then
            vTmp.AddItemText(vPropName)
          else
            vTmp.AddItemText(QuotedStr(vPropName, '"'));
        end
        else
          vTmp.AddItemText('""');
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
  v1: Integer;
begin
  if aSource in [tstAll, tstInitialized, tstVisible] then
    vRows := fData
  else
    vRows := SelectedRows;
  vCols.InitArray([], JSON_FAST_FLOAT);
  for v1 := 0 to Header.Columns.Count-1 do
  begin
    vCol := TTisGridColumn(Header.Columns[v1]);
    if ((coVisible in vCol.Options) or not aColumnsVisibleOnly) and (vCol.DataType <> cdtPassword) then
      vCols.AddItemText(vCol.PropertyName);
  end;
  vRes.InitObject([],JSON_FAST);
  vRows.Reduce(vCols.ToRawUtf8DynArray, False, vRes);
  result := vRes.ToJson;
end;

procedure TTisGrid.UpdateSelectedAndTotalLabel;
var
  vTotal, vSelected: Integer;
begin
  if not Assigned(fSelectedAndTotalLabel) then
    exit;
  if not fData.IsVoid then
    vTotal := fData.Count
  else
    vTotal := 0;
  vSelected := SelectedCount;
  if vSelected > 0 then
    fSelectedAndTotalLabel.Caption := Format('Selected / Total : %d / %d', [vSelected, vTotal])
  else
    fSelectedAndTotalLabel.Caption := Format('Total : %d elements', [vTotal]);
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
  result := (TREEMODE_OPTIONS <= TreeOptions.PaintOptions);
end;

function TTisGrid.IsTreeModeKeyParent: Boolean;
begin
  result := IsTreeMode and (KeyFieldsNames <> '') and (ParentKeyFieldsNames <> '');
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

initialization
  TTisGridEditLink.SetupControlClasses;
  TTisGridFilterOptions.InitClass;

end.
