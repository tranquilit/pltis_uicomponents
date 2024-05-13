// -----------------------------------------------------------------
//    This file is part of Tranquil IT Software
//    Copyright (C) 2012 - 2024  Tranquil IT https://www.tranquil.it
//    All Rights Reserved.
// ------------------------------------------------------------------
unit tis.ui.resourcestrings;

{$i tis.ui.defines.inc}

interface

procedure Translate(const aDirectory, aLang: string);

resourcestring
  /// grid
  rsGridNoRecordFind = 'No more record found for "%s"';
  rsGridPrintOn = 'Printed on';
  rsGridPage = 'Page';
  rsGridConfirmation = 'Confirm';
  rsGridUndoLastUpdate = 'Undo last change';
  rsGridRevertRecord = 'Revert to initial record';
  rsGridFind = 'Search...';
  rsGridFindNext = 'Find next';
  rsGridFindReplace = 'Find and replace...';
  rsGridCopy = 'Copy';
  rsGridCopyCell = 'Copy cell';
  rsGridClearCell = 'Clear cell';
  rsGridCopySpecial = 'Copy special...';
  rsGridCut = 'Cut';
  rsGridPaste = 'Paste';
  rsGridInsert = 'Insert';
  rsGridDelete = 'Delete';
  rsGridDeleteRows = 'Delete selected rows';
  rsGridConfDeleteRow = 'Confirm the deletion of the %d selected rows ?';
  rsGridSelectAll = 'Select all rows';
  rsGridExportSelected = 'Export selected rows to file...';
  rsGridExportAll = 'Export all rows to file...';
  rsGridPrint = 'Print...';
  rsGridExpandAll = 'Expand all';
  rsGridCollapseAll = 'Collapse all';
  rsGridCustomizeColumns = 'Customize columns...';
  rsGridAdvancedCustomizeColumns = 'Advanced customize of table...';
  rsGridShowHideColumns = 'Show/hide columns';
  rsGridShowAllColumns = 'Show all columns';
  rsGridHideAllColumns = 'Hide all columns';
  rsGridRestoreDefaultColumns = 'Restore default columns';
  rsGridRemoveCustomColumn = 'Remove custom column';
  rsGridFilter = 'Filter';
  rsGridFilterClear = 'Clear filter';
  rsGridFilterCustomExpression = 'Custom expression';
  rsGridFilterCustomExpressionCaption = 'Type a custom expression:'#13'(you can add "*" at the beginning and/or end for partial match)';
  rsGridFilterCustomExpressionRemove = 'Remove custom expression';
  rsGridFilterClearAll = 'Clear all filters';
  rsGridFilterEnabled = 'Enable AutoFilter';
  rsGridChartShow = 'Show chart';
  rsGridChartOthersLabel = 'Others';

  /// searchedit
  rsSearchEditClearAll = 'Clear all';

  /// toolbar
  rsToolBarCustomizeToolbar = 'Customize the toolbar';
  rsToolBarConfiguration = 'Toolbar Configuration';
  rsToolBarAvailableCommands = 'Available commands';
  rsToolBarCommands = 'Toolbar commands';
  rsToolBarRestore = 'Restore';

  // form timeout
  rsFormTimeoutCaption = '%s (%d seconds remaining...)';

  // frameviewer
  rsFrameViewerSelectAll = 'Select all';
  rsFrameViewerCopy = 'Copy';
  rsFrameViewerCopyAsHtml = 'Copy as HTML';

implementation

uses
  SysUtils, LCLTranslator;

procedure Translate(const aDirectory, aLang: string);
var
  vDir: TFileName;
begin
  vDir := IncludeTrailingPathDelimiter(aDirectory);
  TranslateUnitResourceStringsEx(
    aLang, vDir, 'tis.ui.resourcestrings.po', 'tis.ui.resourcestrings');
  TranslateUnitResourceStringsEx(
    aLang, vDir, 'lclstrconsts.po', 'lclstrconsts');
end;

end.

