// -----------------------------------------------------------------
//    This file is part of Tranquil IT Software
//    Copyright (C) 2012 - 2022  Tranquil IT https://www.tranquil.it
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
  rsGridShowAllColumns = 'Show all columns';
  rsGridHideAllColumns = 'Hide all columns';
  rsGridRestoreDefaultColumns = 'Restore default columns';

  /// searchedit
  rsSearchEditClearAll = 'Clear all';

  /// toolbar
  rsToolBarCustomizeToolbar = 'Customize the toolbar';
  rsToolBarConfiguration = 'Toolbar Configuration';
  rsToolBarAvailableCommands = 'Available commands';
  rsToolBarCommands = 'Toolbar commands';
  rsToolBarRestore = 'Restore';

implementation

uses
  SysUtils, Translations;

procedure Translate(const aDirectory, aLang: string);
begin
  Translations.TranslateResourceStrings(
    IncludeTrailingPathDelimiter(aDirectory) + 'tis.ui.resourcestrings.%s.po', aLang, '');
end;

end.

