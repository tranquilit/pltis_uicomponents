# Tranquil IT Components

All components created at Tranquil IT https://www.tranquil.it

Copyright (C) 2012 - 2021 — All Rights Reserved.

## Components

Most components are related to User Interface.

### TTisGrid Component

Based on TVirtualStringTree grid, this grid works with JSON and mORMot's TDocVariantData at the backend.

Main Features:

- JSON/TDocVariantData data backend
- design time component Editor to configure title, name, datatype, width, etc, either using sample JSON data from clipboard or Add/Remove column buttons
- autoconfigure itself, if a TDocVariantData is assigned to grid.Data property
- handle both expanded or compact JSON data from mORMot providers (array of arrays or array of json objects)
- user can hide/show columns in runtime
- user can save state/configuration
- Popup menu for Search, Copy line/cell, delete/select rows, export to CSV file
- user can customize Popup menu, either hide/show some itens or include new ones
- user can paste JSON directly on grid

<img src="./images/tisgrid.png">

### TTisTagEditor Component

Based on Andreas Rejbrand's work, this is our modified, improved, and Lazarus converted version.

Main Features:

- tags can be created at runtime by typing, of course :)
- tags can be created by the Tags property
- custom colors can be defined for each tag
- for each tag a "hidden" Variant value can be added
- allows drag and drop tags
- each tag can have a "X" that allows you to delete the tag with 1 click
- a custom icon can be defined to replace the "X"
- custom events, such Before/After add/delete, for handling user actions
- input properties to define forbidden chars, max tags, allow duplicates, etc
- autocomplete option, with properties to define a list of items, style, search ascending/case sensitive, etc

<img src="./images/tistageditor.png">

### TTisControlSynEditor Component

Based on Lazarus TSynEdit component, this is a modified version specifically made for our WAPT Packages control files edition.
It mainly allows to add markups based on control keys.

Main Features:

- Error markups: Set all the line with red waves under the value (one per key)
- Info markups: Highlight a specific value with linked to a control key (multiple per key)
- Custom colors can be defined for each info
- Find starting position of given control key
- Find current control key linked to the cursor position

<img src="./images/tiscontrolsyneditor.png">

### TTisLvlGraphControl Component

Based on TLvlGraphControl component, this is a modified version made for our Wapt dependencies visualization.

Main Features:

- Possibility to zoom in/out
- Have the mouse right click used to select
- Have a specific PopupMenu on Nodes
- Allow to have selected/focused nodes
- Keep the focused node in the visible part of the graph
- Each Node can have two images instead of one
- Highlight in and/or out edges of selected nodes
- Drag the graph with the middle mouse button pressed

<img src="./images/tislvlgraphcontrol.png">

### TTisSearchEdit Component

Based on simple TEdit component, this component can be used to do searching asynchronously for do not freeze the UI.

Main Features:

- AutoSearch property for enable/disable asynchronous search
- Buttons collection, which enables you to add any "helper buttons" you want
- Button Kind property, it has default implementations or bkCustom to do whatever you want
- SearchInterval property in milliseconds for delaying the search
- OnSearch event that has the logic for searching

<img src="./images/tissearchedit.png">

## Dependencies

First of all, you should have these libraries installed in your environment:
- [mORMot 2](https://github.com/synopse/mORMot2) — An Open Source Client-Server ORM/SOA framework in modern Object Pascal
- [pltis_utils](https://github.com/tranquilit/pltis_utils) — utilities 
- [virtualtreeview_package](https://gitlab.com/freepascal.org/lazarus/lazarus/-/tree/main/components/virtualtreeview)

On Lazarus it has been tested using these versions:

- FPC 3.2.0 win32/win64
- Lazarus 2.0.12 r64642

## Demo

You may want to take a look in the [demo project](./demo) to see each component working!

