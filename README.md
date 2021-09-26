# Tranquil IT Components

All components created at Tranquil IT https://www.tranquil.it

Copyright (C) 2012 - 2021 — All Rights Reserved.

## Components

Most components are related to User Interface.

### TTisGrid Component

Based on TVirtualStringTree grid, this grid works with JSON and mORMot's TDocVariantData for databackend.

Main Features:

- 

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
- autocomplete with properties do define items, sorted, style, etc

<img src="./images/tistageditor.png">

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

