
Alternative Excel VBA Framework
===============================

QuantLibXL version 0.9.6 shipped with an Excel VBA Framework.
This directory contains an alternative implementation of an Excel VBA Framework.
The relative strengths of the two projects are:

- 0.9.6 Framework: More stable, more business functionality
- Alternative Framework: More modular design

Ideally the two projects should be merged at some point.

Prerequisites
=============

This demo requires Excel 2003.

Starting the application
========================

Start an empty Excel session and load QuantLibXL.xla.  Invoke menu item
QuantLibXL -> Load -> Test, this should load the XLL and a test workbook.

Creating a shortcut (link, icon)
================================

Start an empty Excel session and load create_shortcut.xla.  A dialog is
displayed, accept the defaults and click OK.  This should create file
QuantLibXL.lnk in the application root directory (the same directory that
contains this ReadMe.txt file).  Now you can use the shortcut to launch a new
standalone Excel session into which the VBA environment is automatically loaded.

Context sensitive menus
=======================

The Framework adds a QuantLibXL menu to Excel's main menu bar and also to the
menu which appears when the user right-clicks in a cell.  The contents of these
menus changes depending on which book, sheet, and range is active.

Start the application and load the demo.  Activate book Demo.xls, this workbook
provides an example of context sensitive menus.

Workbook Design
===============

To add a new worksheet that is owned and recognized by the application, do
QuantLibXL -> Development -> Insert New Worksheet.  Formatting of your new sheet
is controlled with styles.  Excel's style dropdown is hidden by default - right
click on a toolbar, select "Customize...", select "Format", locate the Style
dropdown and drag it onto a toolbar.  All of the styles recognized by the
Framework are present in the template worksheet.

Each sheet contains hidden ("program") rows and columns where
application-specific calculations may be implemented, hit Ctrl-Shift-H to
hide/unhide these ranges.

The application's ownership of books and sheets is controlled with names, do
QuantLibXL -> Development -> Manage Names to configure these values.  Edit
Menu.xla to control which menu items are visible for a given sheet or book.

Loader
======

The application allows different environments to be configured, the files
required by each environment can be specified and dependencies are supported.
This is implemented in Loader.xla.

Keyboard Shortcuts
==================

These are configured in Keyboard.xla:

Ctrl-Shft-E : display the error message, if any, that is associated with the
              active range.
Ctrl-Shft-H : hide/unhide program columns and rows
Ctrl-Shft-R : reset the application.  Stopping  the app in the debugger
              disables the menus and this keystroke combination restarts them.

Code Comments
=============

Each VBA addin includes a comment block in the ThisWorkbook object summarizing
the module's purpose.

