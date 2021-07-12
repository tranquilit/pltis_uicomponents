# Tranquil IT Core Units

## Folder Content

This folder hosts the Core Units.

## Core Units

With "Core Units", we mean units implementing shared basic functionality of our framework:

- Uncoupled reusable bricks to process JSON;
- Other higher level features;
- Cross-Platform and Cross-Compiler: ensure the same code would compile on both FPC and Delphi, on any support platform, regardless the RTL, Operating System, or CPU.

## Units Presentation

### tis.core.os

Cross-platform functions shared by all framework units
- Clipboard adapter for JSON

Aim of this unit is to centralize most used OS-specific API calls, like a `SysUtils` unit on steroids, to avoid `$ifdef/$endif` in "uses" clauses.

In practice, no "Windows", nor "Linux/Unix" reference should be needed in regular units, once `tis.core.os` is included.

