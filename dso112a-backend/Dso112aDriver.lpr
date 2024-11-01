(* Lazarus+FPC 2.0.0+3.0.4 on Linux Lazarus+FPC 2.0.0+3.0.4 on Linux Lazarus+FP *)

library Dso112aDriver;

(* This is the backend (instrument driver) of a program to capture data from    *)
(* and possibly control an oscilloscope such as the JYE Tech DSO112A. Generally *)
(* speaking the supported instruments use an RS232 serial interface although    *)
(* this might be hidden with the physical connection being USB.                 *)
(*                                                                              *)
(* The instrument driver may call entry points in the frontend using either     *)
(* static or dynamic linkage (as selected for the frontend interface to the     *)
(* backend), both styles of linkage copy menu entries from an instrument-       *)
(* specific form to the main one.                               MarkMLl.        *)

{$mode objfpc}{$H+}

(* If this is built as a separate project then assume that the linkage mode     *)
(* must be dynamic: if it were static then it wouldn't be a separate project.   *)
(* The choice between static and dynamic linkage is relevant in other units so  *)
(* that the correct code to interface with the frontend in generated, but not   *)
(* here.                                                                        *)

{$ifndef USE_DYNAMIC         }
  {$error Requires -dUSE_DYNAMIC in Project -> Compiler -> Other -> Custom Options }
{$endif USE_DYNAMIC          }
{$ifdef USE_STATIC           }
  {$error Project is for a dynamically-linked instrument driver, static linkage is not appropriate. }
{$endif USE_STATIC           }

(* Note manual addition of cmem below, this is required to allow strings and    *)
(* objects to be shared/transferred between the main program and a shared       *)
(* library. Note also relative position of HeapTrc, if cmem is used it is not   *)
(* possible to specify this at the project level (i.e. use FPC's -gh option).   *)

uses
{$ifdef USE_CMEM }
  cmem, {$ifdef USE_HEAPTRC } HeapTrc, {$endif }
{$endif }
  Interfaces, // this includes the LCL widgetset
  Forms, DsoBackendCode;

(* Keep things simple by not importing Frontend/FrontendDynamic etc. here, the  *)
(* appropriate unit is selected in DsoBackendCode.                              *)

{$R *.res}

exports SharedLibraryMagicNumber, SharedLibraryBindMainProgramExports,
                SharedLibraryBindMainProgramError, LoadConfiguration,
                BackendAboutText, BackendMenuFile, BackendMenuConfig,
                BackendMenuHelp, PortName, PortDriverName,
                PortProductDescription, PortSerialDescription,
                PortDefaultParams, ParseRawToScopeStruct,
                ParseTestToScopeStruct, ModuleAboutText;

end.

