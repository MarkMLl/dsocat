(* Lazarus+FPC 2.1.0+3.2.0 on Linux Lazarus+FPC 2.1.0+3.2.0 on Linux Lazarus+FP *)

unit Backend; (* Describes backend to frontend, located in frontend directory   *)

(* This is an alternative to the BackendDynamic unit, for use when static       *)
(* linkage is used to simplify debugging. It will probably be necessary to add  *)
(* both ../common and ../backend to the frontend's unit files search path (-Fu) *)
(* noting that ../backend might be a symlink to a default or dummy backend      *)
(* instrument driver, static linkage in particular implies a rather convoluted  *)
(* dependency chain and if anything odd appears to be happening (e.g. source    *)
(* display during debugging looks OK but the assembler listing doesn't track    *)
(* sourcecode changes) start off by using the Lazarus IDE's "Clean Directory"   *)
(* facility.                                                                    *)
(*                                                                              *)
(* A major role of this unit is to ensure that the (backend magic number        *)
(* encoded in the) DsoBackendCode unit is the same both when it is compiled     *)
(* into the main program and when it is compiled into a dynamically-loaded      *)
(* instrument driver, and to fail gracefully if there is a mismatch. There is   *)
(* also a comparable check for the frontend magic number.       MarkMLl.        *)

(* NOTE ON PROGRAM STRUCTURE: The name "Backend" refers either to a statically- *)
(* linked unit, or to an object implemented in the BackendDynamic unit which    *)
(* serves as a wrapper for an on-demand dynamically-loaded library. In either   *)
(* case the frontend may refer to e.g. Backend.SharedLibraryMagicNumber() even  *)
(* though the access semantics are different.                                   *)
(*                                                                              *)
(* Most entry points referred to by Backend or BackendDynamic are in a unit     *)
(* named like xxxBackendCode. If accessed statically this is found via the      *)
(* "backend" directory symlink, if accessed dynamically this is in a shared     *)
(* library (.so) built in the symlink target e.g. dso112a-backend. In either    *)
(* case, xxxBackendCode has an associated form which is used as a placeholder   *)
(* for menu items etc. which are copied onto the main form at runtime.          *)

{$mode objfpc}{$H+}

interface

{$ifdef USE_DYNAMIC }
  {$error Static backend unit is being used when the linkmode is dynamic. }
{$endif USE_DYNAMIC }

uses
  Classes, SysUtils, Menus, Serial, ScopeStruct;

const
  EntryCount= 0;
  BackendLoad= true;
  BackendReload= true;
  BackendGood= true;
  BackendIsLoaded= true;
  BackendUpdated= false;
  BackendError= '';
  BackendMagicNumber_RoutineName= '[Not required]';
  SharedLibraryBindMainProgramError= '[No error]';

(* Assume this is always true since static linkage has succeeded.
*)
function RoutineExists(const name: string): boolean;

(* Restore to initial state, i.e. among other things unload the backend if
  loaded. This probably does nothing in the case of static linkage.
*)
procedure BackendUnload;

(* After the successful completion of BackendLoad, perform any application-
  specific initialisation. This used to be in BackendLoad but was moved out to
  allow checks for magic numbers etc. to be completed, the corresponding
  termination code has been left in backendUnload.
*)
procedure BackendPostLoad(projectName: string; lastButton: integer= -1);

(* Return a 32-bit unsigned magic number, this does not require any action from
  the memory manager. Despite being generated by backend code, this should be
  changed to track the functions and parameters described by backendprocs.inc,
  which is to be considered definitive.
*)
function SharedLibraryMagicNumber: longword;

(* This is called by the frontend, but implemented by the backend. Tell the
  backend to do whatever's necessary to get access to entry points exported by
  the frontend, at least up to the point where it checks that magic numbers
  match. Return the frontend's magic number if this is OK, except that zero is
  reserved to indicate that a formatted error message is available via the
  SharedLibraryBindMainProgramError function.
*)
function SharedLibraryBindMainProgramExports: longword;

(* This is normally called automatically when the backend is loaded, but may
  also be invoked on e.g. receipt of a HUP.
*)
function LoadConfiguration(base: string= ''): boolean;

(* Return a string containing the library's "About" text, hopefully including
  build date and time and Subversion release.

  This is actually a pointer into the library's heap, so it is essential that
  this is shared with the main program- typically by use of the cmem library.
*)
function BackendAboutText: string;

(* Return an object containing a TMenuItem, possibly with children.

  This is actually a pointer into the library's heap, so it is essential that
  this is shared with the main program- typically by use of the cmem library.
*)
function BackendMenuFile: TMenuItem;

(* Return an object containing a TMenuItem, possibly with children; it is the
  caller's responsibility to free this.

  This is actually a pointer into the library's heap, so it is essential that
  this is shared with the main program- typically by use of the cmem library.
*)
function BackendMenuConfig: TMenuItem;

(* Return an object containing a TMenuItem, possibly with children; it is the
  caller's responsibility to free this.

  This is actually a pointer into the library's heap, so it is essential that
  this is shared with the main program- typically by use of the cmem library.
*)
function BackendMenuHelp: TMenuItem;

(* This is the magic number from backendprocs.inc at the time the frontend was
  built, it should match the one returned by the backend shared library.
*)
function FrontendMagicNumber: longword;

  //############################################################################
 //      1         2         3         4         5         6         7         8
// 45678901234567890123456789012345678901234567890123456789012345678901234567890
//
// Entry points above define the convention I am using to interface to both a
// static and dynamic library. Entry points below are application-specific.

(* Return a pattern for the type of port the device is expected to be connected
  to, e.g. ttyUSB for most USB-connected serial ports. Return blank to test
  every port.
*)
function PortName(): string;

(* Return the expected driver (kernel module) name, as found in the /sys tree.
  Return blank for no tests.
*)
function PortDriverName(): string;

(* Return the device product description, as found in the /sys tree. Return
  blank for no tests.
*)
function PortProductDescription(): string;

(* Return the device serial number, as found in the /sys tree. Note that this
  is a text string of undefined format i.e. cannot be assumed to be numeric or
  even a number expressed in some customary base, return blank for no tests.
*)
function PortSerialDescription(): string;

(* Return the parameters to be used by default when setting up the serial port.
  If this returns true then assume that using these are mandatory since the
  instrument has no accessible controls for Baud rate etc.
*)
function PortDefaultParams(out BitsPerSec: LongInt; out ByteSize: Integer;
                                        out Parity: TParityType; out StopBits: Integer;
                                        out Flags: TSerialFlags): boolean;

(* Parse and delete the parameter, returning an object representing the
  instrument state or nil on error.
*)
function ParseRawToScopeStruct(raw: TStringList): TScopeStruct;

(* Generate test data, adhering as closely as possible to the behaviour of
  ParseRawToScopeStruct() above.
*)
function ParseTestToScopeStruct(testType: TTestType): TScopeStruct;


implementation

uses
  Forms, DsoFrontendCode, DsoBackendCode;


(* Assume this is always true since static linkage has succeeded.
*)
function RoutineExists(const name: string): boolean;

begin
  result := true
end { RoutineExists } ;


(* Restore to initial state, i.e. among other things unload the backend if
  loaded. This probably does nothing in the case of static linkage.
*)
procedure BackendUnload;

begin
end { BackendUnload } ;


(* After the successful completion of BackendLoad, perform any application-
  specific initialisation. This used to be in BackendLoad but was moved out to
  allow checks for magic numbers etc. to be completed, the corresponding
  termination code has been left in BackendUnload.
*)
{$undef BACKEND_MAGIC_NUMBER   }
{$undef BACKEND_ENTRY_POINTS   }
{$define BACKEND_POSTLOAD_PROC }
{$i backendprocs.inc           }


(* Return a 32-bit unsigned magic number, this does not require any action from
  the memory manager. Despite being generated by backend code, this should be
  changed to track the functions and parameters described by backendprocs.inc,
  which is to be considered definitive.
*)
function SharedLibraryMagicNumber: longword;

begin
  result := DsoBackendCode.SharedLibraryMagicNumber
end { SharedLibraryMagicNumber } ;


(* This is called by the frontend, but implemented by the backend. Tell the
  backend to do whatever's necessary to get access to entry points exported by
  the frontend, at least up to the point where it checks that magic numbers
  match. Return the frontend's magic number if this is OK, except that zero is
  reserved to indicate that a formatted error message is available via the
  SharedLibraryBindMainProgramError function.
*)
function SharedLibraryBindMainProgramExports: longword;

begin
  result := DsoBackendCode.SharedLibraryBindMainProgramExports // Check frontend magic number?
end { SharedLibraryBindMainProgramExports } ;


(* This is normally called automatically when the backend is loaded, but may
  also be invoked on e.g. receipt of a HUP.
*)
function LoadConfiguration(base: string= ''): boolean;

begin
  result := DsoBackendCode.LoadConfiguration(base)
end { LoadConfiguration } ;


(* Return a string containing the library's "About" text, hopefully including
  build date and time and Subversion release.

  This is actually a pointer into the library's heap, so it is essential that
  this is shared with the main program- typically by use of the cmem library.
*)
function BackendAboutText: string;

begin
  result := DsoBackendCode.BackendAboutText
end { BackendAboutText } ;


(* Return an object containing a TMenuItem, possibly with children.

  This is actually a pointer into the library's heap, so it is essential that
  this is shared with the main program- typically by use of the cmem library.
*)
function BackendMenuFile: TMenuItem;

begin
  result := DsoBackendCode.BackendMenuFile
end { BackendMenuFile } ;


(* Return an object containing a TMenuItem, possibly with children; it is the
  caller's responsibility to free this.

  This is actually a pointer into the library's heap, so it is essential that
  this is shared with the main program- typically by use of the cmem library.
*)
function BackendMenuConfig: TMenuItem;

begin
  result := DsoBackendCode.BackendMenuConfig
end { BackendMenuConfig } ;


(* Return an object containing a TMenuItem, possibly with children; it is the
  caller's responsibility to free this.

  This is actually a pointer into the library's heap, so it is essential that
  this is shared with the main program- typically by use of the cmem library.
*)
function BackendMenuHelp: TMenuItem;

begin
  result := DsoBackendCode.BackendMenuHelp
end { BackendMenuHelp } ;


(* This is the magic number from backendprocs.inc at the time the frontend was
  built, it should match the one returned by the backend shared library.
*)
function FrontendMagicNumber: longword;

{$define BACKEND_MAGIC_NUMBER  }
{$undef BACKEND_ENTRY_POINTS   }
{$undef BACKEND_POSTLOAD_PROC  }
{$i backendprocs.inc           }

begin
  result := BackendMagicNumber
end { FrontendMagicNumber } ;


  //############################################################################
 //      1         2         3         4         5         6         7         8
// 45678901234567890123456789012345678901234567890123456789012345678901234567890
//
// Entry points above define the convention I am using to interface to both a
// static and dynamic library. Entry points below are application-specific.


(* Return a pattern for the type of port the device is expected to be connected
  to, e.g. ttyUSB for most USB-connected serial ports. Return blank to test
  every port.
*)
function PortName(): string;

begin
  result := DsoBackendCode.PortName()
end { PortName } ;


(* Return the expected driver (kernel module) name, as found in the /sys tree.
  Return blank for no tests.
*)
function PortDriverName(): string;

begin
  result := DsoBackendCode.PortDriverName()
end { PortDriverName } ;


(* Return the device product description, as found in the /sys tree. Return
  blank for no tests.
*)
function PortProductDescription(): string;

begin
  result := DsoBackendCode.PortProductDescription()
end { PortProductDescription } ;


(* Return the device serial number, as found in the /sys tree. Note that this
  is a text string of undefined format i.e. cannot be assumed to be numeric or
  even a number expressed in some customary base, return blank for no tests.
*)
function PortSerialDescription(): string;

begin
  result := DsoBackendCode.PortSerialDescription()
end { PortSerialDescription } ;


(* Return the parameters to be used by default when setting up the serial port.
  If this returns true then assume that using these are mandatory since the
  instrument has no accessible controls for Baud rate etc.
*)
function PortDefaultParams(out BitsPerSec: LongInt; out ByteSize: Integer;
                                        out Parity: TParityType; out StopBits: Integer;
                                        out Flags: TSerialFlags): boolean;

begin
  result := DsoBackendCode.PortDefaultParams(BitsPerSec, ByteSize, Parity,
                                                                StopBits, Flags)
end { PortDefaultParams } ;


(* Parse and delete the parameter, returning an object representing the
  instrument state or nil on error.
*)
function ParseRawToScopeStruct(raw: TStringList): TScopeStruct;

begin
  result := DsoBackendCode.ParseRawToScopeStruct(raw)
end { ParseRawToScopeStruct } ;


(* Generate test data, adhering as closely as possible to the behaviour of
  ParseRawToScopeStruct() above.
*)
function ParseTestToScopeStruct(testType: TTestType): TScopeStruct;

begin
  result := DsoBackendCode.ParseTestToScopeStruct(testType)
end { ParseTestToScopeStruct } ;


(* No initialisation or finalisation code so doesn't need to be imported by the *)
(* main unit, which reduces the number of places where the name of the unit has *)
(* to be specified.                                                             *)

end.

