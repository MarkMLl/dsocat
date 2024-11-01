(* Lazarus+FPC 2.1.0+3.2.0 on Linux Lazarus+FPC 2.1.0+3.2.0 on Linux Lazarus+FP *)

unit BackendDynamic; (* Describes backend to frontend, located in frontend directory *)

(* This is an alternative to the Backend unit, for use when dynamic linkage is  *)
(* used to allow one of several backends to be selected, or a backend reloaded  *)
(* without interrupting the frontend program. It will probably be necessary to  *)
(* add both ../common and ../backend to the frontend's unit files search path   *)
(* (-Fu) noting that ../backend might be a symlink to a default or dummy        *)
(* backend instrument driver, static linkage in particular implies a rather     *)
(* convoluted dependency chain and if anything odd appears to be happening      *)
(* (e.g. source display during debugging looks OK but the assembler listing     *)
(* doesn't track sourcecode changes) start off by using the Lazarus IDE's       *)
(* "Clean Directory" facility.                                                  *)
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

{$ifdef USE_STATIC }
  {$error Dynamic backend unit is being used when the linkmode is static. }
{$endif USE_STATIC }

uses
  Classes, SysUtils, DynaMod, Menus, Serial, ScopeStruct;

(* These are the type descriptions of fields used to hold references to the
  methods listed in backendprocs.inc (below). It is important that these types
  be considered subordinate to the definitions in the include file, since as
  well as being used here the include file is treated by the backend shared
  library as the authoritative list of what it is to export.
*)
type
  TBackendMagicNumber= function(): longword;
  TBackendBindFrontendExports= function(): longword;
  TBackendBindFrontendError= function(): string;
  TBackendLoadConfiguration= function(base: string= ''): boolean;
  TBackendAboutText= function(): string;
  TBackendMenuFile= function(): TMenuItem;
  TBackendMenuConfig= function(): TMenuItem;
  TBackendMenuHelp= function(): TMenuItem;
  TPortName= function(): string;
  TPortDriverName= function(): string;
  TPortProductDescription= function(): string;
  TPortSerialDescription= function(): string;
  TPortDefaultParams= function(out BitsPerSec: LongInt; out ByteSize: Integer;
                                          out Parity: TParityType; out StopBits: Integer;
                                          out Flags: TSerialFlags): boolean;
  TParseRawToScopeStruct= function(raw: TStringList): TScopeStruct;
  TParseTestToScopeStruct= function(testType: TTestType): TScopeStruct;

(* This is a one-off exported name to allow the caller to make an explicit
  enquiry as to whether an expected function is implemented- if not, then
  something's wrong. Note that whether module and routine names are case-
  sensitive (and/or whether the toolchain is case-preserving) is operating
  system specific: Linux is both case-sensitive and case-preserving.
*)
const
  BackendMagicNumber_RoutineName= 'SharedLibraryMagicNumber';

type
  TBackend= class(TDynamicModule)
            private
              fEntryCount: integer;
            protected
              FBackendMagicNumber: TBackendMagicNumber;
              FBackendBindFrontendExports: TBackendBindFrontendExports;
              FBackendBindFrontendError: TBackendBindFrontendError;
              FBackendLoadConfiguration: TBackendLoadConfiguration;
              FBackendAboutText: TBackendAboutText;
              FBackendMenuFile: TBackendMenuFile;
              FBackendMenuConfig: TBackendMenuConfig;
              FBackendMenuHelp: TBackendMenuHelp;
              FPortName: TPortName;
              FPortDriverName: TPortDriverName;
              FPortProductDescription: TPortProductDescription;
              FPortSerialDescription: TPortSerialDescription;
              FPortDefaultParams: TPortDefaultParams;
              FParseRawToScopeStruct: TParseRawToScopeStruct;
              FParseTestToScopeStruct: TParseTestToScopeStruct;
              constructor Create;
              destructor Destroy2;
            public
              procedure Destroy;
{$undef BACKEND_MAGIC_NUMBER   }
{$define BACKEND_ENTRY_POINTS  }
{$undef BACKEND_POSTLOAD_PROC  }
{$i backendprocs.inc           }
              function FrontendMagicNumber: longword;
              property EntryCount: integer read fEntryCount;
            end;

(* There is a single instance of TBackend, enforce read-only access to it.
*)
function Backend: TBackend;

(* Generate a backend shared-library name, load it, and confirm basic operation
  returning false on error.
*)
function BackendLoad: boolean;

(* Restore to initial state, i.e. among other things unload the backend if
  loaded.
*)
procedure BackendUnload;

(* As an indivisible operation, unload the current backend and load a
  replacement.
*)
function BackendReload: boolean;

(* Without interfering with normal operation, load a candidate backend and
  check that its magic number etc. is OK.
*)
function BackendGood: boolean;

(* Return true if a backend shared-library is loaded.
*)
function BackendIsLoaded: boolean;

(* After the successful completion of BackendLoad, perform any application-
  specific initialisation. This used to be in BackendLoad but was moved out to
  allow checks for magic numbers etc. to be completed, the corresponding
  termination code has been left in backendUnload.
*)
procedure BackendPostLoad(projectName: string; lastButton: integer= -1);

(* Return the name of a new backend file if available, otherwise blank.
*)
function BackendUpdated: string;

(* Last error message, non-destructive.
*)
function BackendError: string;


implementation

uses
  Forms, DsoFrontendCode;

{$MACRO ON}
{$DEFINE MAINFORM__:= FormDsoCat }

const   libNameExt= '.so';

var
  xBackend: TBackend= nil;
  backendName: string= '';


(* There is a single instance of TBackend, enforce read-only access to it.
*)
function Backend: TBackend;

begin
  result := xBackend
end { Backend } ;


  //############################################################################
 //      1         2         3         4         5         6         7         8
// 45678901234567890123456789012345678901234567890123456789012345678901234567890


function frontToBack1(const str: string): string;

const
  pattern= 'frontend';
  lower1= 'front';
  lower2= 'back';
  upper1= 'Front';
  upper2= 'Back';

var     i: integer;

begin
  result := str;
  i := Pos(pattern, LowerCase(result));
  if i > 0 then
    if result[i] in ['a'..'z'] then begin
      Delete(result, i, Length(lower1));
      Insert(lower2, result, i)
    end else begin
      Delete(result, i, Length(upper1));
      Insert(upper2, result, i)
    end
end { frontToBack1 } ;


function frontToBack2(const str: string): string;

const
  pattern= 'dsocat';
  lower1= 'dsocat';
  lower2= 'dsobackend';
  upper1= 'DsoCat';
  upper2= 'DsoBackend';

var     i: integer;

begin
  result := str;
  i := Pos(pattern, LowerCase(result));
  if i > 0 then
    if result[i] in ['a'..'z'] then begin
      Delete(result, i, Length(lower1));
      Insert(lower2, result, i)
    end else begin
      Delete(result, i, Length(upper1));
      Insert(upper2, result, i)
    end
end { frontToBack2 } ;


function getBackendDirectory(): string;

var     directory: string;

begin
  directory := ExtractFileDir(ParamStr(0));
  result := frontToBack1(directory)
end { getBackendDirectory } ;


function getBackendLibNameBase(): string;

var     name: string;

begin
  name := ExtractFileName(ParamStr(0));
  result := frontToBack2(name);
end { getBackendLibNameBase } ;


function getBackendDirectoryAndBase(): string;

begin
  result := getBackendDirectory() + '/' + getBackendLibNameBase()
end { getBackendDirectoryAndBase } ;


(* Create and return a list of visible backend shared libraries, assuming ISO-  *)
(* format timestamps.                                                           *)
//
function getSortedLibNames(): TStringList;

var
  pattern: string;
  searchRec : TSearchRec;

begin
  result := nil;
  pattern := getBackendDirectoryAndBase() + '*' + libNameExt;
  try
    if FindFirst (pattern, faAnyFile , searchRec) = 0 then begin
      result := TStringList.Create;
      result.Sorted := true;
      repeat
        result.Append(searchRec.Name)
      until FindNext(searchRec) <> 0
    end
  finally
    findClose(searchRec)
  end
end { getSortedLibNames } ;


(* Get the name of the most-recently-created backend shared library, assuming
  ISO-format timestamps. There is no provision here for appended CPU type etc.
*)
function libName(): string;

var     libNames: TStringList= nil;

begin
  result := getBackendDirectoryAndBase() + libNameExt;
  libNames := getSortedLibNames();
  if libNames <> nil then begin
    if libNames.Count > 0 then
      result := getBackendDirectory() + '/' + libNames[libNames.Count - 1];
    libNames.Free
  end
end { libName } ;


(* This is called by the main thread and waits until any state machines etc.
  are in a quiescent state. This is really only relevant for multithreaded
  programs, but the check is probably worth making in all cases; it will very
  often be implemented deeper into application-specific code.
*)
procedure WaitForBackendQuiescent;

begin
  if Assigned(backend) then
    while not Backend.EntryCount = 0 do
      Application.ProcessMessages
end { WaitForBackendQuiescent } ;


(* Generate a backend shared-library name, load it, and confirm basic operation
  returning false on error.
*)
function BackendLoad: boolean;

begin
  WaitForBackendQuiescent;
  Assert(xBackend = nil);
  xBackend := TBackend.Create;
  result := Assigned(xBackend) and xBackend.ModuleExists
end { BackendLoad } ;


(* Restore to initial state, i.e. among other things unload the backend if
  loaded.
*)
procedure BackendUnload;


  procedure deepDelete(mi: TMenuItem);

  var   i: integer;
        temp: TMenuItem;

  begin
    with mi do begin                    (* I'm not entirely happy with this...  *)
      i := Count - 1;
      while i >= 0 do begin
        if Items[i].Tag = PtrInt(xBackend) then begin (* Using PtrUInt here would be better *)
          deepDelete(Items[i]);
          temp := Items[i];
          Delete(i);
          temp.Free
        end;
        i -= 1
      end
    end
  end { deepDelete } ;


begin
  WaitForBackendQuiescent;

(* For each standard menu item, do a depth-first traversal looking for tags     *)
(* that have come from the backend and delete the corresponding entries. Mark   *)
(* the enabled state of the item itself from its tag.                           *)

  deepDelete(MAINFORM__.MenuItemFile);
  MAINFORM__.MenuItemFile.Enabled := MAINFORM__.MenuItemFile.Tag <> 0;
  MAINFORM__.MenuItemFile.Tag := 0;

  deepDelete(MAINFORM__.MenuItemConfig);
  MAINFORM__.MenuItemConfig.Enabled := MAINFORM__.MenuItemConfig.Tag <> 0;
  MAINFORM__.MenuItemConfig.Tag := 0;

  deepDelete(MAINFORM__.MenuItemHelp);
  MAINFORM__.MenuItemHelp.Enabled := MAINFORM__.MenuItemHelp.Tag <> 0;
  MAINFORM__.MenuItemHelp.Tag := 0;
  FreeAndNil(xBackend)                  (* Should use the Destroy2 destructor   *)
end { BackendUnload } ;


(* As an indivisible operation, unload the current backend and load a           *)
(* replacement.                                                                 *)
//
function BackendReload: boolean;

begin
  WaitForBackendQuiescent;
  BackendUnload;
  result := BackendLoad
end { BackendReload } ;


(* Without interfering with normal operation, load a candidate backend and
  check that its magic number etc. is OK.
*)
function BackendGood: boolean;

var     candidate: TBackend;
        savedBackendName: string;

begin
  result := false;
  savedBackendName := backendName;
  candidate := TBackend.Create;         (* Finds most recent name               *)
  try
    if not candidate.RoutineExists(BackendMagicNumber_RoutineName) then
      exit;
    if candidate.SharedLibraryMagicNumber <> candidate.FrontendMagicNumber then
      exit;
    if candidate.SharedLibraryBindMainProgramExports = 0 then
      exit
  finally
    candidate.Destroy2;
    backendName := savedBackendName
  end;
  result := true
end { BackendGood } ;


(* Return true if a backend shared-library is loaded.
*)
function BackendIsLoaded: boolean;

begin
  result := false;
  if Assigned(xBackend) then
    result := xBackend.ModuleInMemory
end { BackendIsLoaded } ;


(* After the successful completion of BackendLoad, perform any application-     *)
(* specific initialisation. This used to be in BackendLoad but was moved out to *)
(* allow checks for magic numbers etc. to be completed, the corresponding       *)
(* termination code has been left in BackendUnload.                             *)
//
{$undef BACKEND_MAGIC_NUMBER   }
{$undef BACKEND_ENTRY_POINTS   }
{$define BACKEND_POSTLOAD_PROC }
{$i backendprocs.inc           }


(* Return the name of a new backend file if available, otherwise blank.
*)
function BackendUpdated: string;

var
  newestBackend: string;

begin
  result := '';
  if backendName = '' then
    exit;
  newestBackend := libName;
  if newestBackend <> backendName then
    result := newestBackend
end { BackendUpdated } ;


(* Last error message, non-destructive.
*)
function BackendError: string;

begin
  if not Assigned(xBackend) then
    result := '[Backend object not created]'
  else
    result := xBackend.LastError
end { BackendError } ;


  //############################################################################
 //      1         2         3         4         5         6         7         8
// 45678901234567890123456789012345678901234567890123456789012345678901234567890


constructor TBackend.Create;

var     scratch: string;

begin
  scratch := libName();
  inherited Create(scratch);
  backendName := scratch;
  fEntryCount := 0;
  FBackendMagicNumber := nil;
  FBackendBindFrontendExports := nil;
  FBackendBindFrontendError := nil;
  FBackendLoadConfiguration := nil;
  FBackendAboutText := nil;
  FBackendMenuFile := nil;
  FBackendMenuConfig := nil;
  FBackendMenuHelp := nil;
  FPortName := nil;
  FPortDriverName := nil;
  FPortProductDescription := nil;
  FPortSerialDescription := nil;
  FPortDefaultParams := nil;
  FParseRawToScopeStruct := nil;
  FParseTestToScopeStruct := nil
end { TBackend.Create } ;


(* This is the actual destructor, only available locally.
*)
destructor TBackend.Destroy2;

begin
  backendName := '';
  inherited Destroy
end { TBackend.Destroy2 } ;


(* This dummy is an attempt to prevent a caller destroying the backend object
  via the function interface.
*)
procedure TBackend.Destroy;

begin
end { TBackend.Destroy } ;


(* Return a 32-bit unsigned magic number, this does not require any action from
  the memory manager. Despite being generated by backend code, this should be
  changed to track the functions and parameters described by backendprocs.inc,
  which is to be considered definitive.
*)
function TBackend.SharedLibraryMagicNumber: longword;

begin
  InterlockedIncrement(fEntryCount);
  try
    LoadRoutine(pointer(FBackendMagicNumber), BackendMagicNumber_RoutineName);
    result := FBackendMagicNumber()
  finally
    InterlockedDecrement(fEntryCount);
    Assert(fEntryCount >= 0)
  end
end { TBackend.SharedLibraryMagicNumber } ;


(* This is the magic number from backendprocs.inc at the time the frontend was
  built, it should match the one returned by the backend shared library.
*)
function TBackend.FrontendMagicNumber: longword;

{$define BACKEND_MAGIC_NUMBER  }
{$undef BACKEND_ENTRY_POINTS   }
{$undef BACKEND_POSTLOAD_PROC  }
{$i backendprocs.inc           }

begin
  result := BackendMagicNumber
end { TBackend.FrontendMagicNumber } ;


(* This is called by the frontend, but implemented by the backend. Tell the
  backend to do whatever's necessary to get access to entry points exported by
  the frontend, at least up to the point where it checks that magic numbers
  match. Return the frontend's magic number if this is OK, except that zero is
  reserved to indicate that a formatted error message is available via the
  SharedLibraryBindMainProgramError function.
*)
function TBackend.SharedLibraryBindMainProgramExports: longword;

begin
  InterlockedIncrement(fEntryCount);
  try
    LoadRoutine(pointer(FBackendBindFrontendExports), 'SharedLibraryBindMainProgramExports');
{$ifdef USE_DYNAMIC_DEBUG }
    WriteLn(StdErr, 'About to make indirect call via FBackendBindFrontendExports.');
    Flush(StdErr);
{$endif USE_DYNAMIC_DEBUG }
    result := FBackendBindFrontendExports();
{$ifdef USE_DYNAMIC_DEBUG }
    WriteLn(StdErr, 'Returned from indirect call via FBackendBindFrontendExports.');
    Flush(StdErr)
{$endif USE_DYNAMIC_DEBUG }
  finally
    InterlockedDecrement(fEntryCount);
    Assert(fEntryCount >= 0)
  end
end { TBackend.SharedLibraryBindMainProgramExports } ;


(* Read and clear any error message from BackendBindFrontendExports.
*)
function TBackend.SharedLibraryBindMainProgramError: string;

begin
  InterlockedIncrement(fEntryCount);
  try
    LoadRoutine(pointer(FBackendBindFrontendError), 'SharedLibraryBindMainProgramError');
    result := FBackendBindFrontendError()
  finally
    InterlockedDecrement(fEntryCount);
    Assert(fEntryCount >= 0)
  end
end { TBackend.SharedLibraryBindMainProgramError } ;


(* This is normally called automatically when the backend is loaded, but may
  also be invoked on e.g. receipt of a HUP.
*)
function TBackend.LoadConfiguration(base: string= ''): boolean;

begin
  InterlockedIncrement(fEntryCount);
  try
    LoadRoutine(pointer(FBackendLoadConfiguration), 'LoadConfiguration');
    result := FBackendLoadConfiguration(base)
  finally
    InterlockedDecrement(fEntryCount);
    Assert(fEntryCount >= 0)
  end
end { TBackend.LoadConfiguration } ;


(* Return a string containing the library's "About" text, hopefully including
  build date and time and Subversion release.

  This is actually a pointer to the library's heap, so it is essential that
  this is shared with the main program- typically by use of the cmem library.
*)
function TBackend.BackendAboutText: string;

begin
  InterlockedIncrement(fEntryCount);
  try
    LoadRoutine(pointer(FBackendAboutText), 'BackendAboutText');
    result := FBackendAboutText()
  finally
    InterlockedDecrement(fEntryCount);
    Assert(fEntryCount >= 0)
  end
end { TBackend.BackendAboutText } ;


(* Return an object containing a TMenuItem, possibly with children.

  This is actually a pointer to the library's heap, so it is essential that
  this is shared with the main program- typically by use of the cmem library.
*)
function TBackend.BackendMenuFile: TMenuItem;

begin
  InterlockedIncrement(fEntryCount);
  try
    LoadRoutine(pointer(FBackendMenuFile), 'BackendMenuFile');
    result := FBackendMenuFile()
  finally
    InterlockedDecrement(fEntryCount);
    Assert(fEntryCount >= 0)
  end
end { TBackend.BackendMenuFile } ;


(* Return an object containing a TMenuItem, possibly with children.

  This is actually a pointer to the library's heap, so it is essential that
  this is shared with the main program- typically by use of the cmem library.
*)
function TBackend.BackendMenuConfig: TMenuItem;

begin
  InterlockedIncrement(fEntryCount);
  try
    LoadRoutine(pointer(FBackendMenuConfig), 'BackendMenuConfig');
    result := FBackendMenuConfig()
  finally
    InterlockedDecrement(fEntryCount);
    Assert(fEntryCount >= 0)
  end
end { TBackend.BackendMenuConfig } ;


(* Return an object containing a TMenuItem, possibly with children.

  This is actually a pointer to the library's heap, so it is essential that
  this is shared with the main program- typically by use of the cmem library.
*)
function TBackend.BackendMenuHelp: TMenuItem;

begin
  InterlockedIncrement(fEntryCount);
  try
    LoadRoutine(pointer(FBackendMenuHelp), 'BackendMenuHelp');
    result := FBackendMenuHelp()
  finally
    InterlockedDecrement(fEntryCount);
    Assert(fEntryCount >= 0)
  end
end { TBackend.BackendMenuHelp } ;


  //############################################################################
 //      1         2         3         4         5         6         7         8
// 45678901234567890123456789012345678901234567890123456789012345678901234567890


(* Return a pattern for the type of port the device is expected to be connected
  to, e.g. ttyUSB for most USB-connected serial ports. Return blank to test
  every port.
*)
function TBackend.PortName(): string;

begin
  InterlockedIncrement(fEntryCount);
  try
    LoadRoutine(pointer(FPortName), 'PortName');
    result := FPortName()
  finally
    InterlockedDecrement(fEntryCount);
    Assert(fEntryCount >= 0)
  end
end { TBackend.PortName } ;


(* Return the expected driver (kernel module) name, as found in the /sys tree.
  Return blank for no tests.
*)
function TBackend.PortDriverName(): string;

begin
  InterlockedIncrement(fEntryCount);
  try
    LoadRoutine(pointer(FPortDriverName), 'PortDriverName');
    result := FPortDriverName()
  finally
    InterlockedDecrement(fEntryCount);
    Assert(fEntryCount >= 0)
  end
end { TBackend.PortDriverName } ;


(* Return the device product description, as found in the /sys tree. Return
  blank for no tests.
*)
function TBackend.PortProductDescription(): string;

begin
  InterlockedIncrement(fEntryCount);
  try
    LoadRoutine(pointer(FPortProductDescription), 'PortProductDescription');
    result := FPortProductDescription()
  finally
    InterlockedDecrement(fEntryCount);
    Assert(fEntryCount >= 0)
  end
end { TBackend.PortProductDescription } ;


(* Return the device serial number, as found in the /sys tree. Note that this
  is a text string of undefined format i.e. cannot be assumed to be numeric or
  even a number expressed in some customary base, return blank for no tests.
*)
function TBackend.PortSerialDescription(): string;

begin
  InterlockedIncrement(fEntryCount);
  try
    LoadRoutine(pointer(FPortSerialDescription), 'PortSerialDescription');
    result := FPortSerialDescription()
  finally
    InterlockedDecrement(fEntryCount);
    Assert(fEntryCount >= 0)
  end
end { TBackend.PortSerialDescription } ;


(* Return the parameters to be used by default when setting up the serial port.
  If this returns true then assume that using these are mandatory since the
  instrument has no accessible controls for Baud rate etc.
*)
function TBackend.PortDefaultParams(out BitsPerSec: LongInt; out ByteSize: Integer;
                                        out Parity: TParityType; out StopBits: Integer;
                                        out Flags: TSerialFlags): boolean;

begin
  InterlockedIncrement(fEntryCount);
  try
    LoadRoutine(pointer(FPortDefaultParams), 'PortDefaultParams');
    result := FPortDefaultParams(BitsPerSec, ByteSize, Parity, StopBits, Flags)
  finally
    InterlockedDecrement(fEntryCount);
    Assert(fEntryCount >= 0)
  end
end { TBackend.PortDefaultParams } ;


(* Parse and delete the parameter, returning an object representing the
  instrument state or nil on error.
*)
function TBackend.ParseRawToScopeStruct(raw: TStringList): TScopeStruct;

begin
  InterlockedIncrement(fEntryCount);
  try
    LoadRoutine(pointer(FParseRawToScopeStruct), 'ParseRawToScopeStruct');
    result := FParseRawToScopeStruct(raw)
  finally
    InterlockedDecrement(fEntryCount);
    Assert(fEntryCount >= 0)
  end
end { TBackend.ParseRawToScopeStruct } ;


(* Generate test data, adhering as closely as possible to the behaviour of
  ParseRawToScopeStruct() above.
*)
function TBackend.ParseTestToScopeStruct(testType: TTestType): TScopeStruct;

begin
  InterlockedIncrement(fEntryCount);
  try
    LoadRoutine(pointer(FParseTestToScopeStruct), 'ParseTestToScopeStruct');
    result := FParseTestToScopeStruct(testType)
  finally
    InterlockedDecrement(fEntryCount);
    Assert(fEntryCount >= 0)
  end
end { TBackend.ParseTestToScopeStruct } ;


{$ifdef USE_DYNAMIC_DEBUG }

const
  testFrontendMagicNumber_RoutineName= 'MainProgramMagicNumber';

type
  testTFrontendMagicNumber= function(): longword;

  testTFrontend= class(TDynamicModule)
                 protected
                   FFrontendMagicNumber: testTFrontendMagicNumber;
                 public
                   function MainProgramMagicNumber: longword;
                 end;


function testTFrontend.MainProgramMagicNumber: longword;

begin
  LoadRoutine(pointer(FFrontendMagicNumber), testFrontendMagicNumber_RoutineName);
  if LastError <> '' then
    WriteLn(StdErr, LastError)
  else
    WriteLn(StdErr, '[LoadRoutine() OK]');
  WriteLn(StdErr, 'About to make indirect call via MainProgramMagicNumber.');
  Flush(StdErr);

  result := FFrontendMagicNumber();

  WriteLn(StdErr, 'Returned from indirect call via MainProgramMagicNumber.');
  Flush(StdErr)
end { testTFrontend.MainProgramMagicNumber } ;


(* If this doesn't work, then it's most unlikely that a backend driver will be
  able to link dynamically to the frontend. What's more, once control has been
  transferred into a dynamically-linked backend debugging will be vastly more
  difficult.
*)
procedure testFrontendDynamicAccess;

var
  xFrontend: testTFrontEnd;

begin
  xFrontend := testTFrontEnd.Create('');
  xFrontend.LoadModule;
  try
    WriteLn(StdErr, 'Main program magic number: ', xFrontend.MainProgramMagicNumber);
    Flush(StdErr)
  finally
    xFrontend.Destroy
  end
end { testFrontendDynamicAccess } ;


initialization
  testFrontendDynamicAccess
{$endif USE_DYNAMIC_DEBUG }
finalization
  if xBackend <> nil then
    xBackend.Destroy2
end.

