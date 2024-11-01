(* Lazarus+FPC 2.1.0+3.2.0 on Linux Lazarus+FPC 2.1.0+3.2.0 on Linux Lazarus+FP *)

unit FrontendDynamic;

(* This is an alternative to the Frontend unit, for use when dynamic linkage is *)
(* used as an implicit side-effect of dynamic linkage being selected for the    *)
(* interface between the frontend program and backend instrument driver.        *)
(*                                                              MarkMLl.        *)

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Dialogs, Dynamod;

(* These are the type descriptions of fields used to hold references to the     *)
(* methods listed in frontendprocs.inc (below). It is important that these      *)
(* types be considered subordinate to the definitions in the include file,      *)
(* since as well as being used here the include file is treated by the backend  *)
(* shared library as the authoritative list of what it is to export.            *)
//
type
  TFrontendMagicNumber= function(): longword;
  TFrontendMessageDlgOpt= function(const aMsg: string; dlgType: TMsgDlgType;
                          buttons: TMsgDlgButtons; helpCtx: longint): integer;
  TFrontendDebugWrite= procedure(const str: string; ln: boolean= false);
  TFrontendDebugWriteLn= procedure(const str: string= '');
  TFrontendDebugWriteF= procedure(const fmt: string; values: array of const);

(* This is a one-off exported name to allow the caller to make an explicit      *)
(* enquiry as to whether an expected function is implemented- if not, then      *)
(* something's wrong. Note that whether module and routine names are case-      *)
(* sensitive (and/or whether the toolchain is case-preserving) is operating     *)
(* system specific: Linux is both case-sensitive and case-preserving.           *)
//
const
  FrontendMagicNumber_RoutineName= 'MainProgramMagicNumber';

type
  TFrontend= class(TDynamicModule)
             protected
               FFrontendMagicNumber: TFrontendMagicNumber;
               FFrontendMessageDlgOpt: TFrontendMessageDlgOpt;
               FFrontendDebugWrite: TFrontendDebugWrite;
               FFrontendDebugWriteLn: TFrontendDebugWriteLn;
               FFrontendDebugWriteF: TFrontendDebugWriteF;
             public
               SharedLibraryBindMainProgramError: string;
{$undef FRONTEND_MAGIC_NUMBER  }
{$define FRONTEND_ENTRY_POINTS }
{$i frontendprocs.inc          }
               function BackendMagicNumber: longword;
             end;

(* There is a single instance of TFrontend, enforce read-only access to it.
*)
function Frontend: TFrontend;

(* This is called by the frontend, but implemented by the backend. Tell the
  backend to do whatever's necessary to get access to entry points exported by
  the frontend, at least up to the point where it checks that magic numbers
  match. Return the frontend's magic number if this is OK, except that zero is
  reserved to indicate that a formatted error message is available via the
  SharedLibraryBindMainProgramError function.
*)
function BackendBindFrontendExports2: longword;

(* Read and clear any error message from BackendBindFrontendExports.
*)
function BackendBindFrontendError2: string;


implementation

var
  xFrontend: TFrontend= nil;


(* There is a single instance of TFrontend, enforce read-only access to it.
*)
function Frontend: TFrontend;

begin
  result := xFrontend
end { Frontend } ;


(* Return a 32-bit unsigned magic number, this does not require any action from
  the memory manager. Despite being generated by frontend code, this should be
  changed to track the functions and parameters described by frontendprocs.inc
  which is to be considered definitive.
*)
function TFrontend.MainProgramMagicNumber: longword;

begin
  LoadRoutine(pointer(FFrontendMagicNumber), FrontendMagicNumber_RoutineName);
{$ifdef USE_DYNAMIC_DEBUG }
  WriteLn(StdErr, 'About to make indirect call via MainProgramMagicNumber.');
  Flush(StdErr);
{$endif USE_DYNAMIC_DEBUG }
  result := FFrontendMagicNumber();
{$ifdef USE_DYNAMIC_DEBUG }
  WriteLn(StdErr, 'Returned from indirect call via MainProgramMagicNumber.');
  Flush(StdErr)
{$endif USE_DYNAMIC_DEBUG }
end { TFrontend.MainProgramMagicNumber } ;


(* Message box optimised for position, for use by the backend.
*)
function TFrontend.MessageDlgOpt(const aMsg: string; dlgType: TMsgDlgType;
                        buttons: TMsgDlgButtons; helpCtx: longint): integer;

begin
  LoadRoutine(pointer(FFrontendMessageDlgOpt), 'MessageDlgOpt');
  result := FFrontendMessageDlgOpt(aMsg, dlgType, buttons, helpCtx)
end { TFrontend.MessageDlgOpt } ;


(* Debugging output to pane on main form, for use by the backend.
*)
procedure TFrontend.DebugWrite(const str: string; ln: boolean= false);

begin
  LoadRoutine(pointer(FFrontendDebugWrite), 'DebugWrite');
  FFrontendDebugWrite(str, ln)
end { TFrontend.DebugWrite } ;


(* Debugging output to pane on main form, for use by the backend.
*)
procedure TFrontend.DebugWriteLn(const str: string= '');

begin
  LoadRoutine(pointer(FFrontendDebugWriteLn), 'DebugWriteLn');
  FFrontendDebugWriteLn(str)
end { TFrontend.DebugWriteLn } ;


(* Debugging output to pane on main form, for use by the backend. This obeys \n
  in the format string, and always tries to "do the right thing" if used in
  conjunction with DebugWrite() above even if this implies a performance hit.
*)
procedure TFrontend.DebugWriteF(const fmt: string; values: array of const);

begin
  LoadRoutine(pointer(FFrontendDebugWriteF), 'DebugWriteF');
  FFrontendDebugWriteF(fmt, values)
end { TFrontend.DebugWriteF } ;


(* This is the magic number from frontendprocs.inc at the time the backend was
  built, it should match the one returned by the frontend main program.
*)
function TFrontend.BackendMagicNumber: longword;

{$define FRONTEND_MAGIC_NUMBER }
{$undef FRONTEND_ENTRY_POINTS  }
{$i frontendprocs.inc          }

begin
  result := FrontendMagicNumber
end { TFrontend.BackendMagicNumber } ;


(* This is called by the frontend, but implemented by the backend. Tell the
  backend to do whatever's necessary to get access to entry points exported by
  the frontend, at least up to the point where it checks that magic numbers
  match. Return the frontend's magic number if this is OK, except that zero is
  reserved to indicate that a formatted error message is available via the
  SharedLibraryBindMainProgramError function.
*)
function BackendBindFrontendExports2: longword;

begin
  result := 0;
  xFrontend := TFrontend.Create('');    (* Blank name indicates main program    *)
  xFrontend.LoadModule;
  xFrontend.SharedLibraryBindMainProgramError := xFrontend.LastError;

(* My original implementation of this returned the error string directly, and   *)
(* ignored the numeric value of the frontend's magic number. However the action *)
(* of calling into the main program from a dynamically-loaded shared library    *)
(* turns out to be somewhat fraught, and the risk of a calling convention or    *)
(* heap management mismatch combined with the difficulty of inspecting the      *)
(* execution sequence with a debugger (because we've already made an indirect   *)
(* call into the backend driver) persuaded me to move error string access into  *)
(* a separate function.                                                         *)

  if xFrontend.SharedLibraryBindMainProgramError = '' then begin
    if not xFrontend.RoutineExists(FrontendMagicNumber_RoutineName) then
      xFrontend.SharedLibraryBindMainProgramError := Format('From backend: unable to get frontend magic number.\n', [])
    else begin
      result := xFrontend.MainProgramMagicNumber;
      if result <> xFrontend.BackendMagicNumber then begin
        xFrontend.SharedLibraryBindMainProgramError := Format('From backend: frontend magic number is %d, should be %d.\n',
                                        [result, xFrontend.BackendMagicNumber]);
        result := 0
      end
    end
  end
end { BackendBindFrontendExports2 } ;


(* Read and clear any error message from BackendBindFrontendExports.
*)
function BackendBindFrontendError2: string;

begin
  result := xFrontend.SharedLibraryBindMainProgramError;
  xFrontend.SharedLibraryBindMainProgramError := ''
end { BackendBindFrontendError2 } ;


finalization
  if xFrontend <> nil then
    xFrontend.Destroy
end.

