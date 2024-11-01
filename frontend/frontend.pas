(* Lazarus+FPC 2.1.0+3.2.0 on Linux Lazarus+FPC 2.1.0+3.2.0 on Linux Lazarus+FP *)

unit Frontend; (* Describes frontend to backend, located in backend directory   *)

(* This is an alternative to the FrontendDynamic unit, for use when static      *)
(* linkage is used to simplify debugging. It will normally be built as part of  *)
(* the frontend project, since the backend project (as distinct from most of    *)
(* the files it contains) will never be built with static linkage. MarkMLl.     *)

{$mode objfpc}{$H+}

interface

{$ifdef USE_DYNAMIC }
  {$error Static frontend unit is being used when the linkmode is dynamic. }
{$endif USE_DYNAMIC }

uses
  Classes, SysUtils, Dialogs, DsoFrontendCode;

 const
  BackendBindFrontendExports2= FrontendMagicNumber; (* Nothing to do, no error  *)
  BackendBindFrontendError2= '';

(* Message box optimised for position, for use by the backend.
*)
function MessageDlgOpt(const aMsg: string; dlgType: TMsgDlgType;
                        buttons: TMsgDlgButtons; helpCtx: longint): integer;

(* Debugging output to pane on main form.
*)
procedure DebugWrite(const str: string; ln: boolean= false);

(* Debugging output to pane on main form.
*)
procedure DebugWriteLn(const str: string= '');

(* Debugging output to pane on main form. This obeys \n in the format string,
  and always tries to "do the right thing" if used in conjunction with
  DebugWrite() above even if this implies a performance hit.
*)
procedure DebugWriteF(const fmt: string; values: array of const);


implementation


(* Message box optimised for position, for use by the backend.
*)
function MessageDlgOpt(const aMsg: string; dlgType: TMsgDlgType;
                        buttons: TMsgDlgButtons; helpCtx: longint): integer;

begin
  result := DsoFrontendCode.MessageDlgOpt(aMsg, dlgType, buttons, helpCtx)
end { MessageDlgOpt } ;


(* Debugging output to pane on main form.
*)
procedure DebugWrite(const str: string; ln: boolean= false);

begin
  DsoFrontendCode.DebugWrite(str, ln)
end { DebugWrite } ;


(* Debugging output to pane on main form.
*)
procedure DebugWriteLn(const str: string= '');

begin
  DsoFrontendCode.DebugWriteLn(str)
end { DebugWriteLn } ;


(* Debugging output to pane on main form. This obeys \n in the format string,
  and always tries to "do the right thing" if used in conjunction with
  DebugWrite() above even if this implies a performance hit.
*)
procedure DebugWriteF(const fmt: string; values: array of const);

begin
  DsoFrontendCode.DebugWriteF(fmt, values)
end { DebugWriteF } ;


end.

