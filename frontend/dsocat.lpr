(* Lazarus+FPC 2.0.0+3.0.4 on Linux Lazarus+FPC 2.0.0+3.0.4 on Linux Lazarus+FP *)

program dsocat;

(* This is the frontend (user interface support) of a program to capture data   *)
(* from and possibly control an oscilloscope such as the JYE Tech DSO112A.      *)
(* Generally speaking the supported instruments use an RS232 serial interface   *)
(* although this might be hidden with the physical connection being USB.        *)
(*                                                                              *)
(* The frontend interfaces with a backend instrument driver, to which it may be *)
(* linked either statically (preferred for debugging) or dynamically, both      *)
(* styles of linkage copy menu entries from an instrument-specific form to the  *)
(* main one.                                                    MarkMLl.        *)

{$mode objfpc}{$H+}

{$undef CHOSEN_LINKMODE      }
{$ifdef USE_STATIC           }
  {$ifndef USE_DYNAMIC       }
    {$define CHOSEN_LINKMODE }
  {$endif USE_DYNAMIC        }
{$endif USE_STATIC           }
{$ifdef USE_DYNAMIC          }
  {$ifndef USE_STATIC        }
    {$define CHOSEN_LINKMODE }
  {$endif USE_STATIC         }
{$endif USE_DYNAMIC          }
{$ifndef CHOSEN_LINKMODE     }
  {$error Requires either (but not both) -dUSE_STATIC or -dUSE_DYNAMIC in Project -> Compiler -> Other -> Custom Options }
{$endif CHOSEN_LINKMODE      }

(* Note manual addition of cmem below, this is required to allow strings and    *)
(* objects to be shared/transferred between the main program and a shared       *)
(* library. Note also relative position of HeapTrc, if cmem is used it is not   *)
(* possible to specify this at the project level (i.e. use FPC's -gh option).   *)

uses
  {$ifdef USE_CMEM }
    cmem, {$ifdef USE_HEAPTRC } HeapTrc, {$endif }
  {$endif }
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Interfaces, // this includes the LCL widgetset
  Forms, tachartlazaruspkg, DsoFrontendCode, xmodem, ScopeStruct, VcdCode,
  FftChart, DsoBackendCode { ,  FftwChart } ;

(* Keep things simple by not importing Backend/BackendDynamic etc. here, the    *)
(* appropriate unit is selected in DsoFrontendCode.                             *)

{$if FPC_FULLVERSION >= 020400 }
  {$R *.res}
{$endif FPC_FULLVERSION        }

exports MainProgramMagicNumber, MessageDlgOpt, DebugWrite, DebugWriteLn,
                                                        DebugWriteF, ModuleAboutText;

(* NOTE ON THE ABOVE: When I last looked at this (circa 2012) the GNU ELF       *)
(* loader in libc etc. explicitly refused to handle secondary entry points,     *)
(* i.e. unlike Windows binaries an ELF binary can be a program /or/ a library   *)
(* but not both, except that any entry points exported from an executable are   *)
(* available to libraries it has itself explicitly loaded. I don't know the     *)
(* extent to which further restrictions are applied by various code signing     *)
(* schemes, and note ongoing discussion about what to do with the kexec() (load *)
(* replacement kernel) binary, whether to restrict ptrace of a signed program,  *)
(* how to sign non-ELF executables (including scripts in text form) etc.        *)
(*                                                                              *)
(* I suspect that this is the result of a political decision on the part of the *)
(* GNU developers and related organisations (the EFF et al.), since it is       *)
(* generally understood that the question of how many entry points are exported *)
(* by a binary is central to the EFF's distinction between an executable and a  *)
(* library, hence to the extent to which the GPL and/or LGPL must be propagated *)
(* to derivative works. It would almost certainly be easy to modify the GNU     *)
(* dynamic loader to relax this restriction, but I suspect that the result      *)
(* would be that the EFF would first demand the sourcecode of the modification  *)
(* (as is their right under the LGPL) and then would argue that any library     *)
(* that made use of it was really an executable and as such was subject to the  *)
(* GPL hence had to be made open-source (potentially raiding the "crown jewels" *)
(* of a developer who'd tried to game the system).                              *)
(*                                                                              *)
(* In short, best not fooled with.                                              *)

begin

(* Lazarus v1 (roughly corresponding to FPC 3.0) introduced this global         *)
(* variable, defaulting to false. It controls error reporting at startup if an  *)
(* expected .lfm is missing, so may be omitted if unsupported by the target LCL *)
(* etc. version.                                                                *)

{$if declared(RequireDerivedFormResource) }
  RequireDerivedFormResource:=True;
{$endif declared                          }

(* Lazarus v2 or later might insert  Application.Scaled := true  here if the    *)
(* project-level application settings include "Use LCL scaling". We probably    *)
(* don't want this since it might have the effect of messing up the pixel-level *)
(* operations we're trying to calibrate, and in any event will make this file   *)
(* incompatible with older versions if that's what's on the hardware in use.    *)

  Application.Initialize;
  Application.CreateForm(TFormDsoCat, FormDsoCat);
  Application.CreateForm(TVCDForm, VCDForm);
  Application.Run;
end.
