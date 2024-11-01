(* Lazarus v0.9.24 Win-32 Lazarus v0.9.24 Win-32 Lazarus v0.9.24 Win-32 Lazarus *)
(* Lazarus v0.9.24 Linux-i386 Lazarus v0.9.24 Linux-i386 Lazarus v0.9.34 Linux- *)

unit DynaMod;

(* This source file defines a class that can be used to manage		*)
(* dynamically loaded DLLs. This class can be used directly e.g.	*)
(*									*)
(*	function ComplexModulus(Real, Imaginary: Double): Double;	*)
(*									*)
(*	var	Routine: function(var XR, XI: Double): Double; stdcall;	*)
(*		Module: TDynamicModule;					*)
(*	begin								*)
(*	  Routine := nil;						*)
(*	  Module := TDynamicModule.Create('NAGAC.DLL');			*)
(*	  try								*)
(*	    Module.LoadRoutine(@Routine, 'A02ABF');			*)
(*	    Result := Routine(Real, Imaginary);				*)
(*	  finally							*)
(*	    Module.Free;						*)
(*	  end;								*)
(*	end;								*)
(*									*)
(* The class can also be used as a base class where the sub-class	*)
(* defines methods for all the routines in a particular DLL, e.g.	*)
(*									*)
(*	type	TNagACModule = class(TDynamicModule)			*)
(*		  protected						*)
(*		    FA02ABF: function(var XR, XI: Double): Double;	*)
(*							stdcall;	*)
(*		  public						*)
(*		    constructor Create;					*)
(*		    function ComplexModulus(Real, Imaginary: Double):	*)
(*							Double;		*)
(*		end;							*)
(*									*)
(*									*)
(*	constructor TNagACModule.Create;				*)
(*									*)
(*	begin								*)
(*	  inherited Create('NAGAC.DLL');				*)
(*	end;								*)
(*									*)
(*									*)
(*	function TNagACModule.ComplexModulus(Real, Imaginary: Double):	*)
(*							Double;		*)
(*	begin								*)
(*	  LoadRoutine(@FA02ABF, 'A02ABF');				*)
(*	  Result := FA02ABF(Real, Imaginary);				*)
(*	end;								*)
(*									*)
(* Contributed by kodekraft@cix (Martin Halliday), minor reformatting	*)
(* and port to Lazarus by MarkMLl.					*)

(* Debugging DLL calling-convention errors has always been difficult,	*)
(* but it can be eased by a runtime check of the stack pointer before	*)
(* and after calling into the DLL. This can either be done using a	*)
(* local variable (itself on the stack, hence susceptible to stack-	*)
(* inconsistency errors) or with a global variable (incompatible with	*)
(* recursive calls and multiple worker threads). Because in general a	*)
(* DLL won't be called self-recursively and a multi-threaded app can be	*)
(* simplified so that only a single thread calls the DLL the latter	*)
(* case is probably preferable, as shown below:				*)
(*									*)
(* (¤$IFOPT C+	¤)							*)
(* VAR	CreateFileCheck: POINTER; 	(¤ N.B. Neither thread- nor ¤)	*)
(* (¤$ENDIF	¤)			(¤ recursion-safe.	    ¤)	*)
(*									*)
(* FUNCTION TIcmp.CreateFile: Thandle;					*)
(*									*)
(* BEGIN								*)
(*   LoadRoutine(@FIcmpCreateFile, 'IcmpCreateFile');			*)
(* (¤$IFOPT C+	¤)							*)
(*   ASM MOV EAX,ESP ; XOR EAX,55555555H ; MOV CreateFileCheck,EAX END;	*)
(* (¤$ENDIF	¤)							*)
(*   CreateFile:= FIcmpCreateFile;					*)
(* (¤$IFOPT C+	¤)							*)
(*   ASM MOV EAX,ESP ; XOR EAX,55555555H ; CMP EAX,CreateFileCheck ;	*)
(*		MOV EAX,0 ; SETE AL ; MOV CreateFileCheck,EAX END;	*)
(*   Assert(BOOLEAN(CreateFileCheck),					*)
(*		'Calling convention error in TIcmp.CreateFile()')	*)
(* (¤$ENDIF	¤)							*)
(* END (¤ TIcmp.CreateFile ¤) ;						*)
(*									*)
(* Note that I've mnagled embedded comments and directives for obvious	*)
(* reasons. MarkMLl.							*)

{$mode objfpc}

interface

type
  TDynamicModule = class
  protected
    FModuleHandle: TLibHandle;
    FAlreadyInMemory: BOOLEAN;
    FModuleName: string;
    FLastError: string;
    procedure SetModuleName(const Value: string);
    function GetModuleHandle: TLibHandle;
    procedure UnloadModule;
  public
    constructor Create(const ModuleName: string);
    destructor Destroy; override;
    procedure LoadModule;
    procedure LoadRoutine(var Routine: Pointer; const Name: string);
    function LoadRoutine(const Name: string): Pointer;
    function ModuleExists: Boolean;
    function RoutineExists(const Name: string): Boolean;
    property ModuleName: string read FModuleName write SetModuleName;
    property ModuleHandle: TLibHandle read GetModuleHandle;
    property ModuleInMemory: boolean read FAlreadyInMemory;
    property LastError: string read FLastError;
  end;


implementation { TDynamicModule }

uses Dynlibs, SysUtils {$if not defined (GetLoadErrorStr) } , DynaMod2 {$endif } ;

type      DynamicModuleException= class(Exception);

{ define TEST_ALIGNMENT }


procedure TDynamicModule.SetModuleName(const Value: string);

(* Case-sensitivity depends on the operating system and filesystem.	*)

begin
  UnloadModule;
  FModuleName := Value;
end;


function TDynamicModule.GetModuleHandle: TLibHandle;
begin
  LoadModule;
  Result := FModuleHandle;
end;


procedure TDynamicModule.UnloadModule;
begin
  if (FModuleHandle <> NilHandle) AND NOT FAlreadyInMemory then begin
    FreeLibrary(FModuleHandle);
    FLastError := GetLoadErrorStr
  end;
  FModuleHandle := NilHandle;
  FAlreadyInMemory:= FALSE
end;


constructor TDynamicModule.Create(const ModuleName: string);

(* Case-sensitivity depends on the operating system and filesystem.	*)

begin
  inherited Create;
  FModuleHandle := NilHandle;           (* Be absolutely explicit about these   *)
  FAlreadyInMemory:= FALSE;             (* since used for presence testing.     *)
  FModuleName := Trim(ModuleName);
  FLastError := '[Undefined or not implemented]'
end;


destructor TDynamicModule.Destroy;
begin
  UnloadModule;
  inherited;
end;


procedure TDynamicModule.LoadModule;

var     revisedFilename: string;


  function readSecondLine(const name: string): string;

  var   txt: TEXT;

  begin
    Assign(txt, name);
    Reset(txt);
    repeat
      ReadLn(txt, result)
    until (Pos('GROUP ( ', result) = 1) or Eof(txt);
    Close(txt)
  end;


begin
  if FModuleHandle = NilHandle then
  begin
    FModuleHandle:= LoadLibrary(FModuleName);
    FLastError := GetLoadErrorStr;
    FAlreadyInMemory:= FModuleHandle <> NilHandle;
    IF NOT FAlreadyInMemory THEN BEGIN
      FModuleHandle := LoadLibrary(FModuleName);
      FLastError := GetLoadErrorStr;
      if FModuleHandle = NilHandle then
{$IFDEF UNIX }
        if (GetLastOSError = 0) and (Pos(': invalid ELF header', LastError) > 0) then begin

(* I believe this is a Debian special: libusb-0.1 has been declared obsolete    *)
(* and "replaced by a linker script, in the hope it will make everybody happy." *)
(* The error message above might be subject to i18n, so users in non-English    *)
(* speaking locales might have at least as much grief as I've just had. Blame   *)
(* Aurelien Jarno :-)                                                           *)

          revisedFilename := LastError;
          SetLength(revisedFilename, Pos(':', revisedFilename) - 1);
          revisedFilename := readSecondLine(revisedFilename);
          if Pos('GROUP ( ', revisedFilename) = 1 then begin
            Delete(revisedFilename, 1, Length('GROUP ( '));
            SetLength(revisedFilename, Pos(' )', revisedFilename) - 1);
            FModuleName := revisedFilename;
            LoadModule;                 (* Recursive                            *)

(* Exits on success, otherwise raises the same exception as below.              *)

            exit
          end
        end;
{$ENDIF }
	raise DynamicModuleException.Create(SysErrorMessage(GetLastOSError)
					+ ' loading ' + FModuleName);
    END
  end;
end;


procedure TDynamicModule.LoadRoutine(var Routine: Pointer; const Name: string);

(* Note that the name of the entry point is case-sensitive.		*)

begin
  if Routine = nil then
  begin
{$ifdef TEST_ALIGNMENT }
{$if defined(CPUX86_64) }
    Assert(PtrUInt(@Routine) mod 8 = 0, 'Internal error: processor-specific stack misalignment for GetProcedureAddress(2) A');
    Assert(PtrUInt(@Name) mod 8 = 0, 'Internal error: processor-specific stack misalignment for GetProcedureAddress(2) B');
{$endif                 }
{$endif TEST_ALIGNMENT }
    Routine := GetProcedureAddress(ModuleHandle, Trim(Name));
    FLastError := GetLoadErrorStr;
    if Routine = nil then
      raise DynamicModuleException.Create(FLastError)
  end
end;


function TDynamicModule.LoadRoutine(const Name: string): Pointer;

(* This is an overloaded addition to Martin's original code. It can     *)
(* result in neater code but is much less efficient since can't check   *)
(* whether it has already been called.                                  *)

begin
{$ifdef TEST_ALIGNMENT }
{$if defined(CPUX86_64) }
  Assert(PtrUInt(@result) mod 8 = 0, 'Internal error: processor-specific stack misalignment for GetProcedureAddress(1) A');
  Assert(PtrUInt(@Name) mod 8 = 0, 'Internal error: processor-specific stack misalignment for GetProcedureAddress(1) B');
{$endif                 }
{$endif TEST_ALIGNMENT }
  RESULT:= GetProcedureAddress(ModuleHandle, Trim(Name));
  FLastError := GetLoadErrorStr;
  if RESULT = nil then
    raise DynamicModuleException.Create(FLastError)
end;


function TDynamicModule.ModuleExists: Boolean;
begin
  try
    Result := ModuleHandle <> NilHandle
  except
    Result := False;
  end;
end;


function TDynamicModule.RoutineExists(const Name: string): Boolean;

(* Note that the name of the entry point is case-sensitive.		*)

var
  Routine: Pointer;
begin
  Routine:= NIL;                        (* We're looking to see if this changes *)
  try
    LoadRoutine(Routine, Trim(Name));
  except
    Routine := nil;
  end;
  FLastError := GetLoadErrorStr;
  Result := Routine <> nil;
end;


(* No unit initialisation code.						*)

end.
