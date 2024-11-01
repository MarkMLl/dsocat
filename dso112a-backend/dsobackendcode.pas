(* Lazarus+FPC 2.1.0+3.2.0 on Linux Lazarus+FPC 2.1.0+3.2.0 on Linux Lazarus+FP *)

unit DsoBackendCode;

{$mode objfpc}{$H+}

(********************************************************************************)
(*                                                                              *)
(* WARNING: expect ParseRawToScopeStruct() to fail if "Verify Method Calls" is  *)
(* enabled, i.e. the -CR option. This might apply only to 64-bit dynamic link.  *)
(*                                                                              *)
(********************************************************************************)

(* This is a bit of optional text which may be enabled/disabled to demonstrate  *)
(* that something changes when the backend is rebuilt.                          *)

{ define SHOW_INTERFACE }

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, Menus, Serial, ScopeStruct;

type

  { TDso112aDriverForm }

  TDso112aDriverForm = class(TForm)
    MainMenu1: TMainMenu;
    MenuItemFile: TMenuItem;
    MenuItemConfig: TMenuItem;
    MenuItemHelp: TMenuItem;
    MenuItemHelpAboutBackend: TMenuItem;
    procedure MenuItemHelpAboutBackendClick(Sender: TObject);
  strict private

  public

  end;

var
  Dso112aDriverForm: TDso112aDriverForm;

{$define BACKEND_MAGIC_NUMBER }
{$define BACKEND_ENTRY_POINTS }
{$i backendprocs.inc          }

(* Copy the program version etc. into the parameter as a C-style string. This
  is intended as an experimental secondary entry point that a hypothetical
  desktop GUI etc. can use to get program version information.
*)
function ModuleAboutText(ver: pchar; verLength: word): word; cdecl; experimental;


implementation

{$R *.lfm}

uses StrUtils, IniFilesAbout { , LineInfo } , lnfodwrf
  {$ifdef USE_STATIC } , Frontend {$else } , FrontendDynamic {$endif USE_STATIC } ;

const
  projName= 'DSO112A';

(* This class helper extends the writeability of 'scope parameters to this      *)
(* unit, the remainder of the program should only see them as readable.         *)

type
  TScopeStructH=
    class helper for TScopeStruct
    private
      function getSamplesPerDivision(): integer;
      procedure setSamplesPerDivision(s: integer);
      function getSecondsPerDivision(): single;
      procedure setSecondsPerDivision(s: single);
      function getVerticalResolution(): cardinal;
      procedure setVerticalResolution(r: cardinal);
      function getHorizontalResolution(): cardinal;
      procedure setHorizontalResolution(r: cardinal);
      function getTriggerChannel(): cardinal;
      procedure setTriggerChannel(c: cardinal);
      function getTriggerLevelVolts(): single;
      procedure setTriggerLevelVolts(s: single);
      function getTriggerMode(): TTriggerMode;
      procedure setTriggerMode(m: TTriggerMode);
      function getTriggerSlope(): TTriggerSlope;
      procedure setTriggerSlope(s: TTriggerSlope);
      property SamplesPerDivision: integer read getSamplesPerDivision write setSamplesPerDivision;
      property SecondsPerDivision: single read getSecondsPerDivision write setSecondsPerDivision;
      property VerticalResolution: cardinal read getVerticalResolution write setVerticalResolution;
      property HorizontalResolution: cardinal read getHorizontalResolution write setHorizontalResolution;
      property TriggerChannel: cardinal read getTriggerChannel write setTriggerChannel;
      property TriggerLevelVolts: single read getTriggerLevelVolts write setTriggerLevelVolts;
      property TriggerMode: TTriggerMode read getTriggerMode write setTriggerMode;
      property TriggerSlope: TTriggerSlope read getTriggerSlope write setTriggerSlope;
    end;


function TScopeStructH.getSamplesPerDivision(): integer;

begin
  result := fSamplesPerDivision
end { TScopeStructH.getSamplesPerDivision } ;


procedure TScopeStructH.setSamplesPerDivision(s: integer);

begin
  fSamplesPerDivision := s
end { TScopeStructH.setSamplesPerDivision } ;


function TScopeStructH.getSecondsPerDivision(): single;

begin
  result := fSecondsPerDivision
end { TScopeStructH.getSecondsPerDivision } ;


procedure TScopeStructH.setSecondsPerDivision(s: single);

begin
  fSecondsPerDivision := s
end { TScopeStructH.setSecondsPerDivision } ;


function TScopeStructH.getVerticalResolution(): cardinal;

begin
  result := fVerticalResolution
end { TScopeStructH.getVerticalResolution } ;


procedure TScopeStructH.setVerticalResolution(r: cardinal);

begin
  fVerticalResolution := r
end { TScopeStructH.setVerticalResolution } ;


function TScopeStructH.getHorizontalResolution(): cardinal;

begin
  result := fHorizontalResolution
end { TScopeStructH.getHorizontalResolution } ;


procedure TScopeStructH.setHorizontalResolution(r: cardinal);

begin
  fHorizontalResolution := r
end { TScopeStructH.setHorizontalResolution } ;


function TScopeStructH.getTriggerChannel(): cardinal;

begin
  result := fTriggerChannel
end { TScopeStructH.getTriggerChannel } ;


procedure TScopeStructH.setTriggerChannel(c: cardinal);

begin
  fTriggerChannel := c
end { TScopeStructH.setTriggerChannel } ;


function TScopeStructH.getTriggerLevelVolts(): single;

begin
  result := fTriggerLevelVolts
end { TScopeStructH.getTriggerLevelVolts } ;


procedure TScopeStructH.setTriggerLevelVolts(s: single);

begin
  fTriggerLevelVolts := s
end { TScopeStructH.getTriggerLevelVolts } ;


function TScopeStructH.getTriggerMode(): TTriggerMode;

begin
  result := fTriggerMode
end { TScopeStructH.getTriggerMode } ;


procedure TScopeStructH.setTriggerMode(m: TTriggerMode);

begin
  fTriggerMode := m
end { TScopeStructH.setTriggerMode } ;


function TScopeStructH.getTriggerSlope(): TTriggerSlope;

begin
  result := fTriggerSlope
end { TScopeStructH.getTriggerSlope } ;


procedure TScopeStructH.setTriggerSlope(s: TTriggerSlope);

begin
  fTriggerSlope := s
end { TScopeStructH.setTriggerSlope } ;


(* Hardware-specific per-channel configuration information and data.            *)

type
  TDso112aChannel= record
                    (* Bits per sample (normally 8) *)
                    VerticalResolution: integer; // D
                    triggerMode: integer;
                    triggerSlope: integer;
                    (* Assumed to be scaled like the samples *)
                    triggerLevel: integer;
                    triggerSource: integer;
                    triggerPosition: smallint;
                    triggerSensitivity: integer;
                    (* The captured data, without extra padding *)
                    samples: array of integer
                  end;

  TDso112aScopeStruct= class
                      public
                        (* Unused (blank) *)
                        channelConfiguration: integer;
                        (* Points (out of 2 ^ VerticalResolution) per vertical division *)
                        quantaPerVDiv: integer; // D
                        (* Samples per horizontal division *)
                        samplesPerHDiv: integer; // D
                        (* Samples per second, either integer or floating point *)
                        samplesPerSec: single; // D
                        (* Enumeration representing front panel control *)
                        timebase: integer; // D
                        (* TBD *)
                        horizontalPosition: integer; // D
                        (* Enumeration: 3 is 20V/dev, 15 is 2mV/div *)
                        verticalSensitivity: integer; // D
                        (* TBD *)
                        couple: integer;
                        (* Vertical position, -128 to +127 *)
                        verticalPosition: integer; // D
                        (* TBD *)
                        verticalOffset: integer; // D
                        (* The significance of each quantum, truncated to 16 bits *)
                        uVoltPerBit: single; // D
                        (* Vertical position, 0 to 255 *)
                        reference: integer; // D
                        (* The channels as declared by the scope architecture *)
                        channels: array of TDso112aChannel;
                        constructor Create(channelCount: integer);
                      end;

// D marks debug output


constructor TDso112aScopeStruct.Create(channelCount: integer);

begin
  inherited Create;
  SetLength(channels, channelCount)
end { TDso112aScopeStruct.Create } ;


(* Return a 32-bit unsigned magic number, this does not require any action from
  the memory manager. Despite being generated by backend code, this should be
  changed to track the functions and parameters described by backendprocs.inc,
  which is to be considered definitive.
*)
function SharedLibraryMagicNumber(): longword;

begin
{$ifdef USE_STATIC }
  result := BackendMagicNumber
{$else             }
  {$ifdef USE_CMEM }
  result := BackendMagicNumber
  {$else           }

(* If the external cmem memory manager is not being used it is not, in general, *)
(* safe for dynamically-linked procedures or functions to be called so return a *)
(* broken magic number.                                                         *)

  result := 0
  {$endif USE_CMEM }
{$endif USE_STATIC }
end { SharedLibraryMagicNumber } ;


(* This is called by the frontend, but implemented by the backend. Tell the     *)
(* backend to do whatever's necessary to get access to entry points exported by *)
(* the frontend, at least up to the point where it checks that magic numbers    *)
(* match. Return the frontend's magic number if this is OK, except that zero is *)
(* reserved to indicate that a formatted error message is available via the     *)
(*  SharedLibraryBindMainProgramError function.                                 *)
//
function SharedLibraryBindMainProgramExports: longword;

begin
  result := BackendBindFrontendExports2 // Check frontend magic number?
end { SharedLibraryBindMainProgramExports } ;


(* Read and clear any error message from BackendBindFrontendExports.
*)
function SharedLibraryBindMainProgramError: string;

begin
  result := BackendBindFrontendError2
end { SharedLibraryBindMainProgramError } ;


(* This is normally called automatically when the backend is loaded, but may    *)
(* also be invoked on e.g. receipt of a HUP.                                    *)
//
function LoadConfiguration(base: string= ''): boolean;

begin
  result := true                        //TODO: Configuration loader
end { LoadConfiguration } ;


(* Return a string containing the library's "About" text, hopefully including
  build date and time and Subversion release.

  This is actually a pointer into the library's heap, so it is essential that
  this is shared with the main program- typically by use of the cmem library.
*)
function BackendAboutText: string;

begin
{$ifdef USE_STATIC }
  result := projName + ' [Statically linked]';
{$else             }
  result := projName + ' ' + AboutText();
{$endif USE_STATIC }

(* Screen and resolution info is unlikely to be useful since we aren't really   *)
(* displaying the form defined in this unit, chop it.                           *)

  if Pos('Screen', result) > 0 then begin
    SetLength(result, Pos('Screen', result) - 1);
    result := Trim(result)
  end;

(* Ditto for widget set.                                                        *)

  if Pos('Linked with the', result) > 0 then begin
    SetLength(result, Pos('Linked with the', result) - 1);
    result := Trim(result)
  end
end { BackendAboutText } ;


(* Return an object containing a TMenuItem, possibly with children; it is the
  caller's responsibility to free this.

  This is actually a pointer into the library's heap, so it is essential that
  this is shared with the main program- typically by use of the cmem library.
*)
function BackendMenuFile: TMenuItem;

begin
  try
    if not Assigned(Dso112aDriverForm) then
      Dso112aDriverForm := TDso112aDriverForm.Create(nil);
    result := Dso112aDriverForm.MenuItemFile
  except
    result := nil
  end
end { BackendMenuFile } ;


(* Return an object containing a TMenuItem, possibly with children; it is the
  caller's responsibility to free this.

  This is actually a pointer into the library's heap, so it is essential that
  this is shared with the main program- typically by use of the cmem library.
*)
function BackendMenuConfig: TMenuItem;

begin
  try
    if not Assigned(Dso112aDriverForm) then
      Dso112aDriverForm := TDso112aDriverForm.Create(nil);
    result := Dso112aDriverForm.MenuItemConfig
  except
    result := nil
  end
end { BackendMenuConfig } ;


(* Return an object containing a TMenuItem, possibly with children; it is the
  caller's responsibility to free this.

  This is actually a pointer into the library's heap, so it is essential that
  this is shared with the main program- typically by use of the cmem library.
*)
function BackendMenuHelp: TMenuItem;


begin
  try
    if not Assigned(Dso112aDriverForm) then
      Dso112aDriverForm := TDso112aDriverForm.Create(nil);
    result := Dso112aDriverForm.MenuItemHelp
  except
    result := nil
  end
end { BackendMenuHelp } ;


(* Return a pattern for the type of port the device is expected to be connected
  to, e.g. ttyUSB for most USB-connected serial ports. Return blank to test
  every port.
*)
function PortName(): string;

begin
  result := 'ttyUSB'
end { PortName } ;


(* Return the expected driver (kernel module) name, as found in the /sys tree.
  Return blank for no tests.
*)
function PortDriverName(): string;

begin
  result := 'usb-serial/drivers/cp210x'
end { PortDriverName } ;


(* Return the device product description, as found in the /sys tree. Return
  blank for no tests.
*)
function PortProductDescription(): string;

begin
  result := 'CP2102 USB to UART Bridge Controller'
end { PortProductDescription } ;


(* Return the device serial number, as found in the /sys tree. Note that this
  is a text string of undefined format i.e. cannot be assumed to be numeric or
  even a number expressed in some customary base, return blank for no tests.
*)
function PortSerialDescription(): string;

begin
  result := '0001'
end { PortSerialDescription } ;


(* Return the parameters to be used by default when setting up the serial port.
  If this returns true then assume that using these are mandatory since the
  instrument has no accessible controls for Baud rate etc.
*)
function PortDefaultParams(out BitsPerSec: LongInt; out ByteSize: Integer;
                                        out Parity: TParityType; out StopBits: Integer;
                                        out Flags: TSerialFlags): boolean;

begin
  BitsPerSec := 115200;                 (* I believe the CP2102 is fixed-speed, *)
  ByteSize := 8;                        (* but in any event the DSO112A manual  *)
  Parity := NoneParity;                 (* specifies these parameters and gives *)
  StopBits := 1;                        (* no information on changing them as   *)
  Flags := [];                          (* far as the instrument is concerned.  *)
  result := true
end { PortDefaultParams } ;


(* If debug info is available for the code address passed as the parameter,
  assumed to be from an exception which might be in the context of a background
  thread, return it as a string. Otherwise return blank.
*)
function DebugInfo(codeAddress: PtrUInt): string;

type
  Tkind= (none, stabs, dwarf);

var
  source, func: shortstring;
  lineNum: longint= -1;
  kind: Tkind= none;

begin
  result := '';
{$if declared(CloseStabs) }
  if kind = none then
    if LineInfo.GetLineInfo(codeAddress, func, source, lineNum) then
      kind := stabs;
{$endif                 }
{$if declared(CloseDwarf) }
  if kind = none then
    if lnfodwrf.GetLineInfo(codeAddress, func, source, lineNum) then
      kind := dwarf;
{$endif                 }
  if kind = none then                   (* Neither available, or neither worked *)
    exit;
  result := ' ';

(* I think a good compromise between directory/file.extension and a simple unit *)
(* name is to lose the extension but keep the directory in case the development *)
(* environment has done anything unexpected with the search paths.              *)

  if source <> '' then begin
//    source := ExtractFileName(source);
    source := ChangeFileExt(source, ''); (* Do the devs really catch all cases? *)
    result += source + '.'
  end;
  if func <> '' then
    result += func + '()';
  if result <> ' ' then
    result += ', ';
  if lineNum > 0 then
    result += 'line ' + IntToStr(lineNum);
  case kind of
    stabs: result += ' (STABS)';
    dwarf: result += ' (DWARF)'
  otherwise
    result := ' (WTF?)'
  end
end { DebugInfo } ;


(* Parse and delete the parameter, returning an object representing the
  instrument state or nil on error.
*)
function ParseRawToScopeStruct(raw: TStringList): TScopeStruct;

{$define DEBUG_VOLTAGE  }
{$define DEBUG_TIMEBASE }
{$define DEBUG_TRIGGER  }

var
  dso112aStruct: TDso112aScopeStruct;
  i, j: integer;
  m: single;
  scratch: string;
  minUnscaledVoltage, maxUnscaledVoltage: integer;
  minScaledVoltage, maxScaledVoltage: single;

begin
  result := nil;
  if raw = nil then
    exit;
  dso112aStruct := TDso112aScopeStruct.Create(1);
  try // except
    try // finally for raw and scopeStruct
      minUnscaledVoltage := 256;
      maxUnscaledVoltage := -257;
      minScaledVoltage := 1.0e9;
      maxScaledVoltage := -1.0e9;
      with dso112aStruct do begin

(* Assume a minimum of 16 header lines plus one data line.                      *)

        if raw.Count < 17 then
          Raise Exception.Create('File too short');

(* Assume the first three lines are fixed.                                      *)

        if Pos('JYDZ,Waveform,', raw[0]) <> 1 then
          Raise Exception.Create('File bad type data line 1');

(* 14 header and parameter fields follow, and the number of vertical and        *)
(* horizontal dots per division on the screen is 25. Vertical position and      *)
(* reference appear to be signed and unsigned 8-bit quantities tracking the     *)
(* marker at the left of the screen.                                            *)

        for i := 1 to 3 do begin
          scratch := ExtractDelimited(i, raw[1], [',']);
          j := StrToIntDef(scratch, -2);
          m := StrToFloatDef(scratch, -2.00);
          Assert(m = j, 'Suspect conversion data line 2 field ' + IntToStr(i));
          case i of
            1: if j <> 14 then
                 Raise Exception.Create('File headers bad length');
            2: quantaPerVDiv := j;
            3: samplesPerHDiv := j
          otherwise
            Raise Exception.Create('Severe error')
          end
        end;
        if raw[2] <> 'ChnNum,RecLen,ChnCfg,SampleRate,Resolution,Timebase,HPos,TrigMode,TrigSlope,TrigLvl,TrigSrc,TrigPos,TrigSen,TBcopy' then
          Raise Exception.Create('File headers bad names line 3');
        for i := 1 to 14 do begin
          scratch := ExtractDelimited(i, raw[3], [',']);
          j := StrToIntDef(scratch, -2);
          m := StrToFloatDef(scratch, -2.00);
          if i <> 4 then
            Assert(m = j, 'Suspect conversion data line 4 field ' + IntToStr(i));
          case i of
             1: if j <> 1 then
                  Raise Exception.Create('Unexpected channel number');
             2: if j mod 512 <> 0 then
                  Raise Exception.Create('Unexpected record length')
                else
                  SetLength(channels[0].samples, j);
             3: channelConfiguration := -1;
             4: samplesPerSec := m;
             5: if j mod 8 <> 0 then
                  Raise Exception.Create('Unexpected vertical resolution')
                else
                  channels[0].VerticalResolution := j;
             6: timebase := j;          (* Enumeration                          *)
             7: horizontalPosition := j;
             8: channels[0].triggerMode := j;
             9: channels[0].triggerSlope := j;
            10: channels[0].triggerLevel := j;
            11: channels[0].triggerSource := -1;
            12: channels[0].triggerPosition := j;
            13: channels[0].triggerSensitivity := -1;
            14: if j <> timebase then
                  Raise Exception.Create('Unexpected second timebase field')
          otherwise
            Raise Exception.Create('Severe internal error')
          end
        end;
        verticalSensitivity := StrToIntDef(raw[4], -2); (* Enumeration          *)
        couple := StrToIntDef(raw[5], -2);
        verticalPosition := smallint(StrToIntDef(raw[6], -2) and $ffff);
        verticalOffset := StrToIntDef(raw[7], -2);
        uVoltPerBit := StrToFloatDef(raw[8], -2.00); (* Truncated, limited use  *)
        if StrToIntDef(raw[9], -2) <> verticalSensitivity then
          Raise Exception.Create('Unexpected second sensitivity field');
        reference := StrToIntDef(raw[10], -2);
        j := raw.Count - 16;
        if j <> Length(channels[0].samples) then
          Raise Exception.Create('Number of records wrong');
        for i := 0 to j - 1 do begin;
          channels[0].samples[i] := StrToIntDef(raw[i + 16], -2);
{$ifdef DEBUG_VOLTAGE }
          if StrToIntDef(raw[i + 16], -2) < minUnscaledVoltage then
            minUnscaledVoltage := StrToIntDef(raw[i + 16], -2);
          if StrToIntDef(raw[i + 16], -2) > maxUnscaledVoltage then
            maxUnscaledVoltage := StrToIntDef(raw[i + 16], -2)
        end;
        Frontend.DebugWriteF('Raw/unscaled voltage configuration: ------\n', []);
        Frontend.DebugWriteF('Quanta per vertical division: %d\n', [quantaPerVDiv]);
        Frontend.DebugWriteF('Resolution: %d\n', [channels[0].VerticalResolution]); // 8
        Frontend.DebugWriteF('Vertical sensitivity: %d\n', [verticalSensitivity]); // 8
        Frontend.DebugWriteF('Vertical position: %d\n', [verticalPosition]); // 0
        Frontend.DebugWriteF('Vertical offset: %d\n', [verticalOffset]); // 0
        Frontend.DebugWriteF('µVolt per bit: %8.3g\n', [uVoltPerBit]); // 20000.0
        Frontend.DebugWriteF('Reference: %d\n', [reference]); // 128
        Frontend.DebugWriteF('Unscaled min and max: %d %d\n', [minUnscaledVoltage, maxUnscaledVoltage]); // 100 122
{$else                }
        end;
{$endif DEBUG_VOLTAGE }
{$ifdef DEBUG_TIMEBASE }
        Frontend.DebugWriteF('Raw/unscaled timebase configuration: -----\n', []);
        Frontend.DebugWriteF('Samples per horizontal division: %d\n', [samplesPerHDiv]);
        Frontend.DebugWriteF('Samples per second: %8.3g\n', [samplesPerSec]);
        Frontend.DebugWriteF('Timebase switch: %d\n', [timebase]);
        Frontend.DebugWriteF('Horizontal position: %d\n', [horizontalPosition])
{$endif DEBUG_TIMEBASE }
      end;

(* That's parsed the text .csv file from the instrument into a binary device-   *)
(* specific data structure. Now scale that into a device-independent structure  *)
(* to pass back to the frontend, this truncates the number of samples slightly  *)
(* so as to ensure an integer number of horizontal divisions (20 if the 'scope  *)
(* is working to 512 divisions, 40 (ish) if it is working with 1024).           *)

      result := TScopeStruct.Create(1);
      try // except for result
        with result do begin
          SamplesPerDivision := dso112aStruct.samplesPerHDiv;
          case dso112aStruct.timebase of
             7: SecondsPerDivision := 50.0;
             8: SecondsPerDivision := 20.0;
             9: SecondsPerDivision := 10.0;
            10: SecondsPerDivision := 5.0;
            11: SecondsPerDivision := 2.0;
            12: SecondsPerDivision := 1.0;
            13: SecondsPerDivision := 0.5;
            14: SecondsPerDivision := 0.2;
            15: SecondsPerDivision := 0.1;
            16: SecondsPerDivision := 50.0e-3;
            17: SecondsPerDivision := 20.0e-3;
            18: SecondsPerDivision := 10.0e-3;
            19: SecondsPerDivision := 5.0e-3;
            20: SecondsPerDivision := 2.0e-3;
            21: SecondsPerDivision := 1.0e-3;
            22: SecondsPerDivision := 0.5e-3;
            23: SecondsPerDivision := 0.2e-3;
            24: SecondsPerDivision := 0.1e-3;
            25: SecondsPerDivision := 50.0e-6;
            26: SecondsPerDivision := 20.0e-6;
            27: SecondsPerDivision := 10.0e-6;
            28: SecondsPerDivision := 5.0e-6;
            29: SecondsPerDivision := 2.0e-6;
            30: SecondsPerDivision := 1.0e-6
          otherwise
            Raise Exception.Create('Unexpected timebase (secs per division) value')
          end
        end { result } ;
        with result.Channels[0] do begin
          case dso112aStruct.verticalSensitivity of
             3: voltsPerDivision := 20.0;
             4: voltsPerDivision := 10.0;
             5: voltsPerDivision := 5.0;
             6: voltsPerDivision := 2.0;
             7: voltsPerDivision := 1.0;
             8: voltsPerDivision := 0.5;
             9: voltsPerDivision := 0.2;
            10: voltsPerDivision := 0.1;
            11: voltsPerDivision := 50.0e-3;
            12: voltsPerDivision := 20.0e-3;
            13: voltsPerDivision := 10.0e-3;
            14: voltsPerDivision := 5.0e-3;
            15: voltsPerDivision := 2.0e-3
          otherwise
            Raise Exception.Create('Unexpected sensitivity (volts per division) value')
          end;
          positionOffsetVolts := (dso112aStruct.verticalPosition / 128) * (5.0 * voltsPerDivision);
          case dso112aStruct.couple of
            0: couple := CoupleDC;
            1: couple := CoupleAC;
            2: couple := CoupleGnd
          otherwise
            Raise Exception.Create('Unexpected couple value')
          end
        end { result.Channels[0] } ;
        with result do begin
          TriggerLevelVolts := (dso112aStruct.channels[0].triggerLevel / 128) * (5.0 * channels[0].voltsPerDivision);
          case dso112aStruct.channels[0].triggerMode of
            0: triggerMode := TriggerModeAuto;
            1: triggerMode := TriggerModeNormal;
            2: triggerMode := TriggerModeSingle
          otherwise
            Raise Exception.Create('Unexpected trigger mode value')
          end;
          case dso112aStruct.channels[0].triggerSlope of
            0: triggerSlope := TriggerSlopeFalling;
            1: triggerSlope := TriggerSlopeRising
          otherwise
            Raise Exception.Create('Unexpected trigger slope value')
          end
        end { result };
        with result.Channels[0] do begin
          j := (Length(dso112aStruct.channels[0].samples) + 1) div 8; (* 1/8th    *)
          case dso112aStruct.channels[0].triggerPosition of
            0: triggerAtSample := j;
            1: triggerAtSample := j * 2;
            2: triggerAtSample := j * 4;
            3: triggerAtSample := j * 6;
            4: triggerAtSample := j * 7
          otherwise
            Raise Exception.Create('Unexpected trigger position value')
          end;

(* The entire array of samples is passed to the frontend. This is typically     *)
(* sized to be an integer power of 2 so will typically be trimmed or slightly   *)
(* padded to give an integer number of divisions for time domain display, but   *)
(* it is convenient to have 2^n samples for the FFT algorithm used to generate  *)
(* the frequency domain display.                                                *)

// I'm not sure this should work. The Channels array is being accessed via a
// read-only property (i.e. no write accessor) which appears to be preventing
// the array from being modified but not the individual (i.e. per-channel)
// elements.
//
// I'm sure that the language designers would shrug it off, and would probably
// say that if I wanted to protect an element I should define it as a class
// rather than a record complete with properties and accessors, but that would
// imply that this unit would have to have a helper for the element type (and
// that depending on the FPC version other units could arbitrarily claim write
// privilege by declaring a helper)... all in all it's a can or worms.

          SetLength(samples, Length(dso112aStruct.channels[0].samples));
          for i := 0 to Length(samples) - 1 do begin
            samples[i] := (dso112aStruct.channels[0].samples[i] - 128 + dso112aStruct.verticalPosition) /
                                        dso112aStruct.quantaPerVDiv * voltsPerDivision;
{$ifdef DEBUG_VOLTAGE }
            if samples[i] < minScaledVoltage then
              minScaledVoltage := samples[i];
            if samples[i] > maxScaledVoltage then
              maxScaledVoltage := samples[i]
{$endif DEBUG_VOLTAGE }
          end;
          result.VerticalResolution := 1 shl dso112aStruct.channels[0].VerticalResolution;
          result.HorizontalResolution := Length(samples);

(* This is based on examination of the LCD display, which appears to be 25      *)
(* pixels per division, and the assumption that each pixel is a sample.         *)

          result.Channels[0].resolvedVolts := result.Channels[0].voltsPerDivision / dso112aStruct.quantaPerVDiv;
          j := Round(result.Channels[0].resolvedVolts * 1.0e6) mod 65536;
//          Assert(j = scopeStruct.uVoltPerBit, 'uVoltPerBit even worse than expected');

{$ifdef DEBUG_VOLTAGE }
          Frontend.DebugWriteF('Scaled voltage configuration: ------------\n', []);
          Frontend.DebugWriteF('Volts per division: %8.3g\n', [voltsPerDivision]);
          Frontend.DebugWriteF('Volts position: %8.3g\n', [positionOffsetVolts]);
          Frontend.DebugWriteF('Axis min and max: %8.3g %8.3g\n', [minVoltsAxis, maxVoltsAxis]);
          case couple of
            CoupleDC:  Frontend.DebugWriteF('Coupled: %s\n', ['DC']);
            CoupleAC:  Frontend.DebugWriteF('Coupled: %s\n', ['AC']);
            CoupleGnd: Frontend.DebugWriteF('Coupled: %s\n', ['Gnd'])
          otherwise
            Frontend.DebugWriteF('Coupled: %s\n', ['Invalid'])
          end;
          Frontend.DebugWriteF('Resolved volts: %8.3g\n', [resolvedVolts]);
          Frontend.DebugWriteF('Scaled min and max: %8.3g %8.3g\n', [minScaledVoltage, maxScaledVoltage]);
          Frontend.DebugWriteF('Vertical resolution: %d\n', [result.VerticalResolution])
{$endif DEBUG_VOLTAGE }
        end { result.Channels[0] } ;
        with result do begin
{$ifdef DEBUG_TIMEBASE }
          Frontend.DebugWriteF('Scaled timebase configuration: -----------\n', []);
          Frontend.DebugWriteF('Samples per division: %d\n', [SamplesPerDivision]);
          Frontend.DebugWriteF('Seconds per division: %8.3g\n', [SecondsPerDivision]);
          Frontend.DebugWriteF('Horizontal resolution: %d\n', [HorizontalResolution]);
{$endif DEBUG_TIMEBASE }
{$ifdef DEBUG_TRIGGER  }
          Frontend.DebugWriteF('Scaled trigger configuration: -----------\n', []);
          Frontend.DebugWriteF('Trigger level: %8.3g\n', [TriggerLevelVolts]);
          case triggerMode of
            TriggerModeAuto:   Frontend.DebugWriteF('Trigger mode: %s\n', ['Auto']);
            TriggerModeNormal: Frontend.DebugWriteF('Trigger mode: %s\n', ['Normal']);
            TriggerModeSingle: Frontend.DebugWriteF('Trigger mode: %s\n', ['Single'])
          otherwise
            Frontend.DebugWriteF('Trigger mode: %s\n', ['Invalid'])
          end;
          case triggerSlope of
            TriggerSlopeFalling: Frontend.DebugWriteF('Trigger slope: %s\n', ['Falling']);
            TriggerSlopeRising:  Frontend.DebugWriteF('Trigger slope: %s\n', ['Rising'])
          otherwise
            Frontend.DebugWriteF('Trigger slope: %s\n', ['Invalid'])
          end;
          Frontend.DebugWriteF('Trigger at sample: %d\n', [Channels[0].triggerAtSample]);
{$endif DEBUG_TRIGGER  }
        end
      except
        on E: Exception do begin
{$if declared(CloseStabs) or declared(CloseDwarf) }
          Frontend.DebugWriteF('ParseRawToScopeStruct: %s\n', [E.Message + ', ' + DebugInfo(PtrUInt(ExceptAddr))]);
{$else  }
          Frontend.DebugWriteF('ParseRawToScopeStruct ' + {$I %LINE% } + ': %s\n', [E.Message]);
{$endif }
          FreeAndNil(result)
        end
      end
    finally
      raw.Free;
      FreeAndNil(dso112aStruct)
    end
  except
    on E: Exception do begin
{$if declared(CloseStabs) or declared(CloseDwarf) }
      Frontend.DebugWriteF('ParseRawToScopeStruct: %s\n', [E.Message + ', ' + DebugInfo(PtrUInt(ExceptAddr))]);
{$else  }
      Frontend.DebugWriteF('ParseRawToScopeStruct ' + {$I %LINE% } + ': %s\n', [E.Message]);
{$endif }

(********************************************************************************)
(*                                                                              *)
(* WARNING: expect ParseRawToScopeStruct() to fail if "Verify Method Calls" is  *)
(* enabled, i.e. the -CR option. This might apply only to 64-bit dynamic link.  *)
(*                                                                              *)
(********************************************************************************)

      FreeAndNil(TScopeStruct(result))
    end
  end
end { ParseRawToScopeStruct } ;


(* Generate test data, adhering as closely as possible to the behaviour of
  ParseRawToScopeStruct() above.
*)
function ParseTestToScopeStruct(testType: TTestType): TScopeStruct;

var
  i: integer;

begin
  result := TScopeStruct.Create(1);
  with result do begin
    SamplesPerDivision := 25;
    SecondsPerDivision := 0.0005;
    VerticalResolution := 256;
    HorizontalResolution := 1024;
    with Channels[0] do begin
      voltsPerDivision := 1.0;
      positionOffsetVolts := 0.0;
      minVoltsAxis := 0.0;
      maxVoltsAxis := 0.0;
      couple := CoupleAC;
      triggerLevelVolts := 0.0;
      triggerMode := TriggerModeAuto;
      triggerSlope := TriggerSlopeRising;
      if testType in [ttPulse, ttRisingEdge, ttFallingEdge] then
        triggerAtSample := HorizontalResolution div 2
      else
        triggerAtSample := 0;
      resolvedVolts := 0.04;
      SetLength(samples, HorizontalResolution);

(* Test waveform frequency is 1 kHz, so with 0.5 mSec/division and 25 samples   *)
(* per division that's 50 samples per cycle. The sine wave should transform to  *)
(* a single sharp peak in the frequency domain, the square wave to decaying odd *)
(* harmonics, and the unit pulse to the sinc function (symmetrical about 0Hz).  *)
(* The unit pulse voltages are chosen to balance the amount of energy above and *)
(* below 0V, in an attempt to minimise the DC component which is obviously not  *)
(* an issue with the periodic sine and square waveforms.                        *)

      for i := 0 to HorizontalResolution - 1 do
        case testType of
          ttSine:       samples[i] := Sin((i * 2 * Pi) / 50.0);
          ttPulse:      case i of
                          (512 - 12)..
                          (512 + 13): samples[i] := +1.95
                         otherwise
                           samples[i] := -0.05
                         end;
          ttRisingEdge:  if i < 512 then
                           samples[i] := -1
                         else
                           samples[i] := +1;
          ttFallingEdge: if i < 512 then
                           samples[i] := +1
                         else
                           samples[i] := -1;
          ttRisingSaw:   samples[i] := (2 - ((i mod 50) / 25)) - 1;
          ttFallingSaw:  samples[i] := ((i mod 50) / 25) - 1;
          ttTriangle:    if Odd(i div 25) then
                           samples[i] := (2 * (2 - ((i mod 50) / 25))) - 1
                         else
                           samples[i] := (2 * ((i mod 50) / 25)) - 1
        otherwise                       (* Square wave                          *)
          if Odd(i div 25) then
            samples[i] := +1.0
          else
            samples[i] := -1.0
        end
    end
  end
end { ParseTestToScopeStruct } ;


{ TDso112aDriverForm }


procedure TDso112aDriverForm.MenuItemHelpAboutBackendClick(Sender: TObject);

var     about: string;

begin
{$ifdef USE_STATIC }
  about := projName + ' [Statically linked, same as frontend]';
{$else             }
  about := projName + ' ' + AboutText;
{$endif USE_STATIC }

(* Screen and resolution info is unlikely to be useful since we aren't really   *)
(* displaying the form defined in this unit, chop it.                           *)

  if Pos('Screen', about) > 0 then begin
    SetLength(about, Pos('Screen', about) - 1);
    about := Trim(about)
  end;

(* Ditto for widget set.                                                        *)

  if Pos('Linked with the', about) > 0 then begin
    SetLength(about, Pos('Linked with the', about) - 1);
    about := Trim(about)
  end;
{$ifdef USE_DYNAMIC }
  about += #$0d#$0a + 'Frontend magic number: ' + IntToStr(Frontend.BackendMagicNumber);
{$endif USE_DYNAMIC }

(* This is a bit of optional text which may be enabled/disabled to demonstrate  *)
(* that something changes when the backend is rebuilt.                          *)

{$ifdef SHOW_INTERFACE }
  about += #$0d#$0a + 'Physical interface: ' + PortProductDescription;
{$endif SHOW_INTERFACE }
  Frontend.MessageDlgOpt(about, mtConfirmation, [mbOk], 0)
end { TDso112aDriverForm.MenuItemHelpAboutBackendClick } ;


(* Copy the program version etc. into the parameter as a C-style string. This
  is intended as an experimental secondary entry point that a hypothetical
  desktop GUI etc. can use to get program version information.
*)
function ModuleAboutText(ver: pchar; verLength: word): word; cdecl; experimental;

var     scratch: string;

begin
{$ifdef USE_STATIC }
  scratch := projName + ' [Statically linked, same as frontend]';
{$else             }
  scratch := projName + ' ' + AboutText;
{$endif USE_STATIC }

(* Screen and resolution info is unlikely to be useful since we aren't really   *)
(* displaying the form defined in this unit, chop it.                           *)

  if Pos('Screen', scratch) > 0 then begin
    SetLength(scratch, Pos('Screen', scratch) - 1);
    scratch := Trim(scratch)
  end;

(* Ditto for widget set.                                                        *)

  if Pos('Linked with the', scratch) > 0 then begin
    SetLength(scratch, Pos('Linked with the', scratch) - 1);
    scratch := Trim(scratch)
  end;
{$ifdef USE_DYNAMIC }
  scratch += #$0d#$0a + 'Frontend magic number: ' + IntToStr(Frontend.BackendMagicNumber);
{$endif USE_DYNAMIC }

(* This is a bit of optional text which may be enabled/disabled to demonstrate  *)
(* that something changes when the backend is rebuilt.                          *)

{$ifdef SHOW_INTERFACE }
  scratch += #$0d#$0a + 'Physical interface: ' + PortProductDescription;
{$endif SHOW_INTERFACE }
  if verLength - 1 < Length(scratch) then
    SetLength(scratch, verLength -1);
  strpcopy(ver, scratch);
  result := Length(scratch)
end { ModuleAboutText } ;


finalization
  FreeAndNil(Dso112aDriverForm)
end.

