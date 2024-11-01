(* Lazarus+FPC 2.1.0+3.2.0 on Linux Lazarus+FPC 2.1.0+3.2.0 on Linux Lazarus+FP *)

unit DsoFrontendCode;

(* Interface with a digital storage 'scope (DSO), such as a JYETech DSO 112A or *)
(* possibly a Tek 222A. Once the overall user interface is roughed out, it will *)
(* be filled in with working code strictly on an "I need this now" basis, so    *)
(* might never be as comprehensive as a dedicated program such as Cat200 from   *)
(* Tektronix (which nobody has seen in living memory, but if nothing else has   *)
(* contributed its name). MarkMLl.                                              *)

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, ComCtrls,
  Menus, StdCtrls, PairSplitter, Spin, TAGraph, TASeries, TATools, TASources,
  Serial, ScopeStruct, Types;

const
  InvalidSerialHandle= TSerialHandle(-1);

type

  { TFormDsoCat }

  TFormDsoCat = class(TForm)
    ChartFrequencyDomain: TChart;
    ChartFrequencyDomainLineSeries1: TLineSeries;
    ChartFrequencyDomainLineSeries2: TLineSeries;
    ChartFrequencyDomainLineSeries3: TLineSeries;
    ChartTimeDomain: TChart;
    ChartTimeDomainConstantLineTriggerTime: TConstantLine;
    ChartTimeDomainConstantLineZeroVolts: TConstantLine;
    ChartTimeDomainLineSeries1: TLineSeries;
    ChartToolsetFrequencyDomain: TChartToolset;
    ChartToolsetFrequencyDomainDataPointClickTool1: TDataPointClickTool;
    ChartToolsetFrequencyDomainPanDragTool1: TPanDragTool;
    ChartToolsetFrequencyDomainZoomDragTool1: TZoomDragTool;
    ChartToolsetTimeDomain: TChartToolset;
    ChartToolsetTimeDomainDataPointClickTool1: TDataPointClickTool;
    ChartToolsetTimeDomainPanDragTool1: TPanDragTool;
    ChartToolsetTimeDomainZoomDragTool1: TZoomDragTool;
    ComboBoxTriggerMode: TComboBox;
    ComboBoxTriggerEdge: TComboBox;
    ComboBoxCoupling: TComboBox;
    ComboBoxSecsPerDiv: TComboBox;
    GroupBoxRightTop: TGroupBox;
    GroupBoxRightBottom: TGroupBox;
    ListChartSourceFrequencyDomainLeftAxis: TListChartSource;
    ListChartSourceFrequencyDomainBottomAxis: TListChartSource;
    ListChartSourceTimeDomainBottomAxis: TListChartSource;
    ListChartSourceTimeDomainLeftAxis: TListChartSource;
    MainMenu1: TMainMenu;
    MemoDebug: TMemo;
    MenuItemFileTestWaveformTriangle: TMenuItem;
    MenuItemFileTestWaveformRisingSaw: TMenuItem;
    MenuItemFileTestWaveformFallingSaw: TMenuItem;
    MenuItemFileTestWaveformRisingEdge: TMenuItem;
    MenuItemFileTestWaveformFallingEdge: TMenuItem;
    MenuItemFileTestWaveformUnitPulse: TMenuItem;
    MenuItemFileTestWaveformSine: TMenuItem;
    MenuItemFileTestWaveformSquare: TMenuItem;
    MenuItemFileTestWaveform: TMenuItem;
    MenuItemConfigFrequencyDomain: TMenuItem;
    MenuItemExportGIF: TMenuItem;
    MenuItemExportPNG: TMenuItem;
    MenuItemFileExportImage: TMenuItem;
    MenuItemISOA5: TMenuItem;
    MenuItemISOA6: TMenuItem;
    MenuItemISOA7: TMenuItem;
    MenuItemInches7x5: TMenuItem;
    MenuItemInches5x4: TMenuItem;
    MenuItemInches4x3: TMenuItem;
    MenuItemPixels1024x768: TMenuItem;
    MenuItemPixels800x600: TMenuItem;
    MenuItemPixels640x480: TMenuItem;
    MenuItemEditAdjustISO: TMenuItem;
    MenuItemEditAdjustInches: TMenuItem;
    MenuItemEditAdjustPixels: TMenuItem;
    MenuItemCopyToClipboard: TMenuItem;
    MenuItemEditRestoreGeometry: TMenuItem;
    MenuItemEdit: TMenuItem;
    MenuItemConfigBackendReloadOnHup: TMenuItem;
    MenuItemConfigBackendReloadOnNewVersion: TMenuItem;
    MenuItemConfigBackendReloadOnRepoRelease: TMenuItem;
    MenuItemConfigBackendSeparator1: TMenuItem;
    MenuItemConfigBackendReloadNow: TMenuItem;
    MenuItemConfigBackend: TMenuItem;
    MenuItemConfigDebugOutput: TMenuItem;
    MenuItemFileExportVCD: TMenuItem;
    MenuItemFileSeparator1: TMenuItem;
    MenuItemFileSeparator2: TMenuItem;
    MenuItemFileConfigurePort: TMenuItem;
    MenuItemFileClosePort: TMenuItem;
    MenuItemFileOpenPort: TMenuItem;
    MenuItemFileXModemReceive: TMenuItem;
    MenuItemFileXModemCapture: TMenuItem;
    MenuItemFileXModem: TMenuItem;
    MenuItemConfig: TMenuItem;
    MenuItemHelpAboutFrontend: TMenuItem;
    MenuItemHelp: TMenuItem;
    MenuItemQuit: TMenuItem;
    MenuItemFileSelectPort: TMenuItem;
    MenuItemFile: TMenuItem;
    PageControlRightTop: TPageControl;
    PaintBoxTextTop: TPaintBox;
    PaintBoxTextBottom: TPaintBox;
    PaintBoxTickLeft: TPaintBox;
    PaintBoxTickRight: TPaintBox;
    PairSplitter1: TPairSplitter;
    PairSplitter1Left: TPairSplitterSide;
    PairSplitter1Right: TPairSplitterSide;
    PairSplitter2: TPairSplitter;
    PairSplitter2Top: TPairSplitterSide;
    PairSplitter2Bottom: TPairSplitterSide;
    PanelDisplay4: TPanel;
    PanelDisplay3: TPanel;
    PanelDisplay2: TPanel;
    PanelButtonsTop: TPanel;
    PanelButtonsBottom: TPanel;
    PanelDisplay1: TPanel;
    SaveDialog1: TSaveDialog;
    ComboBoxVoltsPerDiv: TComboBox;
    StatusBar1: TStatusBar;
    TabSheetCh1: TTabSheet;
    TabSheetCh2: TTabSheet;
    Timer1: TTimer;
    procedure ChartToolsetFrequencyDomainDataPointClickTool1PointClick(
      ATool: TChartTool; APoint: TPoint);
    procedure ChartToolsetTimeDomainDataPointClickTool1PointClick(ATool: TChartTool;
      APoint: TPoint);
    procedure FormCloseQuery(Sender: TObject; var CanClose: boolean);
    procedure FormCreate(Sender: TObject);
    procedure MenuItemConfigBackendReloadNowClick(Sender: TObject);
    procedure MenuItemConfigDebugOutputClick(Sender: TObject);
    procedure MenuItemConfigFrequencyDomainClick(Sender: TObject);
    procedure MenuItemCopyToClipboardClick(Sender: TObject);
    procedure MenuItemEditAdjust1024x768Click(Sender: TObject);
    procedure MenuItemEditAdjust4x3Click(Sender: TObject);
    procedure MenuItemEditAdjust5x4Click(Sender: TObject);
    procedure MenuItemEditAdjust640x480Click(Sender: TObject);
    procedure MenuItemEditAdjust7x5Click(Sender: TObject);
    procedure MenuItemEditAdjust800x600Click(Sender: TObject);
    procedure MenuItemEditAdjustA5Click(Sender: TObject);
    procedure MenuItemEditAdjustA6Click(Sender: TObject);
    procedure MenuItemEditAdjustA7Click(Sender: TObject);
    procedure MenuItemEditRestoreGeometryClick(Sender: TObject);
    procedure MenuItemExportGIFClick(Sender: TObject);
    procedure MenuItemExportPNGClick(Sender: TObject);
    procedure MenuItemFileClick(Sender: TObject);
    procedure MenuItemFileClosePortClick(Sender: TObject);
    procedure MenuItemFileExportVCDClick(Sender: TObject);
    procedure MenuItemFileOpenPortClick(Sender: TObject);
    procedure MenuItemFileTestWaveformFallingEdgeClick(Sender: TObject);
    procedure MenuItemFileTestWaveformFallingSawClick(Sender: TObject);
    procedure MenuItemFileTestWaveformRisingEdgeClick(Sender: TObject);
    procedure MenuItemFileTestWaveformRisingSawClick(Sender: TObject);
    procedure MenuItemFileTestWaveformTriangleClick(Sender: TObject);
    procedure MenuItemFileXModemCaptureClick(Sender: TObject);
    procedure MenuItemFileXModemReceiveClick(Sender: TObject);
    procedure MenuItemHelpAboutFrontendClick(Sender: TObject);
    procedure MenuItemQuitClick(Sender: TObject);
    procedure MenuItemFileTestWaveformSineClick(Sender: TObject);
    procedure MenuItemFileTestWaveformSquareClick(Sender: TObject);
    procedure MenuItemFileTestWaveformUnitPulseClick(Sender: TObject);
    procedure PaintBoxTextBottomPaint(Sender: TObject);
    procedure PaintBoxTextTopPaint(Sender: TObject);
    procedure PaintBoxTickLeftPaint(Sender: TObject);
    procedure PaintBoxTickRightPaint(Sender: TObject);
    procedure PairSplitter2TopResize(Sender: TObject);
    procedure Timer1Timer(Sender: TObject);
  strict private
    portHandle: TSerialHandle;
    portName: string;                   (* From menu item                       *)
    bitsPerSec: longint;                (* These only good after file menu      *)
    byteSize, stopBits: integer;        (* clicked at least once.               *)
    parity: TParityType;
    flags: TSerialFlags;
    portParamsLocked: boolean;
    fScopeStruct: TScopeStruct;
    fOriginalFormWidth, fOriginalFormHeight, fWidthSplitterOriginalPosition,
                                        fHeightSplitterOriginalPosition: integer;
    maxFftAmplitude: double;
    procedure drawScreens(Sender: TObject);
    procedure restoreOriginalGeometry;
    procedure setGeometryForScreen(x, y: integer);
    procedure OnAfterShow(afterShowParam: PtrInt);
  public
    function GetProjectName(): string;

    (* Message box optimised for position.
    *)
    FUNCTION MessageDlgOpt(CONST aMsg: STRING; dlgType: TMsgDlgType;
                            buttons: TMsgDlgButtons; helpCtx: LONGINT): INTEGER;

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

    property ProjectName: string read GetProjectName;
  end;

var
  FormDsoCat: TFormDsoCat;

{$define FRONTEND_MAGIC_NUMBER }
{$define FRONTEND_ENTRY_POINTS }
{$i frontendprocs.inc          }

(* Copy the program version etc. into the parameter as a C-style string. This
  is intended as an experimental secondary entry point that a hypothetical
  desktop GUI etc. can use to get program version information.
*)
function ModuleAboutText(ver: pchar; verLength: word): word; cdecl; experimental;


implementation

{$R *.lfm}

{ TFormDsoCat }

uses
  StrUtils, BaseUnix, Termio, Regexpr, ClipBrd, LCLType, LCLIntf, IniFilesAbout,
  DateUtils, XModem, VcdCode, FftChart { , FftwChart }
{$ifdef USE_STATIC } , Backend {$else } , Dynamod, BackendDynamic {$endif USE_STATIC } ;

const
  ProjName= 'DSO-Cat';

(* The location of the series to actually be displayed might vary depending on  *)
(* what other traces are loaded as "furniture" and their relative order- in     *)
(* particular constant series representing zero volts and trigger times.        *)

  ch1Data= 0;                           (* Index into ChartTimeDomain.Series[]  *)
  zeroVolts= 1;
  triggerTime= 2;
  allFftBins= false;
  scaleFftToUnity= false;

(* Comboboxes etc. for voltage and time units-per-division.                     *)

  v2m= 0;
  v5m= 1;
  v10m= 2;
  v20m= 3;
  v50m= 4;
  v100m= 5;
  v200m= 6;
  v500m= 7;
  v1= 8;
  v2= 9;
  v5= 10;
  v10= 11;
  v20= 12;

  t1u= 0;
  t2u= 1;
  t5u= 2;
  t10u= 3;
  t20u= 4;
  t50u= 5;
  t100u= 6;
  t200u= 7;
  t500u= 8;
  t1m= 9;
  t2m= 10;
  t5m= 11;
  t10m= 12;
  t20m= 13;
  t50m= 14;
  t100m= 15;
  t200m= 16;
  t500m= 17;
  t1= 18;
  t2= 19;
  t5= 20;
  t10= 21;
  t20= 22;
  t50= 23;

var
  seenQuit: boolean= false;
  seenHUP: boolean= false;              (* Set by hangup signal, maskable       *)
  seenHUP2: boolean= false;             (* Set from seenHUP or by update check  *)


procedure hupHandler(sig: longint; info: PSigInfo; context: PSigContext); cdecl;

begin
  case sig of
    SIGHUP: seenHup := true
  else
  end
end { hupHandler } ;


(* Read manufacturer name etc. This is a standard file operation handling data
  generated by the kernel, so I'm being fairly relaxed about the risk of
  overrun etc.
*)
function cat(const fn: string): string;

var
  handle: THandle;
  buffer: array[0..255] of byte;

begin
  handle := FileOpen(fn, fmOpenRead);
  if handle >= 0 then
    try
      FillByte(buffer, SizeOf(buffer), 0);
      if FileRead(handle, buffer, SizeOf(buffer) - 1) > 0 then
        result := Trim(StrPas(Addr(buffer)))  (* Discard trailing EOL etc.    *)
      else
        result := ''
    finally
      FileClose(handle)
    end
  else
    result := ''
end { cat } ;


(* Prior to FPC 3.2.0 FileExists() also matched directories. This function is
  optimised by version to avoid a spurious call if FileExists() fails on FPC
  3.0.4 or older.
*)
function fileOrDirectoryExists(const name: RawByteString; followLink: boolean= true): boolean;

begin
{$if FPC_FULLVERSION < 030200 }         (* Requires FPC 2.2.4 minimum           *)
  result := FileExists(name { , followLink } )
{$else  }
  result := FileExists(name, followLink) or DirectoryExists(name, followLink)
{$endif }
end { fileOrDirectoryExists } ;


type
  TWalkTestPredicate= function(const dir: string; const name: string;
                        const content: string; const extra: string): boolean;


(* Look in the directory specified as the first parameter (terminated by /) and
  either return the canonical path if a directory matching the second parameter
  is encountered or recurse into any other directories. If the third parameter
  exists then the directory that is found must contain it, however it is not
  appended to the result and the result is not terminated by /, in other cases
  the result is terminated by /. The final parameter is an optional test which
  is applied after all naming requirements have been met.

  We know that this is being used on the /sys filesystem which contains numerous
  recursive symlinks, so need to look at real directories *only*. The format of
  the directory names appears to have been stable from Linux 2.6 to at least 4,
  however the position in the tree that the hidraw directory is located depends
  on e.g. whether the mouse device is connected by USB or Bluetooth:

  USB: /sys/devices/pci0000:00/0000:00:03.1/usb4/4-3/4-3:1.0/0003:1BCF:0053.0009/hidraw

  Bt:  /sys/devices/virtual/misc/uhid/0005:248A:8266.000D/hidraw

  The hidraw directory (note: no appended number) contains an hidrawN directory
  (note: one or more decimal digits appended) which should match something in
  /dev giving us a file name which can be handled by open(), ioctl() and so on.
*)
function WalkDirs(dir: string; pattern: string; content: string= '';
                        wtp: TWalkTestPredicate= nil; extra: string= ''): string;

(* This is lifted from my Logitech G600 interface program.                      *)

var
  searchRec: TUnicodeSearchRec;
  sorter: TStringList;
  i: integer;
  candidate: string;


  function wtp2(const dir: string; const name: string; const content: string;
                                        const extra: string): boolean; inline;

  begin
    if assigned(wtp) then
      result := wtp(dir, name, content, extra)
    else
      result := true
  end { wtp2 } ;


begin
  result := '';
  if FindFirst(dir + '*'{%H-}, faDirectory, searchRec) = 0 then begin
    sorter := TStringList.Create;
    sorter.Sorted := true;
    try

(* It is not safe to assume that the kernel returns names in a natural or       *)
(* reproducible order, particularly when looking at e.g. /sys, so handle it in  *)
(* two stages.                                                                  *)

      repeat
        if searchRec.Name[1] = '.' then (* Always ignore . and .. completely    *)
          continue;

(* For everything in the directory, make sure it's a subdirectory and not a     *)
(* symlink. Don't try to use searchRec.Attr for this.                           *)

        if FileGetAttr({%H-}dir + searchRec.Name) and (faDirectory + faSymLink{%H-}) = faDirectory then
          sorter.Add(searchRec.Name{%H-})
      until FindNext(searchRec) <> 0;
      for i := 0 to sorter.Count - 1 do begin
        candidate := sorter[i];         (* Make visible for debugging           *)

(* Does the subdirectory name match the pattern?                                *)

        if IsWild(candidate, pattern, false) then

(* If there's content specified does it exist in the subdirectory, a symlink    *)
(* being acceptable in this case? Note that this also determines whether / is   *)
(* appended to the result, the rationale being that if we are explicitly        *)
(* looking for some content then we're likely to be continuing to process the   *)
(* tree in some form. If all conditions are satisfactory then apply one final   *)
(* test passed as a function, which can be used to distinguish between e.g.     *)
(* multiple endpoints associated with the same USB device.                      *)

          if content = '' then
            if wtp2(dir, candidate, content, extra) then
              exit(dir + candidate)
            else begin end              (* Watch for dangling else here         *)
          else
            if fileOrDirectoryExists(dir + candidate + '/' + content) then
              if wtp2(dir, candidate, content, extra) then
                exit(dir + candidate + '/');

(* We've got a subdirectory but either it doesn't match the pattern or it lacks *)
(* the expected content: recurse.                                               *)

        result := WalkDirs(dir + candidate + '/', pattern, content, wtp, extra);

(* If recursion found a hit then return it.                                     *)

        if result <> '' then
          exit
      end
    finally
      Sorter.Free;
      FindClose(searchRec)
    end
  end
end { WalkDirs } ;


(* With the naming requirements met, make one final check that the serial
  device has a plausible description.
*)
function testDriver(const dir: string; const name: string; const content: string;
                                                const extra: string): boolean;

var
  scratch: string;

begin

(* The directory parameter will look something like /sys/devices/pci0000:00/0000:00:1d.0/usb2/2-2/2-2:1.0/ttyUSB2/tty *)
(* ...ttyUSB2/driver should be a symlink to /sys/bus/usb-serial/drivers/cp210x  *)
(* but might be distressingly relative.                                         *)

  scratch := dir + name + '/' + content;
  scratch := fpReadLink(scratch);
  result := pos('/' + extra, scratch) > 0
end { testDriver } ;


(* Return true if the device named in the form /dev/ttyUSBn appears to be using
  a plausible device driver module.
*)
function usingDriver(const device, driver: string): boolean;

var
  name, path: string;

begin
  result := false;
  name := ExtractFilename(device);
  path := WalkDirs('/sys/devices/', name, 'driver', @testDriver, driver); (* Appends /  *)
  result := path <> ''
end { usingDriver } ;


(* With the naming requirements met, make one final check that the serial
  device has a plausible description.
*)
function testProduct(const dir: string; const name: string; const content: string;
                                                const extra: string): boolean;

var
  scratch: string;

begin

(* The directory parameter will look something like /sys/devices/pci0000:00/0000:00:1d.0/usb2/2-2/2-2:1.0/ttyUSB2/tty *)
(* ...ttyUSB2/driver should be a symlink to /sys/bus/usb-serial/drivers/cp210x  *)
(* but might be distressingly relative.                                         *)

  scratch := dir + name + '/' + content;
  if not fileOrDirectoryExists(scratch) then
    scratch := ''
  else
    scratch := cat(scratch);
  result := extra = scratch
end { testProduct } ;


(* Return true if the device named in the form /dev/ttyUSBn has a plausible
  product description.
*)
function usingProduct(const device, product: string): boolean;

var
  name, path: string;

begin
  result := false;
  name := ExtractFilename(device);
  path := WalkDirs('/sys/devices/', name, '../../product', @testProduct, product); (* Appends /  *)
  result := path <> ''
end { usingProduct } ;


(* With the naming requirements met, make one final check that the serial
  device has a plausible description.
*)
function testSerial(const dir: string; const name: string; const content: string;
                                                const extra: string): boolean;

var
  scratch: string;

begin

(* The directory parameter will look something like /sys/devices/pci0000:00/0000:00:1d.0/usb2/2-2/2-2:1.0/ttyUSB2/tty *)
(* ...ttyUSB2/driver should be a symlink to /sys/bus/usb-serial/drivers/cp210x  *)
(* but might be distressingly relative.                                         *)

  scratch := dir + name + '/' + content;
  if not fileOrDirectoryExists(scratch) then
    scratch := ''
  else
    scratch := cat(scratch);
  result := extra = scratch
end { testSerial } ;


(* Return true if the device named in the form /dev/ttyUSBn has a plausible
  serial number.
*)
function usingSerial(const device, serial: string): boolean;

var
  name, path: string;

begin
  result := false;
  name := ExtractFilename(device);
  path := WalkDirs('/sys/devices/', name, '../../serial', @testSerial, serial); (* Appends /  *)
  result := path <> ''
end { usingSerial } ;


var
  portsListCache: TStringList= nil;


(* Assume that the list contains name=value pairs, and order by value.
*)
function compareValues(List: TStringList; Index1: Integer; Index2: Integer):Integer;

var
  v1, v2: integer;

begin
  v1 := StrToIntDef(ExtractDelimited(2, List[Index1], ['=']), -99999);
  v2 := StrToIntDef(ExtractDelimited(2, List[Index2], ['=']), +99999);
  if v1 = v2 then
    result := 0
  else
    if v1 < v2 then
      result := -1
    else
      result := +1
end { compareValues } ;


(* A numbered list of ports is prepared at startup, or any time this is called
  and no port is open. The list is cached as necessary and also returned as a
  StringList, which should be freed by the caller.
*)
FUNCTION ListPorts(currentPort: TSerialHandle= InvalidSerialHandle): TStringList;

(* This is lifted from a port I'm working on of some very old DOS/Windows code  *)
(* implementing a proprietary protocol.                                         *)

var
  searchRec: TSearchRec;
  majorList, minorList: TStringList;
  r: TRegExpr;
  i, j: integer;


  (* The parameter is a number, extract as implemented in the clib macros.
  *)
  function major(dev: qword): dword;

//  __NTH (gnu_dev_major (unsigned long long int __dev))
//  {
//    return ((__dev >> 8) & 0xfff) | ((unsigned int) (__dev >> 32) & ~0xfff);
//  }

  begin
    result := ((dev shr 8) and $0fff) or ((dev shr 32) and $fffff000)
  end { major } ;


  (* The parameter is a number, extract as implemented in the clib macros.
  *)
  function minor(dev: qword): dword;

//  __NTH (gnu_dev_minor (unsigned long long int __dev))
//  {
//    return (__dev & 0xff) | ((unsigned int) (__dev >> 12) & ~0xff);
//  }

  begin
    result := (dev and $00ff) or ((dev shr 12) and $ffffff00)
  end { minor } ;


  (* The parameter is a number as documented in the inode structure.
  *)
  function isChr(mode: dword): boolean;

  begin
    result := mode and &0120000 = &0020000
  end { isChr } ;


  (* The parameter is the entire device name.
  *)
  function major(const devName: string): dword;

  var
    devStat: Stat;

  begin
    FillByte(devStat{%H-}, SizeOf(devStat), 0);
    if FpStat(devName, devStat) <> 0 then
      result := 0
    else
      result := major(devStat.st_rdev)
  end { major } ;


  (* The parameter is the entire device name.
  *)
  function minor(const devName: string): dword;

  var
    devStat: Stat;

  begin
    FillByte(devStat{%H-}, SizeOf(devStat), 0);
    if FpStat(devName, devStat) <> 0 then
      result := 0
    else
      result := minor(devStat.st_rdev)
  end { minor } ;


  (* The parameter is the entire device name.
  *)
  function isChr(const devName: string): boolean;

  var
    devStat: Stat;

  begin
    FillByte(devStat{%H-}, SizeOf(devStat), 0);
    if FpStat(devName, devStat) <> 0 then
      result := false
    else
      result := isChr(devStat.st_mode)
  end { isChr } ;


  (* The parameter is the first part of the device name, i.e. with no appended
    digits.
  *)
  function blacklisted(const {%H-}devName: string): boolean;

  begin
    result := false
  end { blacklisted } ;


begin
  if currentPort <> InvalidSerialHandle then begin
    if portsListCache = nil then
      result := nil
    else begin
      result := TStringList.Create;
      result.Assign(portsListCache)
    end;
    exit
  end;
  if portsListCache = nil then
    portsListCache := TStringList.Create
  else
    portsListCache.Clear;
  majorList := TStringList.Create;
  minorList := TStringList.Create;
  r := TRegExpr.Create;
  r.Expression := '(tty\D+)\d+';
  try

(* Run through all devices named like /dev/tty[[:alpha:]]+[[:digit:]]+ checking *)
(* that they're character-mode and not blacklisted and saving the major device  *)
(* number. The final result is a list of devices without appended digits, plus  *)
(* the associated major device number.                                          *)

    majorList.Sorted := true;
    majorList.Duplicates := dupIgnore;
    IF FindFirst('/dev/tty*', faSysFile{%H-}, searchRec) = 0 THEN
      REPEAT
        if r.Exec(searchRec.Name) and isChr('/dev/' + searchRec.Name) then
          if not blacklisted('/dev/' + r.Match[1]) then
            majorList.Add('/dev/' + r.Match[1] + '=' + IntToStr(major('/dev/' + searchRec.Name)))
      UNTIL FindNext(searchRec) <> 0;
    FindClose(searchRec);

(* Sort by major device number. Enumeration order might be determined by the    *)
(* order that kernel modules are loaded, in practice it's probably best to make *)
(* no assumption in which case the default Quicksort is probably appropriate.   *)

    majorList.Sorted := false;
    majorList.CustomSort(@compareValues);

(* For each major device get the actually-available physical devices. This      *)
(* assumes that udev or equivalent is being used to create devices dynamically, *)
(* i.e. won't work with Linux kernels older than 2.6 (circa 2003), in fact it   *)
(* doesn't even bother trying to check for statically-allocated systems since   *)
(* this would end up in a minefield of heuristics looking at the age of the     *)
(* kernel, whether it was possible to decide whether udev support was compiled  *)
(* into the kernel, whether systemd or equivalent was running and so on: it's   *)
(* quite simply not worth it for a change made 15 years ago particularly since  *)
(* this code "fails safe" by possibly listing more devices than actually exist  *)
(* rather than by hiding some which can't be opened as a result.                *)

    for i := 0 to majorList.Count - 1 do begin
      minorList.Clear;
      IF FindFirst(majorList.Names[i] + '*', faSysFile{%H-}, searchRec) = 0 THEN
        REPEAT
          minorList.Add('/dev/' + searchRec.Name + '=' + IntToStr(minor('/dev/' + searchRec.Name)))
        UNTIL FindNext(searchRec) <> 0;
      FindClose(searchRec);

(* In the context of the current major device, sort by minor device number. The *)
(* enumeration order might be reversed, which suggests that something like a    *)
(* comb sort would be appropriate; however this can't be relied on so again use *)
(* the default Quicksort.                                                       *)

      minorList.CustomSort(@compareValues);

(* Discarding major and minor device number, append the name to the cache.      *)

      for j := 0 to MinorList.Count - 1 do
        portsListCache.Append(minorList.Names[j])
    end;

(* The cached result should first have traditional serial devices /dev/ttySx,   *)
(* ISDN devices /dev/ttyIx, USB devices /dev/ttyUSBx with additional support    *)
(* for devices implemented by multiport cards etc. inserted as appropriate      *)
(* based on the major device numbers which were allocated approximately         *)
(* chronologically. It should also have any "Low-density serial ports" found    *)
(* to be present, hopefully at the end of the list, where those are e.g. on-    *)
(* chip console ports and are distinguished by minor rather than major device   *)
(* number:                                                                      *)
(*                                                                              *)
(*   4 /dev/ttySx                                                               *)
(*  43 /dev/ttyIx                                                               *)
(* 188 /dev/ttyUSBx                                                             *)
(* 204 /dev/ttyAMAx (undocumented Raspberry Pi serial console port) etc.        *)
(*                                                                              *)
(* The overall result will hopefully be "correct" both from the system and user *)
(* POV. See Documentation/devices.txt or Documentation/admin-guide/devices.txt  *)
(* in the Linux source tree.                                                    *)

  finally
    r.Free;
    minorList.Free;
    majorList.Free
  end;
  result := TStringList.Create;
  result.Assign(portsListCache)
end { ListPorts } ;


(* We know what kind of serial device is in the DSO, so look for that specific
  driver (Linux kernel module).
*)
function selectAsDefault(const portName: string): boolean;

begin

(* If all the potential device tests are blank then we can make no useful       *)
(* decision about whether a port is probably the device we want to talk to. In  *)
(* this case assume that the first device in the list will remain selected.     *)

  if Backend.PortName() + Backend.PortDriverName() + Backend.PortProductDescription() +
                                        Backend.PortSerialDescription() = '' then
    exit(false);
  result := true;

(* At least one of the tests is non-blank, so we can probably make some useful  *)
(* decisions. For example, we probably know something about the name of the     *)
(* port, e.g. it's going to be one of the /dev/ttyUSBn ports for a DSO112A so   *)
(* we don't have to waste time checking /dev/ttySn. If the port name is         *)
(* plausible then walk part of the /sys tree to try to find the driver name, in *)
(* the case of the DSO112A we know that this will be a Silicon Labs cp210x      *)
(* since it's internal to the device, and possibly continue by looking at the   *)
(* product description and serial number.                                       *)

  if Backend.PortName() <> '' then begin
    DebugWriteF('Testing port %s against pattern %s\n', [portName, Backend.PortName()]);
    if Pos(Backend.PortName(), portName) = 0 then
      exit(false)
  end;
  if Backend.PortDriverName() <> '' then begin
    DebugWriteF('Testing against driver name %s\n', [Backend.PortDriverName()]);
    if not usingDriver(portName, Backend.PortDriverName()) then
      exit(false)
  end;
  if Backend.PortProductDescription() <> '' then begin
    DebugWriteF('Testing against product description %s\n', [Backend.PortProductDescription()]);
    if not usingProduct(portName, Backend.PortProductDescription()) then
      exit(false)
  end;
  if Backend.PortSerialDescription() <> '' then begin
    DebugWriteF('Testing against serial description %s\n', [Backend.PortSerialDescription()]);
    if not usingSerial(portName, Backend.PortSerialDescription()) then
      exit(false)
  end

(* If either there's no useful tests we can make or all tests have been         *)
(* successful, return true. The result of this will be that either the first    *)
(* device in the list or the last device that matches will have its radio       *)
(* button set.                                                                  *)

end { selectAsDefault } ;


procedure TFormDsoCat.MenuItemFileClick(Sender: TObject);

var
  i: integer;
  portNames: TStringList;

begin
  portParamsLocked := Backend.PortDefaultParams(bitsPerSec, byteSize, parity, stopBits, flags);
  if portHandle = InvalidSerialHandle then begin
    StatusBar1.SimpleText := Format('%30s', ['Enumerating ports...']);
    MenuItemFileSelectPort.Clear;
    Application.ProcessMessages;

(* Initialise the menu from the known ports.                                    *)

    portNames := ListPorts();
    try
      for i := 0 to portNames.Count - 1 do begin
        MenuItemFileSelectPort.Add(TMenuItem.Create(nil));
        with MenuItemFileSelectPort.Items[i] do begin
          Caption := portnames[i];
          GroupIndex := 1;
          RadioItem := true;
          if i = 0 then                 (* First device selected by default     *)
            Checked := true
          else

(* Let's assume that because we're adding devices in sequence, that the most    *)
(* recent that the OS has seen plugged in appears last. Select this as the      *)
(* device to be used if it matches certain critera.                             *)

(********************************************************************************)
(*                                                                              *)
(* DON'T TRY DEBUGGING THIS INTERACTIVELY. There's analogous code that can be   *)
(* enabled in the program startup, and debugging click handlers in situ results *)
(* in nothing but grief.                                                        *)
(*                                                                              *)
(********************************************************************************)

            Checked := selectAsDefault(Caption);
          AutoCheck := true
        end
      end
    finally
      FreeAndNil(portNames)
    end;
    StatusBar1.SimpleText := '';
    Application.ProcessMessages;
    MenuItemFileOpenPort.Enabled := true;
    MenuItemFileConfigurePort.Enabled := false;
    MenuItemFileClosePort.Enabled := false;
    MenuItemFileXModem.Enabled := false;
    MenuItemFileTestWaveform.Enabled := true
  end else begin
    MenuItemFileOpenPort.Enabled := false;
    MenuItemFileConfigurePort.Enabled := not portParamsLocked;
    MenuItemFileClosePort.Enabled := true;
    MenuItemFileXModem.Enabled := true;
    MenuItemFileTestWaveform.Enabled := false
  end
end { TFormDsoCat.MenuItemFileClick } ;


procedure TFormDsoCat.MenuItemFileClosePortClick(Sender: TObject);

begin
  SerClose(portHandle);
  portHandle := InvalidSerialhandle;
  StatusBar1.SimpleText := Format('%16s', ['Port closed'])
end { TFormDsoCat.MenuItemFileClosePortClick } ;


procedure TFormDsoCat.MenuItemFileExportVCDClick(Sender: TObject);

var
  nameBlank: boolean;

begin
  nameBlank := Trim(SaveDialog1.Filename) = '';
  if nameBlank then
    SaveDialog1.Filename := projName + '_' + StringReplace(IsoNow(), ' ', 'T', [rfReplaceAll]);
  SaveDialog1.DefaultExt := 'vcd';

(* Possibly replace this later with a custom setup form, in particular to       *)
(* select transition voltage between false and true.                            *)

  if SaveDialog1.Execute then
    ExportVcd(fScopeStruct, SaveDialog1.Filename);
  if nameBlank then
    SaveDialog1.Filename := ''
end { TFormDsoCat.MenuItemFileExportVCDClick } ;


procedure TFormDsoCat.MenuItemFileOpenPortClick(Sender: TObject);

var
  i: integer;


  function oneChar(parity: TParityType): char;

  begin
    case parity of
      OddParity:  result := 'O';
      EvenParity: result := 'E'
    otherwise
      result := 'N'
    end
  end { oneChar } ;


begin
  portName := '/dev/null/';
  for i := 0 to MenuItemFileSelectPort.Count - 1 do
    if MenuItemFileSelectPort.Items[i].Checked then
      portName := MenuItemFileSelectPort.Items[i].Caption;
  portHandle := SerOpen(portName);
{$ifdef UNIX }
  if portHandle > 0 then
    if fpIoctl(portHandle, TIOCEXCL, nil) <> 0 then begin (* Mandatory lock,    *)
      SerClose(portHandle);             (* unlike flock() (if it even works in  *)
      portHandle := -1                  (* this context) or a lock file as used *)
    end;                                (* by various gettys etc.               *)
{$endif UNIX }
  if portHandle <= 0 then begin
    MessageDlgOpt('Unable to open ' + portName, mtError, [mbOk], 0);
    portHandle := InvalidSerialHandle
  end else begin
    SerSetParams(portHandle, bitsPerSec, byteSize, parity, stopBits, flags);
// TODO : Override any or all port parameters from .ini file.
    StatusBar1.SimpleText := Format('%24s %16d %8d-%s-%d', [portName, bitsPerSec,
                                        byteSize, oneChar(parity), stopBits])
  end
end { TFormDsoCat.MenuItemFileOpenPortClick } ;


(* Convert a floating point number to a normalised value in the range 0.1 to
  99.9 and a scale factor, then return as a string comprising a number and a
  standard scaling character.
*)
function engineeringFormat(r: single; noSuffix: boolean= false): string;

var
  normalised, scale: single;
  m, f: string;

begin
  if Abs(r) < 100.0e-6 then begin
    scale := 1.0e6;
    m := ' µ'
  end else
    if Abs(r) < 100.0e-3 then begin
      scale := 1.0e3;
      m := ' m'
    end else
      if Abs(r) >= 0.1e3 then begin
        scale := 1.0e-3;
        m := ' k'
      end else
        if Abs(r) >= 0.1e6 then begin
          scale := 1.0e-6;
          m := ' M'
        end else
          begin
            scale := 1.0;
            m := ' '
          end;
  normalised := r * scale;
  if Abs(normalised) < 1.0 then
    f := '%.3f'
  else
    if Abs(normalised) < 10.0 then
      f := '%.2f'
    else
      f := '%.1f';
  result := Format(f, [normalised]);
  if not noSuffix then
    result += m
end { engineeringFormat } ;


(* No output, use a debugger on this.
*)
procedure testEngineeringFormat;

var
  scratch: string;

begin
  scratch := engineeringFormat(0.099);
  scratch := engineeringFormat(0.1);
  scratch := engineeringFormat(0.99);
  scratch := engineeringFormat(1.0);
  scratch := engineeringFormat(9.9);
  scratch := engineeringFormat(10.0);
  scratch := engineeringFormat(99.0);
  scratch := engineeringFormat(100.0);

  scratch := engineeringFormat(0.099e-6);
  scratch := engineeringFormat(1.0e-6);
  scratch := engineeringFormat(99.9e-6);
  scratch := engineeringFormat(100.0e-6);

  scratch := engineeringFormat(0.099e-3);
  scratch := engineeringFormat(1.0e-3);
  scratch := engineeringFormat(99.9e-3);
  scratch := engineeringFormat(100.0e-3);

  scratch := engineeringFormat(0.099e-0);
  scratch := engineeringFormat(1.0e-0);
  scratch := engineeringFormat(99.9e-0);
  scratch := engineeringFormat(100.0e-0)
end { testEngineeringFormat } ;


(* fScopeStruct should contain configuration information and data, draw the
  time- and frequency-domain screens ar well as supporting furniture.
*)
procedure TFormDsoCat.drawScreens(Sender: TObject);

{$define DEBUG_VOLTAGE }
{$undef DEBUG_TIMEBASE }

var
  i, j: integer;
  markPos: single;

begin
  if fScopeStruct <> nil then
    try
      TLineSeries(ChartTimeDomain.Series[ch1Data]).Clear;

(* Get the vertical (voltage) and possibly horizontal (timebase) scaling from   *)
(* the instrument, apply to the graph and displayed controls as appropriate.    *)

// TODO : Work needed here to accommodate devices with different discrete voltage/time control values

// In practice I might (probably will) junk most of this and use one of the
// marginal panels to display the settings that have arrived from the backend,
// leaving the interactive controls as a way of specifying what the backend
// should do when it's being driven by the PC (i.e. bypassing its own on-screen
// or frontpanel controls).
//
// Code here is for the DSO112A, a Tek222A looks something like
//
// acq[acq_Ch2VoltsDiv]:= (scratch1 & 0x0f) + 1 ? 0.005 : 0.01 : 0.02 : 0.05 :
//                       0.1 : 0.2 : 0.5 : 1.0 : 2.0 : 5.0 : 10.0 :
//                                               20.0 : 50.0 : 0.0;
// acq[acq_SecDiv]:= ((scratch2 & 0x1f) + 1) ? 0.00000005 :
//                               0.0000001 : 0.0000002 : 0.0000005 :
//                               0.000001 : 0.000002 : 0.000005 :
//                               0.00001 : 0.00002 : 0.00005 :
//                               0.0001 : 0.0002 : 0.0005 :
//                               0.001 : 0.002 : 0.005 :
//                               0.01 : 0.02 : 0.05 :
//                               0.1 : 0.2 : 0.5 :
//                               1.0 : 2.0 : 5.0 :
//                              10.0 : 20.0 : 0.0;
//
// i.e. 5mV..50V and 50nS..20S per division, with an increment scheme which is
// broadly compatible.

      i := Trunc(fScopeStruct.Channels[0].VoltsPerDivision * 1.0e3); (* mV per division     *)
      case i of
        0..3:    ComboBoxVoltsPerDiv.ItemIndex := v2m;  (* 2 mV    *)
        4..7:    ComboBoxVoltsPerDiv.ItemIndex := v5m;  (* 5 mV    *)
        8..16:   ComboBoxVoltsPerDiv.ItemIndex := v10m; (* 10 mV   *)
        17..35:  ComboBoxVoltsPerDiv.ItemIndex := v20m; (* 20 mV   *)
        36..75:  ComboBoxVoltsPerDiv.ItemIndex := v50m; (* 50 mV   *)
        76..149: ComboBoxVoltsPerDiv.ItemIndex := v100m; (* 100 mV *)
        150..
        349:     ComboBoxVoltsPerDiv.ItemIndex := v200m; (* 200 mV *)
        350..
        749:     ComboBoxVoltsPerDiv.ItemIndex := v500m; (* 500 mV *)
        750..
        1499:    ComboBoxVoltsPerDiv.ItemIndex := v1;   (* 1.0 V   *)
        1500..
        3499:    ComboBoxVoltsPerDiv.ItemIndex := v2;   (* 2.0 V   *)
        3500..
        7499:    ComboBoxVoltsPerDiv.ItemIndex := v5;   (* 5.0 V   *)
        7500..
        14999:   ComboBoxVoltsPerDiv.ItemIndex := v10;  (* 10 V    *)
      otherwise
        ComboBoxVoltsPerDiv.ItemIndex := v20            (* 20 V    *)
      end;
      ComboBoxCoupling.ItemIndex := Ord(fScopeStruct.Channels[0].Couple);

// TODO : Work needed here to accommodate devices with different discrete voltage/time control values

      i := Trunc(fScopeStruct.SecondsPerDivision * 1.0e6); (* uS per division     *)
      case i of
        0..1:     ComboBoxSecsPerDiv.ItemIndex := t1u;  (* 1 µS    *)
        2..3:     ComboBoxSecsPerDiv.ItemIndex := t2u;  (* 2 µS    *)
        4..7:     ComboBoxSecsPerDiv.ItemIndex := t5u;  (* 5 µS    *)
        8..16:    ComboBoxSecsPerDiv.ItemIndex := t10u; (* 10 µS   *)
        17..35:   ComboBoxSecsPerDiv.ItemIndex := t20u; (* 20 µS   *)
        36..75:   ComboBoxSecsPerDiv.ItemIndex := t50u; (* 50 µS   *)
        76..149:  ComboBoxSecsPerDiv.ItemIndex := t100u; (* 100 µS *)
        150..
        349:      ComboBoxSecsPerDiv.ItemIndex := t200u; (* 200 µS *)
        350..
        749:      ComboBoxSecsPerDiv.ItemIndex := t500u; (* 500 µS *)
        750..
        1499:     ComboBoxSecsPerDiv.ItemIndex := t1m;  (* 1 mS    *)
        1500..
        3499:     ComboBoxSecsPerDiv.ItemIndex := t2m;  (* 2 mS    *)
        3500..
        7499:     ComboBoxSecsPerDiv.ItemIndex := t5m;  (* 5 mS    *)
        7500..
        14999:    ComboBoxSecsPerDiv.ItemIndex := t10m; (* 10 mS   *)
        15000..
        34999:    ComboBoxSecsPerDiv.ItemIndex := t20m; (* 20 mS   *)
        35000..
        74999:    ComboBoxSecsPerDiv.ItemIndex := t50m; (* 50 mS   *)
        75000..
        149999:   ComboBoxSecsPerDiv.ItemIndex := t100m; (* 100 mS *)
        150000..
        349999:   ComboBoxSecsPerDiv.ItemIndex := t200m; (* 200 mS *)
        350000..
        749999:   ComboBoxSecsPerDiv.ItemIndex := t500m; (* 500 mS *)
        750000..
        1499999:  ComboBoxSecsPerDiv.ItemIndex := t1;   (* 1 S     *)
        1500000..
        3499999:  ComboBoxSecsPerDiv.ItemIndex := t2;   (* 2 S     *)
        3500000..
        7499999:  ComboBoxSecsPerDiv.ItemIndex := t5;   (* 5 S     *)
        7500000..
        14999999: ComboBoxSecsPerDiv.ItemIndex := t10;  (* 10 S    *)
        15000000..
        34999999: ComboBoxSecsPerDiv.ItemIndex := t20   (* 20 S    *)
      otherwise
        ComboBoxSecsPerDiv.ItemIndex := t50             (* 50 S    *)
      end;
      ComboBoxTriggerMode.ItemIndex := Ord(fScopeStruct.TriggerMode);
      ComboBoxTriggerEdge.ItemIndex := Ord(fScopeStruct.TriggerSlope);

// TODO : Work needed here to accommodate devices with different discrete voltage/time control values

(* At design time and program start the chart component's default grid is used  *)
(* in order that something appears on the screen that looks vaguely plausible,  *)
(* the disadvantage of this is that the division size varies as the display is  *)
(* zoomed in and out. Once we have data replace this grid with one derived from *)
(* specialist components, which will not zoom allowing the usual "per division" *)
(* scope calibration to be used.                                                *)

// WORKING NOTE: vertical grid scaling is messed up by the presence of labels,
// which appear to compress the display in order to give themselves adequate
// room. I don't know whether this can be fixed by telling TAChart to position
// labels towards the centre rather than towards the edge.

      if not Assigned(ChartTimeDomain.LeftAxis.Marks.Source) then
        ChartTimeDomain.LeftAxis.Marks.Source := ListChartSourceTimeDomainLeftAxis;
      if not Assigned(ChartTimeDomain.BottomAxis.Marks.Source) then
        ChartTimeDomain.BottomAxis.Marks.Source := ListChartSourceTimeDomainBottomAxis;

(* Transfer collected data and scaling to the chart component.                  *)

// TODO : Eliminate dependence on comboboxes here since they will probably be greyed out when not being used for instrument control.

      try
        with ChartTimeDomain.LeftAxis do begin
          Range.UseMin:= false;
          Range.UseMax:= false
        end;
        with ChartTimeDomain.LeftAxis.Marks do
          if Assigned(Source) then begin
            Assert(Source is TListChartSource);
            (Source as TListChartSource).Clear
          end else begin
            Range.UseMin:= false;
            Range.UseMax:= false
          end;
        case ComboBoxVoltsPerDiv.ItemIndex of
          v2m:   begin
                   fScopeStruct.Channels[0].minVoltsAxis := -10.0e-3 + fScopeStruct.Channels[0].positionOffsetVolts;
                   fScopeStruct.Channels[0].maxVoltsAxis := +10.0e-3 + fScopeStruct.Channels[0].positionOffsetVolts
                 end;
          v5m:   begin
                   fScopeStruct.Channels[0].minVoltsAxis := -25.0e-3 + fScopeStruct.Channels[0].positionOffsetVolts;
                   fScopeStruct.Channels[0].maxVoltsAxis := +25.0e-3 + fScopeStruct.Channels[0].positionOffsetVolts
                 end;
          v10m:  begin
                   fScopeStruct.Channels[0].minVoltsAxis := -50.0e-3 + fScopeStruct.Channels[0].positionOffsetVolts;
                   fScopeStruct.Channels[0].maxVoltsAxis := +50.0e-3 + fScopeStruct.Channels[0].positionOffsetVolts
                 end;
          v20m:  begin
                   fScopeStruct.Channels[0].minVoltsAxis := -100.0e-3 + fScopeStruct.Channels[0].positionOffsetVolts;
                   fScopeStruct.Channels[0].maxVoltsAxis := +100.0e-3 + fScopeStruct.Channels[0].positionOffsetVolts
                 end;
          v50m:  begin
                   fScopeStruct.Channels[0].minVoltsAxis := -250.0e-3 + fScopeStruct.Channels[0].positionOffsetVolts;
                   fScopeStruct.Channels[0].maxVoltsAxis := +250.0e-3 + fScopeStruct.Channels[0].positionOffsetVolts
                 end;
          v100m: begin
                   fScopeStruct.Channels[0].minVoltsAxis := -0.5 + fScopeStruct.Channels[0].positionOffsetVolts;
                   fScopeStruct.Channels[0].maxVoltsAxis := +0.5 + fScopeStruct.Channels[0].positionOffsetVolts
                 end;
          v200m: begin
                   fScopeStruct.Channels[0].minVoltsAxis := -1.0 + fScopeStruct.Channels[0].positionOffsetVolts;
                   fScopeStruct.Channels[0].maxVoltsAxis := +1.0 + fScopeStruct.Channels[0].positionOffsetVolts
                 end;
          v500m: begin
                   fScopeStruct.Channels[0].minVoltsAxis := -2.5 + fScopeStruct.Channels[0].positionOffsetVolts;
                   fScopeStruct.Channels[0].maxVoltsAxis := +2.5 + fScopeStruct.Channels[0].positionOffsetVolts
                 end;
          v1:    begin
                   fScopeStruct.Channels[0].minVoltsAxis := -5.0 + fScopeStruct.Channels[0].positionOffsetVolts;
                   fScopeStruct.Channels[0].maxVoltsAxis := +5.0 + fScopeStruct.Channels[0].positionOffsetVolts
                 end;
          v2:    begin
                   fScopeStruct.Channels[0].minVoltsAxis := -10.0 + fScopeStruct.Channels[0].positionOffsetVolts;
                   fScopeStruct.Channels[0].maxVoltsAxis := +10.0 + fScopeStruct.Channels[0].positionOffsetVolts
                 end;
          v5:    begin
                   fScopeStruct.Channels[0].minVoltsAxis := -25.0 + fScopeStruct.Channels[0].positionOffsetVolts;
                   fScopeStruct.Channels[0].maxVoltsAxis := +25.0 + fScopeStruct.Channels[0].positionOffsetVolts
                 end;
          v10:   begin
                   fScopeStruct.Channels[0].minVoltsAxis := -50.0 + fScopeStruct.Channels[0].positionOffsetVolts;
                   fScopeStruct.Channels[0].maxVoltsAxis := +50.0 + fScopeStruct.Channels[0].positionOffsetVolts
                 end;
          v20:   begin
                   fScopeStruct.Channels[0].minVoltsAxis := -100.0 + fScopeStruct.Channels[0].positionOffsetVolts;
                   fScopeStruct.Channels[0].maxVoltsAxis := +100.0 + fScopeStruct.Channels[0].positionOffsetVolts
                 end
        otherwise
          Raise Exception.Create('Bad voltage setting')
        end;
        with ChartTimeDomain.LeftAxis do begin
          Range.Min := fScopeStruct.Channels[0].minVoltsAxis;
          Range.Max := fScopeStruct.Channels[0].maxVoltsAxis;
          Range.UseMin:= true;
          Range.UseMax:= true
        end;
        with ChartTimeDomain.LeftAxis.Marks do
          if Assigned(Source) then begin
            Assert(Source is TListChartSource);
            with Source as TListChartSource do begin
              markPos := fScopeStruct.Channels[0].minVoltsAxis;

(* There are 11 horizontal marker lines in total. The minimum and maximum start *)
(* off hidden under the border, but become visible when labels are added to     *)
(* annotate signal levels and times.                                            *)

              for i := 0 to 10 do begin
                Add(0.0, markPos, engineeringFormat(markPos));
                markPos += fScopeStruct.Channels[0].voltsPerDivision
              end
            end
          end else begin
            Range.Min := fScopeStruct.Channels[0].minVoltsAxis;
            Range.Max := fScopeStruct.Channels[0].maxVoltsAxis;
            Range.UseMin:= true;
            Range.UseMax:= true
          end
      except
      end;

      with ChartTimeDomain.BottomAxis do begin
        Range.Min := fScopeStruct.MinTimeAxis();
        Range.Max := fScopeStruct.MaxTimeAxis();
        Range.UseMin:= true;
        Range.UseMax:= true
      end;
      with ChartTimeDomain.BottomAxis.Marks do
        if Assigned(Source) then begin
          Assert(Source is TListChartSource);
          with Source as TListChartSource do begin
            Clear;
            markPos := fScopeStruct.MinTimeAxis();

(* Assume that the number of samples per division is constant (typically 25)    *)
(* and that the number of samples is an integer multiple of this. Do not try to *)
(* preserve the conventional number of horizontal divisions (typically 20) by   *)
(* changing the time per division from that reported by the 'scope, but instead *)
(* increase the number of divisions as necessary.                               *)

            j := fScopeStruct.SamplesDisplayed() div fScopeStruct.SamplesPerDivision;
            DebugWriteF('Time axis %d divisions each %ssec\n', [j ,
                                engineeringFormat((fScopeStruct.MaxTimeAxis() -
                                                fScopeStruct.MinTimeAxis()) / j)]);
            for i := 0 to j - 1 do begin
              Add(markPos, 0.0, engineeringFormat(markPos));
              markPos += (fScopeStruct.MaxTimeAxis() - fScopeStruct.MinTimeAxis()) / j
            end
          end
        end else begin
          Range.Min := fScopeStruct.MinTimeAxis();
          Range.Max := fScopeStruct.MaxTimeAxis();
          Range.UseMin:= true;
          Range.UseMax:= true
        end;

      TLineSeries(ChartTimeDomain.Series[ch1Data]).BeginUpdate;

(* Special case: on the assumption that samplesPerHDiv is 25, stretch 1024 to   *)
(* 1025. Anything else is trimmed to an integer number of divisions.            *)

      DebugWriteF('Populating chart with %d samples\n', [fScopeStruct.SamplesDisplayed()]);
      for i := 0 to fScopeStruct.SamplesDisplayed() - 1 do
        if i <= Length(fScopeStruct.Channels[0].samples) - 1 then
          TLineSeries(ChartTimeDomain.Series[ch1Data]).AddXY(fScopeStruct.MinTimeAxis() +
                                          (1.0 / fScopeStruct.SamplesPerSecond()) * i,
                                          fScopeStruct.Channels[0].samples[i])
        else begin
          TLineSeries(ChartTimeDomain.Series[ch1Data]).AddXY(fScopeStruct.MinTimeAxis() +
                                          (1.0 / fScopeStruct.SamplesPerSecond()) * i,
                                          fScopeStruct.Channels[0].samples[
                                          Length(fScopeStruct.Channels[0].samples) - 1]);
          DebugWriteF('Using duplicated value for %dth sample\n', [i + 1])
        end
    finally
      TLineSeries(ChartTimeDomain.Series[ch1Data]).EndUpdate;
      TLineSeries(ChartFrequencyDomain.Series[ch1Data]).Clear;
      MenuItemFileExportVCD.Enabled := true;
      MenuItemConfigFrequencyDomain.Enabled := true;
      MenuItemConfigFrequencyDomain.Checked := false;
      ChartTimeDomain.Visible := true;
      ChartFrequencyDomain.Visible := false;
      PaintBoxTextTop.Repaint;
      PaintBoxTextBottom.Repaint;
      PaintBoxTickLeft.Repaint;
      PaintBoxTickRight.Repaint
    end
end { TFormDsoCat.drawScreens } ;


(* Receive an XModem transfer and process it for screen display.
*)
procedure TFormDsoCat.MenuItemFileXModemCaptureClick(Sender: TObject);

var
  savedStatus: string;

begin
  if fScopeStruct <> nil then
    fScopeStruct.Free;
  savedStatus := StatusBar1.SimpleText;
  StatusBar1.SimpleText := '        Waiting for data...';
  Application.ProcessMessages;
  fScopeStruct := Backend.ParseRawToScopeStruct(GetXModem(portHandle));
  StatusBar1.SimpleText := savedStatus;
  Application.ProcessMessages;
  drawScreens(sender)                   (* Reads data from fScopeStruct         *)
end { TFormDsoCat.MenuItemFileXModemCaptureClick } ;


(* Receive an XModem transfer to a file.
*)
procedure TFormDsoCat.MenuItemFileXModemReceiveClick(Sender: TObject);

var
  nameBlank: boolean;
  received: TStringList;

begin
  received := GetXModem(portHandle);
  try
    if received <> nil then begin
      nameBlank := Trim(SaveDialog1.Filename) = '';
      if nameBlank then
        SaveDialog1.Filename := projName + '_' + StringReplace(IsoNow(), ' ', 'T', [rfReplaceAll]);
      SaveDialog1.DefaultExt := 'csv';
      if SaveDialog1.Execute then
        try
          received.SaveToFile(SaveDialog1.Filename)
        except
          MessageDlgOpt('Unable to write ' + SaveDialog1.Filename, mtError, [mbOk], 0)
        end
    end
  finally
    FreeAndNil(received);
    if nameBlank then
      SaveDialog1.Filename := ''
  end
end { TFormDsoCat.MenuItemFileXModemReceiveClick } ;


function TFormDsoCat.GetProjectName(): string;

begin
  result := projName
end { TFormDsoCat.GetProjectName } ;


(* Message box optimised for position.
*)
FUNCTION TFormDsoCat.MessageDlgOpt(CONST aMsg: STRING; dlgType: TMsgDlgType;
                        buttons: TMsgDlgButtons; helpCtx: LONGINT): INTEGER;

VAR     x, y: INTEGER;

BEGIN
  x:= (Left + Width DIV 2 + Screen.Width DIV 2) DIV 2;
  y:= (Top + Height DIV 2 + Screen.Height DIV 2) DIV 2;
  RESULT:= MessageDlgPos(aMsg, dlgType, buttons, helpCtx, x, y)
END { TFormDsoCat.MessageDlgOpt } ;


(* Debugging output to pane on main form.
*)
procedure TFormDsoCat.DebugWrite(const str: string; ln: boolean= false);

begin
  if not MenuItemConfigDebugOutput.Checked then
    exit;
  if MemoDebug.lines.Count = 0 then
    MemoDebug.lines.Append('');
  MemoDebug.lines[MemoDebug.lines.Count - 1] :=
                                MemoDebug.lines[MemoDebug.lines.Count - 1] + str;
  if ln then
    MemoDebug.lines.Append('');
  Application.ProcessMessages           (* Make sure it appears immediately     *)
end { TFormDsoCat.DebugWrite } ;


(* Debugging output to pane on main form.
*)
procedure TFormDsoCat.DebugWriteLn(const str: string= '');

begin
  DebugWrite(str, true)
end { TFormDsoCat.DebugWriteLn } ;


(* Debugging output to pane on main form. This obeys \n in the format string,
  and always tries to "do the right thing" if used in conjunction with
  DebugWrite() above even if this implies a performance hit.
*)
procedure TFormDsoCat.DebugWriteF(const fmt: string; values: array of const);

// See FormatEx() in e.g. Borg-NG for possible enhancements.

var
  scratch: string;
  list: TStringList;
  i: integer;

begin
  if not MenuItemConfigDebugOutput.Checked then
    exit;
  scratch := Format(fmt, values);
  scratch := StringReplace(scratch, '\n', #$0a, [rfReplaceAll]);
  if Pos(#$0a, scratch) = 0 then
    DebugWrite(scratch)
  else
    if Pos(#$0a, scratch) = Length(scratch) then begin
      SetLength(scratch, Length(scratch) - 1);
      DebugWriteLn(scratch)
    end else begin
      list := TStringList.Create;
      try
        list.Text := scratch;

(* Respect anything already on the final line.                                  *)

        for i := 0 to list.Count - 1 do begin
          MemoDebug.lines[MemoDebug.lines.Count - 1] :=
                                MemoDebug.lines[MemoDebug.lines.Count - 1] + list[i];
          MemoDebug.lines.Append('')
        end;

(* If the format pattern didn't end with \n then leave the final line ready for *)
(* more to be appended to it rather than starting a new one.                    *)

        Assert(MemoDebug.lines[MemoDebug.lines.Count - 1] = '', 'Last debug output line unexpectedly not blank');
        if RightStr(fmt, 2) <> '\n' then
          MemoDebug.lines.Delete(MemoDebug.lines.Count - 1)
      finally
        list.Free
      end
    end;
  Application.ProcessMessages           (* Make sure it appears immediately     *)
end { TFormDsoCat.DebugWriteF } ;


procedure TFormDsoCat.MenuItemHelpAboutFrontendClick(Sender: TObject);

var
  about: string;

begin
  about := AboutText();
{$ifdef USE_DYNAMIC }
  about += #$0d#$0a + 'Backend magic number: ' + IntToStr(Backend.FrontendMagicNumber);
{$endif USE_DYNAMIC }
  MessageDlgOpt(ProjectName + ' ' + about, mtConfirmation, [mbOk], 0)
end { TFormDsoCat.MenuItemHelpAboutClick } ;


procedure TFormDsoCat.FormCloseQuery(Sender: TObject; var CanClose: boolean);

var
  temp: string;

begin
  temp:= Application.MainForm.Caption;
  IF Pos(' ', temp) > 0 THEN
    SetLength(temp, Pos(' ', temp) - 1);
  temp:= 'Terminate ' + temp + '?';
  CanClose:= MessageDlgOpt(temp, mtWarning, [mbYes, mbNo], 0) = mrYes
end { TFormDsoCat.FormCloseQuery } ;


procedure TFormDsoCat.ChartToolsetTimeDomainDataPointClickTool1PointClick(
  ATool: TChartTool; APoint: TPoint);

// See https://wiki.freepascal.org/TAChart_Tutorial:_Chart_Tools

var
  x, y: Double;
begin
  with ATool as TDatapointClickTool do
    if (Series <> nil) then
      try                               (* Doesn't like click on constant line  *)

(* LineSeries.Marks.Style must be smsLabel which is not the default.            *)

        with (Series as TLineSeries) do
          if ListSource.Item[PointIndex]^.Text = '' then begin
              x := GetXValue(PointIndex);
              y := GetYValue(PointIndex);
              DebugWriteF('TD sample %d (%8.3g,%8.3g)\n', [PointIndex, x, y]);
              ListSource.SetText(PointIndex, engineeringFormat(x) + 'sec' +
                                        #13#10 + engineeringFormat(y) + 'V')
          end else
            ListSource.SetText(PointIndex, '')
      except
      end
end { TFormDsoCat.ChartToolsetTimeDomainDataPointClickTool1PointClick } ;


procedure TFormDsoCat.ChartToolsetFrequencyDomainDataPointClickTool1PointClick(
  ATool: TChartTool; APoint: TPoint);

// See https://wiki.freepascal.org/TAChart_Tutorial:_Chart_Tools

// TODO : Save real and imaginary parts. Don't implement without considering point selection in MenuItemConfigFrequencyDomainClick().

var
  x, y: Double;
  hertzPerBin: single;


  function binToCentreFreq(bin: single): single; inline;

  begin
    result := (Round(bin) * hertzPerBin) + (hertzPerBin / 2.0)
  end { binToCentreFreq } ;


begin
  x := fScopeStruct.SamplesPerSecond(); (* Visible for debugging                *)
  if allFftBins then
    y := TLineSeries(ChartFrequencyDomain.Series[ch1Data]).Count
  else
    y := TLineSeries(ChartFrequencyDomain.Series[ch1Data]).Count * 2;
  hertzPerBin := x / y;
  with ATool as TDatapointClickTool do
    if (Series <> nil) then
      try                               (* Doesn't like click on constant line  *)

(* LineSeries.Marks.Style must be smsLabel which is not the default.            *)

        with (Series as TLineSeries) do
          if ListSource.Item[PointIndex]^.Text = '' then begin
              x := GetXValue(PointIndex);
              y := GetYValue(PointIndex);
              DebugWriteF('FD bin %d (%8.3g,%8.3g)\n', [PointIndex, x, y]);
              if scaleFftToUnity then
                ListSource.SetText(PointIndex, engineeringFormat(binToCentreFreq(x)) + 'Hz' +
                                        #13#10 + engineeringFormat(y))
              else
                ListSource.SetText(PointIndex, engineeringFormat(binToCentreFreq(x)) + 'Hz' +
                                        #13#10 + engineeringFormat(y) + 'V')
          end else
            ListSource.SetText(PointIndex, '')
      except
      end
end { TFormDsoCat.ChartToolsetFrequencyDomainDataPointClickTool1PointClick } ;


procedure TFormDsoCat.FormCreate(Sender: TObject);

const
  startParam= 0;

begin
  portHandle := InvalidSerialHandle;
  fScopeStruct := nil;

(* Keep the QueueAsyncCall() at the end, so that initialisation can continue    *)
(* after all form initialisation etc. has completed.                            *)

  Application.QueueAsyncCall(@OnAfterShow, startParam)
end { TFormDsoCat.FormCreate } ;


procedure TFormDsoCat.MenuItemConfigBackendReloadNowClick(Sender: TObject);

begin
  seenHUP2 := true
end { TFormDsoCat.MenuItemConfigBackendReloadNowClick } ;


procedure TFormDsoCat.MenuItemConfigDebugOutputClick(Sender: TObject);

begin
  MemoDebug.Enabled := MenuItemConfigDebugOutput.Checked
end { TFormDsoCat.MenuItemConfigDebugOutputClick } ;


procedure TFormDsoCat.MenuItemConfigFrequencyDomainClick(Sender: TObject);

const
  dumpData= true;
  dontProtect= false;

var
  markPos: double;
  i, fdCount: integer;

begin
  if allFftBins then
    fdCount := Length(fScopeStruct.Channels[0].samples)
  else
    fdCount := Length(fScopeStruct.Channels[0].samples) div 2;

(* If we are currently displaying the time domain, populate the frequency       *)
(* domain chart using an FFT and display it. Note the extra checks so that we   *)
(* only clear/populate the frequency domain chart from the instrument data      *)
(* structure (which will usually be sized to an integer power of 2 so might be  *)
(* slightly shorter or longer than the time domain chart) when the frequency    *)
(* domain chart is blank, so as not to lose any on-screen labels added by the   *)
(* user.                                                                        *)

// TODO : Save real and imaginary parts. Don't implement without considering point selection in MenuItemConfigFrequencyDomainClick().
// I think that max(rms) will never be smaller than max(max(real), max(imaginary))
// or strictly I probably mean span() there. Scale the real and imaginary charts
// together so that their zero is in the middle and neither goes too high or low,
// then scale the RMS chart with its zero at the bottom not going too high...
// we can probably assume that there's a x2 in there. Real red, imaginary indigo
// and RMS black, with RMS drawn on top (not sure if this makes it first or last).

  if ChartTimeDomain.Visible and MenuItemConfigFrequencyDomain.Checked then
    if dontProtect or (TLineSeries(ChartFrequencyDomain.Series[ch1Data]).Count = 0) then begin
      maxFftAmplitude := PopulateFrequencyDomain(fScopeStruct, ChartFrequencyDomain,
                                        ch1Data, scaleFftToUnity, allFftBins);
      if dumpData then begin
        for i := 0 to Length(fScopeStruct.Channels[ch1Data].samples) - 1 do
          DebugWriteF('T %d: %8.3g\n', [i, fScopeStruct.Channels[ch1Data].samples[i]]);
        for i := 0 to TLineSeries(ChartFrequencyDomain.Series[ch1Data]).Count - 1 do
          DebugWriteF('F %d: %8.3g\n', [i, TLineSeries(ChartFrequencyDomain.Series[ch1Data]).GetYValue(i)])
      end
    end;

(* At design time and program start the chart component's default grid is used  *)
(* in order that something appears on the screen that looks vaguely plausible,  *)
(* the disadvantage of this is that the division size varies as the display is  *)
(* zoomed in and out. Once we have data replace this grid with one derived from *)
(* specialist components, which will not zoom allowing the usual "per division" *)
(* scope calibration to be used.                                                *)

// WORKING NOTE: vertical grid scaling is messed up by the presence of labels,
// which appear to compress the display in order to give themselves adequate
// room. I don't know whether this can be fixed by telling TAChart to position
// labels towards the centre rather than towards the edge.

  if scaleFftToUnity then begin
    if not Assigned(ChartFrequencyDomain.LeftAxis.Marks.Source) then
      ChartFrequencyDomain.LeftAxis.Marks.Source := ListChartSourceFrequencyDomainLeftAxis;
    with ChartFrequencyDomain.LeftAxis do begin
      Range.Min := 0.0;
      Range.Max := 1.1;
      Range.UseMin:= true;
      Range.UseMax:= true
    end;
    with ChartFrequencyDomain.LeftAxis.Marks do
      if Assigned(Source) then begin
        Assert(Source is TListChartSource);
        with Source as TListChartSource do begin
          (Source as TListChartSource).Clear;
          markPos := 0.0;
          for i := 0 to 11 do begin
            Add(0.0, markPos, engineeringFormat(markPos));
            markPos += 0.1
          end
        end
      end
  end;

// Real and imaginary scaled to +-(maxFtfAmplitude / 2) ?

  if not Assigned(ChartFrequencyDomain.BottomAxis.Marks.Source) then
    ChartFrequencyDomain.BottomAxis.Marks.Source := ListChartSourceFrequencyDomainBottomAxis;
  with ChartFrequencyDomain.BottomAxis do begin
    Range.Min := 0;
    Range.Max := fdCount - 1;
    Range.UseMin:= true;
    Range.UseMax:= true
  end;
  with ChartFrequencyDomain.BottomAxis.Marks do
    if Assigned(Source) then begin
      Assert(Source is TListChartSource);
      with Source as TListChartSource do begin
        Clear;
        markPos := 0;
        for i := 0 to 32 do begin
          Add(markPos, 0.0, engineeringFormat(markPos));
          markPos += TLineSeries(ChartFrequencyDomain.Series[ch1Data]).Count / 32
        end
      end
    end;
  ChartTimeDomain.Visible := not MenuItemConfigFrequencyDomain.Checked;
  ChartFrequencyDomain.Visible := MenuItemConfigFrequencyDomain.Checked;
  PaintBoxTextTop.Repaint;
  PaintBoxTextBottom.Repaint;
  PaintBoxTickLeft.Repaint;
  PaintBoxTickRight.Repaint
end { TFormDsoCat.MenuItemConfigFrequencyDomainClick } ;


procedure TFormDsoCat.MenuItemCopyToClipboardClick(Sender: TObject);

var
  bitmap: TBitmap;
  everything: TRect;

// See also
//
// https://wiki.lazarus.freepascal.org/Clipboard
// https://www.thoughtco.com/basic-clipboard-operations-cut-copy-paste-1058406

begin
  bitmap := TBitmap.Create;
  try
    bitmap.Width := PanelDisplay1.Width;
    bitmap.Height := PanelDisplay1.Height;
    everything.Left := 0;
    everything.Top := 0;
    everything.Right := PanelDisplay1.Width - 1;
    everything.bottom := PanelDisplay1.Height - 1;
    bitmap.Canvas.CopyRect(everything, PanelDisplay1.Canvas, everything);
// Clipboard.Clear;
    Clipboard.Assign(bitmap)
  finally
    bitmap.Free
  end
end { TFormDsoCat.MenuItemCopyToClipboardClick } ;


procedure TFormDsoCat.MenuItemEditAdjust4x3Click(Sender: TObject);

begin
{$ifdef HAS_LCL3 }
  setGeometryForScreen(4 * Screen.PrimaryMonitor.PixelsPerInch,
                                3 * Screen.PrimaryMonitor.PixelsPerInch)
{$else }
  setGeometryForScreen(4 * Screen.PixelsPerInch,
                                3 * Screen.PixelsPerInch)
{$endif HAS_LCL3 }
end { TFormDsoCat.MenuItemEditAdjust4x3Click } ;


procedure TFormDsoCat.MenuItemEditAdjust5x4Click(Sender: TObject);

begin
{$ifdef HAS_LCL3 }
  setGeometryForScreen(5 * Screen.PrimaryMonitor.PixelsPerInch,
                                4 * Screen.PrimaryMonitor.PixelsPerInch)
{$else }
  setGeometryForScreen(5 * Screen.PixelsPerInch,
                                4 * Screen.PixelsPerInch)
{$endif HAS_LCL3 }
end { TFormDsoCat.MenuItemEditAdjust5x4Click } ;


procedure TFormDsoCat.MenuItemEditAdjust7x5Click(Sender: TObject);

begin
{$ifdef HAS_LCL3 }
  setGeometryForScreen(7 * Screen.PrimaryMonitor.PixelsPerInch,
                                5 * Screen.PrimaryMonitor.PixelsPerInch)
{$else }
  setGeometryForScreen(7 * Screen.PixelsPerInch,
                                5 * Screen.PixelsPerInch)
{$endif HAS_LCL3 }
end { TFormDsoCat.MenuItemEditAdjust7x5Click } ;


procedure TFormDsoCat.MenuItemEditAdjust640x480Click(Sender: TObject);

begin
  setGeometryForScreen(640, 480)
end { TFormDsoCat.MenuItemEditAdjust640x480Click } ;


procedure TFormDsoCat.MenuItemEditAdjust800x600Click(Sender: TObject);

begin
  setGeometryForScreen(800, 600)
end { TFormDsoCat.MenuItemEditAdjust800x600Click } ;


procedure TFormDsoCat.MenuItemEditAdjust1024x768Click(Sender: TObject);

begin
  setGeometryForScreen(1024, 768)
end { TFormDsoCat.MenuItemEditAdjust1024x768Click } ;


procedure TFormDsoCat.MenuItemEditAdjustA5Click(Sender: TObject);

begin
{$ifdef HAS_LCL3 }
  setGeometryForScreen(Round(8.3 * Screen.PrimaryMonitor.PixelsPerInch),
                                Round(5.8 * Screen.PrimaryMonitor.PixelsPerInch))
{$else }
  setGeometryForScreen(Round(8.3 * Screen.PixelsPerInch),
                                Round(5.8 * Screen.PixelsPerInch))
{$endif HAS_LCL3 }
end { TFormDsoCat.MenuItemEditAdjustA5Click } ;


procedure TFormDsoCat.MenuItemEditAdjustA6Click(Sender: TObject);

begin
{$ifdef HAS_LCL3 }
  setGeometryForScreen(Round(5.8 * Screen.PrimaryMonitor.PixelsPerInch),
                                Round(4.1 * Screen.PrimaryMonitor.PixelsPerInch))
{$else }
  setGeometryForScreen(Round(5.8 * Screen.PixelsPerInch),
                                Round(4.1 * Screen.PixelsPerInch))
{$endif HAS_LCL3 }
end { TFormDsoCat.MenuItemEditAdjustA5Click } ;


procedure TFormDsoCat.MenuItemEditAdjustA7Click(Sender: TObject);

begin
{$ifdef HAS_LCL3 }
  setGeometryForScreen(Round(4.1 * Screen.PrimaryMonitor.PixelsPerInch),
                                Round(2.9 * Screen.PrimaryMonitor.PixelsPerInch))
{$else }
    setGeometryForScreen(Round(4.1 * Screen.PixelsPerInch),
                                Round(2.9 * Screen.PixelsPerInch))
{$endif HAS_LCL3 }
end { TFormDsoCat.MenuItemEditAdjustA7Click } ;


procedure TFormDsoCat.MenuItemEditRestoreGeometryClick(Sender: TObject);

begin
  restoreOriginalGeometry
end { TFormDsoCat.MenuItemEditRestoreGeometryClick } ;


// TODO : Consider wiki upload, see https://www.mediawiki.org/wiki/API:Upload

// Above would probably need a form to specify the type of file, whether it
// should be retained locally, and so on. A good start would probably be to have
// the forms (TBD) that control PNG and GIF properties to have the option of
// putting the filename on the clipboard so that if it's uploaded the name is
// readily available to paste into a wiki page being edited.

// http://www.mtxia.com/js/Downloads/Scripts/Korn/Wiki/index.content.shtml
// hence http://www.mtxia.com/js/Downloads/Scripts/Korn/Functions/wikiAutoLoad/wikiAutoLoad_k93.txt
// might be useful since it doesn't rely on Python libraries etc.

procedure TFormDsoCat.MenuItemExportGIFClick(Sender: TObject);

// Expect this to be disabled, GIF writing is not supported by the LCL due to
// unjustified caution brought about by long-expired patents held by Unisys and
// others.
//
// Discussion https://forum.lazarus.freepascal.org/index.php?topic=22383.0
// hence https://forum.lazarus.freepascal.org/index.php?action=dlattach;topic=22383.0;attach=7222
// Also https://bugs.freepascal.org/view.php?id=25476
// hence https://bugs.freepascal.org/file_download.php?file_id=19347&type=bug
// Original code http://melander.dk/delphi/gifimage/
// hence also http://www.tolderlund.eu/delphi/
//
// I believe that last link is still open-source but that it doesn't support
// FPC. The attachment in the first link has FPC support and much dead wood
// trimmed. It should be possible to merge as necessary, but it's probably not
// possible to merge it into the LCL since there is a "no derivatives" clause
// in the original licence. I could probably get away with bundling it as part
// of the source of this program.

// TODO : GIF support.

var
  nameBlank: boolean;
  gif: TGIFImage;
  everything: TRect;

begin
  nameBlank := Trim(SaveDialog1.Filename) = '';
  if nameBlank then
    SaveDialog1.Filename := projName + '_' + StringReplace(IsoNow(), ' ', 'T', [rfReplaceAll]);
  SaveDialog1.DefaultExt := 'gif';

(* Possibly replace this later with a custom setup form to select compression   *)
(* etc.                                                                         *)

  if SaveDialog1.Execute then begin
    gif := TGIFImage.Create;
    try
      gif.Width := PanelDisplay1.Width;
      gif.Height := PanelDisplay1.Height;
      everything.Left := 0;
      everything.Top := 0;
      everything.Right := PanelDisplay1.Width - 1;
      everything.bottom := PanelDisplay1.Height - 1;
      gif.Canvas.CopyRect(everything, PanelDisplay1.Canvas, everything);
// TODO : Apparently unimplemented in LCL- why?
      gif.SaveToFile(SaveDialog1.Filename)
    finally
      gif.Free
    end
  end;
  if nameBlank then
    SaveDialog1.Filename := ''
end { TFormDsoCat.MenuItemExportGIFClick } ;


procedure TFormDsoCat.MenuItemExportPNGClick(Sender: TObject);

var
  nameBlank: boolean;
  png: TPortableNetworkGraphic;
  everything: TRect;

begin
  nameBlank := Trim(SaveDialog1.Filename) = '';
  if nameBlank then
    SaveDialog1.Filename := projName + '_' + StringReplace(IsoNow(), ' ', 'T', [rfReplaceAll]);
  SaveDialog1.DefaultExt := 'png';

(* Possibly replace this later with a custom setup form to select compression   *)
(* etc.                                                                         *)

  if SaveDialog1.Execute then begin
    png := TPortableNetworkGraphic.Create;
    try
      png.Width := PanelDisplay1.Width;
      png.Height := PanelDisplay1.Height;
      everything.Left := 0;
      everything.Top := 0;
      everything.Right := PanelDisplay1.Width - 1;
      everything.bottom := PanelDisplay1.Height - 1;
      png.Canvas.CopyRect(everything, PanelDisplay1.Canvas, everything);
      png.SaveToFile(SaveDialog1.Filename)
    finally
      png.Free
    end
  end;
  if nameBlank then
    SaveDialog1.Filename := ''
end { TFormDsoCat.MenuItemExportPNGClick } ;


(* As a prelude to graphical output, restore the form to its original size.
  This will normally be followed by an adjustment to the form's size to get the
  screen (TAChart) to a specified size in pixels or inches.
*)
procedure TFormDsoCat.restoreOriginalGeometry;

begin
  Width := fOriginalFormWidth;
  Height := fOriginalFormHeight;
  PairSplitter1.UpdatePosition;         (* Make sure field in object tracks grip *)
  PairSplitter1.Position := fWidthSplitterOriginalPosition;
  PairSplitter2.UpdatePosition;         (* Make sure field in object tracks grip *)

// Patch that obviates the necessity of the UpdatePosition() calls above
// "Comitted to trunk (r61909) [2.1.0] and requested back-porting to fixes [2.0.4]."

  PairSplitter2.Position := fHeightSplitterOriginalPosition;
  Application.ProcessMessages
end { TFormDsoCat.restoreOriginalGeometry } ;


procedure TFormDsoCat.setGeometryForScreen(x, y: integer);

var
  widthAdjustment, heightAdjustment: integer;

begin
  restoreOriginalGeometry;
  widthAdjustment := x - PanelDisplay1.Width;
  heightAdjustment := y - PanelDisplay1.Height;
  Width := Width + widthAdjustment;
  PairSplitter1.Position := PairSplitter1.Position + widthAdjustment;
  Height := Height + heightAdjustment;
  PairSplitter2.Position := PairSplitter2.Position + heightAdjustment;
  Application.ProcessMessages
end { TFormDsoCat.setGeometryForScreen } ;


(* If the platform is Lazarus, irrespective of the underlying operating system,
  this is called as a result of a QueueAsyncCall() at the end of the
  FormCreate() method. It is guaranteed to be after all form creation etc. has
  completed.

  The parameter is zero during normal program startup, nonzero during restart
  or shutdown.
*)
procedure TFormDsoCat.OnAfterShow(afterShowParam: PtrInt);

var     i: integer;
        scratch: string;


  procedure catchsignals;

  var     action: SigActionRec;

  begin
    FillChar(action, SizeOf(action), 0);
    action.Sa_Handler := @hupHandler;
    if fpSigAction(SIGHUP, @action, nil) <> 0 then
      DebugWriteLn('Warning: SIGHUP not hooked, error ' + IntToStr(fpGetErrNo()))
  end { catchSignals } ;


begin
  if afterShowParam = 0 then begin

(* First time through: set up user interface.                                   *)

    fOriginalFormWidth := Width;
    fOriginalFormHeight := Height;
    fWidthSplitterOriginalPosition := PairSplitter1.Position;
    fHeightSplitterOriginalPosition := PairSplitter2.Position;

{$ifdef USE_STATIC }
    MenuItemConfigBackend.Enabled := false; (* No reload etc. menu entries      *)
{$endif USE_STATIC }
    DebugWriteLn('Running...');
    Application.ProcessMessages;
    catchSignals
  end else begin
    DebugWriteLn('Restarting...');
    Application.ProcessMessages;

(* Not the first time through: close existing port etc.                         *)

    MenuItemConfigBackendReloadNow.Enabled := false;
    Timer1.Enabled := false;
    Sleep(3 * Timer1.Interval div 2);
    seenHUP := false;
    seenHUP2 := false;
    MenuItemFileClosePortClick(Self);
    i := 15000;
    while (Backend.EntryCount > 0) and (i > 0) do begin
      Sleep(1);
      Application.ProcessMessages;
      Dec(i)
    end;
    BackendUnload
  end;

(* Load the backend instrument driver. This will either be statically linked    *)
(* which implies- in this implementation- support for only a single type of     *)
(* device, or it will be dynamically loaded which implies that (only) the       *)
(* default driver must accompany the main executable. As things stand, not      *)
(* being able to open a backend driver is assumed to be non-fatal since the     *)
(* frontend program might still be able to read and display saved files.        *)
(*                                                                              *)
(* If the backend can be opened there is a check not only that it presents the  *)
(* magic number expected by the frontend, but also that it is happy with the    *)
(* magic number presented to it by the frontend. If either of these tests fail  *)
(* it might be possible to recover the situation by copying an updated backend  *)
(* to the same location as the frontend executable.                             *)

(* Not being able to open the backend shared-library might not be a fatal       *)
(* error, since there might be an adequate default handler. However getting bad *)
(* version info from it almost certainly is.                                    *)

  if BackendLoad then begin
    if Backend.RoutineExists(BackendMagicNumber_RoutineName) then begin
      if Backend.SharedLibraryMagicNumber = Backend.FrontendMagicNumber then begin
        DebugWriteF('Backend magic number OK (%d).\n', [Backend.SharedLibraryMagicNumber]);
        if Backend.SharedLibraryBindMainProgramExports <> 0 then begin (* No error *)
          scratch := Backend.BackendAboutText;
          if Pos(#$0d, scratch) > 0 then
            SetLength(scratch, Pos(#$0d, scratch) - 1);
          DebugWriteF('Loaded backend, %s.\n', [scratch]);
          BackendPostLoad(projectName)
        end else begin
          with MenuItemConfigDebugOutput do begin
            Tag := Ord(Checked);
            Checked := true
          end;
          scratch := Backend.SharedLibraryBindMainProgramError;
          DebugWriteF('Backend unable to bind frontend exports: %s\n', [scratch]);
          BackendUnload;
          DebugWriteLn('Continuing without a custom backend.');
          with MenuItemConfigDebugOutput do begin
            Checked := Tag <> 0;
            Tag := 0
          end
        end
      end else begin
        with MenuItemConfigDebugOutput do begin
          Tag := Ord(Checked);
          Checked := true
        end;
        DebugWriteF('Backend magic number is %d, should be %d.\n',
                    [Backend.SharedLibraryMagicNumber, Backend.FrontendMagicNumber]);
        DebugWriteLn('***** THIS IS A SERIOUS ERROR AND WILL PROBABLY RESULT IN A CRASH. *****');
        with MenuItemConfigDebugOutput do begin
          Checked := Tag <> 0;
          Tag := 0
        end
      end
    end else begin
      with MenuItemConfigDebugOutput do begin
        Tag := Ord(Checked);
        Checked := true
      end;
      DebugWriteLn('Unable to get backend magic number, attempting to unload.');
      BackendUnload;
      DebugWriteLn('Continuing without a custom backend.');
      with MenuItemConfigDebugOutput do begin
        Checked := Tag <> 0;
        Tag := 0
      end
    end
  end else begin
    with MenuItemConfigDebugOutput do begin
      Tag := Ord(Checked);
      Checked := true
    end;
    DebugWriteF('Unable to load backend %s.\n', [BackendError]);
    DebugWriteLn('Continuing without a custom backend.');
    with MenuItemConfigDebugOutput do begin
      Checked := Tag <> 0;
      Tag := 0
    end
  end;

(* Monitor the flag that indicates that a HUP signal has been seen. Assume that *)
(* signals might arrive at inconvenient times relative to the state of the main *)
(* thread and LCL.                                                              *)

  seenHUP := false;
  seenHUP2 := false;
  Timer1.Enabled := true;
  MenuItemConfigBackendReloadOnHup.Enabled := true;
  MenuItemConfigBackendReloadOnNewVersion.Enabled := true;
//  MenuItemConfigBackendReloadOnRepoRelease.Enabled := true;
  MenuItemConfigBackendReloadNow.Enabled := true
end { TFormDsoCat.OnAfterShow } ;


procedure TFormDsoCat.MenuItemQuitClick(Sender: TObject);

var
  reallyQuit: boolean;

begin
  FormCloseQuery(sender, {%H-}reallyQuit);
  if reallyQuit then
    Application.Terminate
end { TFormDsoCat.MenuItemQuitClick } ;


procedure TFormDsoCat.MenuItemFileTestWaveformSineClick(Sender: TObject);

begin
  if fScopeStruct <> nil then
    fScopeStruct.Free;
  fScopeStruct := Backend.ParseTestToScopeStruct(ttSine);
  drawScreens(sender)                   (* Reads data from fScopeStruct         *)
end { TFormDsoCat.MenuItemTestWaveformSineClick } ;


procedure TFormDsoCat.MenuItemFileTestWaveformSquareClick(Sender: TObject);

begin
  if fScopeStruct <> nil then
    fScopeStruct.Free;
  fScopeStruct := Backend.ParseTestToScopeStruct(ttSquare);
  drawScreens(sender)                   (* Reads data from fScopeStruct         *)
end { TFormDsoCat.MenuItemTestWaveformSquareClick } ;


procedure TFormDsoCat.MenuItemFileTestWaveformUnitPulseClick(Sender: TObject);

begin
  if fScopeStruct <> nil then
    fScopeStruct.Free;
  fScopeStruct := Backend.ParseTestToScopeStruct(ttPulse);
  drawScreens(sender)                   (* Reads data from fScopeStruct         *)
end { TFormDsoCat.MenuItemTestWaveformUnitPulseClick } ;


procedure TFormDsoCat.MenuItemFileTestWaveformFallingEdgeClick(Sender: TObject);

begin
  if fScopeStruct <> nil then
    fScopeStruct.Free;
  fScopeStruct := Backend.ParseTestToScopeStruct(ttFallingEdge);
  drawScreens(sender)                   (* Reads data from fScopeStruct         *)
end { TFormDsoCat.MenuItemFileTestWaveformFallingEdgeClick } ;


procedure TFormDsoCat.MenuItemFileTestWaveformRisingEdgeClick(Sender: TObject);

begin
  if fScopeStruct <> nil then
    fScopeStruct.Free;
  fScopeStruct := Backend.ParseTestToScopeStruct(ttRisingEdge);
  drawScreens(sender)                   (* Reads data from fScopeStruct         *)
end { TFormDsoCat.MenuItemFileTestWaveformRisingEdgeClick } ;


procedure TFormDsoCat.MenuItemFileTestWaveformFallingSawClick(Sender: TObject);

begin
  if fScopeStruct <> nil then
    fScopeStruct.Free;
  fScopeStruct := Backend.ParseTestToScopeStruct(ttFallingSaw);
  drawScreens(sender)                   (* Reads data from fScopeStruct         *)
end { TFormDsoCat.MenuItemFileTestWaveformFallingSawClick } ;


procedure TFormDsoCat.MenuItemFileTestWaveformRisingSawClick(Sender: TObject);

begin
  if fScopeStruct <> nil then
    fScopeStruct.Free;
  fScopeStruct := Backend.ParseTestToScopeStruct(ttRisingSaw);
  drawScreens(sender)                   (* Reads data from fScopeStruct         *)
end { TFormDsoCat.MenuItemFileTestWaveformRisingSawClick } ;


procedure TFormDsoCat.MenuItemFileTestWaveformTriangleClick(Sender: TObject);

begin
  if fScopeStruct <> nil then
    fScopeStruct.Free;
  fScopeStruct := Backend.ParseTestToScopeStruct(ttTriangle);
  drawScreens(sender)                   (* Reads data from fScopeStruct         *)
end { TFormDsoCat.MenuItemFileTestWaveformTriangleClick } ;


(* Chart resized, redraw on-screen scaling etc.
*)
procedure TFormDsoCat.PaintBoxTextBottomPaint(Sender: TObject);

var
  scratch: string;
  resolvedSeconds, hertzPerBin, binsPerDivision, hertzPerDivision: single;

begin
  with PaintBoxTextBottom do begin
    Canvas.Pen.Width := 0;
    Canvas.Brush.Style := bsSolid;
    if ChartTimeDomain.Visible then
      Canvas.Brush.Color := clBlack
    else
      Canvas.Brush.Color := clLime;
    Canvas.Clear;
    if Assigned (fScopeStruct) then begin
      SelectObject(Canvas.Handle, GetStockObject(SYSTEM_FIXED_FONT));
      Canvas.Font.Height := PaintBoxTextBottom.Height div 2;
      if ChartTimeDomain.Visible then begin
        Canvas.Font.Color := clLime;
        resolvedSeconds := fScopeStruct.SecondsPerDivision / fScopeStruct.SamplesPerDivision;
        scratch := engineeringFormat(fScopeStruct.SecondsPerDivision) + 'sec/div';
        Canvas.TextOut(PaintBoxTextTop.Height, PaintBoxTextTop.Height div 4, scratch);
        scratch := '';
        if MenuItemConfigDebugOutput.Checked then
          scratch := '(' + engineeringFormat(resolvedSeconds) + 'sec × ' +
                        IntToStr(TLineSeries(ChartTimeDomain.Series[ch1Data]).Count) + ')  ';
        scratch += engineeringFormat(fScopeStruct.MinTimeAxis, true) + '...' +
                                        engineeringFormat(fScopeStruct.MaxTimeAxis) + 'sec';
        Canvas.TextOut(PaintBoxTextBottom.Width - Canvas.TextWidth(scratch) - PaintBoxTextBottom.Height,
                                        PaintBoxTextBottom.Height div 4, scratch)
      end else begin
        Canvas.Font.Color := clBlack;

(* The frequency domain chart is populated with contiguous bins each containing *)
(* an amplitude. The first (left-most, index 0) bin is DC, the second is the    *)
(* sample rate divided by the number of input samples. Normally, the number of  *)
(* bins will be half the number of time domain samples due to the symmetry of   *)
(* FFT output, and the upper frequency will be the Nyquist frequency which will *)
(* be half the sampling frequency. The chart will be divided horizontally by    *)
(* 32, irrespective of the number of bins.                                      *)

        hertzPerBin := fScopeStruct.SamplesPerSecond / TLineSeries(ChartTimeDomain.Series[ch1Data]).Count;
        binsPerDivision := TLineSeries(ChartFrequencyDomain.Series[ch1Data]).Count / 32;
        hertzPerDivision := hertzPerBin * binsPerDivision;
        scratch := engineeringFormat(hertzPerDivision) + 'Hz/div';
        Canvas.TextOut(PaintBoxTextTop.Height, PaintBoxTextTop.Height div 4, scratch);
        scratch := 'DC to ';
        if allFftBins then
          scratch += engineeringFormat(fScopeStruct.SamplesPerSecond) + 'Hz'
        else
          scratch += engineeringFormat(fScopeStruct.SamplesPerSecond / 2) + 'Hz';
        scratch += ' in ' + IntToStr(TLineSeries(ChartFrequencyDomain.Series[ch1Data]).Count) + ' bins';
        Canvas.TextOut(PaintBoxTextBottom.Width - Canvas.TextWidth(scratch) - PaintBoxTextBottom.Height,
                                        PaintBoxTextBottom.Height div 4, scratch)
      end
    end
  end
end { TFormDsoCat.PaintBoxTextBottomPaint } ;


(* Chart resized, redraw on-screen scaling etc.
*)
procedure TFormDsoCat.PaintBoxTextTopPaint(Sender: TObject);

var
  scratch: string;

begin
  with PaintBoxTextTop do begin
    Canvas.Pen.Width := 0;
    Canvas.Brush.Style := bsSolid;
    if ChartTimeDomain.Visible then
      Canvas.Brush.Color := clBlack
    else
      Canvas.Brush.Color := clLime;
    Canvas.Clear;
    if Assigned (fScopeStruct) then begin
        SelectObject(Canvas.Handle, GetStockObject(SYSTEM_FIXED_FONT));
        Canvas.Font.Height := PaintBoxTextTop.Height div 2;

(* Height is in pixels, coordinates of both the canvas and the text are         *)
(* relative to the top left. The text has an additional ascender allowance      *)
(* above capitals, so in practice the top of an "X" displayed as here will be   *)
(* somewhat below the midline. Probably see TCanvas.GetTextMetrics()            *)

//      Canvas.TextOut(PaintBoxTextTop.Height, PaintBoxTextTop.Height div 2, 'X')

        if ChartTimeDomain.Visible then begin
          Canvas.Font.Color := clLime;
          scratch := engineeringFormat(fScopeStruct.Channels[0].VoltsPerDivision) + 'V/div';
          Canvas.TextOut(PaintBoxTextTop.Height, PaintBoxTextTop.Height div 4, scratch);
          scratch := '';
          if MenuItemConfigDebugOutput.Checked then
            scratch := '(' + engineeringFormat(fScopeStruct.Channels[0].resolvedVolts) +
                        'V × ' + IntToStr(fScopeStruct.VerticalResolution) + ')  ';
          scratch += engineeringFormat(fScopeStruct.Channels[0].minVoltsAxis, true) + '...' +
                                        engineeringFormat(fScopeStruct.Channels[0].maxVoltsAxis) + 'V';
          Canvas.TextOut(PaintBoxTextTop.Width - Canvas.TextWidth(scratch) - PaintBoxTextTop.Height,
                                        PaintBoxTextTop.Height div 4, scratch)
       end else begin
         Canvas.Font.Color := clBlack;
         if scaleFftToUnity then
           scratch := '0.1/div'
         else
           scratch := 'Max ' + EngineeringFormat(maxFftAmplitude) + 'V';
         Canvas.TextOut(PaintBoxTextTop.Height, PaintBoxTextTop.Height div 4, scratch);
         scratch := 'Linear';
         Canvas.TextOut(PaintBoxTextTop.Width - Canvas.TextWidth(scratch) - PaintBoxTextTop.Height,
                                       PaintBoxTextTop.Height div 4, scratch)
       end
    end
  end
end { TFormDsoCat.PaintBoxTextTopPaint } ;


(* Chart resized, redraw on-screen scaling etc.
*)
procedure TFormDsoCat.PaintBoxTickLeftPaint(Sender: TObject);

begin
  with PaintBoxTickLeft do begin
    Canvas.Pen.Width := 0;
    Canvas.Brush.Style := bsSolid;
    if ChartTimeDomain.Visible then begin
      Canvas.Brush.Color := clBlack;
      Canvas.Clear
    end else begin
      Canvas.Brush.Color := clLime;
      Canvas.Clear
    end
  end
end { TFormDsoCat.PaintBoxTickLeftPaint } ;


(* Chart resized, redraw on-screen scaling etc.
*)
procedure TFormDsoCat.PaintBoxTickRightPaint(Sender: TObject);

begin
  with PaintBoxTickRight do begin
    Canvas.Pen.Width := 0;
    Canvas.Brush.Style := bsSolid;
    if ChartTimeDomain.Visible then begin
      Canvas.Brush.Color := clBlack;
      Canvas.Clear
    end else begin
      Canvas.Brush.Color := clLime;
      Canvas.Clear
    end
  end
end { TFormDsoCat.PaintBoxTickRightPaint } ;


(* The pane containing the chart etc. has been resized, invalidate the eye-
  candy used for scaling information etc. to force them to be redrawn.
*)
procedure TFormDsoCat.PairSplitter2TopResize(Sender: TObject);

begin
  PaintBoxTextTop.Invalidate;
  PaintBoxTextBottom.Invalidate;
  PaintBoxTickLeft.Invalidate;
  PaintBoxTickRight.Invalidate
end { TFormDsoCat.PairSplitter2TopResize } ;


(* Once the frontend is event-driven, redraw the buttons/lights if they've been *)
(* updated at a time when this wasn't safe and keep an eye open for HUP signals *)
(* etc.                                                                         *)
//
procedure TFormDsoCat.Timer1Timer(Sender: TObject);

const
  debugAlways= true;                    (* Normally true                        *)

var
  scratch: string;
  debugEnabled: boolean;

begin
  Timer1.Enabled := false;
  debugEnabled := MenuItemConfigDebugOutput.Checked;
  MenuItemConfigDebugOutput.Checked := MenuItemConfigDebugOutput.Checked or debugAlways;
  try
    if seenHUP then begin
      DebugWriteLn('Got hangup signal (SIGHUP).');
      if MenuItemConfigBackendReloadOnHUP.Checked then
        seenHUP2 := true
      else begin
        DebugWrite('Reloading configuration... ');
        if Backend.LoadConfiguration(projectName) then
          DebugWriteLn('OK.')
        else
          DebugWriteLn('Failed.')
      end;
      seenHUP := false
    end;

(* seenHUP2 might have been set by receipt of an actual HUP signal, or by the   *)
(* occasional check whether an updated backend shared library is available.     *)

    if seenHUP2 and (Backend.EntryCount <= 0) then
      if BackendUpdated <> '' then begin                (* Reload backend       *)
        DebugWriteLn('Reloading backend...');
        seenHUP2 := false;

(* Not being able to open the backend shared-library might not be a fatal       *)
(* error, since there might be an adequate default handler. However getting bad *)
(* version info from it almost certainly is.                                    *)

        if BackendGood then
          if BackendReload then begin
            if Backend.RoutineExists(BackendMagicNumber_RoutineName) then begin
              if Backend.SharedLibraryMagicNumber = Backend.FrontendMagicNumber then begin
                DebugWriteF('Backend magic number OK (%d).\n', [Backend.SharedLibraryMagicNumber]);
                if Backend.SharedLibraryBindMainProgramExports <> 0 then begin
                  scratch := Backend.BackendAboutText;
                  SetLength(scratch, Pos(#$0d, scratch) - 1);
                  DebugWriteF('Reloaded backend, %s.\n', [scratch]);
                  BackendPostLoad(projectName)
                end else begin
                  with MenuItemConfigDebugOutput do begin
                    Tag := Ord(Checked);
                    Checked := true
                  end;
                  scratch := Backend.SharedLibraryBindMainProgramError;
                  DebugWriteF('Backend unable to bind frontend exports: %s\n', [scratch]);
                  BackendUnload;
                  DebugWriteLn('Continuing without a custom backend.');
                  with MenuItemConfigDebugOutput do begin
                    Checked := Tag <> 0;
                    Tag := 0
                  end
                end
              end else begin
                with MenuItemConfigDebugOutput do begin
                  Tag := Ord(Checked);
                  Checked := true
                end;
                DebugWriteF('Backend magic number is %d, should be %d.\n',
                            [Backend.SharedLibraryMagicNumber, Backend.FrontendMagicNumber]);
                DebugWriteLn('***** THIS IS A SERIOUS ERROR AND WILL PROBABLY RESULT IN A CRASH. *****');
                with MenuItemConfigDebugOutput do begin
                  Checked := Tag <> 0;
                  Tag := 0
                end
              end
            end else begin
              with MenuItemConfigDebugOutput do begin
                Tag := Ord(Checked);
                Checked := true
              end;
              DebugWriteLn('Unable to get backend magic number, attempting to unload.');
              BackendUnload;
              DebugWriteLn('Continuing without a custom backend.');
              with MenuItemConfigDebugOutput do begin
                Checked := Tag <> 0;
                Tag := 0
              end
            end
          end else begin
            with MenuItemConfigDebugOutput do begin
              Tag := Ord(Checked);
              Checked := true
            end;
            DebugWriteF('Unable to reload backend %s.\n', [BackendError]);
            DebugWriteLn('Continuing without a custom backend.');
            with MenuItemConfigDebugOutput do begin
              Checked := Tag <> 0;
              Tag := 0
            end
          end
        else begin
          with MenuItemConfigDebugOutput do begin
            Tag := Ord(Checked);
            Checked := true
          end;
          DebugWriteLn('Candidate backend is incompatible. Disabling auto-reload and continuing.');
          if MenuItemConfigBackendReloadOnNewVersion.Checked then
            MenuItemConfigBackendReloadOnNewVersion.Click;
          if MenuItemConfigBackendReloadOnRepoRelease.Checked then
            MenuItemConfigBackendReloadOnRepoRelease.Click;
          with MenuItemConfigDebugOutput do begin
            Checked := Tag <> 0;
            Tag := 0
          end
        end
      end else begin
        DebugWrite('Backend unchanged, reloading configuration... ');
        seenHUP2 := false;
        if Backend.LoadConfiguration(projectName) then
          DebugWriteLn('OK.')
        else
          DebugWriteLn('Failed.')
      end
    else
      if SecondOfTheMinute(Now) mod 15 = 0 then begin
        scratch := BackendUpdated;
        if scratch <> '' then begin
          DebugWriteLn('New backend ' + scratch + ' available.');
          seenHUP2 := true
        end
      end
  finally
    MenuItemConfigDebugOutput.Checked := debugEnabled;
    if not SeenQuit then
      Timer1.Enabled := true
  end
end { TFormDsoCat.Timer1Timer } ;


(* Return a 32-bit unsigned magic number, this does not require any action from
  the memory manager. Despite being generated by frontend code, this should be
  changed to track the functions and parameters described by frontendprocs.inc
  (i.e. THIS FILE), which is to be considered definitive.
*)
function MainProgramMagicNumber: longword;

begin
{$ifdef USE_CMEM }
  result := FrontendMagicNumber
{$else           }

(* If the external cmem memory manager is not being used it is not, in general, *)
(* safe for dynamically-linked procedures or functions to be called so return a *)
(* broken magic number.                                                         *)

  result := 0
{$endif USE_CMEM }
end { MainProgramMagicNumber } ;


(* Message box optimised for position, for use by the backend.
*)
function MessageDlgOpt(const aMsg: string; dlgType: TMsgDlgType;
                        buttons: TMsgDlgButtons; helpCtx: longint): integer;

begin
  FormDsoCat.MessageDlgOpt(aMsg, dlgType, buttons, helpCtx)
end { MessageDlgOpt } ;


(* Debugging output to pane on main form, for use by the backend.
*)
procedure DebugWrite(const str: string; ln: boolean= false);

begin
  FormDsoCat.DebugWrite(str, ln)
end { DebugWrite } ;


(* Debugging output to pane on main form, for use by the backend.
*)
procedure DebugWriteLn(const str: string= '');

begin
  FormDsoCat.DebugWriteLn(str)
end { DebugWriteLn } ;


(* Debugging output to pane on main form, for use by the backend. This obeys \n
  in the format string, and always tries to "do the right thing" if used in
  conjunction with DebugWrite() above even if this implies a performance hit.
*)
procedure DebugWriteF(const fmt: string; values: array of const);

// See FormatEx() in e.g. Borg-NG for possible enhancements.

begin
  FormDsoCat.DebugWriteF(fmt, values)
end { DebugWriteF } ;


(* Copy the program version etc. into the parameter as a C-style string. This
  is intended as an experimental secondary entry point that a hypothetical
  desktop GUI etc. can use to get program version information.
*)
function ModuleAboutText(ver: pchar; verLength: word): word; cdecl; experimental;

var     scratch: string;

begin
  scratch := FormDsoCat.ProjectName + ' ' + AboutText;
  if verLength - 1 < Length(scratch) then
    SetLength(scratch, verLength -1);
  strpcopy(ver, scratch);
  result := Length(scratch)
end { ModuleAboutText } ;


(* This is a dummy version of the click event handler which handles port name
  generation, which is difficult to debug in situ.
*)
procedure testPortNamesGeneration;

var
  i: integer;
  portNames: TStringList;
  caption: string;
  {%H-}checked: boolean;

begin
(* Initialise the menu from the known ports.                                    *)

  portNames := ListPorts();
  try
    for i := 0 to portNames.Count - 1 do begin
      Caption := portnames[i];
      if i = 0 then
        Checked := true
      else

(* Let's assume that because we're adding devices in sequence, that the most    *)
(* recent that the OS has seen plugged in appears last. Select this as the      *)
(* device to be used if it matches certain critera.                             *)

        Checked := selectAsDefault(Caption);
    end
  finally
    FreeAndNil(portNames)
  end
end { testPortNamesGeneration } ;


initialization
//  testPortNamesGeneration; Halt;
  testEngineeringFormat
end.

