(* Lazarus+FPC 2.1.0+3.2.0 on Linux Lazarus+FPC 2.1.0+3.2.0 on Linux Lazarus+FP *)

unit VcdCode;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ScopeStruct;

type
  TVCDForm = class(TForm)
  private

  public

  end;

var
  VCDForm: TVCDForm;

(* Export the captured data in VCD form. The transition between true and false
  is assumed to be the trigger level, there are no skipped or inserted preamble
  points, and so on.
*)
procedure ExportVCD(scopeStruct: TScopeStruct; const filename: string);

implementation

{$R *.lfm}

uses
  DsoFrontendCode, IniFilesAbout;


(* Export the captured data in VCD form. The transition between true and false
  is assumed to be the trigger level, there are no skipped or inserted preamble
  points, and so on.
*)
procedure ExportVCD(scopeStruct: TScopeStruct; const filename: string);

var
  vcdFile: Text;
  scratch: String;
  i: integer;
  currentTime: longword;
  currentValue: boolean;

begin
  try
{$I- }
    AssignFile(vcdFile, filename);
    Rewrite(vcdFile);
    WriteLn(vcdFile, '$date ' + ISONow() + ' $end');
    scratch:= AboutText();
    Delete(scratch, 1, Pos('Built', scratch) - 1);
    i := Pos(#$0a, scratch);
    if i > 0 then
      SetLength(scratch, i - 1);
    i := Pos(#$0d, scratch);
    if i > 0 then
      SetLength(scratch, i - 1);
    WriteLn(vcdFile, '$version ' + FormDsoCat.ProjectName + ' ' + scratch + ' $end');
    WriteLn(vcdFile, '$timescale 100 ns $end'); (* Timestamps use 10000000 multiplier *)
    WriteLn(vcdFile, '$scope module logic $end');
    WriteLn(vcdFile, '$var wire 1 ! Channel_1 $end');
    WriteLn(vcdFile, '$comment Level transition: ', scopeStruct.TriggerLevelVolts, ' Volts $end');
    WriteLn(vcdFile, '$comment Raw sampling rate: ', scopeStruct.SamplesPerSecond(), ' per second $end');
    WriteLn(vcdFile, '$upscope $end');
    WriteLn(vcdFile, '$enddefinitions $end');
    WriteLn(vcdFile, '$dumpvars');
    currentTime := 0;
    currentValue := scopeStruct.Channels[0].samples[0] < scopeStruct.TriggerLevelVolts;
    if currentValue then
      WriteLn(vcdFile, '1!')
    else
      WriteLn(vcdFile, '0!');
    WriteLn(vcdFile, '$end');
    i := Length(scopeStruct.Channels[0].samples);       (* For debugging        *)
    for i := 1 to Length(scopeStruct.Channels[0].samples) - 1 do
      if (scopeStruct.Channels[0].samples[i] < scopeStruct.TriggerLevelVolts) <> currentValue then begin
        currentTime := Trunc(((i - 1) * scopeStruct.SamplesPerSecond()) / 10000000.0); (* 100 ns timescale *)
        WriteLn(vcdFile, '#' + IntToStr(currentTime));
        currentValue := not currentValue;
        if currentValue then
          WriteLn(vcdFile, '1!')
        else
          WriteLn(vcdFile, '0!')
      end;
      Close(vcdFile)
{$I+ }
  except
    FormDsoCat.MessageDlgOpt('Unable to write ' + fileName, mtError, [mbOk], 0)
  end
end { ExportVCD } ;


end.

