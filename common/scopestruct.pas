(* Lazarus+FPC 2.1.0+3.2.0 on Linux Lazarus+FPC 2.1.0+3.2.0 on Linux Lazarus+FP *)

unit ScopeStruct;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

(* Update both frontend and backend magic numbers if anything changes here.     *)

type

  (* Coupling mode.
  *)
  TCouple= (CoupleDC, CoupleAC, CoupleGnd, CoupleNone);

  (* Trigger mode.
  *)
  TTriggerMode= (TriggerModeAuto, TriggerModeNormal, TriggerModeSingle);

  (* Trigger slope.
  *)
  TTriggerSlope= (TriggerSlopeFalling, TriggerSlopeRising);

  (* Per-channel configuration information and data. The number of samples will
    normally be the same on all channels on an oscilloscope, but this is not
    necessarily the case on a device with pretensions towards being a logic
    analyzer.
  *)
  TChannel= record
              voltsPerDivision: single; // D
              positionOffsetVolts: single; // D
              minVoltsAxis, maxVoltsAxis: single; // D
              couple: TCouple;
              resolvedVolts: single; // D
              triggerAtSample: integer;
              samples: array of single
            end;

// D marks debug output

  ATChannel= array of TChannel;

  (* Device-independent state and captured data.
  *)
  TScopeStruct= class
                protected
                  fSamplesPerDivision: integer;
                  fSecondsPerDivision: single;
                  fVerticalResolution: cardinal;
                  fHorizontalResolution: cardinal;
                  fTriggerChannel: cardinal;
                  fTriggerLevelVolts: single; // D
                  fTriggerMode: TTriggerMode;
                  fTriggerSlope: TTriggerSlope;
                  fChannels: ATChannel;
                public
                  constructor Create(channelCount: integer);

                  (* This is derived from the samples-per-division (normally constant for a given
                    instrument) and the seconds-per-division (normally a knob setting).
                  *)
                  function SamplesPerSecond(): cardinal;

                  (* The time domain display will always have an integer number of divisions, but
                    only 2^n (where n is an integer) samples will be passed to an FFT to generate
                    a frequency domain display. Treat 1024 samples as special, since results will
                    generally be better if in this particular case a single sample is duplicated
                    rather than multiple samples trimmed.
                  *)
                  function SamplesDisplayed(channel: integer= 0): cardinal;

                  (* Minimum horizontal axis value in the time domain.
                  *)
                  function MinTimeAxis(channel: integer= 0): single;

                  (* Maximum horizontal axis value in the time domain.
                  *)
                  function MaxTimeAxis(channel: integer= 0): single;

(* These properties enforce read-only access to the setup information as far as *)
(* the main program is concerned, but are overridden by read/write properties   *)
(* in a helper the DsoBackendCode unit which allows the driver to set them up   *)
(* to match the physical instrument.                                            *)

                  property SamplesPerDivision: integer read fSamplesPerDivision;
                  property SecondsPerDivision: single read fSecondsPerDivision;
                  property VerticalResolution: cardinal read fVerticalResolution;
                  property HorizontalResolution: cardinal read fHorizontalResolution;
                  property TriggerChannel: cardinal read fTriggerChannel;
                  property TriggerLevelVolts: single read fTriggerLevelVolts;
                  property TriggerMode: TTriggerMode read fTriggerMode;
                  property TriggerSlope: TTriggerSlope read fTriggerSlope;

                  (* Note that despite the fact that this is a read-only property, it is effectively
                    returning a pointer to an array of records the content of which is unprotected.
                  *)
                  property Channels: ATChannel read fChannels;
                end;

  (* Various types of test data.
  *)
  TTestType=(ttSine, ttSquare, ttPulse, ttRisingEdge, ttFallingEdge, ttRisingSaw, ttFallingSaw, ttTriangle);


implementation

constructor TScopeStruct.Create(channelCount: integer);

begin
  inherited Create;
  fSamplesPerDivision := 0;
  fSecondsPerDivision := 1.0e-3;        (* Reasonable front-panel defaults      *)
  fVerticalResolution := 0;
  fHorizontalResolution := 0;
  fTriggerChannel := 0;
  fTriggerLevelVolts := 2.0;
  fTriggerMode := TriggerModeAuto;
  fTriggerSlope := TriggerSlopeFalling;
  SetLength(fChannels, channelCount);
  with fChannels[0] do begin
    voltsPerDivision := 2.0;
    positionOffsetVolts := 0.0;
    minVoltsAxis := 0.0;
    maxVoltsAxis := 0.0;
    couple := CoupleDC;
    triggerAtSample := 0;               (* Can't set until number of samples known *)
    SetLength(samples, 0)
  end
end { TScopeStruct.Create } ;


(* This is derived from the samples-per-division (normally constant for a given
  instrument) and the seconds-per-division (normally a knob setting).
*)
function TScopeStruct.SamplesPerSecond(): cardinal;

begin
  result := Trunc(SamplesPerDivision / SecondsPerDivision)
end { TScopeStruct.SamplesPerSecond } ;


(* The time domain display will always have an integer number of divisions, but
  only 2^n (where n is an integer) samples will be passed to an FFT to generate
  a frequency domain display. Treat 1024 samples as special, since results will
  generally be better if in this particular case a single sample is duplicated
  rather than multiple samples trimmed.
*)
function TScopeStruct.SamplesDisplayed(channel: integer= 0): cardinal;

begin
  result := Length(fChannels[channel].samples);
  if result = 1024 then
    result := 1025
  else
    result := (result div SamplesPerDivision) * SamplesPerDivision
end { TScopeStruct.SamplesDisplayed } ;


(* Minimum horizontal axis value in the time domain.
*)
function TScopeStruct.MinTimeAxis(channel: integer= 0): single;

begin
  result := (0 - fChannels[channel].triggerAtSample) / samplesPerSecond;
end { TScopeStruct.MinTimeAxis } ;


(* Maximum horizontal axis value in the time domain.
*)
function TScopeStruct.MaxTimeAxis(channel: integer= 0): single;

begin
  result := (samplesDisplayed(channel) -
                          fChannels[channel].triggerAtSample) / samplesPerSecond
end { TScopeStruct.MaxTimeAxis } ;


end.

