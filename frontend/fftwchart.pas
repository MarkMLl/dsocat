(* Lazarus+FPC 2.1.0+3.2.0 on Linux Lazarus+FPC 2.1.0+3.2.0 on Linux Lazarus+FP *)

unit FftwChart;

(* This is based on the FFTW interface example distributed with FPC, adjusted   *)
(* to present the same interface as the FFT unit to check that the algorithm is *)
(* correct. However despite the eminence of FFTW, it is best not used as the    *)
(* default algorithm since it is not present in all distreaux and variants.     *)
(*                                                              MarkMLl         *)

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, TAGraph, TASeries, ScopeStruct;

(* Take the time domain series in the first parameter and use an FFT to populate
the frequency domain series in the second scaled linearly in the range 0.0 to
1.0; the returned value is the maximum amplitude (i.e. the divisor in the
scaling operation).
*)
function PopulateFrequencyDomain(ss: TScopeStruct; fd: TChart; index: integer= 0;
                                                        scaleToUnity: boolean= false;
                                                        allBins: boolean= false): double;


implementation

uses
  fftw_s, Math;


(* Take the time domain series in the first parameter and use an FFT to populate
the frequency domain series in the second scaled linearly in the range 0.0 to
1.0; the returned value is the maximum amplitude (i.e. the divisor in the
scaling operation).
*)
function PopulateFrequencyDomain(ss: TScopeStruct; fd: TChart; index: integer= 0;
                                                        scaleToUnity: boolean= false;
                                                        allBins: boolean= false): double;

type TtraceSequence= array[0..2] of integer;

const
  arrayTop= 65535;                      (* Intended to be bigger than needed    *)
  minRms= 0.0;                          (* By definition                        *)
  seriesRms= 0;
  seriesRe= 1;
  seriesIm= 2;
  traceSequence: TTraceSequence= (seriesRms, seriesRe, seriesIm);

type
  Acomplex_single= array[0..arrayTop] of Complex_single;
  PAcomplex_single= ^Acomplex_single;

var
  NumSamples: cardinal;
  pin, pout: PAcomplex_single;
  plan: fftw_plan_single;
  i, firstBin, lastBin: integer;
  scale, maxRms, ampRe, ampIm, ampRms: double;


  (* Trying to propagate phase information to the sign of the result doesn't
    really contribute anything useful, and with simple numerical methods is
    likely to end up messy.
  *)
  function cabs(r, i: single): single;

  begin
    result := Sqrt(Sqr(r) + Sqr(i))
  end { cabs } ;


begin
  NumSamples := Length(ss.Channels[index].samples);

  {You can use getmem, but only fftw_getmem ensures 3DNOW/SSE
   algorithms.}

  fftw_getmem(pin, NumSamples * sizeof(complex_single));
  fftw_getmem(pout, NumSamples * sizeof(complex_single));

  {FFTW will now generate an algoritm to compute the FFT:}

  plan := fftw_plan_dft_1d(NumSamples, Pcomplex_single(pin), Pcomplex_single(pout),
                                        fftw_forward, [fftw_estimate]);

  {Put code to fill ics^ with input data here.}

  for i := 0 to NumSamples - 1 do begin
    pin^[i].re := ss.Channels[index].samples[i];
    pin^[i].im := 0.0;
    pout^[i].re := 0.0;
    pout^[i].im := 0.0
  end;
  fftw_execute(plan); {Execute FFT}

  {Output in ocs^}

  TLineSeries(fd.Series[seriesRms]).Clear;
  TLineSeries(fd.Series[seriesRms]).BeginUpdate;
  if (fd.Series.Count > 1) and fd.Series[seriesRe].Active then begin
    TLineSeries(fd.Series[seriesRe]).Clear;
    TLineSeries(fd.Series[seriesRe]).BeginUpdate
  end;
  if (fd.Series.Count > 2) and fd.Series[seriesIm].Active then begin
    TLineSeries(fd.Series[seriesIm]).Clear;
    TLineSeries(fd.Series[seriesIm]).BeginUpdate
  end;

(* Assume that the FFT result is symmetrical about the Nyquist frequency, and   *)
(* only display the lower part.                                                 *)

// TODO : Save real and imaginary parts. Don't implement without considering point selection in MenuItemConfigFrequencyDomainClick().

  firstBin := 0;
  if allBins then
    lastBin := NumSamples - 1
  else
    lastBin := NumSamples div 2 - 1;
  if scaleToUnity then begin
    scale := 0.0;                       (* Scaled to maximum RMS                *)
    for i := firstBin to lastBin do begin
      ampRms := cabs(pout^[i].re, pout^[i].im);
      if ampRms > scale then
        scale := ampRms
    end
  end else
    scale := (lastBin - firstBin) / 2;  (* Scaled by number of bins             *)
  maxRms := minRms;

(* Assume that neither Abs(real) nor Abs(imaginary) will be larger than the RMS *)
(* (geometric) sum, but that both the real and imaginary traces will be broadly *)
(* symmetrical about zero while the RMS trace will be folded from zero to its   *)
(* positive maximum. Hence the real and imaginary traces need to be scaled to a *)
(* greater extent than the RMS.                                                 *)

  for i := firstBin to lastBin do begin
    if scale > 0.0 then begin
      ampRe := pout^[i].re / (scale * 2);
      ampIm := pout^[i].im / (scale * 2);
      ampRms := cabs(pout^[i].re, pout^[i].im) / scale
    end else begin
      ampRe := 0.0;
      ampIm := 0.0;
      ampRms := 0.0
    end;
    if ampRms > maxRms then
      maxRms := ampRms;

// TODO : Order of addition should depend on traceSequence, or fix series order in IDE.

    TLineSeries(fd.Series[seriesRms]).AddXY(i, ampRms); (* Amplitudes added sequentially *)
    if (fd.Series.Count > 1) and fd.Series[seriesRe].Active then
      TLineSeries(fd.Series[seriesRe]).AddXY(i, ampRe);
    if (fd.Series.Count > 2) and fd.Series[seriesIm].Active then
      TLineSeries(fd.Series[seriesIm]).AddXY(i, ampIm)
  end;
  TLineSeries(fd.Series[seriesRms]).EndUpdate;
  if (fd.Series.Count > 1) and fd.Series[seriesRe].Active then
    TLineSeries(fd.Series[seriesRe]).EndUpdate;
  if (fd.Series.Count > 2) and fd.Series[seriesIm].Active then
    TLineSeries(fd.Series[seriesIm]).EndUpdate;
  fftw_destroy_plan(plan);
  fftw_freemem(pout);
  fftw_freemem(pin);

(* The output charts are scaled +-(maxRms / 2) in the case of the real and      *)
(* imaginary components, and 0..maxRms in the case of the RMS geometric sum.    *)

  result := maxRms
end { PopulateFrequencyDomain } ;


end.

