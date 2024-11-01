(* Lazarus+FPC 2.1.0+3.2.0 on Linux Lazarus+FPC 2.1.0+3.2.0 on Linux Lazarus+FP *)

unit FftChart;

(* This is an FFT implementation by Don Cross from "Numerical Recipes in        *)
(* Fortran" at http://groovit.disjunkt.com/analog/time-domain/fft.html and      *)
(* http://www2.ic.uff.br/~ferraz/REDES/NA/Alunos/FFT/fourier.pas. I have not    *)
(* used the standard fftw library since it is not installed by default on at    *)
(* least some linux distreaux, and I want to avoid the risk that "icing on the  *)
(* cake" breaks the entire program. MarkMLl.                                    *)

{$mode objfpc}{$H+}

(*==========================================================================

    fourier.pas  -  Don Cross <dcross@intersrv.com>

    This is a Turbo Pascal Unit for calculating the Fast Fourier Transform
    (FFT) and the Inverse Fast Fourier Transform (IFFT).
    Visit the following URL for the latest version of this code.
    This page also has a C/C++ version, and a brief discussion of the
    theory behind the FFT algorithm.

       http://www.intersrv.com/~dcross/fft.html#pascal

    Revision history [most recent first]:

1996 December 11 [Don Cross]
    Improved documentation of the procedure CalcFrequency.
    Fixed some messed up comments in procedure ifft.

1996 December 6 [Don Cross]
    Made procedure 'fft_integer' more efficient when buffer size changes
    in successive calls:  the buffer is now only resized when the input
    has more samples, not a differing number of samples.
    Also changed the way 'fft_integer_cleanup' works so that it is
    more "bullet-proof".

1996 December 4 [Don Cross]
    Adding the procedure 'CalcFrequency', which calculates the FFT
    at a specific frequency index p=0..n-1, instead of the whole
    FFT.  This is O(n) instead of O(n*log(n)).

1996 November 30 [Don Cross]
    Adding a routine to allow FFT of an input array of integers.
    It is called 'fft_integer'.

1996 November 18 [Don Cross]
    Added some comments.

1996 November 17 [Don Cross]
    Wrote and debugged first version.

==========================================================================*)

{ N+,E+}   (* Allows code to use type 'double' and run on any iX86 machine *)
{$R-}      (* Turn off range checking...we violate array bounds rules *)


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
  Math;


function IsPowerOfTwo ( x: word ): boolean;
var   i, y:  word;
begin
    y := 2;
    for i := 1 to 15 do begin
        if x = y then begin
            IsPowerOfTwo := TRUE;
            exit;
        end;
        y := y SHL 1;
    end;

    IsPowerOfTwo := FALSE;
end;


function NumberOfBitsNeeded ( PowerOfTwo: word ): word;
var     i: word;
begin
    for i := 0 to 16 do begin
        if (PowerOfTwo AND (1 SHL i)) <> 0 then begin
            NumberOfBitsNeeded := i;
            exit;
        end;
    end;
end;


function ReverseBits ( index, NumBits: word ): word;
var     i, rev: word;
begin
    rev := 0;
    for i := 0 to NumBits-1 do begin
        rev := (rev SHL 1) OR (index AND 1);
        index := index SHR 1;
    end;

    ReverseBits := rev;
end;


procedure FourierTransform (
    AngleNumerator:  double;
    NumSamples:   word;
    var  RealIn:   array of double;
    var  ImagIn:   array of double;
    var  RealOut:  array of double;
    var  ImagOut:  array of double );
var
    NumBits, i, j, k, n, BlockSize, BlockEnd: word;
    delta_angle, delta_ar: double;
    alpha, beta: double;
    tr, ti, ar, ai: double;
begin
    if not IsPowerOfTwo(NumSamples) or (NumSamples<2) then begin
        write ( 'Error in procedure Fourier:  NumSamples=', NumSamples );
        writeln ( ' is not a positive integer power of 2.' );
        halt;
    end;

    NumBits := NumberOfBitsNeeded (NumSamples);
    for i := 0 to NumSamples-1 do begin
        j := ReverseBits ( i, NumBits );
        RealOut[j] := RealIn[i];
        ImagOut[j] := ImagIn[i];
    end;

    BlockEnd := 1;
    BlockSize := 2;
    while BlockSize <= NumSamples do begin
        delta_angle := AngleNumerator / BlockSize;
        alpha := sin ( 0.5 * delta_angle );
        alpha := 2.0 * alpha * alpha;
        beta := sin ( delta_angle );

        i := 0;
        while i < NumSamples do begin
            ar := 1.0;    (* cos(0) *)
            ai := 0.0;    (* sin(0) *)

            j := i;
            for n := 0 to BlockEnd-1 do begin
                k := j + BlockEnd;
                tr := ar*RealOut[k] - ai*ImagOut[k];
                ti := ar*ImagOut[k] + ai*RealOut[k];
                RealOut[k] := RealOut[j] - tr;
                ImagOut[k] := ImagOut[j] - ti;
                RealOut[j] := RealOut[j] + tr;
                ImagOut[j] := ImagOut[j] + ti;
                delta_ar := alpha*ar + beta*ai;
                ai := ai - (alpha*ai - beta*ar);
                ar := ar - delta_ar;
                INC(j);
            end;

            i := i + BlockSize;
        end;

        BlockEnd := BlockSize;
        BlockSize := BlockSize SHL 1;
    end;
end;


(*---------------------------------------------------------------------------
  procedure fft

  Calculates the Fast Fourier Transform of the array of complex numbers
  represented by 'RealIn' and 'ImagIn' to produce the output complex
  numbers in 'RealOut' and 'ImagOut'.
---------------------------------------------------------------------------*)
procedure fft (
    NumSamples:   word;
    var  RealIn:   array of double;
    var  ImagIn:   array of double;
    var  RealOut:  array of double;
    var  ImagOut:  array of double );
begin
    FourierTransform ( 2*PI, NumSamples, RealIn, ImagIn, RealOut, ImagOut );
end;


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
  minRms= 0.0;                          (* By definition                        *)
  seriesRms= 0;
  seriesRe= 1;
  seriesIm= 2;
  traceSequence: TTraceSequence= (seriesRms, seriesRe, seriesIm);

var
  NumSamples:   word;
  RealIn:   array of double;
  ImagIn:   array of double;
  RealOut:  array of double;
  ImagOut:  array of double;
  i, firstBin, lastBin: integer;
  scale, maxRms, ampRe, ampIm, ampRms: double;


  (* Trying to propagate phase information to the sign of the result doesn't
    really contribute anything useful, and with simple numerical methods is
    likely to end up messy.
  *)
  function cabs(r, i: double): double;

  begin
    result := Sqrt(Sqr(r) + Sqr(i));
  end { cabs } ;


begin
  NumSamples := Length(ss.Channels[index].samples);
  Assert(IsPowerOfTwo(NumSamples), 'FFT library limitation: number of samples must be an integer power of 2');
  SetLength(RealIn, NumSamples);
  SetLength(ImagIn, NumSamples);
  SetLength(RealOut, NumSamples);
  SetLength(ImagOut, NumSamples);
  for i := 0 to NumSamples - 1 do begin
    RealIn[i] := double(ss.Channels[index].samples[i]);
    ImagIn[i] := 0.0;
    RealOut[i] := 0.0;                  (* Just in case                         *)
    ImagOut[i] := 0.0
  end;
  FourierTransform (2*PI, NumSamples, RealIn, ImagIn, RealOut, ImagOut);
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
      ampRms := cabs(RealOut[i], ImagOut[i]);
      if ampRms > scale then
        scale := ampRms
    end
  end else
    scale := (lastBin - firstBin) / 2;  (* Scaled by number of bins             *)

(* Assume that neither Abs(real) nor Abs(imaginary) will be larger than the RMS *)
(* (geometric) sum, but that both the real and imaginary traces will be broadly *)
(* symmetrical about zero while the RMS trace will be folded from zero to its   *)
(* positive maximum. Hence the real and imaginary traces need to be scaled to a *)
(* greater extent than the RMS.                                                 *)

  maxRms := minRms;
  for i := firstBin to lastBin do begin
    if scale > 0.0 then begin
      ampRe := RealOut[i] / (scale * 2);
      ampIm := ImagOut[i] / (scale * 2);
      ampRms := cabs(RealOut[i], ImagOut[i]) / scale
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

(* The output charts are scaled +-(maxRms / 2) in the case of the real and      *)
(* imaginary components, and 0..maxRms in the case of the RMS geometric sum.    *)

  result := maxRms
end { PopulateFrequencyDomain } ;


end.

