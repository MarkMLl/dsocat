(* Lazarus+FPC 2.1.0+3.2.0 on Linux Lazarus+FPC 2.1.0+3.2.0 on Linux Lazarus+FP *)

unit xmodem;

(* Handle the XModem protocol. Generally speaking the data being transferred is *)
(* assumed to be a text file, and the host or device to which we are connected  *)
(* is assumed to be local and reliable so delay handling can be fairly naive.   *)
(*                                                              MarkMLl.        *)

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Serial;

(* Collect a file and return it as a StringList which must eventually be freed
  by the caller. Any error results in memory being freed and nil returned.
*)
function GetXModem(serialHandle: TSerialHandle): TStringList;


implementation

uses
  DsoFrontendCode;

procedure idle(h: TSerialHandle);

begin
  Application.ProcessMessages
end { idle } ;


(* Collect a file and return it as a StringList which must eventually be freed
  by the caller. Any error results in memory being freed and nil returned.
*)
function GetXModem(serialHandle: TSerialHandle): TStringList;

{ define DEBUG_XMODEM }

label
  nakForHeader, askForHeader;

const
  soh= $01;
  eot= $04;
  ack= $06;
  nak= $15;
  eof= $1a;

(* How many bad packets (header+data+checksum) should we tolerate before we     *)
(* give up?                                                                     *)

  nakLimit= 5;

(* Should we implement the common practice of naking the first eot since it's   *)
(* not protected by a checksum, and expecting the other end to repeat it?       *)

  eotLimit= 1;

(* Once we're confident that we've got a real EOT should we Ack it or terminate *)
(* silently? It's probably good practice to send an Ack since if the other end  *)
(* is expecting one it saves a timeout, but there is some small risk that       *)
(* sending an unexpected byte could mess up the protocol handler.               *)

  ackEot= true;

var
  orphan: string= '';
  header: array[0..2] of byte;
  data: array[0..127] of byte;
  checksum: array[0..0] of byte;
  response: byte;
  expectedSeq: byte= 1;
  i, acc: integer;
  eotCount: integer= 0;
  nakCount: integer= -15;               (* Very tolerant while prompting for    *)
                                        (* first packet.                        *)

  procedure dump(sending: boolean; bytes: array of byte; ascii: boolean= false);

  const
    asciiFriendly= false;

  var
    i: integer;

  begin
    if sending then
      DebugWrite('> ')
    else
      DebugWrite('< ');
    for i := 0 to High(bytes) do
      if ascii and asciiFriendly then
        DebugWrite(Chr(bytes[i]))
      else
        DebugWrite(HexStr(bytes[i], 2) + ' ');
    DebugWriteLn
  end { dump } ;


begin
  if serialHandle = InvalidSerialHandle then begin
    DebugWriteLn('===== Bad serial handle =====');
    exit(nil)
  end;
  result := TStringList.Create;
  SerialIdle := @idle;
  DebugWriteLn('===== Starting XModem get operation =====');
  try
    while true do begin

(* Send an initial NAK, or ACK/NAK the last data packet, expecting to receive a *)
(* three-byte header.                                                           *)

      FillByte(header, SizeOf(header), 0);
nakForHeader:
      if header[0] = eot then           (* Special case: short EOT response is  *)
        eotCount += 1;                  (* first NAKed and only when repeated   *)
      if eotCount >= eotLimit then      (* is it obeyed.                        *)
        break;
      response := nak;
      nakCount += 1;
      if nakCount >= nakLimit then
        break;
{$ifdef DEBUG_XMODEM }
      DebugWriteLn('== Sending Nak');
{$endif DEBUG_XMODEM }
askForHeader:
      SerWrite(serialHandle, response, 1);
      dump(true, response);
      FillByte(header, SizeOf(header), 0);
{$ifdef DEBUG_XMODEM }
      DebugWriteLn('== Expect header');
{$endif DEBUG_XMODEM }
      if SerReadTimeout(serialHandle, header, SizeOf(header), 1000) <> SizeOf(header) then begin
        dump(false, header);
        goto nakForHeader
      end;
      dump(false, header);
      eotCount := 0;
{$ifdef DEBUG_XMODEM }
      DebugWriteLn('== Expect SOH');
{$endif DEBUG_XMODEM }
      if header[0] <> soh then
        goto nakForHeader;
{$ifdef DEBUG_XMODEM }
      DebugWriteLn('== Expect packet ' + IntToStr(expectedSeq));
{$endif DEBUG_XMODEM }
      if header[1] <> expectedSeq then
        goto nakForHeader;
      if (header[1] + header[2]) mod 256 <> $ff then
        goto nakForHeader;

(* Expect to receive a 128-byte data packet, then a checksum.                   *)

      FillByte(data, SizeOf(data), eof);
{$ifdef DEBUG_XMODEM }
      DebugWriteLn('== Expect data');
{$endif DEBUG_XMODEM }
      if SerReadTimeout(serialHandle, data, SizeOf(data), 5000) <> SizeOf(data) then begin
        dump(false, data, true);
        goto nakForHeader
      end;
      dump(false, data, true);
      FillByte(checksum, SizeOf(checksum), 0);
{$ifdef DEBUG_XMODEM }
      DebugWriteLn('== Expect checksum');
{$endif DEBUG_XMODEM }
      if SerReadTimeout(serialHandle, checksum, SizeOf(checksum), 100) <> SizeOf(checksum) then begin
        dump(false, checksum);
        goto nakForHeader
      end;
      dump(false, checksum);
      acc := 0;
      for i := 0 to SizeOf(data) - 1 do
        acc := (acc + data[i]) mod 256;
      if acc <> checksum[0] then
        goto nakForHeader;

(* Data is OK. Commit it, increment the expected sequence number, and prompt    *)
(* for the next header.                                                         *)

      for i := 0 to SizeOf(data) - 1 do
        if data[i] = $0a then begin
          result.Append(orphan);
          orphan := ''
        end else
          orphan += Chr(data[i]);

      expectedSeq := (expectedSeq + 1) mod 256;
      response := ack;
      nakCount := 0;
{$ifdef DEBUG_XMODEM }
      DebugWriteLn('== Sending Ack');
{$endif DEBUG_XMODEM }
      goto askForHeader
    end
  finally
    SerialIdle := nil
  end;

(* We get here after a break in response to repeated EOTs, or too many          *)
(* consecutive NAKs.                                                            *)

  if (eotCount >= eotLimit) and ackEot then begin
    response := ack;
{$ifdef DEBUG_XMODEM }
    DebugWriteLn('== Sending Ack');
{$endif DEBUG_XMODEM }
    SerWrite(serialHandle, response, 1);
    dump(true, response)
  end;
  if nakCount >= nakLimit then begin
    FreeAndNil(result);
    DebugWriteLn('===== XModem get operation failed =====')
  end else
    DebugWriteLn('===== Completed XModem get operation =====')
end { GetXModem } ;


end.

