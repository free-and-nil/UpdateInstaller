//
//  Original author: Olaf Hess
//  This work is published from: Germany.
//
//  To the extent possible under law, Olaf Hess has waived all copyright and
//  related or neighboring rights to this source code:
//  http://creativecommons.org/publicdomain/zero/1.0/
//
//  Unless expressly stated otherwise, the person who associated a work with
//  this deed makes no warranties about the work, and disclaims liability for
//  all uses of the work, to the fullest extent permitted by applicable law.
//

{$I ..\switches.inc}

{$IFDEF FPC}
	{$MODE DELPHI}
{$ENDIF}

{$WARN SYMBOL_PLATFORM OFF}

unit DebugHelperU;

interface

{$MINENUMSIZE 4}

uses Windows;

function DebuggerPresent : Boolean; {$IFDEF SUPPORTS_INLINE} inline; {$ENDIF}

{$IFNDEF FPC}
  {$IFNDEF DELPHI2010_UP}
var
	IsDebuggerPresent : function : BOOL; stdcall = NIL;
  {$ENDIF}
{$ENDIF}

implementation

{$IFDEF FPC}
uses SysUtils;
{$ELSE}
uses SysUtils,
     VerifyU;
{$ENDIF}

(* ---- *)

function DebuggerPresent : Boolean;
begin
{$IFDEF FPC}
    Result := IsDebuggerPresent;
{$ELSE}
    Result := DebugHook <> 0;
{$ENDIF}
end; { DebuggerPresent }

(* ---- *)

{$IFNDEF FPC}
  {$IFNDEF DELPHI2010_UP}

var
    hKernel32 : THandle = 0;

(* ---- *)

{$I GetProcAddress.inc}

(* ---- *)

initialization
begin
    hKernel32 := LoadLibrary (kernel32);

    if (hKernel32 <> 0) then
		IsDebuggerPresent := GetProcAddress (hKernel32, 'IsDebuggerPresent');
end; { initialization }

(* ---- *)

finalization
begin
	if (hKernel32 <> 0) then
		VerifyApi (FreeLibrary (hKernel32));
end; { finalization }

(* ---- *)

  {$ENDIF}  // DELPHI2010_UP
{$ENDIF}  // FPC

end.
