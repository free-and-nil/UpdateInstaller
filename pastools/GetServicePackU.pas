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

unit GetServicePackU;

interface

function GetServicePack : Word;

implementation

uses Windows
{$IFNDEF UNICODE}
	 , WinNT_ImportU
{$ENDIF}
	 ;

(* ---- *)

function GetServicePack : Word;
{ Aufruf funktioniert nur ab Windows NT4 SP6 }

var
    VerInfoEx : TOSVersionInfoEx;
    pVerInfo : POSVersionInfo;

begin
	Result := 0;

    FillChar (VerInfoEx{%H-}, SizeOf (TOSVersionInfoEx), #0);
    VerInfoEx.dwOSVersionInfoSize := SizeOf (TOSVersionInfoEx);

    pVerInfo := POSVersionInfo (@VerInfoEx);

    if (GetVersionEx (pVerInfo^)) then
        Result := VerInfoEx.wServicePackMajor;
end; { GetServicePack }

(* ---- *)

end.
