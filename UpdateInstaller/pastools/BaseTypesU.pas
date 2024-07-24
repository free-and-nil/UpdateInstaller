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

unit BaseTypesU;

interface

{$IFDEF DELPHI7_UP}
uses Types;
{$ENDIF}

type
    PTFileString = ^TFileString;
    TFileString = String;
{$IFNDEF FPC}
    LONG = LongInt;
{$ENDIF}

type
{$IFDEF DELPHI7_UP}
    TaString = TStringDynArray;
{$ELSE}
	TaString = array of String;  // TYPES.PAS: TStringDynArray
{$ENDIF}

{$IFNDEF FPC}
  {$IFNDEF UNICODE}
    NativeInt = Integer;
    NativeUInt = Cardinal;
    TCharSet = Set of Char;
  {$ENDIF}
{$ENDIF}

implementation

end.

