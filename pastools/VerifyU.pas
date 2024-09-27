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

unit VerifyU;

interface

(**
{$IFDEF DELPHI3}
procedure Verify (const {%H-}bExpression: Boolean; const {%H-}sMsg: String
{$IFDEF DELPHI4}
    = ''
{$ENDIF}
    );
{$ENDIF}
{$IFDEF SUPPORTS_INLINE}
{ Delphi 2005 already supports "inline", but there's a bug in 2006:
  http://www.thedelphigeek.com/2007/02/nasty-inline-codegen-bug-in-bds-2006.html }
    inline;
{$ENDIF}
**)

procedure VerifyApi (const {%H-}bExpression: Boolean);
{$IFDEF SUPPORTS_INLINE}
    inline;
{$ENDIF}

implementation

uses SysUtils;

(* ---- *)

(**
{$IFDEF DELPHI3}
procedure Verify (const bExpression: Boolean; const sMsg: String);
begin
{$IFDEF FPC}
    {$IFDEF DEBUG}
        Assert (bExpression, sMsg);
    {$ENDIF}
{$ELSE}
    {$IFOPT C+}  // ASSERTIONS ON
        Assert (bExpression, sMsg);
    {$ENDIF}
{$ENDIF}
end; { Verify }
{$ENDIF}
**)

(* ---- *)

procedure VerifyApi (const bExpression: Boolean);
begin
{$IFOPT C+}  // ASSERTIONS ON
    if (bExpression = false) then
  {$IFDEF DELPHI6_UP}
        RaiseLastOSError;
  {$ELSE}
        RaiseLastWin32Error;
  {$ENDIF DELPHI6}
{$ENDIF IFOPT}
end; { VerifyApi }

(* ---- *)

end.

