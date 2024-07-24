// {$I GetProcAddress.inc}

(**
const
{$IFDEF UNICODE}
    AWSuffix = 'W';
{$ELSE}
    AWSuffix = 'A';
{$ENDIF}
**)

(* ---- *)

function GPA (const hModule: Windows.HMODULE; lpProcName: PAnsiChar) : FARPROC;
{$IFNDEF DEBUG}
	{$IFDEF DELPHI2007} inline; {$ENDIF DELPHI2007}
{$ENDIF DEBUG}

begin
	Assert (hModule <> 0);
	Assert (lpProcName <> NIL);

	Result := GetProcAddress (hModule, lpProcName);
    Assert (Result <> NIL);
end; { GPA }

(* ---- *)

// EOF
