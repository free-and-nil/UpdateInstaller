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
{$WARN SYMBOL_DEPRECATED OFF}

unit LogToTempU;

interface

uses SysUtils;

type
	TLogFunc = procedure (const sLogMsg: String; const bNewLine: Boolean = true);

function DeleteExistingLogFile : Boolean;
procedure DisableLogFunction;
procedure EnableLogFunction;
function GetLogFileName : TFileName;
procedure LogDisableConsoleOutput;
procedure LogEnableConsoleOutput;
procedure LogInsertDateTimeHeader;
procedure LogMsg (const sMsg: String;
				  const bNewLine: Boolean = true); overload;
procedure LogMsg (const sMsg: String; const Args: array of const;
				  const bNewLine: Boolean = true); overload;
procedure LogMsgCrLf (const sMsg: String); overload;
procedure LogMsgCrLf (const sMsg: String; const Args: array of const); overload;
{$IFDEF UNICODE}
procedure SaveLogFileAsAnsiFile;
procedure SaveLogFileAsUnicodeFile;
{$ENDIF}
procedure SetLogFileName (const sFileName: TFileName);
procedure SetLogFunc (const LogFunction: TLogFunc);

implementation

uses SyncObjs, Windows,
{$IFDEF FPC}
     RtlConsts,
{$ELSE}
     SysConst,
{$ENDIF}
  	 Win32ToolsU, VerifyU;

var
{$IFDEF UNICODE}
    bAnsiFile : Boolean = true;
{$ENDIF}
	CriticalSection : SyncObjs.TCriticalSection = NIL;
    LogFunc : TLogFunc = NIL;
    SavedLogFunc : TLogFunc = NIL;  // Not thread safe
	sLogFileName : TFileName;

(* ---- *)

procedure LogToConsole (const sMsg: String; const bNewLine: Boolean);
begin
	if (bNewLine) then
		WriteLn (sMsg)
    else Write (sMsg);
end; { LogToConsole }

(* ---- *)

function DeleteExistingLogFile : Boolean;
begin
	if (FileExists (sLogFileName)) then
		Result := SysUtils.DeleteFile (sLogFileName)
    else Result := true;
end; { DeleteExistingLogFile }

(* ---- *)

procedure DisableLogFunction;
begin
    Assert (not Assigned (SavedLogFunc));

    SavedLogFunc := LogFunc;  // Not thread safe
    LogFunc := NIL;
end; { DisableLogFunction }

(* ---- *)

procedure EnableLogFunction;
begin
    Assert (Assigned (SavedLogFunc));

    LogFunc := SavedLogFunc;  // Not thread safe
    SavedLogFunc := NIL;
end; { EnableLogFunction }

(* ---- *)

function GetLogFileName : TFileName;
begin
	Result := sLogFileName;
end; { GetLogFileName }

(* ---- *)

{$IFDEF MSWINDOWS}
procedure WriteToFile (const sOut: String; const bNewLine: Boolean);

const
    cCrLf = #13#10;

var
    hFile : THandle;
    dwBytesWritten : DWord;
    bFileExists : Boolean;

begin
	Assert (sLogFileName <> '');

	CriticalSection.Acquire;

    bFileExists := FileExists (sLogFileName);

	try
        if (bFileExists) then
            hFile := THandle (FileOpen (sLogFileName, fmOpenReadWrite))
        else hFile := THandle (FileCreate (sLogFileName));

        if (hFile = HFILE_ERROR) then
            RaiseLastWin32Error;

        try
            if (bFileExists) then
                SetFilePointer (hFile, 0, NIL, FILE_END);

            Win32Check (WriteFile (hFile, PChar (sOut)^,
                                   Length (sOut) * SizeOf (Char),
                                   dwBytesWritten{%H-}, NIL));

            if (bNewLine) then
                Win32Check (WriteFile (hFile, PChar (cCrLF)^,
                                       Length (cCrLf) * SizeOf (Char),
                                       dwBytesWritten, NIL));

            VerifyApi (FlushFileBuffers (hFile));

        finally
            FileClose (THandle (hFile));
        end; { try / finally }

	finally
		CriticalSection.Release;
	end; { try / finally }
end; { WriteToFile }

{$ELSE}

procedure WriteToFile (const sOut: String; const bNewLine: Boolean);

var
	LogFile : TextFile;

begin
	Assert (sLogFileName <> '');

	CriticalSection.Acquire;

	try
		AssignFile (LogFile, sLogFileName);

		if (FileExists (sLogFileName)) then
			Append (LogFile)
		else ReWrite (LogFile);

		if (IOResult = 0) then
			try { In Datei ausgeben }
            	if (bNewLine) then
					WriteLn (LogFile, sOut)
                else Write (LogFile, sOut);

				Flush (LogFile);

			finally
				CloseFile (LogFile);
			end; { try / finally }

	finally
		CriticalSection.Release;
	end; { try / finally }
end; { WriteToFile }
{$ENDIF}

(* ---- *)

{$IFDEF UNICODE}
  {$IFDEF MSWINDOWS}
procedure WriteToFileAnsi (const sOut: AnsiString; const bNewLine: Boolean);

const
    cCrLf = AnsiString (#13#10);

var
    hFile : THandle;
    dwBytesWritten : DWord;
    bFileExists : Boolean;

begin
	Assert (sLogFileName <> '');

	CriticalSection.Acquire;

    bFileExists := FileExists (sLogFileName);

	try
        if (bFileExists) then
            hFile := THandle (FileOpen (sLogFileName, fmOpenReadWrite))
        else hFile := THandle (FileCreate (sLogFileName));

        if (hFile = HFILE_ERROR) then
            RaiseLastWin32Error;

        try
            if (bFileExists) then
                SetFilePointer (hFile, 0, NIL, FILE_END);

            Win32Check (WriteFile (hFile, PAnsiChar (sOut)^, Length (sOut),
                                   dwBytesWritten{%H-}, NIL));

            if (bNewLine) then
                Win32Check (WriteFile (hFile, PAnsiChar (cCrLF)^,
                                       Length (cCrLf), dwBytesWritten, NIL));

            VerifyApi (FlushFileBuffers (hFile));

        finally
            FileClose (THandle (hFile));
        end; { try / finally }

	finally
		CriticalSection.Release;
	end; { try / finally }
end; { WriteToFile }
  {$ELSE}  // {$IFDEF MSWINDOWS}

procedure WriteToFileAnsi (const sOut: AnsiString; const bNewLine: Boolean);

var
	LogFile : TextFile;

begin
	Assert (sLogFileName <> '');

	CriticalSection.Acquire;

	try
		AssignFile (LogFile, sLogFileName);

		if (FileExists (sLogFileName)) then
			Append (LogFile)
		else ReWrite (LogFile);

		if (IOResult = 0) then
			try { In Datei ausgeben }
            	if (bNewLine) then
					WriteLn (LogFile, sOut)
                else Write (LogFile, sOut);

				Flush (LogFile);

			finally
				CloseFile (LogFile);
			end; { try / finally }

	finally
		CriticalSection.Release;
	end; { try / finally }
end; { WriteToFileAnsi }
  {$ENDIF}
{$ENDIF}

(* ---- *)

procedure LogDisableConsoleOutput;
begin
	LogFunc := NIL;
end; { LogDisableConsoleOutput }

(* ---- *)

procedure LogEnableConsoleOutput;
begin
    LogFunc := LogToConsole;
end; { LogEnableConsoleOutput }

(* ---- *)

procedure LogInsertDateTimeHeader;

var
    sDateTime : String;
    iLen : Integer;

begin
    sDateTime := ' ' + DateTimeToStr (Now) + ' ';
    iLen := 39 - (Length (sDateTime) div 2);
    LogMsg ('%s%s%s',
            [StringOfChar ('-', iLen), sDateTime, StringOfChar ('-', iLen)]);
end; { LogInsertDateTimeHeader }

(* ---- *)

procedure LogMsg (const sMsg: String;
				  const bNewLine: Boolean = true);
begin
	if (@LogFunc <> NIL) then
    	LogFunc (sMsg, bNewLine);

{$IFDEF UNICODE}
    if (bAnsiFile) then
        WriteToFileAnsi (AnsiString (sMsg), bNewLine)
    else
{$ENDIF}
	WriteToFile (sMsg, bNewLine);
end; { LogMsg }

(* ---- *)

procedure LogMsg (const sMsg: String; const Args: array of const;
				  const bNewLine: Boolean = true);
begin
	LogMsg (Format (sMsg, Args), bNewLine);
end; { LogMsg }

(* ---- *)

procedure LogMsgCrLf (const sMsg: String);
begin
    LogMsg (sMsg + #13#10, true);
end; { LogMsgCrLf }

(* ---- *)

procedure LogMsgCrLf (const sMsg: String; const Args: array of const);
begin
    LogMsg (sMsg + #13#10, Args, true);
end; { LogMsgCrLf }

(* ---- *)

{$IFDEF UNICODE}
procedure SaveLogFileAsAnsiFile;
begin
    bAnsiFile := true;
end; { SaveLogFileAsAnsiFile }

(* ---- *)

procedure SaveLogFileAsUnicodeFile;
begin
    bAnsiFile := false;
end; { SaveLogFileAsUnicodeFile }
{$ENDIF}

(* ---- *)

procedure SetDefaultLogFileName;
begin
    SetLogFileName (GetTempDir +
					ExtractFileName (ChangeFileExt (ParamStr (0), '.log')));
end; { SetDefaultLogFileName }

(* ---- *)

procedure SetLogFileName (const sFileName: TFileName);

var
	sDirectory : String;

begin
	Assert (sFileName <> '');

    if (DirectoryExists (sFileName)) then
        raise EInOutError.CreateFmt (SInvalidFileName + '"%s"', [sFileName]);

    sDirectory := ExtractFilePath (sFileName);

    if (Pos ('\', sFileName) > 0) then
    begin
        if (sDirectory <> '') then
        begin
            if not (DirectoryExists (sDirectory)) then
                if not (ForceDirectories (sDirectory)) then
                    raise EInOutError.CreateFmt (SCannotCreateDir + '"%s"',
                                                 [sDirectory]);
        end { if }
        else raise EInOutError.CreateFmt (SInvalidFileName + ' "%s"',
        								  [sFileName]);

		sLogFileName := sFileName;
    end { if }
    else sLogFileName := ExtractFilePath (ParamStr (0)) + sFileName;
end; { SetLogFileName }

(* ---- *)

procedure SetLogFunc (const LogFunction: TLogFunc);
begin
	Assert (Assigned (LogFunction));

	LogFunc := LogFunction;
end; { SetLogFunc }

(* ---- *)

initialization
begin
    SetDefaultLogFileName;

	CriticalSection := SyncObjs.TCriticalSection.Create;

	if (IsConsole) then
        LogEnableConsoleOutput;
end; { initialization }

(* ---- *)

finalization
begin
	CriticalSection.Free;
end; { finalization }

(* ---- *)

end.

