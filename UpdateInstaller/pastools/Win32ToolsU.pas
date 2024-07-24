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

unit Win32ToolsU;

interface

uses Windows, SysUtils,
     BaseTypesU;

type
	TDriveType = (dtRemovable, dtFixed, dtRemote, dtCDROM, dtRamDisk, dtError);
	TKeyType = (ktCapsLock, ktNumLock, ktScrollLock);
    TPowerStatus =
        (psCharging, psDischarging, psUndefined, psNoBattery, psNotCharging);
	TWindowsVersion = (wvWin32s, wvWin9x, wvWinNT, wvError);

    TYieldProc = procedure of object;
    TOutputProc = procedure (const sOutput: String) of object;

const
	DOMAIN_ALIAS_RID_ADMINS 				   = $00000220;
	DOMAIN_ALIAS_RID_NETWORK_CONFIGURATION_OPS = $0000022C;
	DOMAIN_ALIAS_RID_POWER_USERS 			   = $00000223;

function AddQuotationMarks (const sPath: String) : String;

(**
function CaptureConsoleOutput (const sCommand, sParameters: String) : String;
																	   overload;

procedure CaptureConsoleOutput (const sCommand, sParameters: String;
							    out sOutput : String;
                                out dwExitCode: DWord); overload;
**)

{$IFDEF SUPPORTS_OVERLOAD}
procedure CaptureConsoleOutput (const sCmdLine: String;
                                const OutputProc: TOutputProc;
                                const sCurrentDirectory: String = '';
						        const dwBufSize: DWord = 4096;
                                const YieldProc: TYieldProc = NIL);
	overload;

{$ENDIF}

function CaptureConsoleOutput (const sCmdLine: String;
                               const OutputProc: TOutputProc;
                               out dwExitCode: DWord;
                               const sCurrentDirectory: String = '';
						       const dwBufSize: DWord = 4096;
                               const YieldProc: TYieldProc = NIL) : Boolean;
	{$IFDEF SUPPORTS_OVERLOAD} overload; {$ENDIF}

function CompareVersions (const sVer1, sVer2: String) : Integer;

function ConvertWindowsStringArray (sWinArray: String) : TaString;

function CreateTempFile (const sDirectory: String;
						 const sPrefix: String = '') : String;

function DevicePathToWin32Path (const sPath: String) : String;

function DisconnectNetworkDrive (const chDriveLetter: Char;
							  const bForce: Boolean = false;
                              const bRaiseException: Boolean = false) : Boolean;

function DisconnectNetworkPath (const sUNC_Path: String;
							  const bForce: Boolean = false;
                              const bRaiseException: Boolean = false) : Boolean;

function EjectMedia (const chDrive: Char) : Boolean;

procedure Execute (const sCmd: String; const sParams: String = '';
				   const wShowWindow: Word = sw_Show;
                   const sCurrentDir: String = '');

function ExitWin (const wFlag:word) : Boolean;

function ExpandEnvironmentVars (const sStr: String) : String;

function ExtractShortPath (const sFileName: String) : String;

function FileExistsEx (const sFileName: String) : Boolean;

{$IFDEF SUPPORTS_OVERLOAD}
function FileVerToUInt64 (const sVersion: String;
{$IFDEF DELPHI2007_UP}
                          out uVersion: UInt64) : Boolean; overload;
{$ELSE}
                          out uVersion: Int64) : Boolean; overload;
{$ENDIF}

{$IFDEF DELPHI2007_UP}
function FileVerToUInt64 (const sVersion: String) : UInt64; overload;
{$ELSE}
function FileVerToUInt64 (const sVersion: String) : Int64; overload;
{$ENDIF}
{$ENDIF}

function GetAppPath (const sAppName: String) : String;

function GetBitsPerPixel : DWord;

function GetClassName (const hWindow: HWnd) : String;

function GetCurrentUserName : String;

procedure GetDriveInfo (const sDrive: String; out sName, sFileSystem: String;
					    out dwSerial: DWord;
                        out bSupportsLongFileNames: Boolean);

function GetDriveName (const sDrive: String) : String;

function GetDriveType (const chDrive: Char) : TDriveType;

function GetEnvironmentVar (const sVarName: String) : String;
{ Variablenname ohne "%...%" angeben! }

function GetExeNameFromWindowHandle (const hWindow: HWnd;
                                     var sExeName: String) : Boolean;

function GetFileSize (const sFileName: String) : Int64;

function GetLastErrorMsg (dwLastError: DWord = 0) : String;
{$IFDEF SUPPORTS_OVERLOAD}
															overload;

function GetLastErrorMsg (out sErrorMsg: String;
						  const dwLastError: DWord = 0) : Boolean; overload;
{$ENDIF}

function GetModuleName : String;

procedure GetNetworkDrive_UNC_Path (const chDrive: Char; out sUNC_Path: String);

function GetParams : String;

function GetPowerStatus (out iBatteryLife: Integer) : TPowerStatus;
{$IFDEF SUPPORTS_OVERLOAD}
    overload;

function GetPowerStatus : TPowerStatus; overload;
{$ENDIF}

function GetServicePack : Word;

function GetShellDllVer (out uMajorVer, uMinorVer: UInt) : Boolean;

function GetSystemDir : String;

function GetSystemName : String;

function GetTempDir : String;

function GetVersionInfo (sFileName: String
{$IFDEF SUPPORTS_DEFAULTPARAMS}
										   = ''
{$ENDIF}
											   ) : String;

function GetVersionInfoEx (sFileName: String
{$IFDEF SUPPORTS_DEFAULTPARAMS}
											 = ''
{$ENDIF}
												 ) : Int64;

function GetWinDir : String;

function GetWindowsVersion : TWindowsVersion;

function IniReadBool (const sIniName, sSection, sKey: String;
					  const bDefault: Boolean = false) : Boolean;
{ The values in the INI file are "0" or "1", not "True" and "False" }

function IniReadInt (const sIniName, sSection, sKey: String;
					 const iDefault: Integer = 0) : Integer;

function IniReadIntEx (const sIniName, sSection, sKey: String;
                       out iResult: Integer) : Boolean;

function IniReadStr (const sIniName, sSection, sKey: String;
					 const sDefault: String = '') : String; overload;

function IniReadStrEx (const sIniName, sSection, sKey: String;
                       var sValue: String) : Boolean;

function IniWriteBool (const sIniName, sSection, sKey: String;
                       const bValue: Boolean) : Boolean;

function IniWriteInt (const sIniName, sSection, sKey: String;
                      const iValue: Integer) : Boolean;

function IniWriteStr (const sIniName, sSection, sKey, sValue: String) : Boolean;

function InsertThousandSeparator (const iSize: Int64) : String;
{$IFDEF DELPHI7_UP}
	overload;

function InsertThousandSeparator (const uSize: UInt) : String; overload;
{$ENDIF}

function Int64HexNoToStr (const iInt64HexNo: Int64) : String;

function Int64ToStr (const iSize: Int64) : String;
{ Insert "ThousandSeparator" in the result string }

function IsAdminLoggedOn : Boolean;

function IsConsoleApplication : Boolean;
function IsDiskInDrive (const chDrive: Char) : Boolean;

function IsFileLocked (const sFileName: String) : Boolean;

function IsJunction (const FD: TWin32FindData) : Boolean;
{$IFDEF SUPPORTS_INLINE}
    {$IFNDEF DEBUG}
        inline;
    {$ENDIF DEBUG}
{$ENDIF SUPPORTS_INLINE}

function IsJunctionOrReparsePoint (const FD: TWin32FindData) : Boolean; overload;

function IsJunctionOrReparsePoint (const FD: TWin32FindData;
								   out bJunction: Boolean) : Boolean; overload;
{$IFNDEF FPC}
    {$IFDEF SUPPORTS_INLINE}
        {$IFNDEF DEBUG}
            inline;
        {$ENDIF DEBUG}
    {$ENDIF SUPPORTS_INLINE}
{$ENDIF}

function IsNetworkConfigurationOperatorLoggedOn : Boolean;

function IsPowerUserLoggedOn : Boolean;

function IsReparsePoint (const FD: TWin32FindData) : Boolean;
{$IFDEF SUPPORTS_INLINE}
    {$IFNDEF DEBUG}
        inline;
    {$ENDIF DEBUG}
{$ENDIF SUPPORTS_INLINE}

function IsRootFolder (sFolder: String) : Boolean;

function IsServicePackInstalled (const wMinimumServicePack: Word) : Boolean;

function IsUserLocalAdmin (out bElevated: Boolean) : Boolean;

function IsValidComputerName (const sName: String) : Boolean;

function IsValidFileName (const sFileName: String;
						  var sCleanedUpFileName: String;
					      const chReplace: Char = '_') : Boolean;

function IsWindows_PE : Boolean;

function IsWorkstation_OS : Boolean;

function KeyIsOn (const KeyType: TKeyType) : Boolean;

function LoadMedia (const chDrive: Char) : Boolean;

function MapNetworkDrive (const sUNC_Path: String;
                          const chDriveLetter: Char;
                          out dwResult: DWord;
						  const sUserName: String = '';
                          const sPassword: String = '';
                          const dwFlags: DWord = 0) : Boolean;
{$IFDEF SUPPORTS_OVERLOAD}
	overload;

procedure MapNetworkDrive (const sUNC_Path: String;
                           const chDriveLetter: Char = #0;
						   const sUserName: String = '';
                           const sPassword: String = '';
                           const dwFlags: DWord = 0); overload;

function MapNetworkDrive (const hWndOwner: HWnd; const sUNC_Path: String;
                          const chDriveLetter: Char;
                          out dwResult: DWord;
						  const sUserName: String = '';
                          const sPassword: String = '';
                          const dwFlags: DWord = 0) : Boolean; overload;

procedure MapNetworkDrive (const hWndOwner: HWnd; const sUNC_Path: String;
                           const chDriveLetter: Char = #0;
						   const sUserName: String = '';
                           const sPassword: String = '';
                           const dwFlags: DWord = 0); overload;

{$ENDIF}

function MakeLargeInt (const nFileSizeHigh, nFileSizeLow: DWord) : Int64;

function PathExists (const sPath: String) : Boolean;

procedure PostParameters (const hWindow: HWnd);

procedure PressShiftKey;

function ProcessesRunning (const asExeNames: array of String;
						   const bWildcards: Boolean = false) : Boolean;

function ProcessRunning (sExeName: String;
						 const bWildcards: Boolean = false) : Boolean; overload;

function ProcessRunning (sExeName: String; out dwProcessID: DWord;
						 const bWildcards: Boolean = false) : Boolean;
	overload;

function ProcessRunning (asExeNames: array of String; out iFoundPos: Integer;
						 const bWildcards: Boolean = false) : Boolean;
	overload;

function ProcessRunningCount (sExeName: String;
						      const bWildcards: Boolean = false) : Integer;

procedure RaiseLastWNetError (const dwLastError: DWord);

function RemoveCopyright (const sStr: String) : String;

function RemoveQuotationMarks (const sPath: String) : String;

function ReplaceEnvironmentVars (sPath: String) : String;

function SetEnvironmentVar (const sName, sValue: String) : Boolean;

function StrToHex (const sStr: String) : String;

function SystemUpTime : TDateTime;

procedure TurnKeyOnOff (const KeyType: TKeyType; const bOn: Boolean);

procedure Unload_SysDLL (const sDLL_Name: String);

procedure WinExecAndWait32 (const sCmdLine: String;
                            const sCurrentDirectory: String = '';
						    const iVisibility: Integer = sw_Show;
                            const bProcessWMPaint: Boolean = false);
	overload;

function WinExecAndWait32 (const sCmdLine: String; var dwExitCode: DWord;
                           const sCurrentDirectory: String = '';
						   const iVisibility: Integer = sw_Show;
                           const bProcessWMPaint: Boolean = false) : Boolean;
	overload;

function WNetGetLastErrorMsg (const dwErrorCode: DWord;
							  out sErrorMsg: String) : Boolean;

implementation

uses Messages,
{$IFDEF FPC}
	 JwaTlHelp32,
{$ELSE}
	 SysConst, TlHelp32,
  {$IFNDEF DELPHI7_UP}
	 D7_CompatibilityU, FileCtrl,
  {$ENDIF}
{$ENDIF}
	 DeviceIoControlU, PasTools, RegistryApiU, VerifyU, Win2000_ImportU,
     WinNT_ImportU, WinVista_ImportU;

{$IFNDEF FPC}
  {$IFNDEF DELPHI2009_UP}
const
	INVALID_FILE_ATTRIBUTES = DWORD ($FFFFFFFF);
  {$ENDIF}
{$ENDIF}

ResourceString
    cFileVerToUIntErrMsg = 'Unable to convert the version "%s" to "UInt64"';
    cLastErrorMsg = '"%s" failed: %s';

(* ---- *)

function AddPercentSign (const sStr: String) : String;
begin
    if (sStr [1] <> '%') then
        Result := '%' + sStr + '%'
    else Result := sStr;
end; { AddPercentSign }

(* ---- *)

function CreateProcess_GetCurrentDirectory (var sDirectory: String) : PChar;
begin
    if (sDirectory <> '') then
        Result := PChar (sDirectory)
    else
    begin
        sDirectory := ExtractFileDir (ParamStr (0));

        if (sDirectory [2] = ':') then
            Result := PChar (sDirectory)
        else
        begin
            Result := NIL;
            sDirectory := '';
        end; { else }
    end; { else }
end; { CreateProcess_GetCurrentDirectory }

(* ---- *)

function LoadOrEjectMedia (const chDrive: Char;
						   const bEject: Boolean) : Boolean;

var
	hDrive : THandle;
    dwReturn, dwIOControlCode : DWord;

begin
{$IFDEF UNICODE}
	Assert (CharInSet (chDrive, ['A'..'Z']));
{$ELSE}
  {$IFDEF SUPPORTS_ASSERT}
	Assert (UpCase (chDrive) in ['A'..'Z']);
  {$ENDIF}
{$ENDIF}

    Result := false;

    hDrive := CreateFile (PChar (Format ('\\.\%s:', [chDrive])), GENERIC_READ,
    					  0, NIL, OPEN_EXISTING, 0, 0);

    if (hDrive = INVALID_HANDLE_VALUE) then
    	exit;

    try
	    if (bEject) then
        	dwIOControlCode := IOCTL_STORAGE_EJECT_MEDIA
    	else dwIOControlCode := IOCTL_STORAGE_LOAD_MEDIA;

        if (DeviceIoControl (hDrive, dwIOControlCode, NIL, 0, NIL, 0, dwReturn{%H-},
        					 NIL)) then
            Result := true
        else RaiseLastOSError;

    finally
		VerifyApi (CloseHandle (hDrive));
    end; { try / finally }
end; { LoadOrEjectMedia }

(* ---- *)

function AddQuotationMarks (const sPath: String) : String;
{ Den Pfad in Anführungszeichen stellen, wenn er ein Leerzeichen enthält }

const
    cQuotationMark = '"';

begin
    if (Pos (' ', sPath) > 0) and (Pos ('"', sPath) = 0) then
        Result := cQuotationMark + sPath + cQuotationMark
    else Result := sPath;
end; { AddQuotationMarks }

(* ---- *)

(**
function CaptureConsoleOutput (const sCommand, sParameters: String) : String;

var
	dwExitCode : DWord;

begin
	CaptureConsoleOutput (sCommand, sParameters, Result, dwExitCode);
end; { CaptureConsoleOutput }
**)

(* ---- *)

(**
procedure CaptureConsoleOutput (const sCommand, sParameters: String;
							    out sOutput : String;
                                out dwExitCode: DWord); overload;
{ http://delphi.wikia.com/wiki/Capture_Console_Output_Realtime_To_Memo
  http://delphi.about.com/cs/adptips2001/a/bltip0201_2.htm }

const
	cReadBuffer = 8192;

var
    saSecurity : TSecurityAttributes;
    hRead, hWrite : THandle;
    suiStartup : TStartupInfo;
    piProcess : TProcessInformation;
    pBuffer : array [0..cReadBuffer] of AnsiChar;
    szCmdLine : array [0..512] of Char;
    dRead, dRunning : DWord;

begin
	Assert (sCommand <> '');

    Sleep (0);

	FillChar (saSecurity{%H-}, SizeOf (TSecurityAttributes), #0);
    saSecurity.nLength := SizeOf (TSecurityAttributes);
    saSecurity.bInheritHandle := True;
    saSecurity.lpSecurityDescriptor := NIL;

    Win32Check (CreatePipe (hRead{%H-}, hWrite{%H-}, @saSecurity, 0));

    try
        FillChar (suiStartup{%H-}, SizeOf (TStartupInfo), #0);
        suiStartup.cb := SizeOf (TStartupInfo);
        suiStartup.hStdInput := hRead;
        suiStartup.hStdOutput := hWrite;
        suiStartup.hStdError := hWrite;
        suiStartup.dwFlags := STARTF_USESTDHANDLES or STARTF_USESHOWWINDOW;
        suiStartup.wShowWindow := SW_HIDE;

        if (sParameters <> '') then
            StrPCopy (szCmdLine, sCommand + ' ' + sParameters)
        else StrPCopy (szCmdLine, sCommand);

        Win32Check (CreateProcess (NIL, szCmdLine, @saSecurity, @saSecurity,
        						   True, NORMAL_PRIORITY_CLASS, NIL, NIL,
                                   suiStartup, piProcess{%H-}));

        repeat
            dRunning  := WaitForSingleObject (piProcess.hProcess, 100);

            repeat
                dRead := 0;
                ReadFile (hRead, {%H-}pBuffer[0], cReadBuffer, dRead, NIL);
                pBuffer [dRead] := #0;

                OemToAnsi (pBuffer, pBuffer);
                sOutput := {%H-}sOutput + String (pBuffer);
            until (dRead < cReadBuffer);
        until (dRunning <> WAIT_TIMEOUT);

        GetExitCodeProcess (piProcess.hProcess, dwExitCode{%H-});

        VerifyApi (CloseHandle (piProcess.hProcess));
        VerifyApi (CloseHandle (piProcess.hThread));

    finally
        VerifyApi (CloseHandle (hRead));
        VerifyApi (CloseHandle (hWrite));
    end; { try / finally }
end; { CaptureConsoleOutput }
**)

(* ---- *)

{$IFDEF SUPPORTS_OVERLOAD}
procedure CaptureConsoleOutput (const sCmdLine: String;
                                const OutputProc: TOutputProc;
                                const sCurrentDirectory: String = '';
						        const dwBufSize: DWord = 4096;
                                const YieldProc: TYieldProc = NIL);

var
    dwExitCode : DWord;

begin
    if not (CaptureConsoleOutput (sCmdLine, OutputProc, dwExitCode,
                                  sCurrentDirectory, dwBufSize, YieldProc)) then
        RaiseLastWin32Error;
end; { CaptureConsoleOutput }
{$ENDIF}

(* ---- *)

function CaptureConsoleOutput (const sCmdLine: String;
                               const OutputProc: TOutputProc;
                               out dwExitCode: DWord;
                               const sCurrentDirectory: String = '';
						       const dwBufSize: DWord = 4096;
                               const YieldProc: TYieldProc = NIL) : Boolean;
// https://thundaxsoftware.blogspot.com/2012/12/capturing-console-output-with-delphi.html

var
    szAppName : array [0..512] of Char;
    pBuffer : PAnsiChar;
    SA : TSecurityAttributes;
    hRead : THandle;
    hWrite : THandle;
    SI : TStartupInfo;
    PI : TProcessInformation;
    dwRead, dwRunning, dwAvailable, dwCreateProcessError : DWORD;
    pchCurDir : PChar;
    sProgramDir : String;

begin
    Assert (sCmdLine <> '');
    Assert (Assigned (OutputProc));

    Result := false;

    SA.nLength := SizeOf (TSecurityAttributes);
    SA.bInheritHandle := true;
    SA.lpSecurityDescriptor := NIL;

    if (CreatePipe (hRead{%H-}, hWrite{%H-}, @SA, 0)) then
    begin
        dwCreateProcessError := 0;

        try
            pBuffer := NIL;

            FillChar (SI{%H-}, SizeOf (TStartupInfo), #0);

            SI.cb := SizeOf (TStartupInfo);
            SI.hStdInput := hRead;
            SI.hStdOutput := hWrite;
            SI.hStdError := hWrite;
            SI.dwFlags := STARTF_USESTDHANDLES or STARTF_USESHOWWINDOW;
            SI.wShowWindow := SW_HIDE;

            StrPCopy (szAppName{%H-}, sCmdLine);
            sProgramDir := sCurrentDirectory;
            pchCurDir := CreateProcess_GetCurrentDirectory (sProgramDir);

            if (CreateProcess (NIL, szAppName, @SA, @SA, true,
                               NORMAL_PRIORITY_CLASS, NIL, pchCurDir, SI,
                               PI{%H-})) then
                try
                    Result := true;

                    GetMem (pBuffer, dwBufSize + 1);

                    repeat
                        dwRunning := WaitForSingleObject (PI.hProcess, 100);
                        PeekNamedPipe (hRead, NIL, 0, NIL, @dwAvailable, NIL);

                        if (dwAvailable > 0) then
                            repeat

                                if (ReadFile (hRead, pBuffer^, dwBufSize,
                                              dwRead{%H-}, NIL)) and
                                   (dwRead > 0) then
                                begin
                                    {%H-}PAnsiChar ({%H-}NativeUInt (pBuffer) +
                                               dwRead)^ := #0;
                                    OemToCharA (pBuffer, pBuffer { pOutBuffer });
                                    OutputProc (String (pBuffer));
                                end; { if }
                            until (dwRead < dwBufSize);

                            if (Assigned (YieldProc)) then
                                YieldProc;
                    until (dwRunning <> WAIT_TIMEOUT);

                finally
                    if (pBuffer <> NIL) then
                        FreeMem (pBuffer);

                    VerifyApi (GetExitCodeProcess (PI.hProcess, dwExitCode{%H-}));

                    VerifyApi (CloseHandle (PI.hProcess));
                    VerifyApi (CloseHandle (PI.hThread));
                end { try / finally }
            else dwCreateProcessError := GetLastError;

        finally
            VerifyApi (CloseHandle (hRead));
            VerifyApi (CloseHandle (hWrite));

            if (dwCreateProcessError <> 0) then
                SetLastError (dwCreateProcessError);
        end; { try / finally }
    end; { if }
end; { CaptureConsoleOutput }

(* ---- *)

{$IFDEF DEBUG}
  {$IFNDEF DELPHI2007_UP}
      {$R-}
  {$ENDIF}
{$ENDIF}
function CompareVersions (const sVer1, sVer2: String) : Integer;

var
    uVer1, uVer2 : UInt64;

begin
    Assert (sVer1 <> '');
    Assert (sVer2 <> '');

    uVer1 := FileVerToUInt64 (sVer1);
    uVer2 := FileVerToUInt64 (sVer2);

    if (uVer1 > uVer2) then
        Result := 1
    else if (uVer1 = uVer2) then
        Result := 0
    else Result := (-1);
end; { CompareVersions }
{$IFDEF DEBUG}
  {$IFNDEF DELPHI2007_UP}
      {$R+}
  {$ENDIF}
{$ENDIF}

(* ---- *)

function ConvertWindowsStringArray (sWinArray: String) : TaString;

    (* ---- *)

    function GetItemsCount (const sMulti_SZ: String) : Integer;

    var
    	iOffset : Integer;

    begin
		Result := 0;

        if (sMulti_SZ <> '') then
        begin
	        iOffset := 1;
            Result := 1;

    		while (iOffset > 0) do
            begin
				iOffset := PosEx (#0, sMulti_SZ, iOffset);

                if (iOffset > 0) then
                begin
					Inc (iOffset);
                    Inc (Result);
                end; { if }
            end; { while }
        end; { if }
    end; { GetItemsCount }

    (* ---- *)

var
    iIndex, iPos : Integer;

begin { ConvertWindowsStringArray }
    SetLength (Result{%H-}, GetItemsCount (TrimRight (sWinArray{%H-})));

    if (Length (Result) > 0) then
    begin
        iIndex := 0;

        while (TrimRight (sWinArray) <> '') do  // "#0#0" am Ende
        begin
            iPos := Pos (#0, sWinArray);  // "#0" ist Trenner

{$IFDEF SUPPORTS_ASSERT}
            Assert (iPos > 0);
{$ENDIF}

            Result [iIndex] := Copy (sWinArray, 1, iPos - 1);
            Inc (iIndex);

            Delete (sWinArray{%H-}, 1, iPos);
        end; { while }
    end; { if }
end; { ConvertWindowsStringArray }

(* ---- *)

function CreateTempFile (const sDirectory: String;
						 const sPrefix: String = '') : String;
{ The function creates a unique file name in "sDirectory" and returns the name.
  Creates the TMP file with 0 bytes size.
  Only the first 3 characters of "sPrefix" get used. }

var
	pszPrefix : PChar;

begin
	SetLength (Result{%H-}, MAX_PATH);

    if (sPrefix <> '') then
    	pszPrefix := PChar (sPrefix)
    else pszPrefix := NIL;

    Win32Check (GetTempFileName (PChar (sDirectory), pszPrefix, 0,
                                 PChar (Result)) <> 0);

    SetLength (Result, lstrlen (PChar (Result)));
end; { GetTempFileName }

(* ---- *)

function DevicePathToWin32Path (const sPath: String) : String;

const
    cLen = 2 * MAX_PATH;

var
    chDrive : Char;
    sDeviceName, sShortPath : String;
    iPos, iLen : Integer;

begin
    iPos := PosEx ('\', sPath, 2);
    iPos := PosEx ('\', sPath, iPos + 1);

    Result := Copy (sPath, iPos, Length (sPath));
    sShortPath := Copy (sPath, 1, iPos - 1);

   	for chDrive := 'A' to 'Z' do
    begin
        SetLength (sDeviceName{%H-}, cLen);

        iLen := QueryDosDevice (PChar (String (chDrive) + ':'),
                                PChar (sDeviceName), cLen);

		if (iLen > 0) then
        begin
            SetLength (sDeviceName, iLen - 2); // #0#0 at the end

            if (SameText (sShortPath, sDeviceName)) then
            begin
                Result := chDrive + ':' + Result;
                exit;
            end; { if }
        end; { if }
    end; { for }

	Result := sPath;
end; { DevicePathToWin32Path }

(* ---- *)

function DisconnectNetworkDrive (const chDriveLetter: Char;
							  const bForce: Boolean = false;
                              const bRaiseException: Boolean = false) : Boolean;
begin
{$IFNDEF UNICODE}
	Assert (UpCase (chDriveLetter) in ['A'..'Z']);
{$ELSE}
  {$IFDEF SUPPORTS_ASSERT}
	Assert (CharInSet (UpCase (chDriveLetter), ['A'..'Z']));
  {$ENDIF}
{$ENDIF}

    Result := DisconnectNetworkPath (chDriveLetter + ':', bForce,
    								 bRaiseException);
end; { DisconnectNetworkDrive }

(* ---- *)

function DisconnectNetworkPath (const sUNC_Path: String;
							  const bForce: Boolean = false;
                              const bRaiseException: Boolean = false) : Boolean;

var
	dwResult : DWord;

begin
{$IFDEF SUPPORTS_ASSERT}
	Assert ((Pos ('\\', sUNC_Path) = 1) or (Pos (':', sUNC_Path) = 2));
{$ENDIF}

    dwResult := WNetCancelConnection2 (PChar (sUNC_Path), 0, bForce);

    Result := dwResult = No_Error;

    if (Result = false) and (bRaiseException) then
    	RaiseLastWNetError (dwResult);
end; { DisconnectNetworkPath }

(* ---- *)

function EjectMedia (const chDrive: Char) : Boolean;
begin
	Result := LoadOrEjectMedia (chDrive, true);
end; { EjectMedia }

(* ---- *)

procedure Execute (const sCmd: String; const sParams: String = '';
				   const wShowWindow: Word = sw_Show;
                   const sCurrentDir: String = '');

var
	StartupInfo : TStartupInfo;
    ProcessInfo : TProcessInformation;
    lpApplicationName, lpCommandLine, lpCurrentDirectory : PChar;

begin
{$IFDEF SUPPORTS_ASSERT}
	Assert (sCmd <> '');
{$ENDIF}

	lpApplicationName := PChar (sCmd);

    if (sParams <> '') then
    	lpCommandLine := PChar (sParams)
    else lpCommandLine := '';

    if (sCurrentDir <> '') then
		lpCurrentDirectory := PChar (sCurrentDir)
    else lpCurrentDirectory := NIL;

	FillChar (StartupInfo{%H-}, SizeOf (TStartupInfo), #0);
    FillChar (ProcessInfo{%H-}, SizeOf (TProcessInformation), #0);

    StartupInfo.cb := SizeOf (TStartupInfo);
    StartupInfo.dwFlags := STARTF_USESHOWWINDOW;
    StartupInfo.wShowWindow := wShowWindow;

	Win32Check (CreateProcess (lpApplicationName, lpCommandLine, NIL, NIL,
    						   false, CREATE_NEW_CONSOLE, NIL,
                               lpCurrentDirectory, StartupInfo, ProcessInfo));
end; { Execute }

(* ---- *)

function ExitWin (const wFlag: word) : Boolean;
{ Quelle: http://www.delphipraxis.net/viewtopic.php?t=56 }

var
	hToken  : THandle;
	tp      : TTokenPrivileges;
	h       : DWord;

begin
	if (Win32Platform = VER_PLATFORM_WIN32_NT) then
	begin { Windows NT }
		OpenProcessToken (GetCurrentProcess, TOKEN_ADJUST_PRIVILEGES, hToken{%H-});
		LookupPrivilegeValue (NIL, 'SeShutdownPrivilege',
							  {%H-}tp.Privileges [0].Luid);
		tp.PrivilegeCount := 1;
		tp.Privileges [0].Attributes := SE_PRIVILEGE_ENABLED;
		h := 0;
		AdjustTokenPrivileges (hToken, False, tp, 0,
							   PTokenPrivileges (NIL)^, h);
		VerifyApi (CloseHandle (hToken));
		result := ExitWindowsEx (wFlag, 0);
	end { if }
	else Result := ExitWindowsEx (wFlag, 0); { Win 9x }
end; { ExitWin }

(* ---- *)

function ExpandEnvironmentVars (const sStr: String) : String;
{ Die mit "%" umschlossene Variable kann an beliebiger Stelle in "sStr" stehen }

const
	cSize = 1024;

var
	sTemp : String;
	dwSize : DWord;

begin
{$IFDEF SUPPORTS_ASSERT}
	Assert (sStr <> '');
{$ENDIF}

	SetLength (sTemp{%H-}, cSize);

	dwSize := ExpandEnvironmentStrings (PChar (sStr), PChar (sTemp), cSize);

	if (dwSize > 0) then
    begin
		Result := Copy (sTemp, 1, dwSize - 1);

		if (Result = sStr) then
			Result := '';
    end { if }
	else Result := '';
end; { ExpandEnvironmentVars }

(* ---- *)

function ExtractShortPath (const sFileName: String) : String;
{ func to shorten the long path name to look like Win 3.1 naming conventions. }
// http://www.blueorbsoft.com/CodeTips/DelphiCodeTips2.html
// See "ExtractShortPath" in SysUtils.pas as well

var
    dwLen : DWord;

begin
{$IFDEF SUPPORTS_ASSERT}
    Assert (sFileName <> '');
{$ENDIF}

    SetLength (Result{%H-}, MAX_PATH);

	dwLen := GetShortPathName (PChar (sFileName), PChar (Result){%H-}, MAX_PATH);

    if (dwLen > 0) then
        SetLength (Result, dwLen)
    else RaiseLastWin32Error;
end; { ExtractShortPath }

(* ---- *)

function FileExistsEx (const sFileName: String) : Boolean;

var
    hFile : THandle;

begin
{$IFDEF SUPPORTS_ASSERT}
    Assert (sFileName <> '');
{$ENDIF}

    hFile := FileOpen (sFileName, 0);

    Result := hFile <> INVALID_HANDLE_VALUE;

    if (Result) then
        FileClose (hFile);
end; { FileExistsEx }

(* ---- *)

{$IFDEF SUPPORTS_OVERLOAD}
function FileVerToUInt64 (const sVersion: String;
{$IFDEF DELPHI2007_UP}
                          out uVersion: UInt64) : Boolean; overload;
{$ELSE}
                          out uVersion: Int64) : Boolean; overload;
{$ENDIF}

    (* ---- *)

    function Convert (const iLeft, iRight: Integer; out wResult: Word) : Boolean;

    var
        sValue : String;
        iResult : Integer;

    begin
        sValue := Copy (sVersion, iLeft + 1, iRight - iLeft);
        iResult := StrToIntDef (sValue, (-1));

        if (iResult <> (-1)) then
        begin
            Result := true;
            wResult := Word (iResult);
        end { if }
        else Result := false;
    end; { Convert }

    (* ---- *)

var
    iLeft, iRight, iByte : Integer;
    wValue : Word;

begin
{$IFDEF SUPPORTS_ASSERT}
    Assert (sVersion <> '');
{$ENDIF}

    Result := false;
    uVersion := 0;

    iByte := 0;
    iRight := Length (sVersion);

    iLeft := PrevPos ('.', sVersion, iRight);

    while (iLeft > 0) do
    begin
        if (Convert (iLeft, iRight, wValue)) then
            Int64Rec (uVersion).Words [iByte] := wValue
        else exit;

        iRight := iLeft - 1;
        iLeft := PrevPos ('.', sVersion, iRight);
        Inc (iByte);
    end; { while }

    if (Convert (0, iRight, wValue)) then
    begin
        Result := true;
        Int64Rec (uVersion).Words [iByte] := wValue
    end; { if }
end; { FileVerToUInt64 }

(* ---- *)

{$IFDEF DELPHI2007_UP}
function FileVerToUInt64 (const sVersion: String) : UInt64; overload;
{$ELSE}
function FileVerToUInt64 (const sVersion: String) : Int64; overload;
{$ENDIF}
begin
    if not (FileVerToUInt64 (sVersion, Result)) then
        raise EConvertError.CreateFmt (cFileVerToUIntErrMsg, [sVersion]);
end; { FileVerToUInt64 }
{$ENDIF}

(* ---- *)

function GetAppPath (const sAppName: String) : String;

const
	cKey = 'SOFTWARE\Microsoft\Windows\CurrentVersion\App Paths\';

begin
{$IFDEF SUPPORTS_ASSERT}
    Assert (sAppName <> '');
{$ENDIF}

	Result := RegReadStr (HKey_Local_Machine, cKey + sAppName, '');
end; { GetAppPath }

(* ---- *)

function GetBitsPerPixel : DWord;

var
	DesktopDC : HDC;

begin
	DesktopDC := GetDC (0);   // Device-Context des Desktops
	Result := GetDeviceCaps (DesktopDC, BITSPIXEL);
	ReleaseDC (0, DesktopDC);
end; { GetBitsPerPixel }

(* ---- *)

function GetClassName (const hWindow: HWnd) : String;

const
	cLen = 128;

begin
	SetLength (Result{%H-}, cLen);
	SetLength (Result, Windows.GetClassName (hWindow, PChar (Result), cLen));
end; { ClassName }

(* ---- *)

function GetCurrentUserName : String;
{ Gibt den Benutzernamen ohne vorgestellte Domäne zurück! }

var
	dwLen : DWord;

begin
	dwLen := MAX_PATH;

	SetLength (Result{%H-}, dwLen);

    Win32Check (GetUserName (PChar (Result), dwLen));
    SetLength (Result, dwLen - 1)
end; { GetCurrentUserName }

(* ---- *)

procedure GetDriveInfo (const sDrive: String; out sName, sFileSystem: String;
					    out dwSerial: DWord;
                        out bSupportsLongFileNames: Boolean);

var
	uMaximumComponentLength, uFileSystemFlags : UInt;

begin
    SetLength (sName{%H-}, MAX_Path);
    SetLength (sFileSystem{%H-}, MAX_Path);

    Win32Check (GetVolumeInformation (PChar (sDrive), PChar (sName), MAX_Path,
    								  PDWord (@dwSerial),
                                      uMaximumComponentLength{%H-},
                                      uFileSystemFlags{%H-},
                                      PChar (sFileSystem), MAX_Path));

    bSupportsLongFileNames := uMaximumComponentLength > 12;

    SetLength (sName, lstrlen (PChar (sName)));
    SetLength (sFileSystem, lstrlen (PChar (sFileSystem)));
end; { GetDriveInfo }

(* ---- *)

function GetDriveName (const sDrive: String) : String;

var
	sFileSystem: String;
	dwSerial : DWord;
	bSupportsLongFileNames: Boolean;

begin
	GetDriveInfo (sDrive, Result, sFileSystem, dwSerial, bSupportsLongFileNames)
end; { GetDriveName }

(* ---- *)

function GetDriveType (const chDrive: Char) : TDriveType;

var
	szDrive : array [1..4] of Char;

begin
    szDrive [1] := chDrive;
    szDrive [2] := ':';
    szDrive [3] := '\';
    szDrive [4] := #0;

	case Windows.GetDriveType (@szDrive [1]) of
(**
		0, 		  { The drive type cannot be determined }
		1 : exit; { The root directory does not exist }
**)
		DRIVE_REMOVABLE : Result := dtRemovable;
		DRIVE_FIXED : Result := dtFixed;
		DRIVE_REMOTE : Result := dtRemote;
		DRIVE_CDROM : Result := dtCDROM;
		DRIVE_RAMDISK : Result := dtRamDisk;
		else Result := dtError;
	end; { case }
end; { GetDriveType }

(* ---- *)

function GetEnvironmentVar (const sVarName: String) : String;
{ Variablenname ohne "%...%" angeben! }

var
    iLen : integer;

begin
	SetLength (Result{%H-}, MAX_PATH);

	iLen := Windows.GetEnvironmentVariable (PChar (sVarName), PChar (Result),
    										MAX_PATH);

	SetLength (Result, iLen);
end; { GetEnvironmentVar }

(* ---- *)

function GetExeNameFromWindowHandle (const hWindow: HWnd;
                                     var sExeName: String) : Boolean;
// https://stackoverflow.com/questions/2397578/how-to-get-the-executable-name-of-a-window

var
    dwProcessId, dwLen : DWord;
    hProcess : THandle;

begin
    Result := false;

    if (GetWindowThreadProcessId (hWindow, dwProcessId{%H-}) <> 0) then
    begin
        hProcess := OpenProcess (PROCESS_QUERY_INFORMATION or PROCESS_VM_READ,
                                 false, dwProcessId);

        if (hProcess <> 0) then
        begin
            SetLength (sExeName, MAX_PATH);

            if (Win32MajorVersion >= 6) then
            begin
                dwLen := MAX_PATH;

                if not (QueryFullProcessImageName (hProcess, 0,
                                                   PChar (sExeName),
                                                   dwLen)) then
                    dwLen := 0;
            end { if }
            else if (Win32Platform = VER_PLATFORM_WIN32_WINDOWS) then
                dwLen := GetModuleFileName (hProcess, PChar (sExeName), MAX_PATH)
            else dwLen := 0;

            if (dwLen > 0) then
            begin
                Result := true;
                SetLength (sExeName, dwLen);
            end; { if }

            CloseHandle (hProcess);
        end; { if }
    end; { if }
end; { GetWindowThreadProcessId }

(* ---- *)

function GetFileSize (const sFileName: String) : Int64;

var
	hFile : THandle;
    dwLow, dwHigh : DWord;

begin
	Result := (-1);

(**
	hFile := CreateFile (PChar (sFileName), 0, FILE_SHARE_READ, NIL,
    					 OPEN_EXISTING, FILE_ATTRIBUTE_NORMAL, 0);
**)

    hFile := FileOpen (sFileName, fmOpenRead or fmShareDenyNone);

    Win32Check (hFile <> INVALID_HANDLE_VALUE);

    try
    	dwHigh := 0;

        dwLow := Windows.GetFileSize (hFile, @dwHigh);

        if (dwLow = $FFFFFFFF) then
            Win32Check (GetLastError = No_Error);

        Int64Rec (Result).Lo := dwLow;
        Int64Rec (Result).Hi := dwHigh;

    finally
    	FileClose (hFile);
    end; { try / finally }
end; { GetFileSize }

(* ---- *)

function GetLastErrorMsg (dwLastError: DWord = 0) : String;

	(* ---- *)

    function MAKELANGID (wPrimaryLanguage: Word; wSubLanguage : Word) : Word;
    begin
    	Result := (wSubLanguage shl 10) or wPrimaryLanguage;
    end; { MAKELANGID }

	(* ---- *)

const
	cFlags = FORMAT_MESSAGE_FROM_SYSTEM or FORMAT_MESSAGE_IGNORE_INSERTS or
             FORMAT_MESSAGE_ALLOCATE_BUFFER;

var
    iLen : Integer;
    pBuffer : PChar;
    ppBuffer : Pointer;

begin { GetLastErrorMsg }
    if (dwLastError = 0) then
    	dwLastError := GetLastError;

    pBuffer := NIL;
    ppBuffer := @pBuffer;

	iLen := FormatMessage (cFlags, NIL, dwLastError,
    					   MAKELANGID (LANG_NEUTRAL, SUBLANG_DEFAULT), // Default language
    					   ppBuffer, 0, NIL);

    if (iLen > 0) then
    begin
        Result := String (pBuffer);
        VerifyApi (LocalFree ({%H-}HLOCAL (pBuffer)) = 0);
    end { if }
    else Result := '';
end; { GetLastErrorMsg }

(* ---- *)

{$IFDEF SUPPORTS_OVERLOAD}
function GetLastErrorMsg (out sErrorMsg: String;
						  const dwLastError: DWord = 0) : Boolean;
begin
	sErrorMsg := GetLastErrorMsg (dwLastError);
    Result := sErrorMsg <> '';
end; { GetLastErrorMsg }
{$ENDIF}

(* ---- *)

function GetModuleName : String;

var
	iLen : Integer;
	ModuleHandle : HModule;

begin
(**
	{ Liefert für DLLs / COM-Objekte den Namen der EXE, die sie geladen hat! }
	sExeName := ExtractFileName (ParamStr (0));

	Assert (sExeName <> '');

	ModuleHandle := GetModuleHandle (PChar (sExeName));

	if (ModuleHandle = 0) then
		ModuleHandle := MainInstance;
**)

	if (ModuleIsLib = false) and (ModuleIsPackage = false) then
    	ModuleHandle := MainInstance
    else ModuleHandle := hInstance;

    Assert (ModuleHandle <> 0);

	SetLength (Result{%H-}, MAX_PATH);

	iLen := GetModuleFileName (ModuleHandle, PChar (Result), MAX_PATH);

    Win32Check (iLen > 0);

	SetLength (Result, iLen);
end; { GetModuleName }

(* ---- *)

procedure GetNetworkDrive_UNC_Path (const chDrive: Char; out sUNC_Path: String);

var
    dwLen, dwResult : DWord;
    sDrive : String;

begin
	sDrive := chDrive + ':';

    dwLen := MAX_Path;
	SetLength (sUNC_Path{%H-}, dwLen);

    dwResult := WNetGetConnection (PChar (sDrive), PChar (sUNC_Path), dwLen);

    if (dwResult = No_Error) then
    	SetLength (sUNC_Path, lstrlen (PChar (sUNC_Path)))
    else RaiseLastWNetError (dwResult);
end; { GetNetworkDrive_UNC_Path }

(* ---- *)

function GetParams : String;

var
    iPos, iStartPos, iIndex : Integer;

begin
(**
    // Does not work properly if a parameter equals ""
    if (ParamCount = 0) then
    begin
        Result := '';
        exit;
    end; { if }
**)

    Result := GetCommandLine;

    // Remove the path to the program's EXE file from the command line
    if (Result [1] = '"') then
        iPos := NextPos ('"', Result, 2)
    else iPos := 1;

    iPos := NextPos (' ', Result, iPos);

    if (iPos > 0) then
    begin
        if (Length (Result) > iPos) then
        begin
            iStartPos := Succ (iPos);

            for iIndex := iStartPos to Length (Result) do
                if (Result [iIndex] = ' ') then
                    iPos := iIndex
                else Break;
        end; { if }

        Delete (Result, 1, iPos);
    end { if }
    else Result := '';
end; { GetParams }

(* ---- *)

function GetPowerStatus (out iBatteryLife: Integer) : TPowerStatus;

var
	SPS : TSystemPowerStatus;

begin
    Result := psUndefined;

    if (@GetSystemPowerStatus = NIL) then
        exit;

	FillChar (SPS{%H-}, SizeOf (TSystemPowerStatus), #0);

	if not (GetSystemPowerStatus (SPS)) then
    	RaiseLastOSError;

    if (SPS.BatteryFlag = 128) then
    begin
        Result := psNoBattery;
    	exit;
    end; { if }

    iBatteryLife := SPS.BatteryLifePercent;

    if (SPS.ACLineStatus = 0) then
    	Result := psDischarging
    else if ((SPS.BatteryFlag and 8) <> 0) then
    	Result := psCharging
    else if (SPS.ACLineStatus = 1) then
        Result := psNotCharging;
end; { GetPowerStatus }

(* ---- *)

{$IFDEF SUPPORTS_OVERLOAD}
function GetPowerStatus : TPowerStatus;

var
    iBatteryLife : Integer;

begin
    Result := GetPowerStatus (iBatteryLife);
end; { GetPowerStatus }
{$ENDIF}

(* ---- *)

function GetServicePack : Word;

var
	sCSDVer : String;
    iLen : Integer;
    OsVersionInfoEx : TOsVersionInfoEx;
    pOsVersionInfo : Windows.POSVersionInfo;

begin
	Result := 0;

    FillChar (OsVersionInfoEx{%H-}, SizeOf (TOsVersionInfoEx), #0);

    OsVersionInfoEx.dwOSVersionInfoSize := SizeOf (TOsVersionInfoEx);

    pOsVersionInfo := Windows.POSVersionInfo (@OsVersionInfoEx);

    if (GetVersionEx (pOsVersionInfo^)) then
		Result := OsVersionInfoEx.wServicePackMajor
    else
    begin
        sCSDVer := Trim (Win32CSDVersion);

        iLen := Length (sCSDVer);

        if (iLen > 0) then
            Result := StrToIntDef (sCSDVer [iLen], 0);
    end; { else }
end; { GetServicePack }

(* ---- *)

function GetShellDllVer (out uMajorVer, uMinorVer: UInt) : Boolean;

const
	cDll = 'shell32.dll';

var
	sFileName : String;

begin
	Result := false;

	sFileName := GetSystemDir + '\' + cDll;

	if (FileExists (sFileName)) then
    begin
    	Result := true;

    	uMajorVer := GetFileVersion (sFileName);

        uMinorVer := LoWord (uMajorVer);
        uMajorVer := HiWord (uMajorVer);
    end; { if }
end; { GetShellDllVer }

(* ---- *)

function GetSystemDir : String;
{ Liefert das Systemverzeichnis ohne abschließendes "\" zurück }

var
    iLen : Integer;

begin
	SetLength (Result{%H-}, MAX_PATH);

    iLen := GetSystemDirectory (PChar (Result), MAX_PATH);

    Win32Check (iLen > 0);
    SetLength (Result, iLen)
end; { GetSystemDir }

(* ---- *)

function GetSystemName : String;

var
    dwSize : DWord;

begin
	dwSize := MAX_COMPUTERNAME_LENGTH;
	SetLength (Result{%H-}, dwSize);
    Inc (dwSize);

    Win32Check (GetComputerName (PChar (Result), dwSize));

    SetLength (Result, dwSize)
end; { GetSystemName }

(* ---- *)

function GetTempDir : String;
{ Path includes a trailing backslash }

var
	iLen : Integer;

begin
	SetLength (Result{%H-}, MAX_PATH);

    iLen := GetTempPath (MAX_PATH, PChar (Result));

    if (iLen = 0) then
    	RaiseLastOSError
    else SetLength (Result, iLen);
end; { GetTempDir }

(* ---- *)

function GetVersionInfo (sFileName: String) : String;
{ Gibt die Versionsnummer der Datei "sFileName" als String zurück }

var
	iVersion : Int64;

begin
	iVersion := GetVersionInfoEx (sFileName);

    if (iVersion <> 0) then
        Result := IntToStr (HiWord (_LARGE_INTEGER (iVersion).HighPart)) + '.' +
                  IntToStr (LoWord (_LARGE_INTEGER (iVersion).HighPart)) + '.' +
                  IntToStr (HiWord (_LARGE_INTEGER (iVersion).LowPart)) + '.' +
                  IntToStr (LoWord (_LARGE_INTEGER (iVersion).LowPart))
    else Result := '';
end; { GetVersionInfo }

(* ---- *)

function GetVersionInfoEx (sFileName: String) : Int64;

var
    iSize : integer;
    Handle : DWord;
    pBuf : pointer;
    puLen : UInt;
    pFixedFileInfo : PVSFixedFileInfo;

begin
    Result := 0;

    if (sFileName = '') then
    	sFileName := ParamStr (0);

    iSize := GetFileVersionInfoSize (PChar (sFileName), Handle{%H-});

	if (iSize = 0) then
		exit;

    GetMem (pBuf, iSize);

    try
        if (GetFileVersionInfo (PChar (sFileName), Handle, iSize, pBuf)) then
            if (VerQueryValue (pBuf, '\', Pointer ({%H-}pFixedFileInfo), puLen{%H-})) then
            begin
                with pFixedFileInfo^ do
                begin
					_LARGE_INTEGER (Result).HighPart := dwFileVersionMS;
                    _LARGE_INTEGER (Result).LowPart := dwFileVersionLS;
				end; { with }
			end; { if }

	finally
		FreeMem (pBuf, iSize);
	end; { try / finally }
end; { GetVersionInfoEx }

(* ---- *)

function GetWinDir : String;
{ Does not include a trailing backslash! }
begin
	SetLength (Result{%H-}, MAX_PATH);
	SetLength (Result, GetWindowsDirectory (PChar (Result), MAX_PATH));
end; { GetWinDir }

(* ---- *)

function GetWindowsVersion : TWindowsVersion;
begin
	case Win32Platform of
		VER_PLATFORM_WIN32s : Result := wvWin32s;
		VER_PLATFORM_WIN32_WINDOWS : Result := wvWin9x;
		VER_PLATFORM_WIN32_NT : Result := wvWinNT;
		else Result := wvError;
	end; { case }
end; { GetWindowsVersion }

(* ---- *)

function IniReadBool (const sIniName, sSection, sKey: String;
					  const bDefault: Boolean = false) : Boolean;
{ The values in the INI file are "0" or "1", not "True" and "False" }

var
    sResult : String;
    iResult : Integer;

begin
    sResult := IniReadStr (sIniName, sSection, sKey);

    if (sResult <> '') then
    begin
        if (Str2Int (sResult, iResult{%H-})) then
        begin
            if (iResult = 0) or (iResult = 1) then
                Result := Boolean (iResult)
            else Result := bDefault;
        end { if }
        else Result := bDefault;
    end { if }
    else Result := bDefault;
end; { IniReadBool }

(* ---- *)

function IniReadInt (const sIniName, sSection, sKey: String;
					 const iDefault: Integer = 0) : Integer;

{ Zeichenkette aus einer INI-Datei lesen.
  -> sIniName : Name und Pfad der INI-Datei. Bei Verwendung eines Namens ohne
				Pfad wird die Datei im Windows-Verzeichnis geschrieben;
  -> sSection : Sektion in der Datei;
  -> sKey : Name des Wertes;
  -> iDefault : Standard Rückgabewert;
  <- Result : Gelesener Wert oder 0 bei Fehler. }

begin
	Result := GetPrivateProfileInt (PChar (sSection), PChar (sKey), iDefault,
    								PChar (sIniName));
end; { IniReadInt }

(* ---- *)

function IniReadIntEx (const sIniName, sSection, sKey: String;
                       out iResult: Integer) : Boolean;

var
    sValue : String;

begin
    sValue := IniReadStr (sIniName, sSection, sKey);

    if (sValue <> '') then
        Result := Str2Int (sValue, iResult{%H-})
    else Result := false;
end; { IniReadIntEx }

(* ---- *)

function IniReadStr (const sIniName, sSection, sKey: String;
					 const sDefault: String = '') : String;
{ Zeichenkette aus einer INI-Datei lesen.
  -> sIniName : Name und Pfad der INI-Datei. Bei Verwendung eines Namens ohne
				Pfad wird die Datei aus dem Windows-Verzeichnis gelesen;
  -> sSection : Sektion in der Datei;
  -> sKey : Name des Wertes;
  -> sDefault : Standard Rückgabewert;
  <- Result : Gelesener Wert oder '' bei Fehler. }

var
	iLen : Integer;

begin
	SetLength (Result{%H-}, MAX_PATH);

	iLen := GetPrivateProfileString (PChar (sSection), PChar (sKey),
    								 PChar (TrimRight (sDefault)),
                                     PChar (Result), MAX_PATH, PChar (sIniName));

	if (iLen > 0) then
		SetLength (Result, iLen)
	else Result := sDefault;
end; { IniReadStr }

(* ---- *)

function IniReadStrEx (const sIniName, sSection, sKey: String;
                       var sValue: String) : Boolean;

begin
    sValue := IniReadStr (sIniName, sSection, sKey);
    Result := sValue <> '';
end; { IniReadStrEx }

(* ---- *)

function IniWriteBool (const sIniName, sSection, sKey: String;
                       const bValue: Boolean) : Boolean;
{ The values in the INI file are "0" or "1", not "True" and "False" }

begin
	Result := IniWriteInt (sIniName, sSection, sKey, Integer (bValue));
end; { IniWriteBool }

(* ---- *)

function IniWriteInt (const sIniName, sSection, sKey: String;
					  const iValue: Integer) : Boolean;
{ Zahl in eine INI-Datei schreiben.
  -> sIniName : Name und Pfad der INI-Datei. Bei Verwendung eines Namens ohne
				Pfad wird die Datei im Windows-Verzeichnis geschrieben;
  -> sSection : Sektion in der Datei;
  -> sKey : Name des Wertes;
  -> iValue : Wert;
  <- Result : TRUE, wenn erfolgreich; sonst FALSE. }

begin
	Result := IniWriteStr (sIniName, sSection, sKey, IntToStr (iValue));
end; { IniWriteInt }

(* ---- *)

function IniWriteStr (const sIniName, sSection, sKey, sValue: String) : Boolean;
{ Zeichenkette in eine INI-Datei schreiben.
  -> sIniName : Name und Pfad der INI-Datei. Bei Verwendung eines Namens ohne
				Pfad wird die Datei im Windows-Verzeichnis geschrieben;
  -> sSection : Sektion in der Datei;
  -> sKey : Name des Wertes;
  -> sValue : Wert;
  <- Result : TRUE, wenn erfolgreich; sonst FALSE. }

begin
	Result := WritePrivateProfileString (PChar (sSection), PChar (sKey),
										 PChar (sValue), PChar (sIniName));
end; { IniWriteStr }

(* ---- *)

function InsertThousandSeparator (const iSize: Int64) : String;

var
    iCount, iLen, iDotCount, iOffset : Integer;

begin
    Result := IntToStr (iSize);

    iLen := Length (Result);
    iDotCount := iLen div 3;
    iOffset := iLen mod 3;

    if (iOffset = 0) then
        Dec (iDotCount);

    if (iDotCount > 0) then
    begin
        if (iOffset > 0) then
            Dec (iOffset, 2)
        else Inc (iOffset, 1);

        for iCount := iDotCount downto 1 do
            Insert (
{$IFDEF FPC}
					FormatSettings.ThousandSeparator,
{$ELSE}
  {$IFDEF DELPHI_XE_UP}
                    FormatSettings.ThousandSeparator,
  {$ELSE}
                    ThousandSeparator,
  {$ENDIF}
{$ENDIF}
                    Result, iOffset + iCount * 3);
    end; { if }
end; { InsertThousandSeparator }

(* ---- *)

{$IFDEF DELPHI7_UP}
function InsertThousandSeparator (const uSize: UInt) : String;

var
    iCount, iLen, iDotCount, iOffset : Integer;

begin
    Result := IntToStr (uSize);

    iLen := Length (Result);
    iDotCount := iLen div 3;
    iOffset := iLen mod 3;

    if (iOffset = 0) then
        Dec (iDotCount);

    if (iDotCount > 0) then
    begin
        if (iOffset > 0) then
            Dec (iOffset, 2)
        else Inc (iOffset, 1);

        for iCount := iDotCount downto 1 do
            Insert (
{$IFDEF DELPHI_XE_UP}
                    FormatSettings.ThousandSeparator,
{$ELSE}
  {$IFDEF FPC}
                    FormatSettings.ThousandSeparator,
  {$ELSE}
					ThousandSeparator,
  {$ENDIF}
{$ENDIF}
                    Result, iOffset + iCount * 3);
    end; { if }
end; { InsertThousandSeparator }
{$ENDIF}

(* ---- *)

function Int64HexNoToStr (const iInt64HexNo: Int64) : String;

var
	sInt64Hex : String;
	iLen, iIndex : Integer;

begin
	Result := '';

	sInt64Hex := Format ('%x', [iInt64HexNo]);

	iLen := Length (sInt64Hex);

	iIndex := 1;

	while (iIndex < iLen) do
	begin
		Result := Result + Char (StrToInt ('$' + Copy (sInt64Hex, iIndex, 2)));
		inc (iIndex, 2);
	end; { while }
end; { Int64HexNoToStr }

(* ---- *)

function Int64ToStr (const iSize: Int64) : String;
{ Insert "ThousandSeparator" in the result string }

var
	iCount, iLen, iDotCount, iOffset : Integer;

begin
	Result := IntToStr (iSize);

	iLen := Length (Result);
	iDotCount := iLen div 3;
	iOffset := iLen mod 3;

	if (iOffset = 0) then
		dec (iDotCount);

	if (iDotCount > 0) then
	begin
		if (iOffset > 0) then
			dec (iOffset, 3);

		inc (iOffset, 1);

		for iCount := iDotCount downto 1 do
			Insert (
{$IFDEF FPC}
            		FormatSettings.ThousandSeparator,
{$ELSE}
  {$IFDEF DELPHI_XE_UP}
                    FormatSettings.ThousandSeparator,
  {$ELSE}
                    ThousandSeparator,
  {$ENDIF}
{$ENDIF}
                    Result, iOffset + iCount * 3);
	end; { if }
end; { Int64ToStr }

(* ---- *)

(**
function IsLocalAdmin : Boolean;

const
	SECURITY_NT_AUTHORITY : TSIDIdentifierAuthority =
		(Value: (0, 0, 0, 0, 0, 5));
	SECURITY_BUILTIN_DOMAIN_RID = $00000020;
	DOMAIN_ALIAS_RID_ADMINS     = $00000220;
	ENGINE_ID            = 1;
    INDEX_SERVER_ID      = 2;
    STOP_LISTS_ID        = 21;
    NEUTRAL_STOP_LIST_ID = 211;
    ENGLISH_STOP_LIST_ID = 212;
	MORPHOLOGY_ID        = 3;
    SOUNDEX_ID           = 4;
    THESAURUS_ID         = 5;
    THES_PROJ_ID         = 51;
    THES_DIC_ID          = 52;
    LOGIN_ID             = 6;
    FILTER_ID            = 7;
    THES_DIC_OFFSET      = 10000;

var
    hAccessToken : THandle;
    ptgGroups : PTokenGroups;
    dwInfoBufferSize : DWORD;
    psidAdministrators : PSID;
    x : Integer;
    bSuccess : BOOL;

begin
    Result := False;

    bSuccess := OpenThreadToken (GetCurrentThread, TOKEN_QUERY, True,
      							 hAccessToken);

    if not (bSuccess) then
    begin
    	if (GetLastError = ERROR_NO_TOKEN) then
    		  bSuccess := OpenProcessToken (GetCurrentProcess, TOKEN_QUERY,
        									hAccessToken);
    end; { if }

    if (bSuccess) then
    begin
        GetMem (ptgGroups, 1024);

        bSuccess := GetTokenInformation (hAccessToken, TokenGroups, ptgGroups,
        								 1024, dwInfoBufferSize);

        VerifyApi (CloseHandle(hAccessToken));

        if (bSuccess) then
        begin
        	AllocateAndInitializeSid (SECURITY_NT_AUTHORITY, 2,
            						  SECURITY_BUILTIN_DOMAIN_RID,
                                      DOMAIN_ALIAS_RID_ADMINS,
                                      0, 0, 0, 0, 0, 0, psidAdministrators);

            {$R-}
            for x := 0 to ptgGroups.GroupCount - 1 do
                if (EqualSid (psidAdministrators, ptgGroups.Groups[x].Sid)) then
                begin
                    Result := True;
                    Break;
                end; { if }
            {$R+}

            VerifyApi (FreeSid (psidAdministrators) = NIL);
    	end; { if }

      	FreeMem (ptgGroups);
    end; { if }
end; { IsLocalAdmin }
**)

(* ---- *)

function IsMemberOfGroup (const DomainAliasRid: DWORD) : Boolean;
{ Returns True if the logged-on user is a member of the specified local
  group. Always returns True on Windows 9x/Me. }
const
  SECURITY_NT_AUTHORITY: TSIDIdentifierAuthority =
    (Value: (0, 0, 0, 0, 0, 5));
  SECURITY_BUILTIN_DOMAIN_RID = $00000020;
  SE_GROUP_ENABLED           = $00000004;
  SE_GROUP_USE_FOR_DENY_ONLY = $00000010;
var
  Sid: PSID;
  Token: THandle;
  GroupInfoSize: DWORD;
  GroupInfo: PTokenGroups;
  I: Integer;

(**
  IsMember: BOOL;
  CheckTokenMembership: function(TokenHandle: THandle; SidToCheck: PSID;
    var IsMember: BOOL): BOOL; stdcall;
**)
begin
  if Win32Platform <> VER_PLATFORM_WIN32_NT then begin
    Result := True;
    Exit;
  end;

  Result := False;

  if not AllocateAndInitializeSid(SECURITY_NT_AUTHORITY, 2,
     SECURITY_BUILTIN_DOMAIN_RID, DomainAliasRid,
     0, 0, 0, 0, 0, 0, Sid{%H-}) then
    Exit;
  try
(**
      { Use CheckTokenMembership if available. MSDN states:
        "The CheckTokenMembership function should be used with Windows 2000 and
        later to determine whether a specified SID is present and enabled in an
        access token. This function eliminates potential misinterpretations of
        the active group membership if changes to access tokens are made in
        future releases." }
      CheckTokenMembership := NIL;
      if Lo(GetVersion) >= 5 then
        CheckTokenMembership := GetProcAddress(GetModuleHandle(advapi32),
          'CheckTokenMembership');
      if Assigned(CheckTokenMembership) then
      begin
        if CheckTokenMembership(0, Sid, IsMember) then
          Result := IsMember;

        exit;
      end;
**)
      GroupInfo := NIL;
      if not OpenThreadToken(GetCurrentThread, TOKEN_QUERY, True,
         {$IFDEF DELPHI3_UP} Token{%H-} {$ELSE} @Token{%H-} {$ENDIF}) then begin
        if GetLastError <> ERROR_NO_TOKEN then
          Exit;
        if not OpenProcessToken(GetCurrentProcess, TOKEN_QUERY,
           {$IFDEF DELPHI3_UP} Token {$ELSE} @Token {$ENDIF}) then
          Exit;
      end;
      try
        GroupInfoSize := 0;
		if (GetTokenInformation (Token, TokenGroups, NIL, 0,
								 GroupInfoSize) = false) and
			(GetLastError <> ERROR_INSUFFICIENT_BUFFER) then
		  Exit;

		GetMem(GroupInfo, GroupInfoSize);

		if not GetTokenInformation(Token, TokenGroups, GroupInfo,
		   GroupInfoSize, GroupInfoSize) then
		  Exit;

		for I := 0 to GroupInfo.GroupCount - 1 do begin
{$IFDEF DEBUG}
    {$R-}
{$ENDIF}
		  if (EqualSid (Sid, GroupInfo.Groups[I].Sid)) then
            with GroupInfo.Groups[I] do
              if ((Attributes and SE_GROUP_ENABLED) <> 0) or
                 ((Attributes and SE_GROUP_USE_FOR_DENY_ONLY) <> 0) then
              begin
                Result := True;
                Break;
              end;
{$IFDEF DEBUG}
    {$R+}
{$ENDIF}
		end;
	  finally
		FreeMem (GroupInfo);
		VerifyApi (CloseHandle(Token));
	  end;

  finally
	VerifyApi (FreeSid (Sid) = NIL);
  end;
end; { IsMemberOfGroup }

(* ---- *)

function IsAdminLoggedOn : Boolean;
{ Returns True if the logged-on user is a member of the "Administrators" local
  group. Always returns True on Windows 9x/Me. }
begin
  	Result := IsMemberOfGroup (DOMAIN_ALIAS_RID_ADMINS);
end; { IsAdminLoggedOn }

(* ---- *)

function IsConsoleApplication : Boolean;
var
    hStdout : THandle;
    dwMode : DWord;
begin
    hStdout := GetStdHandle (STD_OUTPUT_HANDLE);

    if (hStdout <> INVALID_HANDLE_VALUE) then
        Result := GetConsoleMode (hStdout, dwMode{%H-})
    else Result := false;
end; { IsConsoleApplication }

(* ---- *)

function IsDiskInDrive (const chDrive: Char) : Boolean;

var
	uOldErrorMode : UInt;
	dwMaxCompLen, dwFileSystem : DWord;

begin
{$IFDEF SUPPORTS_ASSERT}
	Assert (chDrive <> #0);
{$ENDIF}

	uOldErrorMode := SetErrorMode (SEM_FAILCRITICALERRORS);

	{ Über diese Funktion können diverse Dateisystemparameter wie FAT oder NTFS,
	  Kompression, maximale Dateinamenslänge etc. ausgelesen werden! }
	Result := GetVolumeInformation (PChar (Format ('%s:\', [chDrive])), NIL, 0,
									NIL, dwMaxCompLen{%H-}, dwFileSystem{%H-},
                                    NIL, 0);

	SetErrorMode (uOldErrorMode);
end; { IsDiskInDrive }

(* ---- *)

function IsFileLocked (const sFileName: String) : Boolean;

var
	hFile : THandle;

begin
{$IFDEF SUPPORTS_ASSERT}
	Assert (sFileName <> '');
	Assert (FileExists (sFileName));
{$ENDIF}

    hFile := FileOpen (sFileName, fmOpenReadWrite);

    if (hFile = HFILE_ERROR) then
    	Result := true
    else
    begin
    	Result := false;
    	FileClose (hFile);
    end; { else }
end; { IsFileLocked }

(* ---- *)

function IsJunction (const FD: TWin32FindData) : Boolean;
{$IFDEF SUPPORTS_INLINE}
    {$IFNDEF DEBUG}
        inline;
    {$ENDIF DEBUG}
{$ENDIF SUPPORTS_INLINE}

const  // Konstanten müssen lokal deklariert werden wg. "inline"
{$IFNDEF DELPHI2007_UP}
	FILE_ATTRIBUTE_REPARSE_POINT = $00000400;
{$ENDIF}
    IO_REPARSE_TAG_MOUNT_POINT = DWORD ($A0000003);

begin
    Result := (FD.dwFileAttributes and FILE_ATTRIBUTE_REPARSE_POINT <> 0) and
              (FD.dwReserved0 and IO_REPARSE_TAG_MOUNT_POINT = 0);
end; { IsJunction }

(* ---- *)

function IsJunctionOrReparsePoint (const FD: TWin32FindData) : Boolean;
{$IFNDEF FPC}
    {$IFDEF SUPPORTS_INLINE}
        {$IFNDEF DEBUG}
            inline;
        {$ENDIF DEBUG}
    {$ENDIF SUPPORTS_INLINE}
{$ENDIF}

var
    bJunction : Boolean;

begin
    Result := IsJunctionOrReparsePoint (FD, bJunction);
end; { IsJunctionOrReparsePoint }

(* ---- *)

function IsJunctionOrReparsePoint (const FD: TWin32FindData;
								   out bJunction: Boolean) : Boolean;
{$IFNDEF FPC}
    {$IFDEF SUPPORTS_INLINE}
        {$IFNDEF DEBUG}
            inline;
        {$ENDIF DEBUG}
    {$ENDIF SUPPORTS_INLINE}
{$ENDIF}

const  // Konstanten müssen lokal deklariert werden wg. "inline"
{$IFNDEF DELPHI2007_UP}
	FILE_ATTRIBUTE_REPARSE_POINT = $00000400;
{$ENDIF}
    IO_REPARSE_TAG_MOUNT_POINT = DWORD ($A0000003);

begin
    Result := (FD.dwFileAttributes and FILE_ATTRIBUTE_REPARSE_POINT <> 0);

    bJunction := (FD.dwReserved0 and IO_REPARSE_TAG_MOUNT_POINT = 0);
end; { IsJunctionOrReparsePoint }

(* ---- *)

function IsNetworkConfigurationOperatorLoggedOn : Boolean;
{ Returns True if the logged-on user is a member of the "Network Configuration
  Operators" local group. Always returns True on Windows 9x/Me. }
begin
  	Result := IsMemberOfGroup (DOMAIN_ALIAS_RID_NETWORK_CONFIGURATION_OPS);
end; { IsNetworkConfigurationOperatorLoggedOn }

(* ---- *)

function IsPowerUserLoggedOn : Boolean;
{ Returns True if the logged-on user is a member of the "Power Users" local
  group. Always returns True on Windows 9x/Me. }
begin
  	Result := IsMemberOfGroup (DOMAIN_ALIAS_RID_POWER_USERS);
end; { IsPowerUserLoggedOn }

(* ---- *)

function IsReparsePoint (const FD: TWin32FindData) : Boolean;
{$IFDEF SUPPORTS_INLINE}
    {$IFNDEF DEBUG}
        inline;
    {$ENDIF DEBUG}
{$ENDIF SUPPORTS_INLINE}

const  // Konstanten müssen lokal deklariert werden wg. "inline"
{$IFNDEF DELPHI2007_UP}
	FILE_ATTRIBUTE_REPARSE_POINT = $00000400;
{$ENDIF}
    IO_REPARSE_TAG_MOUNT_POINT = DWORD ($A0000003);

begin
    Result := (FD.dwFileAttributes and FILE_ATTRIBUTE_REPARSE_POINT <> 0) and
              (FD.dwReserved0 and IO_REPARSE_TAG_MOUNT_POINT <> 0);
end; { IsReparsePoint }

(* ---- *)

function IsRootFolder (sFolder: String) : Boolean;

var
	iIndex, iPos, iCount : Integer;

begin
	if not (DirectoryExists (sFolder)) then
    	RaiseLastWin32Error;

    if (Pos ('\\', sFolder) = 1) then
    begin
    	Result := true;

    	repeat
        	iPos := Pos ('\\', sFolder);

            if (iPos > 0) then
            	Delete (sFolder{%H-}, 1, 1);
        until (iPos = 0);

        iCount := 0;

    	for iIndex := 1 to Length (sFolder) do
        	if (sFolder [iIndex] = '\') then
            begin
            	Inc (iCount);
                iPos := iIndex;

                if (iCount > 3) then
                begin
                	Result := false;
                	Break;
                end; { if }
            end; { if }

            if (iCount < 4) then
                if (iCount = 3) then
                    Result := Length (sFolder) = iPos;
    end { else }
	else
    	if (Length (sFolder) > 1) and (sFolder [2] = ':') then
    		Result := Length (sFolder) <= 3  // <> C:\
        else Result := false;
end; { IsRootFolder }

(* ---- *)

function IsServicePackInstalled (const wMinimumServicePack: Word) : Boolean;
begin
	Result := GetServicePack >= wMinimumServicePack;
end; { IsServicePackInstalled }

(* ---- *)

function IsUserLocalAdmin (out bElevated: Boolean) : Boolean;
begin
    Result := IsAdminLoggedOn;

    if (Result) then
    begin
        if (Win32MajorVersion >= 6) then
            bElevated := IsUserAnAdmin
        else bElevated := true;
    end { if }
    else bElevated := false;
end; { IsUserLocalAdmin }

(* ---- *)

function IsValidComputerName (const sName: String) : Boolean;

var
	iIndex : Integer;

begin
	Result := true;

	for iIndex := 1 to Length (sName) do
{$IFDEF UNICODE}
    	if not (CharInSet (UpCase (sName [iIndex]),
        				   ['A'..'Z', '0'..'9', '-'])) then
{$ELSE}
    	if not (UpCase (sName [iIndex]) in ['A'..'Z', '0'..'9', '-']) then
{$ENDIF}
		begin
        	Result := false;
            Break;
        end; { if }
end; { IsValidComputerName }

(* ---- *)

function IsValidFileName (const sFileName: String;
						  var sCleanedUpFileName: String;
					      const chReplace: Char = '_') : Boolean;

var
	i : Integer;

begin
	Result := true;
    sCleanedUpFileName := '';

	for i := 1 to Length (sFileName) do
{$IFDEF UNICODE}
    	if (CharInSet (sFileName [i],
        			   ['\', '/', ':', '*', '?', '"', '<', '>', '|'])) then
{$ELSE}
    	if (sFileName [i] in ['\', '/', ':', '*', '?', '"', '<', '>', '|']) then
{$ENDIF}
        begin
        	Result := false;

            if (chReplace <> #0) then
            	sCleanedUpFileName := sCleanedUpFileName + chReplace;
        end { if }
        else sCleanedUpFileName := sCleanedUpFileName + sFileName [i];
end; { IsValidFileName }

(* ---- *)

function IsWindows_PE : Boolean;

const
	cKey = 'SOFTWARE\Microsoft\Windows NT\CurrentVersion';

var
	sEditionID : String;

begin
	sEditionID := RegReadStr (HKey_Local_Machine, cKey, 'EditionID');
    Result := LowerCase (sEditionID) = 'windowspe';
end; { IsWindows_PE }

(* ---- *)

function IsWorkstation_OS : Boolean;

const
	cKey = 'SYSTEM\CurrentControlSet\Control\ProductOptions';
    cValueName = 'ProductType';

begin
	if (Win32Platform = VER_PLATFORM_WIN32_NT) then
        Result := RegReadStr (HKey_Local_Machine, cKey, cValueName) = 'WinNT'
    else Result := true;  // Windows 9x
end; { IsWorkstation_OS }

(* ---- *)

function KeyIsOn (const KeyType: TKeyType) : Boolean;

var
	iVirtKey : Integer;

begin
	iVirtKey := 0;

	case KeyType of
    	ktCapsLock   : iVirtKey := vk_Capital;
        ktNumLock    : iVirtKey := vk_NumLock;
        ktScrollLock : iVirtKey := vk_Scroll;
    end; { case }

    Result := Odd (GetKeyState (iVirtKey));
end; { KeyIsOn }

(* ---- *)

function LoadMedia (const chDrive: Char) : Boolean;
begin
	Result := LoadOrEjectMedia (chDrive, false);
end; { LoadMedia }

(* ---- *)

function MapNetworkDrive (const sUNC_Path: String;
                          const chDriveLetter: Char;
                          out dwResult: DWord;
						  const sUserName: String = '';
                          const sPassword: String = '';
                          const dwFlags: DWord = 0) : Boolean;

var
	sLocalName : String;
	NetResource : TNetResource;
    pszLocalName, pszUserName, pszPassword : PChar;

begin
{$IFDEF SUPPORTS_ASSERT}
	Assert (sUNC_Path <> '');
{$ENDIF}
{$IFDEF UNICODE}
	Assert (CharInSet (UpCase (chDriveLetter), ['A'..'Z', #0]));
{$ELSE}
  {$IFDEF SUPPORTS_ASSERT}
	Assert (UpCase (chDriveLetter) in ['A'..'Z', #0]);
  {$ENDIF}
{$ENDIF}

	if (chDriveLetter <> #0) then
    begin
		sLocalName := chDriveLetter + ':';
        pszLocalName := PChar (sLocalName);
    end { if }
    else pszLocalName := NIL;

    if (sUserName <> '') then
        pszUserName := PChar (sUserName)
    else pszUserName := NIL;

    // If lpPassword is NULL, the function uses the current default password
    // associated with the user specified by the lpUserName parameter.
    pszPassword := PChar (sPassword);

	FillChar (NetResource{%H-}, SizeOf (TNetResource), #0);

	with NetResource do
	begin
		dwType := RESOURCETYPE_DISK;
		lpLocalName	 := pszLocalName;
		lpRemoteName := PChar (sUNC_Path);
	end; { with }

    dwResult := WNetAddConnection2 (NetResource, pszPassword, pszUserName,
    								dwFlags);

    Result := dwResult = No_Error;
end; { MapNetworkDrive }

(* ---- *)

{$IFDEF SUPPORTS_OVERLOAD}
procedure MapNetworkDrive (const sUNC_Path: String;
                           const chDriveLetter: Char = #0;
						   const sUserName: String = '';
                           const sPassword: String = '';
                           const dwFlags: DWord = 0);

var
    dwResult : DWord;

begin
    if not (MapNetworkDrive (sUNC_Path, chDriveLetter, dwResult, sUserName,
    						 sPassword, dwFlags)) then
    	RaiseLastWNetError (dwResult);
end; { MapNetworkDrive }

(* ---- *)

function MapNetworkDrive (const hWndOwner: HWnd; const sUNC_Path: String;
                          const chDriveLetter: Char;
                          out dwResult: DWord;
						  const sUserName: String = '';
                          const sPassword: String = '';
                          const dwFlags: DWord = 0) : Boolean;

var
	sLocalName : String;
	NetResource : TNetResource;
    pszLocalName, pszUserName, pszPassword : PChar;

begin
{$IFDEF SUPPORTS_ASSERT}
	Assert (sUNC_Path <> '');
{$ENDIF}
{$IFDEF UNICODE}
	Assert (CharInSet (UpCase (chDriveLetter), ['A'..'Z', #0]));
{$ELSE}
  {$IFDEF SUPPORTS_ASSERT}
	Assert (UpCase (chDriveLetter) in ['A'..'Z', #0]);
  {$ENDIF}
{$ENDIF}

	if (chDriveLetter <> #0) then
    begin
		sLocalName := chDriveLetter + ':';
        pszLocalName := PChar (sLocalName);
    end { if }
    else pszLocalName := NIL;

    if (sUserName <> '') then
        pszUserName := PChar (sUserName)
    else pszUserName := NIL;

    // If lpPassword is NULL, the function uses the current default password
    // associated with the user specified by the lpUserName parameter.
    pszPassword := PChar (sPassword);

	FillChar (NetResource{%H-}, SizeOf (TNetResource), #0);

	with NetResource do
	begin
		dwType := RESOURCETYPE_DISK;
		lpLocalName	 := pszLocalName;
		lpRemoteName := PChar (sUNC_Path);
	end; { with }

    dwResult := WNetAddConnection3 (hWndOwner, NetResource, pszPassword,
                                    pszUserName, dwFlags);

    Result := dwResult = No_Error;
end; { MapNetworkDrive }

(* ---- *)

procedure MapNetworkDrive (const hWndOwner: HWnd; const sUNC_Path: String;
                           const chDriveLetter: Char = #0;
						   const sUserName: String = '';
                           const sPassword: String = '';
                           const dwFlags: DWord = 0); overload;

var
    dwResult : DWord;

begin
    if not (MapNetworkDrive (hWndOwner, sUNC_Path, chDriveLetter, dwResult,
                             sUserName, sPassword, dwFlags)) then
    	RaiseLastWNetError (dwResult);
end; { MapNetworkDrive }
{$ENDIF}

(* ---- *)

function MakeLargeInt (const nFileSizeHigh, nFileSizeLow: DWord) : Int64;
begin
    Int64Rec (Result).Lo := nFileSizeLow;
    Int64Rec (Result).Hi := nFileSizeHigh;
end; { MakeLargeInt }

(* ---- *)

function PathExists (const sPath: String) : Boolean;

{ http://stackoverflow.com/questions/8233842/how-to-check-if-directory-exist-using-c-and-winapi
  GetFileAttributes() returns INVALID_FILE_ATTRIBUTES when a failure occurs. You
  have to use GetLastError() to find out what that failure actually is. If it
  returns ERROR_PATH_NOT_FOUND, ERROR_FILE_NOT_FOUND, ERROR_INVALID_NAME, or
  ERROR_BAD_NETPATH then it really does not exist. But if it returns most any
  other error, then something actually exists at the specified path but the
  attributes are simply not accessible. Remy Lebeau Dec 18 '12 at 2:10 }

var
	dwAttributes : DWord;

begin
{$IFDEF SUPPORTS_ASSERT}
	Assert (sPath <> '');
{$ENDIF}

	dwAttributes := GetFileAttributes (PChar (sPath));

    if (dwAttributes = INVALID_FILE_ATTRIBUTES) then
    	Result := false
    else Result := (dwAttributes and FILE_ATTRIBUTE_DIRECTORY) <> 0;
end; { PathExists  }

(* ---- *)

procedure PostParameters (const hWindow: HWnd);

var
	iIndex : integer;
	pCopyData : PCopyDataStruct;
	pszParam : PChar;

begin
	if (hWindow = 0) or (ParamCount = 0) then
		exit;

	for iIndex := 1 to ParamCount do
	begin
		pCopyData := MemAlloc (SizeOf (TCopyDataStruct));

		FillChar (pCopyData^, SizeOf (TCopyDataStruct), #0);

		GetMem (pszParam, Length (ParamStr (iIndex)) + 1);

		StrPCopy (pszParam, ParamStr (iIndex));

		with pCopyData^ do
		begin
			cbData := Length (ParamStr (iIndex)) + 1;
			lpData := pszParam;
		end; { with }

		SendMessage (hWindow, wm_CopyData, 0, {%H-}NativeInt (pCopyData));

		MemDispose (Pointer (pCopyData));
	end; { for }
end; { PostParameters }

(* ---- *)

procedure PressShiftKey;

var
	KBState: TKeyboardState;
    Code : Byte;

begin
	Code := VK_LSHIFT;

    GetKeyboardState (KBState{%H-});

    if (Win32Platform = VER_PLATFORM_WIN32_NT) then
    begin
    	if (Boolean (KBState [Code]) <> true) then
        begin
            keybd_event (Code, MapVirtualKey (Code, 0),
            			 KEYEVENTF_EXTENDEDKEY, 0);
            keybd_event (Code, MapVirtualKey (Code, 0),
            			 KEYEVENTF_EXTENDEDKEY or KEYEVENTF_KEYUP, 0);
	    end; { if }
    end { if }
    else
    begin
        KBState [Code] := Ord (true);
        SetKeyboardState (KBState);
    end; { else }
end; { PressShiftKey }

(* ---- *)

function ProcessesRunning (const asExeNames: array of String;
						   const bWildcards: Boolean = false) : Boolean;
{ Liefert "true" zurück, wenn einer der Prozesse läuft. Statt dem vollen
  Prozessnamen können auch Wildcards verwendet werden. }

var
	iFoundPos : Integer;

begin
	Result := ProcessRunning (asExeNames, iFoundPos, bWildcards);
end; { ProcessesRunning }

(* ---- *)

function ProcessRunning (sExeName: String;
						 const bWildcards: Boolean = false) : Boolean;
{ -> sExeName : Name der EXE-Datei ohne Pfad. Statt dem vollen Prozessnamen
				kann auch nur der Beginn angegeben werden. }

var
	dwProcessID : DWord;

begin
    Result := ProcessRunning (sExeName, dwProcessID, bWildcards);
end; { ProcessRunning }

(* ---- *)

function ProcessRunning (sExeName: String; out dwProcessID: DWord;
						 const bWildcards: Boolean = false) : Boolean;
{ -> sExeName : Name der EXE-Datei ohne Pfad. Statt dem vollen Prozessnamen
				kann auch nur der Beginn angegeben werden. }

var
	hSnapShot : THandle;
	ProcessEntry32 : TProcessEntry32;
    bFound : Boolean;

begin
    Result := false;

	hSnapShot := CreateToolhelp32Snapshot (TH32CS_SNAPPROCESS, 0);

    Win32Check (hSnapShot <> INVALID_HANDLE_VALUE);

    {%H-}sExeName := LowerCase (sExeName);

    FillChar (ProcessEntry32{%H-}, SizeOf (TProcessEntry32), #0);

    ProcessEntry32.dwSize := SizeOf (TProcessEntry32);

    if (Process32First (hSnapShot, ProcessEntry32)) then
        repeat
        	if (bWildcards) then
            	bFound := MatchString (sExeName,
                					   LowerCase (ProcessEntry32.szExeFile))
        	else bFound := Pos (sExeName,
            					LowerCase (ProcessEntry32.szExeFile)) = 1;

            if (bFound) then
            begin
                Result := true;
                dwProcessID := ProcessEntry32.th32ProcessID;

                Break;
            end; { if }
        until (Process32Next (hSnapShot, ProcessEntry32) = false);

    VerifyApi (CloseHandle (hSnapShot));
end; { ProcessRunning }

(* ---- *)

function ProcessRunning (asExeNames: array of String; out iFoundPos: Integer;
						 const bWildcards: Boolean = false) : Boolean;
{ Liefert "true" zurück, wenn einer der Prozesse läuft. Statt dem vollen
  Prozessnamen können auch Wildcards verwendet werden. "iFoundPos" entspricht
  der Position im Array, wenn ein Prozeß gefunden wurde. }


	(* ---- *)

    function MatchNames (const sProcessName: String) : Boolean;

    var
    	iIndex : Integer;
        bFound : Boolean;

    begin
    	Result := false;

	    for iIndex := 0 to High (asExeNames) do
        begin
        	if (bWildcards) then
            	bFound := MatchString (asExeNames [iIndex], sProcessName)
        	else bFound := Pos (asExeNames [iIndex], sProcessName) = 1;

            if (bFound) then
            begin
                Result := true;
                iFoundPos := iIndex;
                Break;
            end; { if }
        end; { for }
    end; { MatchNames }

    (* ---- *)

var
	hSnapShot : THandle;
	ProcessEntry32 : TProcessEntry32;
	iIndex: Integer;

begin { ProcessRunning }
	Result := false;

    for iIndex := 0 to High (asExeNames) do
    begin
{$IFDEF SUPPORTS_ASSERT}
    	Assert (asExeNames [iIndex] <> '');
{$ENDIF}
        {%H-}asExeNames [iIndex] := LowerCase (asExeNames [iIndex]);
    end; { for }

	hSnapShot := CreateToolhelp32Snapshot (TH32CS_SNAPPROCESS, 0);

    Win32Check (Integer (hSnapShot) <> (-1));

    FillChar (ProcessEntry32{%H-}, SizeOf (TProcessEntry32), #0);

    ProcessEntry32.dwSize := SizeOf (TProcessEntry32);

    if (Process32First (hSnapShot, ProcessEntry32)) then
        repeat
            if (MatchNames (LowerCase (ProcessEntry32.szExeFile))) then
            begin
                Result := true;
                Break;
            end; { if }
        until (Process32Next (hSnapShot, ProcessEntry32) = false);

    VerifyApi (CloseHandle (hSnapShot));
end; { ProcessRunning }

(* ---- *)

function ProcessRunningCount (sExeName: String;
						      const bWildcards: Boolean = false) : Integer;
{ -> sExeName : Name der EXE-Datei ohne Pfad. Statt dem vollen Prozessnamen
				kann auch nur der Beginn angegeben werden. }

var
	hSnapShot : THandle;
	ProcessEntry32 : TProcessEntry32;
    bFound : Boolean;

begin
    Result := 0;

	hSnapShot := CreateToolhelp32Snapshot (TH32CS_SNAPPROCESS, 0);

    Win32Check (hSnapShot <> INVALID_HANDLE_VALUE);

    {%H-}sExeName := LowerCase (sExeName);

    FillChar (ProcessEntry32{%H-}, SizeOf (TProcessEntry32), #0);

    ProcessEntry32.dwSize := SizeOf (TProcessEntry32);

    if (Process32First (hSnapShot, ProcessEntry32)) then
        repeat
        	if (bWildcards) then
            	bFound := MatchString (sExeName,
                					   LowerCase (ProcessEntry32.szExeFile))
        	else bFound := Pos (sExeName,
            					LowerCase (ProcessEntry32.szExeFile)) = 1;

            if (bFound) then
                Inc (Result);
        until (Process32Next (hSnapShot, ProcessEntry32) = false);

    VerifyApi (CloseHandle (hSnapShot));
end; { ProcessRunningCount }

(* ---- *)

procedure RaiseLastWNetError (const dwLastError: DWord);

var
    sErrorMsg : String;

begin
	if (WNetGetLastErrorMsg (dwLastError, sErrorMsg)) then
    	raise EOSError.Create (sErrorMsg)
    else
{$IFDEF DELPHI7_UP}
    RaiseLastOSError;
{$ELSE}
    RaiseLastWin32Error;
{$ENDIF}

end; { RaiseLastWNetError }

(* ---- *)

function RemoveCopyright (const sStr: String) : String;

const
    cCopyright : String = Char ($A9);
    cRegistered : String = Char ($AE);
{$IFDEF UNICODE}
    cTrademark : String = Char (8482);
{$ELSE}
    cTrademark : String = Char ($99);
{$ENDIF}

begin
    Result := ReplaceString (sStr, ['(R)', '(r)', '(TM)', '(tm)'], ' ');
    Result := ReplaceString (Result, [cCopyright, cRegistered, cTrademark], '');
    Result := ReplaceString (Trim (Result), '  ', ' ');
end; { RemoveCopyright }

(* ---- *)

function RemoveQuotationMarks (const sPath: String) : String;
begin
{$IFDEF SUPPORTS_ASSERT}
	Assert (sPath <> '');
{$ENDIF}

	if (sPath [1] = '"') and (sPath [Length (sPath)] = '"') then
		Result := Copy (sPath, 2, Length (sPath) - 2)
	else Result := sPath;
end; { RemoveQuotationMarks }

(* ---- *)

function ReplaceEnvironmentVars (sPath: String) : String;
{ Die Zeichenkette "%Variablenname%" durch den Wert der Variablen
  ersetzen }

var
	iFirstPos, iLastPos : Integer;
	sValue, sVar : String;

begin
{$IFDEF SUPPORTS_ASSERT}
	Assert (sPath <> '');
{$ENDIF}

    repeat
        iFirstPos := Pos ('%', sPath);

        if (iFirstPos > 0) then
        begin
            iLastPos := NextPos ('%', sPath, iFirstPos + 1);

            if (iLastPos = 0) then
                Break;

            sVar := Copy (sPath, iFirstPos + 1, iLastPos - (iFirstPos + 1));

            { Die '%'-Zeichen müssen auch gelöscht werden! }
            Delete (sPath{%H-}, iFirstPos, Length (sVar) + 2);

            sValue := GetEnvironmentVar (sVar);

            Insert (sValue, sPath, iFirstPos);
        end; { if }
    until (iFirstPos = 0);

    Result := sPath;
end; { ReplaceEnvironmentVars }

(* ---- *)

function SetEnvironmentVar (const sName, sValue: String) : Boolean;
{ Setzt die Variable "sName" auf den Wert "sValue". Ist "sValue" leer, so wird
  "sName" gelöscht. Achtung: Arbeitet nur mit dem Environment des aktuell ange-
  meldeten Anwenders. }

begin
{$IFDEF SUPPORTS_ASSERT}
	Assert (sName <> '');
{$ENDIF}

(**
    with TRegistry.Create do
        try
            RootKey := HKey_Current_User;
            LazyWrite := false;

            if (OpenKey ('Environment', true)) then
                try
                	if (sValue <> '') then
                    begin
                    	WriteString (sName, sValue);
                        Result := true;
                    end { if }
                    else Result := DeleteValue (sName);

                except
                    on ERegistryException do
                        Result := false;
                end; { try / except }

		finally
			CloseKey;
			Free;
		end; { try / finally }

**)
    if (sValue <> '') then
        Result := RegWriteStr (HKey_Current_User, 'Environment', true,
        					   sName, sValue)
    else Result := RegValueDelete (HKey_Current_User, 'Environment', sName);

	if (Result) then
		SendMessage (HWnd_Broadcast, wm_SettingChange, 0, 0);
end; { SetEnvironmentVar }

(* ---- *)

function StrToHex (const sStr: String) : String;

var
	iIndex : Integer;

begin
	Result := '';

	if (sStr = '') then
		exit;

	Result := '$';

	for iIndex := 1 to Length (sStr) do
		Result := Result + Format ('%1.x', [Word (sStr [iIndex])]);
end; { StrToHex }

(* ---- *)

function SystemUpTime : TDateTime;
// http://stackoverflow.com/questions/1645896/system-uptime-in-delphi-2009

var
    iCount, iFreq : Int64;

begin
    if (QueryPerformanceCounter (iCount{%H-})) and
       (QueryPerformanceFrequency (iFreq{%H-})) then
    begin
        iCount := iCount div iFreq;
        Result := iCount / SecsPerDay;
    end { if }
    else Result := {%H-}GetTickCount / SecsPerDay / MSecsPerSec;
end; { SystemUpTime }

(* ---- *)

(**
function SystemUpTime : String;

var
	dtUpTime : TDateTime;

begin
	dtUpTime := SystemUpTime;
    Result := Format ('%d days, %s',
                      [Trunc (dtUpTime),
                       FormatDateTime ('hh:nn:ss.z', Frac (dtUpTime))]);
end; { SystemUpTime }
**)

(* ---- *)

procedure TurnKeyOnOff (const KeyType: TKeyType; const bOn: Boolean);

var
	KBState: TKeyboardState;
	Code: Byte;

begin
	case KeyType of
        ktScrollLock: Code := VK_SCROLL;
        ktCapsLock: Code := VK_CAPITAL;
        ktNumLock: Code := VK_NUMLOCK;
        else Code := 0;
    end; { case }

    GetKeyboardState (KBState{%H-});

    if (Win32Platform = VER_PLATFORM_WIN32_NT) then
    begin
    	if (Boolean (KBState [Code]) <> bOn) then
        begin
            keybd_event (Code, MapVirtualKey (Code, 0),
            			 KEYEVENTF_EXTENDEDKEY, 0);
            keybd_event (Code, MapVirtualKey (Code, 0),
            			 KEYEVENTF_EXTENDEDKEY or KEYEVENTF_KEYUP, 0);
	    end; { if }
    end { if }
    else
    begin
        KBState [Code] := Ord (bOn);
        SetKeyboardState (KBState);
    end; { else }
end; { TurnKeyOnOff }

(* ---- *)

procedure Unload_SysDLL (const sDLL_Name: String);

var
	hLibModule : HModule;
    iLen : integer;
    sModulePath : String;

begin
    SetLength (sModulePath{%H-}, MAX_PATH);
    iLen := GetSystemDirectory (PChar (sModulePath), MAX_PATH);
    SetLength (sModulePath, iLen);

	sModulePath := sModulePath + '\' + sDLL_Name;

	hLibModule := GetModuleHandle (PChar (sModulePath));

    if (hLibModule > 0) then
		hLibModule := HModule (FreeLibrary (hLibModule) = TRUE);

    if (hLibModule = 0) then
    begin
    	MessageBox (GetDesktopWindow,
        			PChar (Format ('Error freeing "%s"', [sDLL_Name])),
        			PChar (ExtractFileName (ParamStr (0))),
                    mb_IconStop or mb_OK);
    end; { if }
end; { Unload_SysDLL }

(* ---- *)

procedure WinExecAndWait32 (const sCmdLine: String;
                            const sCurrentDirectory: String = '';
						    const iVisibility: Integer = sw_Show;
                            const bProcessWMPaint: Boolean = false);

var
	dwExitCode : DWord;

begin
	if not (WinExecAndWait32 (sCmdLine, dwExitCode{%H-}, sCurrentDirectory,
                              iVisibility, bProcessWMPaint)) then
{$IFDEF DELPHI7_UP}
		RaiseLastOSError;
{$ELSE}
		RaiseLastWin32Error;
{$ENDIF}
end; { WinExecAndWait32 }

(* ---- *)

function WinExecAndWait32 (const sCmdLine: String; var dwExitCode: DWord;
                           const sCurrentDirectory: String = '';
						   const iVisibility: Integer = sw_Show;
                           const bProcessWMPaint: Boolean = false) : Boolean;
{ http://www.swissdelphicenter.ch/torry/showcode.php?id=93 }

	(* ---- *)

    procedure WaitFor (ProcessHandle: THandle);
    { V1 by Pat Ritchey, V2 by P.Below }

    var
        Msg : TMsg;
        dwRet : DWORD;

    begin
        repeat
            dwRet := MsgWaitForMultipleObjects (
                               1,             { 1 handle to wait on }
                               ProcessHandle{%H-}, { the handle }
                               False,         { wake on any event }
                               INFINITE,      { wait without timeout }
                               QS_PAINT or    { wake on paint messages }
                               QS_SENDMESSAGE { or messages from other threads }
                               );

            if (dwRet = WAIT_FAILED) then
            	exit; { can do little here }

            if (dwRet = (WAIT_OBJECT_0 + 1)) then
                { Woke on a message, process paint messages only. Calling
                  PeekMessage gets messages send from other threads processed. }
                while (PeekMessage (Msg{%H-}, 0, WM_PAINT, WM_PAINT,
                                    PM_REMOVE)) do
                	DispatchMessage(Msg)
        until (dwRet = WAIT_OBJECT_0)
    end; { WaitFor }

    (* ---- *)

var { by Pat Ritchey }
    StartupInfo : TStartupInfo;
    ProcessInfo : TProcessInformation;
    pchCurrentDir : PChar;
    sProgramDir : String;

begin
{$IFDEF SUPPORTS_ASSERT}
	Assert (sCmdLine <> '');
    Assert (sCmdLine [1] <> ' ');
{$ENDIF}

    dwExitCode := MAXDWORD;

    FillChar (StartupInfo{%H-}, Sizeof (StartupInfo), #0);
    StartupInfo.cb := Sizeof (StartupInfo);
    StartupInfo.dwFlags := STARTF_USESHOWWINDOW;
    StartupInfo.wShowWindow := iVisibility;

    sProgramDir := sCurrentDirectory;
    pchCurrentDir := CreateProcess_GetCurrentDirectory (sProgramDir);

    if (CreateProcess (NIL,         // lpApplicationName
                       PChar (sCmdLine), // pointer to command line string
                       NIL,         // pointer to process security attributes
                       NIL,         // pointer to thread security attributes
                       false,       // handle inheritance flag
                       CREATE_NEW_CONSOLE or NORMAL_PRIORITY_CLASS, // creation flags
                       NIL,         // pointer to new environment block
                       pchCurrentDir, // pointer to current directory name
                       StartupInfo, // pointer to STARTUPINFO
                       ProcessInfo{%H-})) then  // pointer to PROCESS_INF
    begin
        Result := true;

    	if (bProcessWMPaint) then
	        WaitFor (ProcessInfo.hProcess)  // v2
        else WaitforSingleObject (ProcessInfo.hProcess, INFINITE);  // v1

        VerifyApi (GetExitCodeProcess (ProcessInfo.hProcess, dwExitCode{%H-}));
        VerifyApi (CloseHandle (ProcessInfo.hProcess));
        VerifyApi (CloseHandle (ProcessInfo.hThread));
    end { if }
    else Result := false
end; { WinExecAndWait32 }

(* ---- *)

function WNetGetLastErrorMsg (const dwErrorCode: DWord;
							  out sErrorMsg: String) : Boolean;

const
	cBufLen = 512;

var
	dwWNetResult, dwLastError : DWord;
    sDescription, sProvider : String;

begin
	if (dwErrorCode <> ERROR_EXTENDED_ERROR) then
    begin
    	sErrorMsg := GetLastErrorMsg (dwErrorCode);
        Result := sErrorMsg <> '';
    end { if }
    else
    begin
    	SetLength (sDescription{%H-}, cBufLen);
        SetLength (sProvider{%H-}, cBufLen);

        dwWNetResult := WNetGetLastError (dwLastError{%H-}, PChar (sDescription),
                                          cBufLen, PChar (sProvider), cBufLen);

        Result := dwWNetResult = NO_ERROR;

        if (Result) then
        begin
            SetLength (sDescription, lstrlen (PChar (sDescription)));
            SetLength (sProvider, lstrlen (PChar (sProvider)));

            sErrorMsg := Format (cLastErrorMsg, [sProvider, sDescription]);
        end; { if }
    end; { else }
end; { WNetGetLastErrorMsg }

(* ---- *)

end.
