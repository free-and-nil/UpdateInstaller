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

{$IFDEF DELPHI7_UP}
	{$WARN SYMBOL_PLATFORM OFF}
	{$WARN SYMBOL_DEPRECATED OFF}
{$ENDIF}

unit Delphi32ToolsU;

interface

uses SysUtils, Windows, Classes;

const
	cAppPathsKey = '\SOFTWARE\Microsoft\Windows\CurrentVersion\App Paths\';

function CloseApp (sAppName: String) : Integer;

function CopyFiles (const sPathAndSearchMask: String;
                    sTargetDir: String;
                    const bOverwriteExisting: Boolean = false) : Integer;

function FillProcessList (const ProcessList: TStrings;
						  const bLowerCase: Boolean = true) : Boolean;

procedure FillScreenResList (const ScreenResList: TStrings;
						     const uMinWidth: UInt = 0;
                             const uMinHeight: UInt = 0;
                             const uMinBitsPerPel: UInt = 0;
                             const uMinHz: Uint = 0);

function GetAppPath (const sApplication: String; out sPath: String) : Boolean;

procedure GetMappedDrives (const MappedDrivesList: TStrings);

function GetParentProcessExeName (out sParentExeName: String) : Boolean;

function GetProgramFilesDir : TFileName;

function GetShellPath (const sExt: String; out sPath: String) : Boolean;

function GetTopLevelWindowNames (const WindowNamesList: TStrings;
							 const bShowWindowClass: Boolean = false) : Boolean;

function GetWindowClassNames (const ClassNameList: TStrings) : Boolean;

function LastPosRange (const achSearch: array of Char;
				       const sStr: String) : Integer;

function PosRange (const achSearch: array of Char;
				   const sStr: String) : Integer;

function RegisterApplication (sInitialDir: TFileName;
							  const sExtension: String = '') : Boolean;

function RegisterExtension (const sExtension: String;
							const iIconIndex: Integer) : Boolean;

function RemoveChars (const sStr: String;
					  const achChars: array of Char) : String;

implementation

uses Messages, Registry,
{$IFDEF FPC}
     Jwatlhelp32,
{$ELSE}
     TlHelp32,
{$ENDIF}
	 PasTools, Win32ToolsU, VerifyU, Delphi_T, RegistryApiU;

ResourceString
	cDevModeMsg = '%dx%d %d bit color depth %d Hz';

(* ---- *)

function CloseApp (sAppName: String) : Integer;

var
    WindowList, ProcessList : TStringList;
    iWindow, iProcess : Integer;
    hWindow : HWnd;
    dwWndProcess_ID, dwProcess_ID : DWord;

begin
    Assert (sAppName <> '');

    Result := 0;

    sAppName := LowerCase (sAppName);

    WindowList := TStringList.Create;
    ProcessList := TStringList.Create;

    try
        GetTopLevelWindowNames (WindowList);
        FillProcessList (ProcessList);

        with ProcessList do
            if (Count > 0) then
                for iProcess := Count - 1 downto 0 do
                    if (Strings [iProcess] <> sAppName) then
                        Delete (iProcess);

        if (ProcessList.Count > 0) then
            for iWindow := 0 to WindowList.Count - 1 do
            begin
                hWindow := HWnd (WindowList.Objects [iWindow]);

                GetWindowThreadProcessId (hWindow, @dwWndProcess_ID);

                with ProcessList do
                    if (Count > 0) then
                        for iProcess := Count - 1 downto 0 do
                        begin
                            dwProcess_ID := DWord (Objects [iProcess]);

                            if (dwWndProcess_ID = dwProcess_ID) then
                            begin
                                inc (Result);
                                PostMessage (hWindow, wm_Close, 0, 0);
                                Delete (iProcess);
                                Break;
                            end; { if }
                        end { for }
                    else Break;
            end; { for }

    finally
        WindowList.Free;
        ProcessList.Free;
    end; { try / finally }
end; { CloseApp }

(* ---- *)

function EnumWindowsFunc (Handle: THandle; List: TStrings) : Boolean; stdcall;

var
	Caption : array [0..511] of Char;

begin
	Assert (Assigned (List));
	Assert (List is TStrings);

    if (GetWindowText (Handle, Caption, SizeOf (Caption) - 1) <> 0) then
    	List.AddObject (Caption, TObject (Handle))
    else List.AddObject ('', TObject (Handle));

    Result := True;
end; { EnumWindowsFunc }

(* ---- *)

function CopyFiles (const sPathAndSearchMask: String;
                    sTargetDir: String;
                    const bOverwriteExisting: Boolean = false) : Integer;

var
    FileList : TStringList;
    iIndex : Integer;

begin
    Assert (sPathAndSearchMask <> '');
    Assert (sTargetDir <> '');

    Win32Check (DirectoryExists (sTargetDir));

    FileList := TStringList.Create;

    try
        FillFileList (FileList, sPathAndSearchMask);

        Result := 0;

        if not (sTargetDir [Length (sTargetDir)] = '\') then
            sTargetDir := sTargetDir + '\';

        for iIndex := 0 to FileList.Count - 1 do
            if (CopyFile (PChar (FileList [iIndex]),
                          PChar (sTargetDir +
                                 ExtractFileName (FileList [iIndex])),
                          not bOverwriteExisting)) then
                Inc (Result)
            else RaiseLastWin32Error;

    finally
        FileList.Free;
    end; { try / finally }
end; { CopyFiles }

(* ---- *)

function FillProcessList (const ProcessList: TStrings;
						  const bLowerCase: Boolean = true) : Boolean;
{ Alle EXE-Namen in Kleinbuchstaben! Im "Object"-Feld eines Eintrags steht
  die Prozeß-ID, nicht das Handle! }

var
	hSnapShot : THandle;
	ProcessEntry32 : TProcessEntry32;

begin
	Assert (Assigned (ProcessList));
	Assert (ProcessList is TStrings);

	Result := false;

    ProcessList.Clear;

	hSnapShot := CreateToolhelp32Snapshot (TH32CS_SNAPPROCESS, 0);

	if (hSnapShot <> INVALID_HANDLE_VALUE) then
	begin
		FillChar (ProcessEntry32{%H-}, SizeOf (TProcessEntry32), #0);

		ProcessEntry32.dwSize := SizeOf (TProcessEntry32);

		if (Process32First (hSnapShot, ProcessEntry32)) then
			repeat
            	with ProcessEntry32 do
                	if (bLowerCase) then
	            		ProcessList.AddObject (LowerCase (TrimRight (szExeFile)),
    	                					   TObject (th32ProcessID))
                    else ProcessList.AddObject (TrimRight (szExeFile),
    	                					    TObject (th32ProcessID))
            until (Process32Next (hSnapShot, ProcessEntry32) = false);

		VerifyApi (CloseHandle (hSnapShot));
    end; { if }
end; { FillProcessList }

(* ---- *)

procedure FillScreenResList (const ScreenResList: TStrings;
						     const uMinWidth: UInt = 0;
                             const uMinHeight: UInt = 0;
                             const uMinBitsPerPel: UInt = 0;
                             const uMinHz: Uint = 0);

var
    iIndex, iWidthHeight : Integer;
    DevMode : TDeviceMode;

begin
	Assert (Assigned (ScreenResList));
	Assert (ScreenResList is TStrings);

    iIndex := 0;

    FillChar (DevMode{%H-}, SizeOf (TDeviceMode), #0);
    DevMode.dmSize := SizeOf (TDeviceMode);

    if (EnumDisplaySettings (NIL, iIndex, DevMode)) then
    	repeat
            with DevMode do
                if (dmPelsWidth >= uMinWidth) and
                   (dmPelsHeight >= uMinHeight) and
                   (dmBitsPerPel >= uMinBitsPerPel) and
                   (dmDisplayFrequency >= uMinHz) then
                begin
                    LongRec (iWidthHeight).Lo := dmPelsWidth;
                    LongRec (iWidthHeight).Hi := dmPelsHeight;
                    ScreenResList.AddObject (Format (cDevModeMsg,
                                                     [dmPelsWidth,
                                                      dmPelsHeight,
                                                      dmBitsperPel,
                                                      dmDisplayFrequency]),
                                             TObject (iWidthHeight));
                end; { if }

            Inc (iIndex);
        until (EnumDisplaySettings (NIL, iIndex, DevMode) = false)
    else RaiseLastWin32Error
end; { FillScreenResList }

(* ---- *)

function GetAppPath (const sApplication: String; out sPath: String) : Boolean;

begin
	Assert (sApplication <> '');

    Result := false;

	with TRegistry.Create do
    	try
        	RootKey := HKey_Local_Machine;

            if (OpenKeyReadOnly (cAppPathsKey + sApplication)) then
            	try
                	sPath := ReadString ('');
                    Result := sPath <> '';

                finally
                	CloseKey;
                end; { try / finally }

        finally
        	Free;
        end; { try / finally }
end; { MyFunction }

(* ---- *)

procedure GetMappedDrives (const MappedDrivesList: TStrings);

type
	PTanrDriveMappings = ^TanrDriveMappings;
    TanrDriveMappings = array [1..26] of TNetResource;

var
    dwResult, dwEntries, dwBufSize : DWord;
    hEnum : THandle;
    panrDriveMappings : PTanrDriveMappings;
    iIndex : Integer;
    chDrive : Char;
    sPath : String;

begin { GetMappedDrives }
	Assert (Assigned (MappedDrivesList));
	Assert (MappedDrivesList is TStrings);

    dwResult := WNetOpenEnum (RESOURCE_CONNECTED, RESOURCETYPE_ANY, 0, NIL,
                              hEnum{%H-});

    if (dwResult <> NO_ERROR) then
    	RaiseLastWNetError (dwResult);

    try
    	MappedDrivesList.Clear;
        dwEntries := $FFFFFFFF;

        repeat
            dwBufSize := SizeOf (TanrDriveMappings);
            panrDriveMappings := MemAlloc (dwBufSize);

            try
                dwResult := WNetEnumResource (hEnum, dwEntries,
                							  panrDriveMappings, dwBufSize);

                if (dwResult = NO_ERROR) then
                begin
                    for iIndex := 1 to dwEntries do
                    begin
                        if (panrDriveMappings^[iIndex].lpLocalName <> NIL) then
                            chDrive := panrDriveMappings^[iIndex].lpLocalName^
                        else chDrive := #0;

                        sPath := panrDriveMappings^[iIndex].lpRemoteName;

                        MappedDrivesList.AddObject (sPath,
                                                    TObject (NativeUInt (chDrive)));
                    end; { for }
                end { if }
                else if (dwResult <> ERROR_NO_MORE_ITEMS) then
                    RaiseLastWNetError (dwResult);

            finally
                MemDispose (Pointer (panrDriveMappings));
            end; { try / finally }
        until (dwResult = ERROR_NO_MORE_ITEMS);

    finally
    	WNetCloseEnum (hEnum);
    end; { try / finally }
end; { GetMappedDrives }

(* ---- *)

function GetParentProcessExeName (out sParentExeName: String) : Boolean;

var
	ProcessList : TStringList;

	(* ---- *)

	function GetParentProcessName (const hParentProcessID: THandle) : Boolean;

	var
		iIndex : Integer;

	begin
		Result := false;

		with ProcessList do
			for iIndex := 0 to Count - 1 do
				if (THandle (Objects [iIndex]) = hParentProcessID) then
				begin
					Result := true;
					sParentExeName := Strings [iIndex];
					exit;
				end; { if }
	end; { GetParentProcessName }

	(* ---- *)

var
	hParentProcess, hSnapShot : THandle;
	ProcessEntry32 : TProcessEntry32;
	sExeFile, sAppExeName : String;

begin { GetParentProcessExeName }
	Result := false;

	ProcessList := TStringList.Create;

	try
		hSnapShot := CreateToolhelp32Snapshot (TH32CS_SNAPPROCESS, 0);

		if (Integer (hSnapShot) <> (-1)) then
		begin
			sAppExeName := LowerCase (ExtractFileName (ParamStr (0)));
			hParentProcess := 0;

			FillChar (ProcessEntry32{%H-}, SizeOf (TProcessEntry32), #0);

			ProcessEntry32.dwSize := SizeOf (TProcessEntry32);

			if (Process32First (hSnapShot, ProcessEntry32)) then
			begin
				repeat
					with ProcessEntry32 do
					begin
						sExeFile := LowerCase (szExeFile);

						if (Pos (sAppExeName, sExeFile) = 1) then
							hParentProcess := th32ParentProcessID;

						ProcessList.AddObject (sExeFile,
											   TObject (th32ProcessID));
					end; { with }
				until (Process32Next (hSnapShot, ProcessEntry32) = false);

				ProcessList.Sort;
			end; { if }

			VerifyApi (CloseHandle (hSnapShot));

			if (hParentProcess <> 0) then
				Result := GetParentProcessName (hParentProcess);
		end { if }

	finally
		ProcessList.Free;
	end; { try / finally }
end; { GetParentProcessExeName }

(* ---- *)

function GetProgramFilesDir : TFileName;
{ <<- Pfad zum Verzeichnis "Program Files" bzw. "Programme" ohne Backslash am
	  Ende. }

begin
	Result := RegReadStr (HKey_Local_Machine,
						  'SOFTWARE\Microsoft\Windows\CurrentVersion',
						  'ProgramFilesDir');
end; { GetProgramFilesDir }

(* ---- *)

function GetShellPath (const sExt: String; out sPath: String) : Boolean;

	(* ---- *)

    function GetDefaultStr (const sKey: String; var sDefault: String) : Boolean;
    begin
    	Result := false;

        with TRegistry.Create do
            try
                RootKey := HKey_Classes_Root;

                if (OpenKeyReadOnly (sKey)) then
                begin
                    sDefault := ReadString ('');
                    CloseKey;

                    Result := sDefault <> '';
                end; { if }

            finally
                Free;
            end; { try / finally }
    end; { GetDefaultStr }

    (* ---- *)

var
	sDefault, sCurVer : String;

begin { GetShellPath }
	Assert (Pos ('.', sExt) = 1);

    Result := false;

    if (GetDefaultStr (sExt, sDefault{%H-})) then
    begin
    	Result := true;

    	while (GetDefaultStr (sDefault + '\CurVer', sCurVer{%H-})) do
        	sDefault := sCurVer;

        sPath := sDefault;
    end; { if }
end; { GetShellPath }

(* ---- *)

function GetTopLevelWindowNames (const WindowNamesList: TStrings;
							 const bShowWindowClass: Boolean = false) : Boolean;

var
	iIndex : Integer;
    hWindow : HWnd;

begin
	Assert (Assigned (WindowNamesList));
	Assert (WindowNamesList is TStrings);

	WindowNamesList.Clear;

	Result := EnumWindows (@EnumWindowsFunc, LParam (WindowNamesList));

    if (bShowWindowClass) then
    	for iIndex := 0 to WindowNamesList.Count - 1 do
          	if (WindowNamesList [iIndex] = '') then
            begin
            	hWindow := HWnd (WindowNamesList.Objects [iIndex]);
            	WindowNamesList [iIndex] := Format ('[class: %s]',
                                                    [GetClassName (hWindow)]);
            end; { if }
end; { GetTopLevelWindowNames }

(* ---- *)

function GetWindowClassNames (const ClassNameList: TStrings) : Boolean;

var
	WindowNamesList : TStringList;
    iIndex : Integer;
    szClassName : array [0..MAX_PATH] of Char;

begin
	Assert (Assigned (ClassNameList));
	Assert (ClassNameList is TStrings);

	ClassNameList.Clear;

    WindowNamesList := TStringList.Create;

    try
		Result := EnumWindows (@EnumWindowsFunc, LParam (WindowNamesList));

        for iIndex := 0 to WindowNamesList.Count - 1 do
        	if (Windows.GetClassName (THandle (WindowNamesList.Objects [iIndex]),
            						  szClassName, MAX_PATH) > 0) then
            	ClassNameList.AddObject (szClassName,
                						 WindowNamesList.Objects [iIndex]);

    finally
    	WindowNamesList.Free;
    end; { try / finally }
end; { GetWindowClassNames }

(* ---- *)

function LastPosRange (const achSearch: array of Char;
				       const sStr: String) : Integer;

var
	iIndex, iLastPos : Integer;

begin
	Result := 0;

    for iIndex := 0 to High (achSearch) do
    begin
    	iLastPos := LastPos (achSearch [iIndex], sStr);

    	if (iLastPos > 0) then
            if (iLastPos > Result) then
            	Result := iLastPos;
    end; { for }
end; { LastPosRange }

(* ---- *)

function PosRange (const achSearch: array of Char;
				   const sStr: String) : Integer;

var
	iIndex, iPos : Integer;

begin
	Result := 0;

    for iIndex := 0 to High (achSearch) do
    begin
    	iPos := Pos (achSearch [iIndex], sStr);

    	if (iPos > 0) then
        	if (Result = 0) then
            	Result := iPos
            else if (iPos < Result) then
            	Result := iPos;
    end; { for }
end; { PosPlus }

(* ---- *)

function RegisterApplication (sInitialDir: TFileName;
                              const sExtension: String = '') : Boolean;
{ Den Pfad zur Anwendungsdatei in der Registry eintragen, so daß sie von der
  Kommandozeile aus mit "start [Programmname]" aufgerufen werden kann.
  Zusätzlich die Dateierweiterung mit der Anwendung verknüpfen.
  ->> sInitialDir : Startverzeichnis des Programms. Wenn "sInitialDir = '' dann
                    das Stammverzeichnis des aktuellen Laufwerks eintragen.
  ->> sExtension : Standarderweiterung für das Programm. Die Erweiterung
                   *ohne* Punkt davor eintragen! Wird ein Leerstring angegeben,
                   wird keine Erweiterung eingetragen! Steht hinter der
                   Erweiterung ein Kommentar (abgetrennt durch ein '|'), dann
                   wird der auch eingetragen.
  <<- Result : TRUE, wenn alles ohne Fehler ablief.

  Funktion funktioniert unter Windows NT / 2000 nur, wenn der Anwender lokale
  Administrationsrechte besitzt. }

const
    cPath = 'Path';

var
    Registry : TRegistry;
    sKey : String;
    i, iDirCount : Integer;
    sAppPath : TFileName;
    bRegisterExtension : Boolean;

begin
    Result := false;

    bRegisterExtension := false;

    Registry := TRegistry.Create;

    try
        with Registry do
        begin
            RootKey := HKey_Local_Machine;

            sKey := cAppPathsKey + ExtractFileName (ParamStr (0));

            if (OpenKey (sKey, true)) then
            begin
                sAppPath := ReadString (''); { "(Standard)"-Schlüssel holen }

                if (sAppPath = ParamStr (0)) then
                begin
                    Result := true; { Keine Änderungen }
                    bRegisterExtension := false;
                end { if }
                else
                begin { Noch keine Pfad eingetragen oder Pfad geändert }
                    bRegisterExtension := sExtension <> '';

                    { Die Angabe von '' als Wert fügt den Programmname als
                      "(Standard)"-Schlüssel ein! }
                    WriteString ('', AddQuotationMarks (ParamStr (0)));

                    if (sInitialDir = '') then
                    begin { Pfad des Startverzeichnisses ermitteln }
                        sInitialDir := ParamStr (0);

                        if (Pos ('\\', sInitialDir) = 1) then
                        begin { Netzwerkpfad -> Freigabeverzeichnis ermitteln }
                            iDirCount := 0;

                            for i := 1 to Length (sInitialDir) do
                                if (sInitialDir [i] = '\') then
                                begin
                                    inc (iDirCount);

                                    if (iDirCount = 4) then
                                    begin { Freigabeverzeichnis }
                                        SetLength (sInitialDir, i);
                                        break;
                                    end; { if }
                                end; { if }

                            if (iDirCount < 4) then
                                sInitialDir := '';
                        end { if }
                        else { Normaler Laufwerkspfad ->
                               Stammverzeichnis holen }
                            if (sInitialDir [3] = '\') then
                                SetLength (sInitialDir, 3)
                            else sInitialDir := '';
                    end; { else }

                    if (sInitialDir <> '') then
                    begin
                        WriteString (cPath, sInitialDir);
                        Result := true;
                    end; { if }
                end; { else }
            end; { if }
        end; { with }

    finally
        Registry.Free;
    end; { try / finally }

    if (bRegisterExtension) then
        Result := RegisterExtension (sExtension, (-1));
end; { RegisterApplication }

(* ---- *)

function RegisterExtension (const sExtension: String;
                            const iIconIndex: Integer) : Boolean;
{ Eine Verknüpfung einer Erweiterung in der Registry eintragen.
  ->> sExtension : Die Erweiterung, *ohne* vorgestellten Punkt angeben! Steht
                   hinter der Erweiterung ein durch '|' abgetrennter Kommentar,
                   wird der auch eingetragen.
  ->> iIconIndex : Programmicon, das für diese Art Datei verwendet werden soll.
                   Ist "iIconIndex" = (-1), wird keines eingetragen.
  <<- Result : TRUE, wenn alles funktioniert hat. }

var
    Registry : TRegistry;
    sKey, sExt, sComment : String;
    sExeName : TFileName;
    iPos : Integer;

begin
	Assert (sExtension <> '');

    Result := false;

    sExeName := RemoveExtension (ExtractFileName (ParamStr (0)));

    iPos := Pos ('|', sExtension);

    if (iPos > 0) then
    begin
        sExt := Copy (sExtension, 1, iPos - 1);
        sComment := Copy (sExtension, iPos + 1, Length (sExtension) - iPos);
    end { if }
    else
    begin
        sExt := sExtension;
        sComment := '';
    end; { else }

    Registry := TRegistry.Create;

    try
        with Registry do
        begin
            RootKey := HKey_Classes_Root;

            if (OpenKey ('\.' + sExt, true)) then
            begin
                { Den Programmnamen als "(Standard)"-Schlüssel eintragen }
                WriteString ('', sExeName);

                sKey := '\' + sExeName;

                if (OpenKey (sKey, true)) then
                begin
                    { Den Kommentar als "(Standard)"-Schlüssel eintragen }
                    if (sComment <> '') then
                        WriteString ('', sComment);

                    if (iIconIndex <> (-1)) then { Icon-Pfad eintragen }
                        if (OpenKey (sKey + '\DefaultIcon', true)) then
                            WriteString ('',
                                    AddQuotationMarks (ParamStr (0)) +
                                    ',' + IntToStr (iIconIndex));

                    sKey := sKey + '\shell';

                    if (OpenKey (sKey, true)) then
                    begin
                        sKey := sKey + '\open';

                        if (OpenKey (sKey, true)) then
                        begin
                            sKey := sKey + '\command';

                            if (OpenKey (sKey, true)) then
                                WriteString ('',
                                    AddQuotationMarks (ParamStr (0)) + ' "%1"');

                            Result := true;
                        end; { if }
                    end; { if }
                end; { if }
            end; { if }
        end; { with }

    finally
        Registry.Free;
    end; { try / finally }
end; { RegisterExtension }

(* ---- *)

function RemoveChars (const sStr: String;
					  const achChars: array of Char) : String;

var
	iIndex, iPos : Integer;

begin
	Result := sStr;

    if (Result <> '') then
        for iIndex := Low (achChars) to High (achChars) do
            for iPos := Length (Result) downto 1 do
                if (Result [iPos] = achChars [iIndex]) then
                    Delete (Result, iPos, 1);
end; { RemoveChars }

(* ---- *)

end.
