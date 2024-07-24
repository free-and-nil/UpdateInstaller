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

unit Delphi_T{ools};

interface

uses
{$IFDEF 16BIT}
    WinTypes, WinProcs,
{$ELSE}
    Windows,
{$ENDIF}
{$IFDEF DELPHI_XE2_UP}
  	Types,
{$ENDIF}
{$IFNDEF DELPHI7_UP}
	FileCtrl,
{$ENDIF}
    Classes, SysUtils;

const
    cItemCount = 'ItemCount';

type
{$IFDEF 16BIT}
    uInt = Word; { Für BlockRead und BlockWrite }
{$ENDIF}
    PTSearchRec = ^TSearchRec;
	TFileCompareResult = (crEqual, crDifferent, crError);
    TFileDetails = (fdAttributes, fdSize, fdLastModifyDate);

{$IFNDEF DELPHI5_UP}
function BoolToStr (B: Boolean; UseBoolStrs: Boolean
	{$IFDEF SUPPORTS_DEFAULTPARAMS}
	 = False
     {$ENDIF}
     ): String;
{$ENDIF}

function CreateFileBackup (const sOrgFile: String; const iGenerations: Integer;
                           sBackupExt: String = '.bak') : Boolean;

function CreateSortedStringList (const Duplicates: TDuplicates
{$IFDEF SUPPORTS_DEFAULTPARAMS}
						         = DupIgnore
{$ENDIF}

{$IFDEF DELPHI7_UP}
								 ; const bCaseSensitive: Boolean = false
{$ENDIF}
{$IFDEF UNICODE}
                                 ; const bOwnsObjects: Boolean = false
{$ENDIF}
						         ) : TStringList;

function DayOfYear (ADate : TDateTime
{$IFDEF SUPPORTS_DEFAULTPARAMS}
						 = 0.0
{$ENDIF}
                     ) : Word;

function DirectoryIsEmpty (const sPath: TFileName; const bIgnoreSubDirs: Boolean
{$IFDEF SUPPORTS_DEFAULTPARAMS}
					       = false
{$ENDIF}
				                  ) : Boolean;

function EraseDirectory (sPath: TFileName) : Boolean;

function FileCompare (const sFile1, sFile2: TFileName;
					  const bIgnoreDate: Boolean
{$IFDEF SUPPORTS_DEFAULTPARAMS}
					  = true
{$ENDIF}
                       							) : TFileCompareResult;

function FilesExist (const chDrive: Char) : Boolean;
{ Prüft, ob im Hauptverzeichnis von "chDrive" Dateien existieren }

function FillFileList (const FileList: TStrings;
                       const sPathAndSearchMask: String;
					   const bScanSubDirs: Boolean
{$IFDEF SUPPORTS_DEFAULTPARAMS}
					    = false
{$ENDIF}
					   ; const bAddFullPath: Boolean
{$IFDEF SUPPORTS_DEFAULTPARAMS}
					    = true
{$ENDIF}
					   ; const bIncludeDirNames: Boolean
{$IFDEF SUPPORTS_DEFAULTPARAMS}
					    = false
{$ENDIF}
					   ; const bClearList: Boolean
{$IFDEF SUPPORTS_DEFAULTPARAMS}
					    = true
{$ENDIF}
					   ; const bAllLowerCase: Boolean
{$IFDEF SUPPORTS_DEFAULTPARAMS}
					    = false
{$ENDIF}
					   ; const bAddSearchRecToList: Boolean
                       // Use "MemDispose" to free memory allocated
{$IFDEF SUPPORTS_DEFAULTPARAMS}
					    = false
{$ENDIF}
					   ; const bFollowReparsePoints: Boolean
{$IFDEF SUPPORTS_DEFAULTPARAMS}
					    = false
{$ENDIF}
					   ; const FileDetails: TFileDetails
{$IFDEF SUPPORTS_DEFAULTPARAMS}
					    = fdAttributes
{$ENDIF}
					   ; const iMaxSearchDepth: Integer
{$IFDEF SUPPORTS_DEFAULTPARAMS}
					    = 0
{$ENDIF}
					   ) : Integer;
{ Für jeden Eintrag steht in "FileList [Objects] entweder der Wert von
  "SearchRec.Attr" oder der "SearchRec" (muss später freigegeben werden!) }

function FormatCommaText (const sText: String) : String;

{$IFNDEF DELPHI5_UP}
procedure FreeAndNil (var Obj);
{$ENDIF}

procedure FreeListObjects (const Strings: TStrings);

procedure FreeFileList (var FileList: TStringList);

{$IFDEF 16BIT}
function GetCurrentDir : TFileName;
{$ENDIF}

function GetCD_DriveLetter : Char;
{ Gibt den Laufwerksbuchstaben des ersten CD-ROM-Laufwerks im PC zurück }

function GetDateString : String;

function GetFileDate (const sFileName: TFileName) : Integer;

function GetFileDateString (const sFileName: TFileName) : String;

function GetFullPath (var sFileName: TFileName) : Boolean;

function LoadListFromIni (const sIniName, sSection: String;
						  const List: TStrings;
                          const sCounter: String
{$IFDEF SUPPORTS_DEFAULTPARAMS}
						  = ''
{$ENDIF}
						  ; const bSkipBlankLines: Boolean
{$IFDEF SUPPORTS_DEFAULTPARAMS}
						  = false
{$ENDIF}
						  ; const bConvertAllToLowerCase: Boolean
{$IFDEF SUPPORTS_DEFAULTPARAMS}
						  = false
{$ENDIF}
                          ; const sPrefix: String
{$IFDEF SUPPORTS_DEFAULTPARAMS}
						  = ''
{$ENDIF}
                          ) : Boolean;
{ Füllt "List" mit Werten aus der INI-Datei "sIniName". "sCounter" gibt den Wert
  in der INI-Datei, Sektion "sSection", an, der die Anzahl Elemente vorgibt.
  Das erste Element hat immer den Bezeichner "1". Ist "sCounter" leer, so liest
  die Funktion so lange Werte ein, bis der Zugriff auf ein Element eine leere
  Zeichenkette zurückgibt.
  ->> sIniName : Name der INI-Datei;
  ->> sSection : Name der Sektion, die die Werte enthält;
  ->> sCounter : Bezeichner für den Wert, der die Anzahl Elemente in der Liste
  				 vorgibt;
  ->> bSkipBlankLines : Sollen leere Werte in der Liste gespeichert werden (
  						(wird nur verwendet, wenn "sCounter <> ''");
  <<- Result : TRUE, wenn Werte gelesen wurden; FALSE, wenn keine Werte gelesen
  			   wurden. }

procedure LoadWindowPosFromIniFile (const hWindow: HWnd; sIniName: String
{$IFDEF SUPPORTS_DEFAULTPARAMS}
																		  = ''
{$ENDIF}
									);

function ReadLine (const Stream: TStream; var sLine: String) : Boolean;

function RenameSource (const sSourceName: TFileName;
					   const sNewExt: String
{$IFDEF SUPPORTS_DEFAULTPARAMS}
						= '.bak'
{$ENDIF}
					   ) : Boolean;

function ReverseStringList (const InputList: TStrings) : TStringList;

function SaveListToIni (const sIniName, sSection: String;
						const List: TStrings) : Boolean;

function SaveStrToInt (const sStr: String) : Integer;

function SaveWindowPosToIniFile (const hWindow: HWnd; const sIniName: String
{$IFDEF SUPPORTS_DEFAULTPARAMS}
  															  				= ''
{$ENDIF}
																	) : Boolean;

function SetFileDate (const sFileName: TFileName;
					  const iFileDate: Integer) : Integer;

function StrToIntEx (const sStr: String; const iDefValue: Integer
{$IFDEF SUPPORTS_DEFAULTPARAMS}
						 = 0
{$ENDIF}
                     ) : Integer;

function WeekOfYear (ADate : TDateTime
{$IFDEF SUPPORTS_DEFAULTPARAMS}
						 = 0.0
{$ENDIF}
                     ) : Word;

implementation

uses WinTools, PasTools,
{$IFDEF DELPHI4_UP}
  	 MultiMon,
{$ENDIF}
{$IFDEF WIN32}
     Win32ToolsU,
{$ENDIF}
	 IniFiles;

const
    cIniWindowPlacement = 'WindowPlacement';
    cIniShowCmd = 'ShowCmd';
    cIniMin_X = 'Min_X';
    cIniMin_Y = 'Min_Y';
    cIniMax_X = 'Max_X';
    cIniMax_Y = 'May_Y';
    cIniNormal_Left = 'Normal_Left';
    cIniNormal_Top = 'Normal_Top';
    cIniNormal_Right = 'Normal_Right';
    cIniNormal_Bottom = 'Normal_Bottom';

(* ---- *)

{$IFNDEF DELPHI5_UP}
function BoolToStr (B: Boolean; UseBoolStrs: Boolean
	{$IFDEF SUPPORTS_DEFAULTPARAMS}
    	 = False
	{$ENDIF}
    ): String;

const
	cSimpleBoolStrs: array [Boolean] of String = ('0', '-1');
    DefaultTrueBoolStr = 'True';   // DO NOT LOCALIZE
    DefaultFalseBoolStr = 'False'; // DO NOT LOCALIZE

begin
    if (UseBoolStrs) then
    begin
        if (B) then
        	Result := DefaultTrueBoolStr
        else Result := DefaultFalseBoolStr;
    end { if }
    else Result := cSimpleBoolStrs [B];
end; { BoolToStr }
{$ENDIF}

(* ---- *)

function FormatCommaText (const sText: String) : String;

var
	iPos : Integer;

begin
	Result := sText;

	iPos := Pos (',', sText);

	while (iPos > 0) do
    begin
    	Insert (' ', Result, iPos + 1);
    	iPos := NextPos (',', Result, iPos + 1);
    end; { while }
end; { FormatCommaText }

(* ---- *)

{$IFNDEF DELPHI5_UP}
procedure FreeAndNil (var Obj);

var
    Temp: TObject;

begin
    Temp := TObject (Obj);
	Pointer (Obj) := nil;
	Temp.Free;
end; { FreeAndNil }
{$ENDIF}

(* ---- *)

procedure FreeFileList (var FileList: TStringList);

var
	iIndex : Integer;
	pSearchRec : Pointer;

begin
	Assert (FileList <> NIL);

	for iIndex := 0 to FileList.Count - 1 do
	begin
		pSearchRec := Pointer (FileList.Objects [iIndex]);
		MemDispose (pSearchRec);
	end; { for }

	FreeAndNil (FileList);
end; { FreeFileList }

(* ---- *)

procedure FreeListObjects (const Strings: TStrings);

var
    iIndex : integer;

begin
	Assert (Strings <> NIL);

    for iIndex := 0 to Strings.Count - 1 do
    	TObject (Strings.Objects [iIndex]).Free
end; { FreeListObjects }

(* ---- *)

{$IFDEF 16BIT}
function GetCurrentDir : TFileName;

var
	psResult : ^TFileName;

begin
	new (psResult);
	GetDir (0, psResult^);
	Result := psResult^;
	Dispose (psResult);
end; { GetCurrentDir }
{$ENDIF}

(* ---- *)

function CreateFileBackup (const sOrgFile: String; const iGenerations: Integer;
                           sBackupExt: String = '.bak') : Boolean;

var
	sFileName : String;
    iIndex : Integer;

begin
	Assert (sOrgFile <> '');
    Assert (iGenerations > 0);
    Assert (sBackupExt <> '');
    Assert (sBackupExt <> '.');

    Result := false;

    if not (FileExists (sOrgFile)) then
    	exit;

    if (sBackupExt [1] <> '.') then
    	sBackupExt := '.' {%H-}+ sBackupExt;

    sBackupExt := sBackupExt + iif (iGenerations > 9, '.%.2d', '%d');

    sFileName := Format (sOrgFile + sBackupExt, [iGenerations]);

    if (FileExists (sFileName)) then
    	DeleteFile (sFileName);

    if (iGenerations > 1) then
	    for iIndex := Pred (iGenerations) downto 1 do
        begin
        	sFileName := Format (sOrgFile + sBackupExt, [iIndex]);

            if (FileExists (sFileName)) then
            	if not (RenameFile (sFileName,
                					Format (sOrgFile + sBackupExt,
                                    		[iIndex +1]))) then
                	DeleteFile (sFileName);
        end { for }
    else sFileName := Format (sOrgFile + sBackupExt, [1]);

    if (RenameFile (sOrgFile, sFileName)) then
    	Result := true
{$IFDEF DEBUG}
  	else RaiseLastOSError;
{$ENDIF}
end; { CreateFileBackup }

(* ---- *)

function CreateSortedStringList (const Duplicates: TDuplicates
{$IFDEF SUPPORTS_DEFAULTPARAMS}
						         = DupIgnore
{$ENDIF}

{$IFDEF DELPHI7_UP}
								 ; const bCaseSensitive: Boolean = false
{$ENDIF}
{$IFDEF UNICODE}
                                 ; const bOwnsObjects: Boolean = false
{$ENDIF}
						         ) : TStringList;
begin
	Result := TStringList.Create;

    if (Assigned (Result)) then
    begin
        Result.Sorted := true;
        Result.Duplicates := Duplicates;
{$IFDEF DELPHI7_UP}
        Result.CaseSensitive := bCaseSensitive;
{$ENDIF}
{$IFDEF UNICODE}
        Result.OwnsObjects := bOwnsObjects;
{$ENDIF}
    end; { if }
end; { CreateSortedStringList }

(* ---- *)

function DayOfYear (ADate : TDateTime
{$IFDEF SUPPORTS_DEFAULTPARAMS}
						 = 0.0
{$ENDIF}
                     ) : Word;
var
    day, month, year : Word;
    FirstOfYear : TDateTime;

begin
	try
    	if (ADate = 0.0) then
        	ADate := Date;

        DecodeDate (ADate, year, month, day);
        FirstOfYear := EncodeDate (year, 1, 1);
        Result := Trunc (ADate - FirstOfYear) + 1;

    except
    	Result := 1;
    end; { try / except }
end; { DayOfYear }

(* ---- *)

function DirectoryIsEmpty (const sPath: TFileName; const bIgnoreSubDirs: Boolean
{$IFDEF SUPPORTS_DEFAULTPARAMS}
					       = false
{$ENDIF}
				                  ) : Boolean;

var
	FileList : TStringList;

begin
	Assert (sPath <> '');
    Assert (DirectoryExists (sPath));

    FileList := TStringList.Create;

    try
        FillFileList (FileList, AddBackSlash (sPath) + '*.*', false, false,
                      not bIgnoreSubDirs);

        Result := FileList.Count = 0;

    finally
		FileList.Free;
    end; { try / finally }
end; { DirectoryIsEmpty }

(* ---- *)

function EraseDirectory (sPath: TFileName) : Boolean;

var
	FileList : TStringList;
    iIndex, iOrgAttr, iNewAttr : Integer;
    sParentDir : TFileName;

begin
{$IFDEF SUPPORTS_ASSERTIONS}
	Assert (sPath <> '');
    Assert (Length (sPath) > 3); { Kein Hauptverzeichnis }
{$ENDIF}

	Result := true;

	FileList := TStringList.Create;

    try
    	FileList.Sorted := true;

    	sPath := AddBackSlash (sPath);

        FillFileList (FileList, sPath + '*.*', true, true, true, true, true);

        with FileList do
        	if (Count > 0) then
                for iIndex := Count - 1 downto 0 do
                begin
                    iOrgAttr := Integer (Objects [iIndex]);
                    iNewAttr := iOrgAttr;

                    if (iOrgAttr and faReadOnly = faReadOnly) then
                        iNewAttr := iNewAttr and not faReadOnly;

                    if (iOrgAttr and faHidden{%H-} = faHidden{%H-}) then
                        iNewAttr := iNewAttr and not faHidden{%H-};

                    if (iOrgAttr and faSysFile{%H-} = faSysFile{%H-}) then
                        iNewAttr := iNewAttr and not faSysFile{%H-};

                    if (iOrgAttr <> iNewAttr) then
                        if (FileSetAttr (Strings [iIndex], iNewAttr) <> 0) then
                        begin
{$IFDEF DELPHI2_UP}
	{$IFDEF DEBUG}
                            Win32Check (false);
	{$ENDIF}
{$ENDIF}
                            Result := false;
                            break;
                        end; { if }

                    if (iOrgAttr and faDirectory = 0) then { Dateien löschen }
                        if not (DeleteFile (Strings [iIndex])) then
                        begin
{$IFDEF DELPHI2_UP}
	{$IFDEF DEBUG}
                            Win32Check (false);
    {$ENDIF}
{$ENDIF}
                            Result := false;
                            break;
                        end { if }
                        else Delete (iIndex);
                end; { for }

{$IFDEF DELPHI2_UP}
        Sleep (0);
{$ENDIF}

        if (Result) then
        begin
            SetLength (sPath, Length (sPath) - 1); { "\" abschneiden }
            sParentDir := sPath;

            iIndex := LastPos ('\', sParentDir);

            if (iIndex > 0) then
            begin
                if (iIndex > 3) then
                    SetLength (sParentDir, iIndex - 1) { Unterverzeichnis }
                else SetLength (sParentDir, iIndex); { Hauptverzeichnis }

                SetCurrentDir (ExtractFileDir (sParentDir));
            end; { if }

{$IFDEF DELPHI2_UP}
            Sleep (0);
{$ENDIF}

			with FileList do
            	if (Count > 0) then { Unterverzeichnisse löschen }
                begin
                	{ Annahme: Nach Sortierung stehen "tiefste"
                    		   Unterverzeichnisse am Ende der Liste }
                	if not (Sorted) then
                    	Sort;

                	for iIndex := Count - 1 downto 0 do
						if not (RemoveDir (Strings [iIndex])) then
                        begin
                        	Result := false;
                            Break;
                        end; { if }
                end; { if }

            if (Result) then
	            Result := RemoveDir (sPath);
        end; { if }

    finally
    	FileList.Free;
    end; { try / finally }
end; { EraseDirectory }

(* ---- *)

function FileCompare (const sFile1, sFile2: TFileName;
					  const bIgnoreDate: Boolean
{$IFDEF SUPPORTS_DEFAULTPARAMS}
					  = true
{$ENDIF}
                       							) : TFileCompareResult;

	(* ---- *)

    function GetSizeAndTime (const sFileName: TFileName;
    						 var lSize, lTime:
                                 {$IFDEF SUPPORTS_INT64} Int64 {$ELSE} LongInt {$ENDIF}
                             ) : Boolean;

    var
		SearchRec : TSearchRec;

    begin
        if (FindFirst (sFileName, faAnyFile, SearchRec) = 0) then
        begin
            with SearchRec do
            begin
                lSize := Size;
                lTime := Time;
            end; { with }

            Result := true;
        end { if }
        else Result := false;

        FindClose (SearchRec);
    end; { GetSizeAndTime }

    (* ---- *)

const
	cBufSize = 4096;

var
	lTime1, lTime2, lSize1, lSize2 : {$IFDEF SUPPORTS_INT64} Int64 {$ELSE} LongInt {$ENDIF};
    File1, File2 : File of Byte;
    byFileMode : Byte;
    pBuf1, pBuf2 : PChar;
    iRead1, iRead2,
    uIndex : Cardinal;

begin { FileCompare }
{$IFDEF SUPPORTS_ASSERTIONS}
	Assert ((sFile1 <> '') and (sFile2 <> ''));
{$ENDIF}

	Result := crError;

    if (GetSizeAndTime (sFile1, lSize1{%H-}, lTime1{%H-}) = false) or
       (GetSizeAndTime (sFile2, lSize2{%H-}, lTime2{%H-}) = false) then
    	exit;

    Result := crDifferent;

    if not (bIgnoreDate) then
    	if (lTime1 <> lTime2) then
        	exit;

    if (lSize1 <> lSize2) then
    	exit;

	byFileMode := FileMode;
    FileMode := fmOpenRead;

    try
		Result := crError;

    	AssignFile (File1, sFile1);
        AssignFile (File2, sFile2);

        Reset (File1);

        if (IOResult = 0) then
        begin
	        Reset (File2);

            if (IOResult = 0) then
            begin
            	pBuf1 := StrAlloc (cBufSize);
            	pBuf2 := StrAlloc (cBufSize);

			    Result := crEqual;

                repeat
                	BlockRead (File1, pBuf1^, cBufSize, iRead1{%H-});
                	BlockRead (File2, pBuf2^, cBufSize, iRead2{%H-});

                    if (iRead1 <> iRead2) then
                    begin
					    Result := crError;
						Break;
                    end; { if }

                    if (iRead1 = 0) then
                    	Break;

                    for uIndex := 0 to iRead1 - 1 do
                        if ({%H-}PChar ({%H-}NativeUInt (pBuf1) + uIndex)^ <>
                            {%H-}PChar ({%H-}NativeUInt (pBuf2) + uIndex)^) then
                        begin
                            Result := crDifferent;
                            Break;
                        end; { if }
                until (Result = crDifferent);

                StrDispose (pBuf1);
                StrDispose (pBuf2);

				CloseFile (File2);
            end; { if }

            CloseFile (File1);
        end; { if }

    finally
    	FileMode := byFileMode;
    end; { try / finally }
end; { FileCompare }

(* ---- *)

function FilesExist (const chDrive: Char) : Boolean;
{ Prüft, ob im Hauptverzeichnis von "chDrive" Dateien existieren }

var
	SearchRec : TSearchRec;
	uSaveMode : UInt;

begin
	uSaveMode := SetErrorMode (SEM_FAILCRITICALERRORS or
							   SEM_NOOPENFILEERRORBOX);

{$IFNDEF CLR}
	FillChar (SearchRec{%H-}, SizeOf (TSearchRec), 0);
{$ENDIF}

	Result := FindFirst (chDrive + ':\*.*', faAnyFile, SearchRec) = 0;

	FindClose (SearchRec);

	SetErrorMode (uSaveMode);
end; { FilesExist }

(* ---- *)

function FillFileList (const FileList: TStrings;
                       const sPathAndSearchMask: String;
					   const bScanSubDirs: Boolean
{$IFDEF SUPPORTS_DEFAULTPARAMS}
					    = false
{$ENDIF}
					   ; const bAddFullPath: Boolean
{$IFDEF SUPPORTS_DEFAULTPARAMS}
					    = true
{$ENDIF}
					   ; const bIncludeDirNames: Boolean
{$IFDEF SUPPORTS_DEFAULTPARAMS}
					    = false
{$ENDIF}
					   ; const bClearList: Boolean
{$IFDEF SUPPORTS_DEFAULTPARAMS}
					    = true
{$ENDIF}
					   ; const bAllLowerCase: Boolean
{$IFDEF SUPPORTS_DEFAULTPARAMS}
					    = false
{$ENDIF}
					   ; const bAddSearchRecToList: Boolean
{$IFDEF SUPPORTS_DEFAULTPARAMS}
					    = false
{$ENDIF}
					   ; const bFollowReparsePoints: Boolean
{$IFDEF SUPPORTS_DEFAULTPARAMS}
					    = false
{$ENDIF}
					   ; const FileDetails: TFileDetails
{$IFDEF SUPPORTS_DEFAULTPARAMS}
					    = fdAttributes
{$ENDIF}
					   ; const iMaxSearchDepth: Integer
{$IFDEF SUPPORTS_DEFAULTPARAMS}
					    = 0  // 0 = No limit
{$ENDIF}
					   ) : Integer;

var
	iStartDirLen : Integer;

    (* ---- *)

    procedure AddItemToList (const sPath: String;
                             const SearchRec: TSearchRec);

    var
        pSearchRec : PTSearchRec;
        sFileName : TFileName;

    begin
        with SearchRec do
        begin
            if (bAllLowerCase) then
                sFileName := LowerCase (sPath + Name)
            else sFileName := sPath + Name;

            if not (bAddFullPath) then  // Nur den relativen Pfad hinzufügen
                Delete (sFileName, 1, iStartDirLen);
        end; { with }

        if (bAddSearchRecToList) then
        begin
            pSearchRec := MemAlloc (SizeOf (TSearchRec));
            move (SearchRec, pSearchRec^, SizeOf (TSearchRec));

(**
            pSearchRec^.Time := SearchRec.Time;
            pSearchRec^.Size := SearchRec.Size;
            pSearchRec^.Attr := SearchRec.Attr;
            pSearchRec^.Name := '';
            pSearchRec^.ExcludeAttr := SearchRec.ExcludeAttr;
            pSearchRec^.FindHandle := 0;
            pSearchRec^.FindData := SearchRec.FindData;
**)

            FileList.AddObject (sFileName, TObject (pSearchRec));
        end { if }
        else
        	case FileDetails of
                fdAttributes : FileList.AddObject (sFileName,
                                                   TObject (SearchRec.Attr));
                fdSize : FileList.AddObject (sFileName,
                                             TObject (SearchRec.Size));
                fdLastModifyDate : FileList.AddObject (sFileName,
                                             		   TObject (SearchRec.Time))
            end; { case FileDetails of }
    end; { AddItemToList }

    (* ---- *)

    function IsReparsePoint (const FD:
                                       {$IFDEF FPC}
                                       TWin32FindDataW
                                       {$ELSE}
                                       TWin32FindData
                                       {$ENDIF}) : Boolean;
    {$IFDEF DELPHI2007_UP}
        {$IFNDEF DEBUG}
            inline;
        {$ENDIF}
    {$ENDIF}

    const
{$IFNDEF DELPHI2007_UP}
        FILE_ATTRIBUTE_REPARSE_POINT = $00000400;
{$ENDIF}
        IO_REPARSE_TAG_MOUNT_POINT = DWORD($A0000003);

    begin
    	if (FD.dwFileAttributes and FILE_ATTRIBUTE_REPARSE_POINT <> 0) and
           (FD.dwReserved0 and IO_REPARSE_TAG_MOUNT_POINT <> 0) then  // Junction
            Result := true
        else Result := false;  // Keine Junction / Reparse Point
    end; { IsReparsePoint }

    (* ---- *)

    function ScanDir (const sPath, sSearchMask: String;
    				  const iSearchDepth: Integer) : Integer;

    var
        SR : TSearchRec;

    begin
        Result := 0;

        if (FindFirst (sPath + sSearchMask,
        			   faAnyFile - faDirectory, SR) = 0) then
        begin
            repeat
                Inc (Result);
                AddItemToList (sPath, SR);
            until (FindNext (SR) <> 0);

            FindClose (SR);
        end; { if }

        if ((iSearchDepth = 0) or (iSearchDepth < iMaxSearchDepth)) then
            if (FindFirst (sPath + '*.*', faAnyFile, SR) = 0) then
            begin
                repeat
                    if (SR.Attr and faDirectory <> 0) and
                       (SR.Name <> '.') and (SR.Name <> '..') then
                    begin
                        if (IsReparsePoint (SR.FindData)) and
                           (bFollowReparsePoints = false) then
                            Continue;

                        if (bIncludeDirNames) then
                            AddItemToList (sPath, SR);

                        if (bScanSubDirs) then
                            Inc (Result,
                                 ScanDir (sPath + SR.Name + '\', sSearchMask,
                                          iif (iMaxSearchDepth > 0,
                                          	   iSearchDepth + 1, 0)));
                    end; { if }
                until (FindNext (SR) <> 0);

                FindClose (SR);
            end; { if }
    end; { ScanDir }

	(* ---- *)

var
    sStartDir, sSearchMask : String;

begin { FillFileList }
    Result := (-1);

	if (bClearList) then
    	FileList.Clear;

    sStartDir := ExtractFilePath (sPathAndSearchMask);
    sSearchMask := ExtractFileName (sPathAndSearchMask);

    if (sSearchMask = '') then
        exit;

    if (sStartDir <> '') then
    	sStartDir := AddBackSlash (sStartDir)
    else sStartDir := '.' + PathDelim;

    iStartDirLen := Length (sStartDir);

	Result := ScanDir (sStartDir, sSearchMask, 0);
end; { FillFileList }

(* ---- *)

function GetCD_DriveLetter : Char;
{ Gibt den Laufwerksbuchstaben des ersten CD-ROM-Laufwerks im PC zurück }

var
	iIndex : Integer;

begin
	Result := #0;

	for iIndex := 65 to 90 do
{$IFNDEF CLR}
    	if (Windows.GetDriveType (PChar (Char (iIndex) + ':\')) = DRIVE_CDROM) then
{$ELSE}
    	if (GetDriveType (Char (iIndex) + ':\') = DRIVE_CDROM) then
{$ENDIF}
        begin
        	Result := Char (iIndex);
            exit;
        end; { if }
end; { GetCD_DriveLetter }

(* ---- *)

function GetDateString : String;

var
	wYear, wMonth, wDay : Word;

begin
	DecodeDate (Date, wYear, wMonth, wDay);

	Result := Format ('%2.d%2.d%2.d', [wYear - 2000, wMonth, wDay]);

    Result := ReplaceChar (Result, ' ', '0');
end; { GetDateString }

(* ---- *)

function GetFileDate (const sFileName: TFileName) : Integer;
{ Das Datum einer Datei als Integer zurückgeben }

var
{$IFDEF CLR}
	hFile : TOpenedFile;
{$ELSE}
	hFile : THandle;
{$ENDIF}

begin
	Result := (-1);

    // FileGetDate, mit FileOpen öffnen und FileClose schließen
    hFile := FileOpen (sFileName, fmOpenRead or fmShareDenyNone);

    if (hFile = HFILE_ERROR) then
        exit;

    try
        Result := FileGetDate (hFile);

    finally
        FileClose (hFile);
    end; { try / finally }
end; { GetFileDate }

(* ---- *)

function GetFileDateString (const sFileName: TFileName) : String;
{ Das Datum einer Datei als String zurückgeben }

var
	iFileDate : Integer;

begin
    iFileDate := GetFileDate (sFileName);

    if (iFileDate <> (-1)) then
        Result := DateToStr (FileDateToDateTime (iFileDate))
    else Result := '';
end; { GetFileDateString }

(* ---- *)

function GetFullPath (var sFileName: TFileName) : Boolean;
{ Prüfen, ob nur ein Dateiname oder ein kompletter Pfad in "sFileName"
  angegeben wurde. Wenn es sich nur um einen Dateinamen handelt, den Namen um
  den Pfad erweitern. Wenn möglich, relative Dateinamen erweitern. }

var
{$IFDEF DELPHI2_UP}
	sNewName : TFileName;
    iLen : Integer;
	pszFilePath : PChar;
{$ELSE}
	sNewName : String [80];  { Funktioniert nicht mit Long Strings }
{$ENDIF}

begin
	Result := false;

{$IFDEF DELPHI2_UP}
	if (FileExists (sFileName)) then
	begin
		SetLength (sNewName{%H-}, MAX_PATH);

		{ Den kompletten Pfad ermitteln (z.B. für "..\test.txt") }
		iLen := GetFullPathName (PChar (sFileName), MAX_PATH, PChar (sNewName),
								 pszFilePath{%H-});

		if (iLen > 0) then
		begin
			SetLength (sNewName, iLen);

			if (FileExists (sNewName)) then
			begin
				sFileName := sNewName;
				Result := true;
			end; { if }
		end; { if }
	end; { if }
{$ELSE}
	if (FileExists (sFileName)) then
	begin { Datei ist da }
		Result := true;

		if (ExtractFilePath (sFileName) = '') then
		begin { Kein Pfad angegeben }
			{ Befindet sich die Datei im aktuellen Verzeichnis? }
			sNewName := GetCurrentDir + '\' + sFileName;

			if (FileExists (sNewName)) then
				sFileName := sNewName;
		end; { if }
	end; { if }
{$ENDIF}
end; { GetFullPath }

(* ---- *)

function LoadListFromIni (const sIniName, sSection: String;
						  const List: TStrings;
                          const sCounter: String
{$IFDEF SUPPORTS_DEFAULTPARAMS}
						  = ''
{$ENDIF}
						  ; const bSkipBlankLines: Boolean
{$IFDEF SUPPORTS_DEFAULTPARAMS}
						  = false
{$ENDIF}
						  ; const bConvertAllToLowerCase: Boolean
{$IFDEF SUPPORTS_DEFAULTPARAMS}
						  = false
{$ENDIF}
                          ; const sPrefix: String
{$IFDEF SUPPORTS_DEFAULTPARAMS}
						  = ''
{$ENDIF}
                          ) : Boolean;

var
	iIndex, iCount : Integer;
    sValue : String;

begin
{$IFDEF SUPPORTS_ASSERTIONS}
	Assert (sIniName <> '');
    Assert (sSection <> '');
    Assert (List <> NIL);
    Assert ((sCounter = '') or (sCounter = cItemCount));
{$ENDIF}

    Result := false;

{$IFDEF WIN32}
    if (sCounter <> '') then
        iCount := IniReadInt (sIniName, sSection, sCounter, 0)
    else iCount := MaxInt;

    if (iCount > 0) then
    begin
        List.Clear;

	    for iIndex := 1 to iCount do
        begin
            sValue := IniReadStr (sIniName, sSection,
                                  sPrefix + IntToStr (iIndex), '');

            if (sValue = '') then
            begin
                if (sCounter = '') then
                    Break;

                if (bSkipBlankLines) then
                    Continue;
            end; { if }

            if (bConvertAllToLowerCase) then
                List.Add (LowerCase (sValue))
    	    else List.Add (sValue);
        end; { for }
    end; { if }

    if (iCount > 0) then
        Result := true;
{$ELSE}
    with TIniFile.Create (sIniName) do
    	try
        	if (sCounter <> '') then
            	iCount := ReadInteger (sSection, sCounter, 0)
            else iCount := MaxInt;

            if (iCount > 0) then
            begin
            	List.Clear;

	            for iIndex := 1 to iCount do
                begin
                	sValue := ReadString (sSection,
                                          sPrefix + IntToStr (iIndex), '');

                    if (sValue = '') then
                    begin
                    	if (sCounter = '') then
                        	break;

                        if (bSkipBlankLines) then
                        	Continue;
                    end; { if }

                    if (bConvertAllToLowerCase) then
                    	List.Add (LowerCase (sValue))
    	        	else List.Add (sValue);
                end; { for }
            end; { if }

            if (iCount > 0) then
                Result := true;

        finally
        	Free;
        end; { try / finally }
{$ENDIF}
end; { LoadListFromIni }

(* ---- *)

{$IFDEF DELPHI4_UP}
function EnumMonitorsProc ({%H-}hMon: HMONITOR; {%H-}DC: HDC; R: PRect;
						   Data: LPARAM) : Boolean; stdcall;

var
	pRect : Windows.PRect;

begin
	Result := true;

	pRect := {%H-}Windows.PRect (Data);

    if (R^.Left < pRect^.Left) then
    	pRect^.Left := R^.Left;

    if (R^.Top < pRect^.Top) then
    	pRect^.Top := R^.Top;

    if (R^.Right > pRect^.Right) then
    	pRect^.Right := R^.Right;

    if (R^.Bottom > pRect^.Bottom) then
    	pRect^.Bottom := R^.Bottom;
end; { EnumMonitorsProc }
{$ENDIF}

(* ---- *)

procedure LoadWindowPosFromIniFile (const hWindow: HWnd; sIniName: String
{$IFDEF SUPPORTS_DEFAULTPARAMS}
																		  = ''
{$ENDIF}
									);

var
    IniFile : TIniFile;
    Rect, DefRect : TRect;
    Placement : TWindowPlacement;
    iShowCmd : Integer;
{$IFDEF DELPHI4_UP}
    ScreenRect : TRect;
{$ENDIF}

begin
	if (sIniName = '') then
    	sIniName := CreateIniName;

    if not (FileExists (sIniName)) then
    	exit;

    GetWindowRect (hWindow, DefRect{%H-});

    IniFile := TIniFile.Create (sIniName);

    try
    	iShowCmd := IniFile.ReadInteger (cIniWindowPlacement, cIniShowCmd, (-1));

    	if (iShowCmd = (-1)) then
        	exit;  // Keine Werte gespeichert

        { Default-Fensterposition, wenn noch keine INI-Datei vorliegt }
        with IniFile do
        begin { Fensterposition einlesen }
            Rect.Left := ReadInteger (cIniWindowPlacement, cIniNormal_Left,
            						  DefRect.Left);
            Rect.Top := ReadInteger (cIniWindowPlacement, cIniNormal_Top,
            						 DefRect.Top);
            Rect.Right := ReadInteger (cIniWindowPlacement, cIniNormal_Right,
            						   DefRect.Right);
            Rect.Bottom := ReadInteger (cIniWindowPlacement, cIniNormal_Bottom,
            							DefRect.Bottom);
        end; { with }

{$IFDEF DELPHI4_UP}
		FillChar (ScreenRect{%H-}, SizeOf (TRect), #0);

		Win32Check (EnumDisplayMonitors (0, NIL, {$IFDEF FPC}@{$ENDIF}EnumMonitorsProc,
        								 {%H-}LPARAM (@ScreenRect)));

        if (Rect.Left < ScreenRect.Left) or
           (Rect.Right > ScreenRect.Right) or
           (Rect.Top < ScreenRect.Top) or
           (Rect.Bottom > ScreenRect.Bottom) then
        	if (iShowCmd <> SW_MAXIMIZE) then
            	Rect := DefRect;
{$ELSE}
		Rect := DefRect;

        {* Bei veränderter Bildschirmauflösung Werte korrigieren *}
        with Rect do
        begin
            if (Left < 1) or (Left > GetSystemMetrics (sm_CXFullScreen)) then
                Left := 1;
            if (Top < 1) or (Top > GetSystemMetrics (sm_CYFullScreen)) then
                Top := 1;

            if (Right > GetSystemMetrics (sm_CXFullScreen)) then
                Right := GetSystemMetrics (sm_CXFullScreen);
            if (Bottom > GetSystemMetrics (sm_CYFullScreen)) then
                Bottom := GetSystemMetrics (sm_CYFullScreen);
        end; { with }
{$ENDIF}

{$IFNDEF CLR}
        { Struktur initialisieren }
        FillChar (Placement{%H-}, SizeOf (TWindowPlacement), #0);
{$ENDIF}

        with Placement do
        begin
            length := SizeOf (TWindowPlacement);

            rcNormalPosition := Rect;

            { Fenstergröße holen }
            showCmd := iShowCmd;

            with IniFile do
            begin
                ptMinPosition.X := ReadInteger (cIniWindowPlacement,
                								cIniMin_X, 0);
                ptMinPosition.Y := ReadInteger (cIniWindowPlacement,
                								cIniMin_Y, 0);
                ptMaxPosition.X := ReadInteger (cIniWindowPlacement,
                								cIniMax_X, 0);
                ptMaxPosition.Y := ReadInteger (cIniWindowPlacement,
                								cIniMax_Y, 0);
            end; { with }
(**
            { Position bei maximiertem Fenster }
            ptMaxPosition := Point (GetSystemMetrics (sm_CXFrame) * (-1),
                                    GetSystemMetrics (sm_CYFrame) * (-1));
**)
        end; { with }

{$IFNDEF CLR}
        SetWindowPlacement (hWindow, @Placement); { Fensterposition setzen }
{$ELSE}
        SetWindowPlacement (hWindow, Placement); { Fensterposition setzen }
{$ENDIF}

    finally
        IniFile.Free;
    end; { try / finally }
end; { LoadWindowPosFromIniFile }

(* ---- *)

function ReadLine (const Stream: TStream; var sLine: String) : Boolean;
// http://stackoverflow.com/questions/11597634/read-line-with-tfilestream-delphi

var
{$IFDEF UNICODE}
    RawLine : UTF8String;
{$ENDIF}
    ch : AnsiChar;

begin
    Result := False;
    ch := #0;

{$IFDEF UNICODE}
    while (Stream.Read (ch, 1) = 1) and (ch <> #13) do
        RawLine := RawLine + UTF8String (ch);

    sLine := String (RawLine);
{$ELSE}
	sLine := '';

    while (Stream.Read (ch, 1) = 1) and (ch <> #13) do
        sLine := sLine + ch;
{$ENDIF}

    if (ch = #13) then
    begin
    	Result := True;

      	if (Stream.Read (ch, 1) = 1) and (ch <> #10) then
        	Stream.Seek (-1, soCurrent);  // unread it if not LF character.
    end { if }
    else
    	if (sLine <> '') then
    		Result := true;
end; { ReadLine }

(* ---- *)

function RenameSource (const sSourceName: TFileName;
					   const sNewExt: String
{$IFDEF SUPPORTS_DEFAULTPARAMS}
						= '.bak'
{$ENDIF}
					   ) : Boolean;

var
	sTargetName : TFileName;

begin
	Assert (FileExists (sSourceName));
    Assert (Pos ('.', sNewExt) = 1);

	Result := false;

    sTargetName := ChangeFileExt (sSourceName, sNewExt);

    if (FileExists (sTargetName)) then
    	if not (DeleteFile (sTargetName)) then
        	exit;

    Result := RenameFile (sSourceName, sTargetName);
end; { RenameSource }

(* ---- *)

function ReverseStringList (const InputList: TStrings) : TStringList;

var
	iIndex : Integer;

begin
	Result := TStringList.Create;

    for iIndex := InputList.Count - 1 downto 0 do
    	Result.Add (InputList [iIndex]);
end; { ReverseStringList }

(* ---- *)

function SaveListToIni (const sIniName, sSection: String;
						const List: TStrings) : Boolean;

var
	iIndex : Integer;

begin
	Result := true;

	with TIniFile.Create (sIniName) do
    	try
        	try
                WriteString (sSection, cItemCount, IntToStr (List.Count));

                for iIndex := 0 to List.Count - 1 do
                    WriteString (sSection, IntToStr (iIndex + 1),
                    			 List [iIndex]);

            except
            	Result := false;
            end; { try / except }

        finally
        	Free;
        end; { try / finally }
end; { SaveListToIni }

(* ---- *)

function SaveStrToInt (const sStr: String) : Integer;
{ Fängt Konvertierungsfehler ab und gibt im Fehlerfall "0" zurück }

begin
    try
        Result := StrToInt (sStr);

    except
        on EConvertError do
            Result := 0;
    end { try / except }
end; { SaveStrToInt }

(* ---- *)

function SaveWindowPosToIniFile (const hWindow: HWnd; const sIniName: String
{$IFDEF SUPPORTS_DEFAULTPARAMS}
  															  				= ''
{$ENDIF}
																	) : Boolean;

var
    IniFile : TIniFile;
    Placement : TWindowPlacement;

begin
	if (sIniName <> '') then
    	IniFile := TIniFile.Create (sIniName)
    else IniFile := TIniFile.Create (CreateIniName);

    try
{$IFNDEF CLR}
        { Struktur initialisieren }
        FillChar (Placement{%H-}, SizeOf (TWindowPlacement), #0);
{$ENDIF}

        Placement.length := SizeOf (TWindowPlacement);

{$IFNDEF CLR}
        GetWindowPlaceMent (hWindow, @Placement); { Fensterposition holen }
{$ELSE}
        GetWindowPlaceMent (hWindow, Placement); { Fensterposition holen }
{$ENDIF}

        with IniFile, Placement do
            try
                { Fenstergröße speichern }
                WriteInteger (cIniWindowPlacement, cIniShowCmd, showCmd);

                { Position speichern }
                WriteInteger (cIniWindowPlacement, cIniNormal_Left,
                			  rcNormalPosition.left);
                WriteInteger (cIniWindowPlacement, cIniNormal_Top,
                			  rcNormalPosition.top);
                WriteInteger (cIniWindowPlacement, cIniNormal_Right,
                			  rcNormalPosition.right);
                WriteInteger (cIniWindowPlacement, cIniNormal_Bottom,
                			  rcNormalPosition.bottom);
                WriteInteger (cIniWindowPlacement, cIniMin_X, ptMinPosition.X);
                WriteInteger (cIniWindowPlacement, cIniMin_Y, ptMinPosition.Y);
                WriteInteger (cIniWindowPlacement, cIniMax_X, ptMaxPosition.X);
                WriteInteger (cIniWindowPlacement, cIniMax_Y, ptMaxPosition.Y);

                Result := true;

            except
	            on E: Exception do
                	Result := false;
            end; { try / except }

    finally
        IniFile.Free;
    end; { try / finally }
end; { SaveWindowPosToIniFile }

(* ---- *)

function SetFileDate (const sFileName: TFileName;
					  const iFileDate: Integer) : Integer;

var
{$IFDEF CLR}
	FileHandle : TOpenedFile;
{$ELSE}
	hFile : THandle;
{$ENDIF}

begin
	Result := (-1);

    // FileGetDate, mit FileOpen öffnen und FileClose schließen
    hFile := FileOpen (sFileName, fmOpenWrite or fmShareDenyNone);

    if (hFile = HFILE_ERROR) then
        exit;

    try
        Result := FileSetDate (hFile, iFileDate);

    finally
        FileClose (hFile);
    end; { try / finally }
end; { SetFileDate }

(* ---- *)

function StrToIntEx (const sStr: String; const iDefValue: Integer
{$IFDEF SUPPORTS_DEFAULTPARAMS}
						 = 0
{$ENDIF}
					 ) : Integer;

begin
	if (sStr = '') then
        Result := iDefValue
    else
        try
            Result := StrToInt (sStr);

        except
            on EConvertError do
                Result := iDefValue;
        end; { try / except }
end; { StrToIntEx }

(* ---- *)

function WeekOfYear (ADate : TDateTime
{$IFDEF SUPPORTS_DEFAULTPARAMS}
						 = 0.0
{$ENDIF}
                     ) : Word;

{ FAQ1822D.txt How can I determine the week number of a given day in the year? }

var
    day, month, year : Word;
    FirstOfYear : TDateTime;

begin
	try
    	if (ADate = 0.0) then
        	ADate := Date
        else ADate := Trunc (ADate);

        DecodeDate (ADate, year, month, day);
        FirstOfYear := EncodeDate (year, 1, 1);
        Result := Trunc (ADate - FirstOfYear) div 7 + 1;

    except
    	Result := 1;
    end; { try / except }
end; { WeekOfYear }

(* ---- *)

end.









