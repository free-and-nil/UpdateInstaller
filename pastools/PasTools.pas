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
    {$UNDEF 16BIT}
{$ENDIF}

unit PasTools;

interface

{$IFDEF 16BIT}
uses BaseType;
{$ELSE}
uses BaseTypesU;
{$ENDIF}

type
  TLongType = record
    	case Word of
            0 : (Long: LongInt);
            1 : (Lo: Word; Hi: Word);
            2 : (B1: Byte; B2: Byte; B3: Byte; B4: Byte);
    end; { LongType }

const
	cDot = '.';

{$IFNDEF DELPHI6_UP}
    PathDelim = '\';
{$ENDIF}
    cInvalidCharactersCount = 9;
	cInvalidCharacters : array [1..cInvalidCharactersCount] of Char =
    	('\', '/', ':', '*', '?', '"', '<', '>', '|');

function AddBackSlash (const sPath: TFileString) : TFileString;

function AddExtension (const sFileName: TFileString;
					   sNewExt: String) : TFileString;

function AllUpper (S: String) : String;

function BeginsWith (const sStr: String; const chFirst: Char) : Boolean;

function ChangeExt (const sFileName: TFileString; sNewExt: String) : TFileString;

{$IFNDEF FPC}
  {$IFNDEF UNICODE}
function CharInSet (const chChar: Char; const CharSet: TCharSet) : Boolean;
  {$ENDIF}
{$ENDIF}

function CheckForParam (sParam: String {$IFDEF SUPPORTS_DEFAULTPARAMS} ;
						const iAtPos: Integer = (-1) {$ENDIF}
                        			                ) : Boolean;
{ Überprüfen, ob der Kommandozeilenparameter "sParam" eingegeben wurde. Dem
  Parameter muss auf der Kommandozeile ein '/' oder '-' vorangestellt sein,
  "sParam" enthält aber nur die Zeichenkette selbst (z.B. "debug"). Gross- und
  Kleinschreibung ist egal. Neuere Delphi-Versionen implementieren eine
  Funktion "FindCmdLineSwitch", die gleiches leistet.
  ->> sParam : Parameter, nach dem gesucht wird.
  <<- Result : TRUE, wenn der Parameter gefunden wurde, sonst FALSE. }

function CheckForParamEx (sParam: String; var iParamPos: Integer) : Boolean;
{ Analog zu "CheckForParam, gibt aber zusätzlich im Parameter "iParamPos" die
  Position zurück, an der das Parameter gefunden wurde. Die Funktion kann auch
  mit Parametern wie "/wait:10" umgehen.
  ->> sParam : Parameter, nach dem gesucht wird (ohne führendes '/' oder '-').
  ->> iParamPos : Position, an der der Parameter gefunden wurde.
  <<- Result : TRUE, wenn der Parameter gefunden wurde, sonst FALSE. }

procedure ClearPassword (var sPassword: String);

function ContainsInvalidChars (const sStr: String;
							   var chInvalid: Char) : Boolean;

function CreatePath (const sFullPath: TFileString) : Boolean;
{ Den Pfad "sFullPath" anlegen.
  ->> sFullPath : kompletter Pfad, der angelegt werden soll.
  <<- Result : TRUE, wenn erfolgreich, sonst FALSE. }

function EndsWith (const sStr: String; const chLast: Char) : Boolean;

{$IFNDEF CLR}
function EraseFile (const sFileName: TFileString) : Boolean;
{$ENDIF}

function FindVariable (const sStr: String; const chDelimiter: Char;
					   var iPos, iLen: Integer; var sVar: String) : Boolean;
{ In "sStr" nach einer Variable suchen, die durch das Zeichen "chDelimter"
  begrenzt wird. Achtung: Versagt, wenn mehr als eine Variable in der
  Zeichenkette steht!
  ->> sStr : Zeichenkette, die durchsucht wird.
  ->> chDelimiter : Begrenzungszeichen (am Anfang und Ende der Variable).
  <<- iPos : Fundstelle (zeigt auf "chDelimter").
  <<- iLen : Länge der Variable inklusive der "chDelimter" an Anfang und Ende.
  <<- sVar : Variablenname (ohne "chDelimiter").
  <<- Result : TRUE, wenn Variable gefunden wurde. }

function GetFileName (const sFullPath: TFileString) : TFileString;
{ Liefert nur den Dateinamen zurück.
  ->> sFullPath : Voller Pfad mit Dateinamen.
  <<- Result : Dateiname ohne Pfad. Leerstring, wenn kein Dateiname da. }

{$IFNDEF CLR}
	{$IFDEF 16BIT}
function GetFileSize (const sFileName: TFileString) : LongInt;
{ Dateigröße der Datei "sFileName" ermitteln.
  ->> sFileName : Name der Datei.
  <<- Result : Größe in Byte oder (-1) im Fehlerfall }
	{$ENDIF}
{$ENDIF}

function GetParamValue (const sParam: String; const sName: String;
						var sValue: String) : Boolean;
{$IFDEF SUPPORTS_OVERLOAD}
  													   overload;
{$ENDIF}
{ Wert eines Parameters im Format "/Name:Wert" zurückgeben.
  ->> sParam: Kompletter Parameter "(ParamStr (?)"
  ->> sName: Erwarteter Name des Parameters mit vorgestelltem "/"
  <<- sValue: Wert.
  <<- Result: True, wenn Name und Wert gefunden. }

{$IFDEF SUPPORTS_OVERLOAD}
function GetParamValue (const sParam: String; const sName: String;
						var iValue: Integer) : Boolean; overload;
{ Wert eines Parameters im Format "/Name:Wert" zurückgeben.
  ->> sParam: Kompletter Parameter "(ParamStr (?)"
  ->> sName: Erwarteter Name des Parameters mit vorgestelltem "/"
  <<- iValue: Wert.
  <<- Result: True, wenn Name und Wert gefunden. }
{$ENDIF}

function GetPath (const sFullPath: String) : String;
{ Gibt den Pfad ohne DateiNamen zurück.
  ->> sFullPath : Voller Pfad mit Dateinamen.
  <<- Result : Der Pfad, ohne '\' am Ende; Leerstring bei Fehler. }

{$IFDEF SUPPORTS_OVERLOAD}
function iif (const bValue: Boolean;
			  const iTrue,iFalse: Integer) : Integer; overload;
											{$IFDEF SUPPORTS_INLINE} inline; {$ENDIF}
{$IFDEF CPUX64}
function iif (const bValue: Boolean;
			  const uTrue, uFalse: UInt64) : UInt64; overload; inline;
{$ENDIF}
function iif (const bValue: Boolean;
			  const iTrue, iFalse: Int64) : Int64; overload;
											{$IFDEF SUPPORTS_INLINE} inline; {$ENDIF}
function iif (const bValue: Boolean;
			  const dTrue, dFalse: Double) : Double; overload;
											{$IFDEF SUPPORTS_INLINE} inline; {$ENDIF}
function iif (const bValue: Boolean;
			  const sTrue, sFalse: String) : String; overload;
											{$IFDEF SUPPORTS_INLINE} inline; {$ENDIF}
function iif (const bValue: Boolean;
			  const bTrue, bFalse: Boolean) : Boolean; overload;
											{$IFDEF SUPPORTS_INLINE} inline; {$ENDIF}
{$IFDEF UNICODE}
function iif (const bValue: Boolean;
			  const pChar1, pChar2: PChar) : PChar; overload; inline;
(**
function iif (const bValue: Boolean;
			  const pChar1, pChar2: PWideChar) : PWideChar; overload; inline;
**)
{$ENDIF}  // {$IFDEF UNICODE}
function iif (const bValue: Boolean;
			  const Pointer1, Pointer2: Pointer) : Pointer; overload;
											{$IFDEF SUPPORTS_INLINE} inline; {$ENDIF}
{$ENDIF}  // {$IFDEF SUPPORTS_OVERLOAD}

function Int2Str (const iInt: Integer) : String;

{$IFDEF VER70}
function IntToHex (const iInt: Integer) : String;
function IntToStr (const I: Longint) : String;
{$ENDIF}

function IsChar (const chChar: Char) : Boolean;
function IsDirectory (const sName: TFileString) : Boolean;
function IsFolderEmpty (const sFolder: TFileString) : Boolean;
function IsLowerCase (const chChar: Char) : Boolean;
function IsNumber (const chChar: Char) : Boolean;
function IsUpperCase (const chChar: Char) : Boolean;

function LastPos (chSearch: Char; const sStr: String) : Integer;
{ Sucht nach dem letzten Vorkommen von "chSearch" in "sStr".
  ->> chSearch : Zeichen, nach dem gesucht wird.
  ->> sStr : Zeichenkette, die durchsucht wird.
  <<- Result : Position des letzten Vorkommens von "chSearch" in "sStr" oder
               0 im Fehlerfall. }

function LastPosEx (chSearch: Char; const sStr: String;
					iStartPos: Integer) : Integer;
{ Sucht nach dem letzten Vorkommen von "chSearch" in "sStr".
  ->> chSearch : Zeichen, nach dem gesucht wird.
  ->> sStr : Zeichenkette, die durchsucht wird.
  ->> iStartPos : Position, ab der rückwärts gesucht wird.
  <<- Result : Position des letzten Vorkommens von "chSearch" in "sStr" oder
               0 im Fehlerfall. }

function LoCase (const AChar: Char) : Char;

function MatchString (const sPattern, sStr: String) : Boolean;

{$IFNDEF BP7}
	{$IFNDEF CLR}
function MemAlloc (uSize: NativeUInt) : Pointer;
function MemBufSize (pBuffer: Pointer) : NativeUInt;
procedure MemDispose (var pBuffer: Pointer);
        {$IFDEF SUPPORTS_OVERLOAD}
    overload;

procedure MemDispose (AObject: TObject); overload;
        {$ENDIF}  // SUPPORTS_OVERLOAD
	{$ENDIF}  // CLR
{$ENDIF}  // DELPHI1

function NextPos (const chSearch: Char; const sSource: String;
				  const iStartPos: Integer) : Integer;
{ Sucht nach dem nächsten Vorkommen des Buchstaben "chSearch" im Suchbegriff
  "sSource" ab der Startposition "iStartPos".
  ->> chSearch : Zu findender Buchstabe.
  ->> sSource : Zu durchsuchende Zeichenkette.
  ->> iStartPos : Position im String, ab der gesucht wird.
  <<- Result : Nächste Fundstelle, oder 0, wenn nicht gefunden. }

procedure NilVar (var P: Pointer); {$IFDEF SUPPORTS_INLINE} inline; {$ENDIF}
{$IFDEF SUPPORTS_OVERLOAD}
  	overload;

procedure NilVar (var T: TObject); {$IFDEF SUPPORTS_INLINE} inline; {$ENDIF}
  	overload;
{$ENDIF}

function NthPos (const sSearch, sSource: String;
				 const iIndex: Integer) : Integer;
{ Nach dem "iIndex" Vorkommen einer Zeichenkette suchen.
  ->> sSearch: Zu findender Begriff.
  ->> sSource : Zu durchsuchende Zeichenkette.
  ->> iIndex : Nach dem "iIndex"-ten Vorkommen suchen.
  <<- Result : Fundstelle, oder 0, wenn nicht gefunden. }

function ParamStrings : String;
{ Alle Parameter zurückgeben }

function PosEx (const sSearch, sSource: String; iStartPos: Integer
{$IFDEF SUPPORTS_OVERLOAD}
				= 1
{$ENDIF}
				) : Integer;
{ Die Suche ab der Position "iIndex" beginnen.
  ->> sSearch: Zu findender Begriff.
  ->> sSource : Zu durchsuchende Zeichenkette.
  ->> iStartPos : Position im String, ab der gesucht wird.
  <<- Result : Fundstelle, oder 0, wenn nicht gefunden. }

function PrevPos (const chSearch: Char; const sSource: String;
				  const iStartPos: Integer) : Integer;
{ Sucht nach dem vorletzten Vorkommen des Buchstaben "chSearch" im Suchbegriff
  "sSource" ab der Startposition "iStartPos".
  ->> chSearch : Zu findender Buchstabe.
  ->> sSource : Zu durchsuchende Zeichenkette.
  ->> iStartPos : Position im String, ab der gesucht wird.
  <<- Result : Nächste Fundstelle, oder 0, wenn nicht gefunden. }

function RemoveChar (const sString: String; const chRemove: Char) : String;
{ Alle Vorkommnisse des Zeichens "chChar" aus "sString" entfernen }

function RemoveExtension (const sFileName: TFileString) : TFileString;
{ Enfernt die Erweiterung aus einem Dateinamen.
  ->> sFileName : Dateiname, ggf. mit vollem Pfad.
  <<- Result : Dateiname ohne Erweiterung und '.', eventuell mit Pfad. }

function RemoveNonAbcNonNumbers (const sLine: String) : String;
{ Alles bis auf Buchstaben von A bis Z sowie Zahlen entfernen }

function RemoveNonNumbers (const sLine: String) : String;
{ Alle Buchstaben aus "sLine" entfernen, nur Zahlen zurückgeben }

function RemoveTrailingBlanks (sLine: String) : String;
{ Alle Leerzeichen und Tabs entfernen, die hinter dem letzten Zeichen in einer
  Zeile stehen. In neueren Delphi-Versionen siehe "Trim".
  ->> sLine : Textzeile.
  <<- Result : Textzeile ohne folgende Tabs und Leerzeichen. }

function ReplaceChar (sStr: String; const chSearch, chReplace: Char) : String;
{ Das Zeichen "chSearch" durch das Zeichen "chReplace" in der Zeichenkette
  "sStr" ersetzen }

function ReplaceString (sStr: String; const sSearch, sReplace: String) : String;
{ Die Zeichenkette "sSearch" durch "sReplace" in "sStr" ersetzen }
{$IFDEF SUPPORTS_OVERLOAD}
    overload;

function ReplaceString (const sStr: String; const asSearch: array of const;
                        const sReplace: String) : String; overload;
{$ENDIF}

function ReverseString (const sSource: String) : String;
{ Die Reihenfolge der Zeichen in einer Zeichenkette umdrehen.
  ->> sSource : Ausgangszeichenkette.
  <<- Result : Gedrehte Zeichenkette. }

{$IFDEF 16BIT}
procedure SetLength (var sStr: String; const wLen: Word);
{$ENDIF}

function SplitNameValuePair (const sNameValue: String; var sName, sValue: String;
                             const chNameValueSeparator: Char
{$IFDEF SUPPORTS_DEFAULTPARAMS}
                             = '='
{$ENDIF}
                             ) : Boolean;

function Str2Int (const sStr: String; var iInt: Integer) : Boolean;
{ Konvertiert eine Zeichenkette in eine Zahl.
  ->> sStr : Zeichenkette.
  ->> iInt : Integer, in dem das Ergebnis gespeichert wird.
  <<- Result : TRUE, wenn erfolgreich konvertiert. Wenn FALSE, dann ist "iInt"
  			   nicht definiert! }

{$IFDEF SUPPORTS_OVERLOAD}
	overload;

{$IFNDEF FPC}
	{$IFDEF UNICODE}

function Str2Int (const sStr: AnsiString; var iInt: Integer) : Boolean; overload;
function Str2Int (const sStr: AnsiString; var uInt: Cardinal) : Boolean; overload;

	{$ENDIF}  // UNICODE
{$ENDIF}  // FPC

function Str2Int (const sStr: String) : Boolean; overload;
function Str2Int (const sStr: String; var uInt: Cardinal) : Boolean; overload;

function Str2Int64 (const sStr: String; out iInt: Int64) : Boolean;
{$ENDIF}  // SUPPORTS_OVERLOAD

function Str2IntDef (const sStr: String; const iDef: Integer
{$IFDEF SUPPORTS_DEFAULTPARAMS}
															 = 0
																) : Integer;
{$ENDIF}

{$IFDEF SUPPORTS_DEFAULTPARAMS}
function Str2Int64Def (const sStr: String; const iDef: Int64 = 0) : Int64;
{$ENDIF}

function StrGetElement (const sStr: String; const chDivider: Char;
						const iIndex: Integer; var sElement: String) : Boolean;
{ Ermittelt ein Element aus einer durch Trennzeichen gegliederten Zeichenkette.
  ->> sStr: Zeichenkette
  ->> chDivider : Trennzeichen
  ->> iIndex: Index des Elements, der ermittelt werden soll. Beginnt mit 1.
  <<- sElement: Gesuchtes Element, wenn gefunden }

{$IFDEF 16BIT}
function Trim (const sStr: String) : String;
{$ENDIF}

implementation

{$IFNDEF BP7}
uses SysUtils;
{$ENDIF}

{$IFDEF DELPHI_XE_UP}
    {$POINTERMATH ON}
{$ENDIF}

(* ---- *)

function AddBackSlash (const sPath: TFileString) : TFileString;
begin
	if (sPath <> '') and (sPath [Length (sPath)] = PathDelim) then
    	AddBackSlash := sPath
    else AddBackSlash := sPath + PathDelim;
end; { AddBackSlash }

(* ---- *)

function AddExtension (const sFileName: TFileString;
					   sNewExt: String) : TFileString;
begin
{$IFDEF SUPPORTS_ASSERT}
	Assert ((sFileName <> '') and (sNewExt <> ''));
{$ENDIF}

	if (sNewExt [1] <> cDot) and (sFileName [Length (sFileName)] <> cDot) then
    	sNewExt := cDot {%H-}+ sNewExt;

    AddExtension := sFileName + sNewExt;
end; { AddExtension }

(* ---- *)

function AllUpper (S: String) : String;

var
    iIndex : Integer;

begin
    for iIndex := 1 to Length (S) do
        {%H-}S [iIndex] := UpCase (S [iIndex]);

    AllUpper := S;
end; { AllUpper }

(* ---- *)

function BeginsWith (const sStr: String; const chFirst: Char) : Boolean;
begin
	if (sStr <> '') then
    	BeginsWith := sStr [1] = chFirst
    else BeginsWith := false;
end; { BeginsWith }

(* ---- *)

function ChangeExt (const sFileName: TFileString; sNewExt: String) : TFileString;

begin
{$IFDEF SUPPORTS_ASSERT}
	Assert ((sFileName <> '') and (sNewExt <> ''));
{$ENDIF}

	if (sNewExt [1] <> cDot) then
    	sNewExt := cDot {%H-}+ sNewExt;

	ChangeExt := RemoveExtension (sFileName) + sNewExt;
end; { ChangeExtension }

(* ---- *)

{$IFNDEF FPC}
  {$IFNDEF UNICODE}
function CharInSet (const chChar: Char; const CharSet: TCharSet) : Boolean;
begin
	Result := chChar in CharSet;
end; { CharInSet }
 {$ENDIF}
{$ENDIF}

(* ---- *)

function CheckForParam (sParam: String {$IFDEF SUPPORTS_DEFAULTPARAMS} ;
						const iAtPos: Integer = (-1) {$ENDIF}
                        			                ) : Boolean;
{ Überprüfen, ob der Kommandozeilenparameter "sParam" eingegeben wurde. Dem
  Parameter muss auf der Kommandozeile ein '/' oder '-' vorangestellt sein,
  Gross- und Kleinschreibung ist egal.
  ->> sParam : Parameter, nach dem gesucht wird.
  <<- Result : TRUE, wenn der Parameter gefunden wurde, sonst FALSE.

  Siehe auch "FindCmdLineSwitch" in Delphi. }

var
    iParamPos : Integer;
    bResult : Boolean;

begin
    bResult := CheckForParamEx (sParam, iParamPos{%H-});

{$IFDEF SUPPORTS_DEFAULTPARAMS}
    if (bResult) and (iAtPos > 0) then
    	bResult := iParamPos = iAtPos;
{$ENDIF}

	CheckForParam := bResult;
end; { CheckForParam }

(* ---- *)

function CheckForParamEx (sParam: String; var iParamPos: Integer) : Boolean;

var
	iColonPos, iIndex : Integer;
    sCurParam : String;

begin
	CheckForParamEx := false;

    if (ParamCount > 0) then
    begin
    	{%H-}sParam := AllUpper (sParam);

    	for iIndex := 1 to ParamCount do
        begin
        	sCurParam := AllUpper (ParamStr (iIndex));

            if (sCurParam [1] = '-') or (sCurParam [1] = '/') then
            	Delete (sCurParam, 1, 1)
            else Continue;

            iColonPos := Pos (':', sCurParam);

            if (iColonPos > 0) then
            	SetLength (sCurParam, iColonPos - 1);

            if (sCurParam = sParam) then
            begin
                iParamPos := iIndex;
            	CheckForParamEx := true;
                exit;
            end; { if }
        end; { for }
    end; { if }
end; { CheckForParamEx }

(* ---- *)

procedure ClearPassword (var sPassword: String);
begin
    if (sPassword <> '') then
    begin
{$IFNDEF BP7}
        FillChar (PChar (sPassword)^, Length (sPassword) * SizeOf (Char), #0);
{$ELSE}
        FillChar (PChar (@sPassword[1])^, Length (sPassword), #0);
{$ENDIF}
        sPassword := '';
    end; { if }
end; { TBiosSettings.ClearPassword }

(* ---- *)

function ContainsInvalidChars (const sStr: String;
							   var chInvalid: Char) : Boolean;

var
	iIndex, iChar : Integer;

begin
    ContainsInvalidChars := false;

    if (sStr = '') then
    	exit;

    for iChar := 1 to cInvalidCharactersCount do
    	for iIndex := 1 to Length (sStr) do
        	if (sStr [iIndex] = cInvalidCharacters [iChar]) then
            begin
            	chInvalid := sStr [iIndex];
            	ContainsInvalidChars := true;
                exit;
            end; { if }
end; { ContainsInvalidChars }

(* ---- *)

function CreatePath (const sFullPath: TFileString) : Boolean;
{ Den Pfad "sFullPath" anlegen.
  ->> sFullPath : kompletter Pfad, der angelegt werden soll.
  <<- Result : TRUE, wenn erfolgreich, sonst FALSE. }

  	(* ---- *)

    function CreateNewDir (const sPath: String) : Boolean;

    begin
        CreateNewDir := false;

        {* Prüfen, ob Verzeichnis schon existiert *}
        ChDir (sPath);

        if (IOResult = 0) then
        begin { Verzeichnis gibt's schon }
            CreateNewDir := true;
            exit;
        end; { if }

        {* Verzeichnis anlegen *}
        MkDir (sPath);

        if (IOResult = 0) then
            CreateNewDir := true;
    end; { CreateNewDir }

    (* ---- *)

var
    i : Integer;

begin { CreatePath }
    CreatePath := false;

    if (Length (sFullPath) <> 0) then { Auf Laufwerksbuchstaben prüfen }
        if not ((sFullPath [2] = ':') and (sFullPath [3] = '\')) then
            exit;


        for i := 4 to Length (sFullPath) do
	        if (sFullPath [i] = '\') then
    	        if (not CreateNewDir (Copy (sFullPath, 1, i - 1))) then
        	        exit;

    { Kompletten Pfad zuletzt prüfen }
    if (sFullPath [Length (sFullPath)] <> '\') then
        if (not CreateNewDir (sFullPath)) then
        	exit;

    CreatePath := true;
end; { CreatePath }

(* ---- *)

function EndsWith (const sStr: String; const chLast: Char) : Boolean;
begin
	if (sStr <> '') then
    	EndsWith := sStr [Length (sStr)] = chLast
    else EndsWith := false;
end; { EndsWith }

(* ---- *)

{$IFNDEF CLR}
function EraseFile (const sFileName: TFileString) : Boolean;

var
	fFile : File;

begin
	Assign (fFile, sFileName);

    Erase (fFile);

    EraseFile := IOResult = 0;
end; { EraseFile }
{$ENDIF}

(* ---- *)

function FindVariable (const sStr: String; const chDelimiter: Char;
					   var iPos, iLen: Integer; var sVar: String) : Boolean;

var
	iNextPos : Integer;

begin
{$IFDEF SUPPORTS_ASSERT}
    Assert (sStr <> '');
{$ENDIF}

	FindVariable := false;

    if (sStr = '') then
    	exit;

    iPos := Pos (chDelimiter, sStr);

    if (iPos > 0) then
    begin
    	iNextPos := NextPos (chDelimiter, sStr, iPos + 1);

        if (iNextPos > 0) then
        begin
            FindVariable := true;
            iLen := (iNextPos - iPos) + 1;
            sVar := Copy (sStr, iPos + 1, iLen - 2);
        end; { if }
    end; { if }
end; { FindVariable }

(* ---- *)

function GetFileName (const sFullPath: TFileString) : TFileString;
{ Liefert nur den Dateinamen zurück.
  ->> sFullPath : Voller Pfad mit Dateinamen.
  <<- Result : Dateiname ohne Pfad. Leerstring, wenn kein Dateiname da. }

var
    iPos : Integer;

begin
    GetFileName := '';

	if (Length (sFullPath) > 0) then
    begin
        iPos := LastPos (PathDelim, sFullPath);

        if (iPos > 0) and (iPos < Length (sFullPath)) then
            GetFileName := Copy (sFullPath, iPos + 1,
                                 Length (sFullPath) - iPos);
    end; { if }
end; { GetFileName }

(* ---- *)

{$IFNDEF CLR}
	{$IFDEF 16BIT}
function GetFileSize (const sFileName: TFileString) : LongInt;

var
	AFile : File of Byte;
    byOldFileMode : Byte;

begin
	byOldFileMode := FileMode;
    FileMode := 0;

	System.Assign (AFile, sFileName);

    System.Reset (AFile);

    if (IOResult = 0) then
    	GetFileSize := FileSize (AFile)
    else GetFileSize := (-1);

    System.Close (AFile);

    FileMode := byOldFileMode;
end; { GetFileSize }
	{$ENDIF}
{$ENDIF}

(* ---- *)

function GetPath (const sFullPath: String) : String;
{ Gibt den Pfad ohne DateiNamen zurück.
  ->> sFullPath : Voller Pfad mit Dateinamen.
  <<- Result : Der Pfad, ohne '\' am Ende; Leerstring bei Fehler. }

var
    iPos : Integer;
    sResult : String;

begin
    sResult := '';

    if (Length (sFullPath) > 0) then
    begin
        iPos := LastPos (PathDelim, sFullPath);

        if (iPos > 0) then
        begin
			sResult := sFullPath;
            SetLength (sResult, iPos - 1);
        end; { if }
    end; { if }

    GetPath := sResult;
end; { GetPath }

(* ---- *)

function GetParamValue (const sParam: String; const sName: String;
						var sValue: String) : Boolean;
{ Wert eines Parameters im Format "/Name:Wert" zurückgeben.
  ->> sParam: Kompletter Parameter "(ParamStr (?)"
  ->> sName: Erwarteter Name des Parameters mit vorgestelltem "/"
  <<- sValue: Wert.
  <<- Result: True, wenn Name und Wert gefunden. }

var
	iPos : Integer;

begin
{$IFDEF SUPPORTS_ASSERT}
	Assert (sParam <> '');
    Assert (sName <> '');
{$ENDIF}

	GetParamValue := false;

	iPos := Pos (':', sParam);

    if (iPos = 0) then
    	exit;

    if (AllUpper (Copy (sParam, 1, iPos - 1)) <> AllUpper (sName)) then
    	exit;

    if not (Length (sParam) > iPos) then
    	exit;

    sValue := Copy (sParam, iPos + 1, Length (sParam) - iPos);

    GetParamValue := true;
end; { GetParamValue }

(* ---- *)

{$IFDEF SUPPORTS_OVERLOAD}
function GetParamValue (const sParam: String; const sName: String;
						var iValue: Integer) : Boolean;
{ Wert eines Parameters im Format "/Name:Wert" zurückgeben.
  ->> sParam: Kompletter Parameter "(ParamStr (?)"
  ->> sName: Erwarteter Name des Parameters mit vorgestelltem "/".
  <<- iValue: Wert.
  <<- Result: True, wenn Name und Wert gefunden. }

var
	sValue : String;

begin
{$IFDEF SUPPORTS_ASSERT}
	Assert (sParam <> '');
    Assert (sName <> '');
{$ENDIF}

	if (GetParamValue (sParam, sName, sValue{%H-})) then
	    Result := Str2Int (sValue, iValue)
	else Result := false;
end; { GetParamValue }

(* ---- *)

function iif (const bValue: Boolean; const iTrue, iFalse: Integer) : Integer;
begin
    if (bValue) then
    	Result := iTrue
    else Result := iFalse;
end; { iif }

(* ---- *)

{$IFDEF CPUX64}
function iif (const bValue: Boolean; const uTrue, uFalse: UInt64) : UInt64;
begin
    if (bValue) then
    	Result := uTrue
    else Result := uFalse;
end; { iif }
{$ENDIF}

(* ---- *)

function iif (const bValue: Boolean; const iTrue, iFalse: Int64) : Int64;
begin
    if (bValue) then
    	Result := iTrue
    else Result := iFalse;
end; { iif }

(* ---- *)

function iif (const bValue: Boolean; const dTrue, dFalse: Double) : Double;
begin
    if (bValue) then
    	Result := dTrue
    else Result := dFalse;
end; { iif }

(* ---- *)

function iif (const bValue: Boolean; const sTrue, sFalse: String) : String;
begin
    if (bValue) then
    	Result := sTrue
    else Result := sFalse;
end; { iif }

(* ---- *)

function iif (const bValue: Boolean; const bTrue, bFalse: Boolean) : Boolean;
begin
    if (bValue) then
    	Result := bTrue
    else Result := bFalse;
end; { iif }

(* ---- *)

{$IFDEF UNICODE}
function iif (const bValue: Boolean; const pChar1, pChar2: PChar) : PChar;
begin
	if (bValue) then
    	Result := pChar1
    else Result := pChar2;
end; { iif }

(* ---- *)

(**
function iif (const bValue: Boolean;
			  const pChar1, pChar2: PWideChar) : PWideChar;
begin
	if (bValue) then
    	Result := pChar1
    else Result := PChar2;
end; { iff }
**)
{$ENDIF}  // {$IFDEF UNICODE}

(* ---- *)

function iif (const bValue: Boolean;
			  const Pointer1, Pointer2: Pointer) : Pointer;
begin
	if (bValue) then
    	Result := Pointer1
    else Result := Pointer2;
end; { iif }

{$ENDIF}  // {$IFDEF SUPPORTS_OVERLOAD4}

(* ---- *)

function Int2Str (const iInt: Integer) : String;
{$IFDEF UNICODE}

var
	sResult : AnsiString;

begin
    Str (iInt, sResult);
    Result := String (sResult);
{$ELSE}
begin
	Str (iInt, Result);
{$ENDIF}
end; { Int2Str }

(* ---- *)

{$IFDEF VER70}
function IntToHex (const iInt: Integer) : String;

const
	hexChars: array [0..$F] of Char = '0123456789ABCDEF';

var
	sHex : String;

begin
	sHex := hexChars [Hi (iInt) shr 4] + hexChars [Hi (iInt) and $F] +
    		hexChars [Lo (iInt) shr 4] + hexChars [Lo (iInt) and $F];

	while (Length (sHex) > 1) and (sHex [1] = '0') do
    	Delete (sHex, 1, 1);

	IntToHex := '$' + sHex;
end; { IntToHex }

(* ---- *)

function IntToStr (const I: Longint) : String;

var
	S : String [11];

begin
	Str (I, S);
  	IntToStr := S;
end; { IntToStr }
{$ENDIF}

(* ---- *)

function IsChar (const chChar: Char) : Boolean;
begin { Funktioniert nur für A bis Z }
	IsChar := ((Byte (chChar) >= 97) and (Byte (chChar) <= 122)) or
    		  ((Byte (chChar) >= 65) and (Byte (chChar) <= 90));
end; { IsChar }

(* ---- *)

function IsDirectory (const sName: TFileString) : Boolean;

var
    SearchRec : TSearchRec;

begin
    FillChar (SearchRec{%H-}, SizeOf (TSearchRec), #0);

    if (FindFirst (sName, faDirectory, SearchRec) = 0) then
    begin
        IsDirectory := true;

{$IFNDEF VER70}
        SysUtils.FindClose (SearchRec);
{$ENDIF}
    end { if }
    else IsDirectory := false;
end; { IsDirectory }

(* ---- *)

function IsFolderEmpty (const sFolder: TFileString) : Boolean;

var
    SearchRec : TSearchRec;

begin
    FillChar (SearchRec{%H-}, SizeOf (TSearchRec), #0);

    if (FindFirst (AddBackSlash (sFolder) + '*.*',
                   faAnyFile, SearchRec) = 0) then
    begin
        IsFolderEmpty := true;

        repeat
            if (SearchRec.Name <> '.') and (SearchRec.Name <> '..') then
            begin
                IsFolderEmpty := false;
                Break;
            end; { if }
        until (FindNext (SearchRec) <> 0);

{$IFNDEF VER70}
        SysUtils.FindClose (SearchRec);
{$ENDIF}
    end { if }
    else IsFolderEmpty := false;
end; { IsFolderEmpty }

(* ---- *)

function IsLowerCase (const chChar: Char) : Boolean;
begin
	IsLowerCase := (Byte (chChar) >= 97) and (Byte (chChar) <= 122);
end; { IsLowerCase }

(* ---- *)

function IsNumber (const chChar: Char) : Boolean;
begin
	IsNumber := (Byte (chChar) >= 48) and (Byte (chChar) <= 57);
end; { IsNumber }

(* ---- *)

function IsUpperCase (const chChar: Char) : Boolean;
begin
	IsUpperCase := (Byte (chChar) >= 65) and (Byte (chChar) <= 90);
end; { IsUpperCase }

(* ---- *)

function LastPos (chSearch: Char; const sStr: String) : Integer;
{ Sucht nach dem letzten Vorkommen von "chSearch" in "sStr".
  ->> chSearch : Zeichen, nach dem gesucht wird.
  ->> sStr : Zeichenkette, die durchsucht wird.
  <<- Result : Position des letzten Vorkommens von "chSearch" in "sStr" oder
               0 im Fehlerfall. }

var
    iPos : Integer;

begin
    LastPos := 0;

    if (Length (sStr) > 0) then
        for iPos := Length (sStr) downto 1 do
            if (sStr [iPos] = chSearch) then
            begin
                LastPos := iPos;
                exit;
            end; { if }
end; { LastPos }

(* ---- *)

function LastPosEx (chSearch: Char; const sStr: String;
					iStartPos: Integer) : Integer;

var
    iLen, iPos : Integer;

begin
    LastPosEx := 0;

    iLen := Length (sStr);

    if (iLen > 0) then
    begin
    	if (iStartPos > iLen) then
        	iStartPos := iLen;

        for iPos := iStartPos downto 1 do
            if (sStr [iPos] = chSearch) then
            begin
                LastPosEx := iPos;
                Break;
            end; { if }
    end; { if }
end; { LastPosEx }

(* ---- *)

function LoCase (const AChar: Char) : Char;
{ Berücksichtigt keine Umlaute etc. }

begin
	if (Byte (AChar) >= 65) and (Byte (AChar) <= 90) then
    	LoCase := Char (Byte (AChar) + 32)
    else LoCase := AChar;
end; { LoCase }

(* ---- *)


function MatchString (const sPattern, sStr: String) : Boolean;
{ Tries to find "sPattern" in "sStr"; case sensitive compare.
  Wildcards "?" and "*" allowed.
  http://www.boyet.com/Articles/Simplepatternmatching.html

  Delphi: See "TMask" in "Masks.pas" }

var
	iPatternLen, iStrLen : Integer;

    (* ---- *)

	function CheckAllAsterisks (const iStartIndex: Integer) : Boolean;

	var
		iIndex : Integer;

	begin
		for iIndex := iStartIndex to iPatternLen do
			if (sPattern [iIndex] <> '*') then
			begin
				CheckAllAsterisks := false;
				exit;
			end; { if }

		CheckAllAsterisks := true;
	end; { CheckAllAsterisks }

	(* ---- *)

	function DoMatch (const iPatternIndex, iStrIndex: Integer) : Boolean;

	var
		bTestMatch : Boolean;

	begin
		if (iStrIndex > iStrLen) then
        begin
			if (iPatternIndex > iPatternLen) then
				DoMatch := true
			else DoMatch := CheckAllAsterisks (iPatternIndex);

            exit;
        end; { if }

		if (iPatternIndex > iPatternLen) then
		begin
			DoMatch := false;
			exit;
		end; { if }

		if (sPattern [iPatternIndex] = '?') then
            DoMatch := DoMatch (iPatternIndex + 1, iStrIndex + 1)
        else if (sPattern [iPatternIndex] = '*') then
        begin
            if (iPatternIndex = iPatternLen) then
            begin
                DoMatch := true;
                exit;
            end; { if }

            bTestMatch := DoMatch (iPatternIndex + 1, iStrIndex);

            if not (bTestMatch) then
                bTestMatch := DoMatch (iPatternIndex, iStrIndex + 1);

            DoMatch := bTestMatch;
        end { else if }
        else if (sPattern [iPatternIndex] <> sStr [iStrIndex]) then
            DoMatch := false
        else DoMatch := DoMatch (iPatternIndex + 1, iStrIndex + 1);
	end; { DoMatch }

    (* ---- *)

begin { MatchString }
    iPatternLen := Length (sPattern);
    iStrLen := Length (sStr);

	MatchString := DoMatch (1, 1);
end; { MatchString }

(* ---- *)

{$IFNDEF BP7}
	{$IFNDEF CLR}

function MemAlloc (uSize: NativeUInt) : Pointer;
{ Siehe Online-Hilfe, analog zu "StrAlloc" }

var
    pResult : Pointer;

begin
    Inc (uSize, SizeOf (Pointer));

    GetMem (pResult, uSize);

    if (pResult = NIL) then
    begin
	    MemAlloc := NIL;
    	exit;
    end; { if }

    NativeUInt (pResult^) := uSize;

    Inc ({%H-}{$IFNDEF FPC} NativeUInt {$ENDIF} (pResult), SizeOf (Pointer));

    MemAlloc := pResult;
end; { MemAlloc }

(* ---- *)

function MemBufSize (pBuffer: Pointer) : NativeUInt;
{ Siehe Online-Hilfe, analog zu "StrBufSize" }

begin
	if (pBuffer = NIL) then
    begin
        MemBufSize := 0;
        exit;
    end; { if }

    Dec ({%H-}{$IFNDEF FPC} NativeUInt {$ENDIF} (pBuffer), SizeOf (Pointer));
    MemBufSize := NativeUInt (pBuffer^) - SizeOf (Pointer);
end; { MemBufSize }

(* ---- *)

procedure MemDispose (var pBuffer: Pointer);
{ Siehe Online-Hilfe, analog zu "StrDispose" }

var
	uSize : NativeUInt;

begin
    if (pBuffer <> NIL) then
    begin
        Dec ({%H-}{$IFNDEF FPC} NativeUInt {$ENDIF} (pBuffer), SizeOf (Pointer));
        uSize := NativeUInt (pBuffer^);
        FreeMem (pBuffer, uSize);

        pBuffer := NIL;
    end; { if }
end; { MemDispose }

 (* ---- *)

{$IFDEF SUPPORTS_OVERLOAD}
procedure MemDispose (AObject: TObject); overload;

var
    uSize : NativeUInt;
    pObject : Pointer;

begin
    if (AObject <> NIL) then
    begin
        pObject := Pointer (AObject);
        Dec ({$IFNDEF FPC} NativeUInt {$ENDIF} (pObject), SizeOf (Pointer));
        uSize := NativeUInt (pObject^);
        FreeMem (Pointer (pObject), uSize);
    end; { if }
end; { MemDispose }

{$ENDIF}  // DELPHI4

	{$ENDIF}  // CLR
{$ENDIF}  // DELPHI1

(* ---- *)

function NextPos (const chSearch: Char; const sSource: String;
				  const iStartPos: Integer) : Integer;
{ Sucht nach dem nächsten Vorkommen des Buchstaben "chSearch" im Suchbegriff
  "sSource" ab der Startposition "iStartPos".
  ->> chSearch : Zu findender Buchstabe.
  ->> sSource : Zu durchsuchende Zeichenkette.
  ->> iStartPos : Position im String, ab der gesucht wird.
  <<- Result : Nächste Fundstelle, oder 0, wenn nicht gefunden. }

var
	iPos : Integer;

begin
	NextPos := 0;

	if (iStartPos > Length (sSource)) then
		exit;

	for iPos := iStartPos to Length (sSource) do
		if (sSource [iPos] = chSearch) then
		begin
			NextPos := iPos;
			exit;
		end; { if }
end; { NextPos }

(* ---- *)

procedure NilVar (var P: Pointer);
begin
	P := NIL;
end; { NilVar }

(* ---- *)
{$IFDEF SUPPORTS_OVERLOAD}
procedure NilVar (var T: TObject);
begin
	T := NIL;
end; { NilVar }
{$ENDIF}

(* ---- *)

function NthPos (const sSearch, sSource: String;
				 const iIndex: Integer) : Integer;

var
	iPos, iCount : Integer;

begin
{$IFDEF SUPPORTS_ASSERT}
	Assert (iIndex > 0);
{$ENDIF}

	iCount := 0;
    iPos := 1;

    repeat
    	iPos := PosEx (sSearch, sSource, iPos);

        if (iPos > 0) then
        begin
        	NthPos := iPos;

        	Inc (iCount);
            Inc (iPos);
        end { if }
        else
        begin
        	NthPos := 0;
            exit;
        end; { else }
    until (iCount = iIndex);
end; { NthPos }

(* ---- *)

function ParamStrings : String;
{ Alle Parameter zurückgeben }

var
	iIndex : Integer;
{$IFDEF VER70}
  	Result : String;
{$ENDIF}

begin
    Result := '';

    if (ParamCount = 0) then
    	exit
    else
    	for iIndex := 1 to ParamCount do
        	if (Pos (' ', ParamStr (iIndex)) > 0) then
            	Result := Result + '"' + ParamStr (iIndex) + '" '
            else Result := Result + ParamStr (iIndex) + ' ';

{$IFNDEF BP7}
	SetLength (Result, Length (Result) - 1);
{$ELSE}
	ParamStrings := Copy (Result, 1, Length (Result) - 1);
{$ENDIF}
end; { ParamStrings }

(* ---- *)

function PosEx (const sSearch, sSource: String; iStartPos: Integer
{$IFDEF SUPPORTS_DEFAULTPARAMS}
				= 1
{$ENDIF}
				) : Integer;

var
	iLen, iPos : Integer;

begin
	PosEx := 0;

	if (iStartPos <= 1) then
    	PosEx := Pos (sSearch, sSource)
    else
    begin
    	iLen := Length (sSource);

    	if (iStartPos <= iLen) then
        begin
	    	iPos := Pos (sSearch, Copy (sSource, iStartPos,
            			 iLen - Pred (iStartPos)));

            if (iPos > 0) then
            	PosEx := Pred (iStartPos) + iPos;
        end; { if }
    end; { else }
end; { PosEx }

(* ---- *)

function PrevPos (const chSearch: Char; const sSource: String;
				  const iStartPos: Integer) : Integer;

var
	iIndex : Integer;

begin
	PrevPos := 0;

	if (iStartPos > Length (sSource)) then
		exit;

	for iIndex := iStartPos downto 1 do
		if (sSource [iIndex] = chSearch) then
		begin
			PrevPos := iIndex;
			exit;
		end; { if }
end; { PrevPos }

(* ---- *)

function RemoveChar (const sString: String; const chRemove: Char) : String;

var
	sResult : String;
    iIndex : Integer;

begin
	sResult := '';

    for iIndex := 1 to Length (sString) do
    	if (sString [iIndex] <> chRemove) then
        	sResult := sResult + sString [iIndex];

    RemoveChar := sResult;
end; { RemoveChar }

(* ---- *)

function RemoveExtension (const sFileName: TFileString) : TFileString;
{ Enfernt die Erweiterung aus einem Dateinamen.
  ->> sFileName : Dateiname, ggf. mit vollem Pfad.
  <<- Result : Dateiname ohne Erweiterung und '.', eventuell mit Pfad. }

var
	iExtPos, iBackSlashPos : Integer;

begin
{$IFDEF SUPPORTS_ASSERT}
	Assert (sFileName <> '');
{$ENDIF}

    RemoveExtension := sFileName;

    iExtPos := LastPos (cDot, sFileName);
    iBackSlashPos := LastPos (PathDelim, sFileName);

    if (iExtPos > 1) then { Punkt gefunden }
        if (iExtPos > iBackSlashPos) then { Gehört der Punkt zum Dateinamen? }
            RemoveExtension := Copy (sFileName, 1, iExtPos - 1);
end; { RemoveExtension }

(* ---- *)

function RemoveNonAbcNonNumbers (const sLine: String) : String;
{ Alles bis auf Buchstaben von A bis Z sowie Zahlen entfernen }

var
	sResult : String;
    iIndex : Integer;
    byChar : Byte;

begin
	sResult := '';

    for iIndex := 1 to Length (sLine) do
    begin
    	byChar := Byte (UpCase (sLine [iIndex]));

    	if ((byChar >= Byte ('0')) and (byChar <= Byte ('9'))) or
    	   ((byChar >= Byte ('A')) and (byChar <= Byte ('Z'))) then
        	sResult := sResult + sLine [iIndex];
    end; { for }

    RemoveNonAbcNonNumbers := sResult;
end; { RemoveNonAbcNonNumbers }

(* ---- *)

function RemoveNonNumbers (const sLine: String) : String;

var
	sResult : String;
    iIndex : Integer;

begin
	sResult := '';

    for iIndex := 1 to Length (sLine) do
    	if (Byte (sLine [iIndex]) >= Byte ('0')) and
           (Byte (sLine [iIndex]) <= Byte ('9')) then
        	sResult := sResult + sLine [iIndex];

	RemoveNonNumbers := sResult;
end; { RemoveNonNumbers }

(* ---- *)

function RemoveTrailingBlanks (sLine: String) : String;

var
    iPos, iNewLen : Integer;

begin
{$IFDEF FPC}
    Result := '';
{$ENDIF}

    if (sLine = '') then
    begin
        RemoveTrailingBlanks := '';
        exit;
    end; { if }

    iNewLen := (-1);

    for iPos := Length (sLine) downto 1 do
        if (Byte (sLine [iPos]) <= 32) then
            iNewLen := iPos - 1
        else
        begin
            if (iNewLen >= 0) then
                SetLength (sLine, iNewLen);

            RemoveTrailingBlanks := sLine;

            exit;
        end; { else }
end; { RemoveTrailingBlanks }

(* ---- *)

function ReplaceChar (sStr: String; const chSearch, chReplace: Char) : String;

var
	iIndex : Integer;

begin
	if (sStr <> '') then
		for iIndex := 1 to Length (sStr) do
        	if (sStr [iIndex] = chSearch) then
            	sStr [iIndex] := chReplace;

    ReplaceChar := sStr;
end; { ReplaceChar }

(* ---- *)

function ReplaceString (sStr: String; const sSearch, sReplace: String) : String;

var
	iPos : Integer;

begin
	Assert (sReplace <> #0);

	if (sStr <> '') and (sSearch <> sReplace) then
    begin
    	iPos := Pos (sSearch, sStr);

        while (iPos > 0) do
        begin
        	Delete (sStr{%H-}, iPos, Length (sSearch));

            if (sReplace <> '') then
            	Insert (sReplace, sStr, iPos);

	    	iPos := Pos (sSearch, sStr);
        end; { while }
    end; { if }

	ReplaceString := sStr;
end; { ReplaceString }

(* ---- *)

{$IFDEF SUPPORTS_OVERLOAD}
function ReplaceString (const sStr: String; const asSearch: array of const;
                        const sReplace: String) : String;
{ Die Zeichenketten in "asSearch" müssen mit einem "#0" abgeschlossen sein!
  Es dürfen keine reinen "Char"-Variablen übergeben werden! }                        

var
    iIndex : Integer;
    sSearch : String;

begin
    Assert (Length (asSearch) > 0);

    Result := sStr;

    for iIndex := 0 to High (asSearch) do
    begin
{$IFDEF UNICODE}
		sSearch := PChar (asSearch [iIndex].VUnicodeString);
{$ELSE}
		sSearch := asSearch [iIndex].VPChar;
{$ENDIF}
		Result := ReplaceString (Result, sSearch, sReplace);
	end; { for }
end; { ReplaceString }
{$ENDIF}

(* ---- *)

function ReverseString (const sSource: String) : String;
{ Die Reihenfolge der Zeichen in einer Zeichenkette umdrehen.
  ->> sSource : Ausgangszeichenkette.
  <<- Result : Gedrehte Zeichenkette. }

var
	iIndex, iLen : Integer;
{$IFNDEF MSWINDOWS}
    sResult : String;
{$ENDIF}

begin
	iLen := Length (sSource);

    if (iLen <= 1) then
        ReverseString := sSource
    else
    begin
{$IFNDEF BP7}
    	SetLength (Result, iLen);

        for iIndex := 1 to iLen do
            Result [iLen - Pred (iIndex)] := sSource [iIndex];
{$ELSE}
        for iIndex := 1 to iLen do
            sResult [iLen - Pred (iIndex)] := sSource [iIndex];

        ReverseString := sResult;
{$ENDIF}
    end; { else }
end; { ReverseString }

(* ---- *)

{$IFDEF 16BIT}
procedure SetLength (var sStr: String; const wLen: Word);
{ Laenge eines Pascal-Strings setzen }

begin
    sStr [0] := Char (wLen);
end; { SetLength }
{$ENDIF}

(* ---- *)

function Str2Int (const sStr: String; var iInt: Integer) : Boolean;

var
	iCode : Integer;

begin
	Val (sStr, iInt, iCode);
    Str2Int := iCode = 0;
end; { Str2Int }

(* ---- *)

function SplitNameValuePair (const sNameValue: String; var sName, sValue: String;
                             const chNameValueSeparator: Char
{$IFDEF SUPPORTS_DEFAULTPARAMS}
                             = '='
{$ENDIF}
                             ) : Boolean;

var
    iPos : Integer;

begin
    iPos := Pos (chNameValueSeparator, sNameValue);

    if (iPos > 1) then
    begin
        sName := Trim (Copy (sNameValue, 1, iPos - 1));
        sValue := Trim (Copy (sNameValue, iPos + 1, Length (sNameValue) - iPos));

        SplitNameValuePair := sName <> '';
    end { if }
    else SplitNameValuePair := false;
end; { SplitNameValuePair }

(* ---- *)

{$IFNDEF FPC}
	{$IFDEF UNICODE}
function Str2Int (const sStr: AnsiString; var iInt: Integer) : Boolean;

var
	iCode : Integer;

begin
	Val (String (sStr), iInt, iCode);
    Str2Int := iCode = 0;
end; { Str2Int }

(* ---- *)

function Str2Int (const sStr: AnsiString; var uInt: Cardinal) : Boolean;

var
	iCode : Integer;

begin
	Val (String (sStr), uInt, iCode);
    Str2Int := iCode = 0;
end; { Str2Int }
	{$ENDIF}
{$ENDIF}

(* ---- *)

{$IFDEF SUPPORTS_OVERLOAD}
function Str2Int (const sStr: String; var uInt: Cardinal) : Boolean; overload;

var
	iCode : Integer;

begin
	Val (sStr, uInt, iCode);
    Str2Int := iCode = 0;
end; { Str2Int }

(* ---- *)

function Str2Int (const sStr: String) : Boolean;

var
	iInt : Integer;

begin
	Result := Str2Int (sStr, iInt{%H-});
end; { Str2Int }

(* ---- *)

function Str2Int64 (const sStr: String; out iInt: Int64) : Boolean;

var
	iCode : Integer;

begin
	Val (sStr, iInt, iCode);
    Result := iCode = 0;
end; { Str2Int }
{$ENDIF}

(* ---- *)

function Str2IntDef (const sStr: String; const iDef: Integer
{$IFDEF SUPPORTS_DEFAULTPARAMS}
															 = 0
																) : Integer;
{$ENDIF}

{$IFDEF BP7}
var
	iResult : Integer;
{$ENDIF}

begin
{$IFNDEF BP7}
	if not (Str2Int (sStr, Result{%H-})) then
    	Result := iDef;
{$ELSE}
	if (Str2Int (sStr, iResult)) then
    	Str2IntDef := iResult
	else Str2IntDef := iDef;
{$ENDIF}
end; { Str2IntDef }

(* ---- *)

{$IFDEF SUPPORTS_OVERLOAD}
function Str2Int64Def (const sStr: String; const iDef: Int64 = 0) : Int64;
begin
	if not (Str2Int64 (sStr, Result)) then
    	Result := iDef;
end; { Str2Int64Def }
{$ENDIF}

(* ---- *)

function StrGetElement (const sStr: String; const chDivider: Char;
						const iIndex: Integer; var sElement: String) : Boolean;

var
	iElement, iPos, iNextPos, iLen : Integer;

begin
	Assert (sStr <> '');
    Assert (iIndex > 0);

	StrGetElement := false;

    iPos := 1;
    iNextPos := Pos (chDivider, sStr);

    if (iNextPos = 0) then
        exit;

    ilen := Length (sStr);
    iElement := 1;

    while (iElement < iIndex) do
    begin
    	iPos := iNextPos;
        iNextPos := NextPos (chDivider, sStr, iPos + 1);

        Inc (iElement);
    end; { while }

    if (iPos = iNextPos) or (iPos = iLen) then
    	exit;

    if (iNextPos = 0) then
    	sElement := Copy (sStr, iPos + 1, iLen - iPos)
    else sElement := Copy (sStr, iPos + 1, Pred (iNextPos) - iPos);

    StrGetElement := true;
end; { StrGetElement }

(* ---- *)

{$IFDEF 16BIT}
function Trim (const sStr: String) : String;

var
    iLeft, iRight : Integer;

begin
    iLeft := 1;
    iRight := Length (sStr);

    while (iLeft <= iRight) and (Byte (sStr [iLeft]) <= Byte (' ')) do
        Inc (iLeft);

    if (iLeft > iRight) then
        Trim := ''
    else
    begin
        while (Byte (sStr [iRight]) <= Byte (' ')) do
            Dec (iRight);

        Trim := Copy (sStr, iLeft, Succ (iRight) - iLeft);
    end; { else }
end; { Trim }
{$ENDIF}

(* ---- *)

end.
