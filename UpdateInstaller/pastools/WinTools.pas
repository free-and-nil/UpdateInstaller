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

{$WARN SYMBOL_DEPRECATED OFF}

UNIT WinTools;

INTERFACE

USES
{$IFDEF 16BIT}
	WinTypes, Win31;
{$ELSE}
	Windows;
{$ENDIF}

CONST
	cCrLf = #13#10;  { Neue Zeile }
	cTab = #9;       { Tabulator }
	cIniExt = '.ini';

function ActivatePrevInstance (const sClassName: String;
							   const sWindowName: String
{$IFDEF SUPPORTS_OVERLOAD}
  														 = ''
{$ENDIF}
                               ) : HWnd;
{ Schon aktive Instanz eines Programms aktivieren }

function AltPressed : Boolean;
{$IFDEF CLR}
	unsafe;
{$ENDIF}
{ Liefert TRUE zurück, wenn [Alt] gedrückt ist }

procedure CenterOverScreen (hWindow: hWnd);
{ Zentriert ein Fenster in der Bildschirmmitte }

procedure CenterOverWindow (hParent, hWindow: hWnd);
{ Zentriert ein Fenster über einem anderen }

function ContainsWildcardChars (const sStr: String) : Boolean;

function ControlPressed : Boolean;
{$IFDEF CLR}
	unsafe;
{$ENDIF}
{ Liefert TRUE zurück, wenn [Control] gedrückt ist }

function CreateEmptyFile (const sFileName: String {$IFDEF SUPPORTS_DEFAULTPARAMS};
				  	 const bRaiseException: Boolean = false {$ENDIF}) : Boolean;

function CreateIniName : String;
{ Anhand des Programmnamens den Namen der INI-Datei bestimmen
  <<- Result : Name der INI-Datei, mit komplettem Programmpfad. }

{$IFDEF 16BIT}
procedure ExtractFileName (const szFilePath: PChar; szFileName: PChar);
{ Extrahiert den Dateinamen aus einer Pfadangabe }

procedure ExtractFilePath (const szFilePath: PChar; szPath: PChar);
{ Extrahiere den Pfad aus einer Pfadangabe mit Dateinamen }
{$ENDIF}

function GetSysColors : LongInt;
{ Die Anzahl Farben feststellen }

function IsLargeFont : Boolean;
{ Ist ein großes Font installiert? }

function KeyPressed (const iKey: Integer) : Boolean;

{$IFDEF MsgBox}
function MsgBox (const hWindow: HWnd; const sMsg, sCaption: String;
                 const iType: Integer) : Integer;
{$ENDIF}

{$IFNDEF VER70}
function MinimizePathName (const Wnd: HWND; const Filename: String) : String;
{$ENDIF}

procedure MyYield;
{$IFDEF CLR}
	unsafe;
{$ENDIF}
{ Kontrolle an andere Programme abgeben. Aufrufendes Programms sollte
  wm_Close abfangen. }

procedure OutputDebugStr (const sMsg: String);
{$IFDEF SUPPORTS_OVERLOAD}
	overload;

procedure OutputDebugStr (const sMsg: String;
                          const Args: array of const); overload;
{$ENDIF}

procedure ProcessMessages (const hCurWnd: HWnd
{$IFDEF SUPPORTS_DEFAULTPARAMS}
    = 0
{$ENDIF}
);
{ Kontrolle an andere Programme abgeben.
  ->> hCurWnd : Handle des aktiven Fensters. }

function PixelsPerInch (hWindow: HWnd
{$IFDEF SUPPORTS_DEFAULTPARAMS}
    = 0
{$ENDIF}
                                           ) : Integer;

function ShiftPressed : Boolean;
{$IFDEF CLR}
	unsafe;
{$ENDIF}
{ Liefert TRUE zurück, wenn [Shift] gedrückt ist }

procedure ShowHorizontalScrollbar (const hListBox: HWnd; const iWidth: Integer);

function TopMost (const hWindow: HWnd; const bTopMost: Boolean
{$IFDEF SUPPORTS_DEFAULTPARAMS}
	= true
{$ENDIF}
) : Boolean;

{$IFDEF 16BIT}
function WinExecAndWait (szPath: PChar; wVisibility: Word): Word;
{$ENDIF}

function YieldControl (const hThisWnd: hWnd) : Boolean;
{ Kontrolle an andere Programme abgeben. Aufrufendes Programms sollte
  wm_Close abfangen.
  -> hThisWnd : Fenster-Handle des Fensters, das YieldControl aufruft.
  <- YieldControl : FALSE, wenn Message = wm_Quit. }

IMPLEMENTATION

{$IFDEF VER70}
USES Strings, CStrings,
{$ELSE}
USES SysUtils, 
{$ENDIF}
{$IFDEF CLR}
	System.Runtime.InteropServices,
{$ENDIF}
{$IFDEF 16BIT}
    WinProcs,
{$ENDIF}
    Messages;

(* ---- *)

function ActivatePrevInstance (const sClassName: String;
							   const sWindowName: String
{$IFDEF SUPPORTS_DEFAULTPARAMS}
  														 = ''
{$ENDIF}
                               ) : HWnd;
{ Schon aktive Instanz eines Programms aktivieren.
  ->> szClassName : Name der Fensterklasse;
  ->> szWindowName : Name des Fensters;
  <<- Result : 0, wenn keine Instanz gefunden, sonst Fenster-Handle der vorigen
               Instanz. }

var
	hPrevInstWnd, hPopUpWnd, hParent : hWnd;
    pszClass, pszWindow : PChar;

begin
{$IFDEF 16BIT}
	if (hPrevInst = 0) then
	begin
		ActivatePrevInstance := 0;
		exit;
	end; { if }
{$ENDIF}

	if (sClassName <> '') then
    	pszClass := PChar (sClassName)
    else pszClass := PChar (NIL);

    if (sWindowName <> '') then
    	pszWindow := PChar (sWindowName)
    else pszWindow := PChar (NIL);

	hPrevInstWnd := FindWindow (pszClass, pszWindow);

	ActivatePrevInstance := hPrevInstWnd;

	if (hPrevInstWnd <> 0) then
	begin { Fenster gefunden ->> nach vorne holen }
		if (IsWindowVisible (hPrevInstWnd) = false) or
           (IsIconic (hPrevInstWnd)) then
        begin
{$IFDEF 16BIT}
            hParent := GetWindowWord (hPrevInstWnd, GWW_HWNDPARENT);
{$ELSE}
			hParent := GetWindowLong (hPrevInstWnd, GWL_HWNDPARENT);
{$ENDIF}

            if (hParent <> 0) then { Delphi-Anwendung? }
				ShowWindow (hParent, sw_Restore)
            else ShowWindow (hPrevInstWnd, sw_Restore);

{$IFNDEF 16BIT}
			Sleep (10);
{$ENDIF}
		end; { if }

		{ Nach offenem Dialog suchen }
		hPopUpWnd := GetLastActivePopUp (hPrevInstWnd);

		if (hPopUpWnd <> hPrevInstWnd) then
			hPrevInstWnd := hPopUpWnd; { Dialog nach vorne holen }

{$IFDEF 16BIT}
		BringWindowToTop (hPrevInstWnd); { Fenster nach vorne holen }
{$ELSE}
		SetForegroundWindow (hPrevInstWnd); { Fenster nach vorne holen }
{$ENDIF}
	end; { if }
end; { ActivatePrevInstance }

(* ---- *)

function AltPressed : Boolean;
begin
	Result := KeyPressed (VK_MENU);
end; { AltPressed }

(* ---- *)

procedure CenterOverScreen (hWindow: hWnd);
{
  Zentriert ein Fenster in der Bildschirmmitte.
  ->> hWindow : Fenster-Handle des zu zentrierenden Fensters.
}

var
    Rect : TRect;
    xScreen, yScreen : Integer;

begin { In Bildschirmmitte zentrieren }
    { Bildschirmauflösung }
    xScreen := GetSystemMetrics (sm_CXScreen);
    yScreen := GetSystemMetrics (sm_CYScreen);

    GetWindowRect (hWindow, Rect{%H-}); { Ausmaße des Kindfensters }

    with Rect do { Fensterposition setzen }
        SetWindowPos (hWindow, hwnd_Top,
                      (xScreen DIV 2) - ((Right - Left) DIV 2),
                      (yScreen DIV 2) - ((Bottom - Top) DIV 2),
                      0, 0, swp_NoSize or swp_NoZOrder);
end; { CenterOverScreen }

(* ---- *)

procedure CenterOverWindow (hParent, hWindow: hWnd);
{
  Zentriert ein Fenster über einem anderen.

  ->> hParent : Fenster-Handle des Elternfensters.
  ->> hWindows : Fenster-Handle des Fensters, das verschoben werden soll.
}

var
  Rect : TRect;
  xParent, yParent, xCorner, yCorner, xScreen, yScreen : Integer;

begin
    { Bildschirmauflösung }
    xScreen := GetSystemMetrics (sm_CXScreen);
    yScreen := GetSystemMetrics (sm_CYScreen);

    GetWindowRect (hParent, Rect{%H-});

    with Rect do
    begin
        xParent := (Right - Left) DIV 2; { Mitte des Parent-Fensters }
        yParent := (Bottom - Top) DIV 2;
        xCorner := Left; { Position des linken, oberen Kante }
        yCorner := Top;
    end; { with }

    GetWindowRect (hWindow, Rect); { Ausmaße des Kindfensters }

    with Rect do { Fensterposition setzen }
    begin
        { Eckpunkte festlegen }
        xCorner := xCorner + xParent - ((Right - Left) DIV 2);
        yCorner := yCorner + yParent - ((Bottom - Top) DIV 2);

        { Fenster soll sichtbar dargestellt werden }
        IF (xCorner < 0) THEN xCorner := 0;
        IF ((xCorner + (Right - Left)) > xScreen) THEN
            xCorner := xScreen - (Right - Left);

        IF (yCorner < 0) THEN yCorner := 0;
        IF ((yCorner + (Bottom - Top)) > yScreen) THEN
            yCorner := yScreen - (Bottom - Top);

        { Fensterposition setzen }
        SetWindowPos (hWindow, hwnd_Top, xCorner, yCorner, 0, 0,
                      swp_NoSize or swp_NoZOrder);
    end; { with }
end; { CenterOverWindow }

(* ---- *)

function ContainsWildcardChars (const sStr: String) : Boolean;
begin
	ContainsWildcardChars := (Pos ('*', sStr) > 0) or (Pos ('?', sStr) > 0);
end; { ContainsWildcardChars }

(* ---- *)

function ControlPressed : Boolean;
{ Liefert TRUE zurück, wenn [Control] gedrückt ist }

begin
    Result := KeyPressed (vk_Control);
end; { ControlPressed }

(* ---- *)

function CreateEmptyFile (const sFileName: String {$IFDEF SUPPORTS_DEFAULTPARAMS};
				  	 const bRaiseException: Boolean = false {$ENDIF}) : Boolean;
// https://riptutorial.com/winapi/example/5736/create-a-file-and-write-to-it

var
	hFile : THandle;
    psFileName : PChar;
{$IFDEF 16BIT}
	iLen : Integer;
{$ENDIF}

begin
{$IFDEF SUPPORTS_ASSERT}
    Assert (sFileName <> '');
{$ENDIF}

{$IFDEF 16BIT}
    iLen := Length (sFileName);

    psFileName := StrAlloc (iLen + 1);
	StrPCopy (psFileName, sFileName);
{$ELSE}
	psFileName := PChar (sFileName);
{$ENDIF}

	hFile := CreateFile (psFileName, GENERIC_WRITE, FILE_SHARE_READ, NIL,
    					 CREATE_NEW, FILE_ATTRIBUTE_NORMAL, 0);

{$IFDEF 16BIT}
	StrDispose (psFileName);
{$ENDIF}

    Result := hFile <> INVALID_HANDLE_VALUE;

    if (Result) then
    	CloseHandle (hFile)
{$IFNDEF 16BIT}
	else
    	if (bRaiseException) then
        	RaiseLastWin32Error;
{$ENDIF}
end; { CreateEmptyFile }

(* ---- *)

function CreateIniName : String;
{ Anhand des Programmnamens den Namen der INI-Datei bestimmen
  <<- Result : Name der INI-Datei, mit komplettem Programmpfad. }

begin
    Result := ChangeFileExt (ParamStr (0), cIniExt);
end; { CreateIniName }

(* ---- *)

procedure DebugString (const {%H-}sDebug: String);
{ Ruft "OutputDebugString" auf und übergibt die Zeichenkette "sDebug" }

{$IFDEF DEBUG}
    {$IFDEF 16BIT}
    var
        iLen : Integer;
        pszDebStr : PChar;

    begin
        iLen := Length (sDebug);

        if (iLen = 0) then
            exit; { Leerer String }

        pszDebStr := StrAlloc (iLen + 1);

        StrPCopy (pszDebStr, sDebug);
        OutputDebugString (pszDebStr);
        StrDispose (pszDebStr);
    {$ELSE}
    begin
    	OutputDebugString (PChar (sDebug));
    {$ENDIF}
{$ELSE}
begin
{$ENDIF}
end; { DebugString }

(* ---- *)

{$IFDEF 16BIT}
procedure ExtractFileName (const szFilePath: PChar; szFileName: PChar);
{ Extrahiert den Dateinamen aus einer Pfadangabe.
  ->> szFilePath : Der Pfad-String (Pfad + Dateiname).
  ->> szFileName : In diese Variable wird der Dateiname kopiert; Speicher muß
                   schon bereitstehen.
                   Ist kein Dateiname in szFilePath enthalten (kein \ in
                   szFilePath), dann kopiert die Funktion szFilePath nach
                   szFileName.}

var
    pchBackSlashPos : PChar;

begin
    { Nach Backslash suchen }
    pchBackSlashPos := StrRScan (szFilePath, '\');

    if (pchBackSlashPos = NIL) then { Kein Backslash gefunden }
        lstrcpy (szFileName, szFilePath)
    else lstrcpy (szFileName, pchBackSlashPos + 1); { Dateiname kopieren }
end; { ExtractFileName }

(* ---- *)

procedure ExtractFilePath (const szFilePath: PChar; szPath: PChar);
{ Extrahiere den Pfad aus einer Pfadangabe mit Dateinamen.
  ->> szFilePath : Der Pfad-String (Pfad + Dateiname).
  ->> szPath     : In diese Variable wird der Pfad kopiert; Speicher muß
                   schon bereitstehen (Pfad ohne Backslash am Ende).
                   Ist kein Dateiname oder Pfad in szFilePath enthalten
                   (kein \ in szFilePath), dann kopiert die Funktion
                   einen leeren String nach szPath.}

var
    pchBackSlashPos : PChar;
    wBackSlashPos : word;

begin
    { Nach Backslash suchen }
    pchBackSlashPos := StrRScan (szFilePath, '\');

    if (pchBackSlashPos = NIL) then { Kein Backslash gefunden }
        szPath^ := #0
    else
    begin
        lstrcpy (szPath, szFilePath); { Vollen Pfad mit Dateiname kopieren }
        wBackSlashPos := pchBackSlashPos - szFilePath; { Länge holen }
        pchBackSlashPos := szPath + wBackSlashPos; { Neue Position setzen }
        pchBackSlashPos^ := #0; { Den Dateinamen abschneiden }
    end; { else }
end; { ExtractFilePath }
{$ENDIF}

(* ---- *)

function GetSysColors : LongInt;
{ Die Anzahl Farben feststellen }

var
    hTmpDC : hDC;

begin
    hTmpDC := GetDC (0);
    GetSysColors := LongInt (1) shl (GetDeviceCaps (hTmpDC, BitsPixel) *
                                     GetDeviceCaps (hTmpDC, Planes));
    ReleaseDC (0, hTmpDC);
end; { GetSysColors }

(* ---- *)

function IsLargeFont : Boolean;
{ Ist ein großes Font installiert? }

var
    dwDialogUnits : LongInt;

begin
    { Die Größe des aktuellen Fonts holen }
    dwDialogUnits := GetDialogBaseUnits;

    IsLargeFont := (LoWord (dwDialogUnits) >= 10) AND { Breite }
                   (HiWord (dwDialogUnits) >= 20);    { Höhe }
end; { IsLargeFont }

(* ---- *)

function KeyPressed (const iKey: Integer) : Boolean;
{*
https://www.delphi-treff.de/tipps-tricks/system/tastatur-und-maus/status-der-strg-alt-shift-tasten-erfragen/

Als Parameter erwartet die Funktion die VirtualKeys der Tasten:

    VK_SHIFT: Shift-Tasten
    VK_CONTROL: STRG-Tasten
    VK_MENU: ALT-Tasten

Falls man zwischen linker und rechter Strg-/Alt-/Shift-Taste unterscheiden will,
muss man folgende Konstanten verwenden. Beachten Sie, dass die Unterscheidung
unter W9X noch nicht berücksichtigt wird:

    VK_LSHIFT: linke Shift-Taste
    VK_RSHIFT: rechte Shift-Taste
    VK_LCONTROL: linke Strg-Taste
    VK_RCONTROL: rechte Strg-Taste
    VK_LMENU: linke Alt-Taste
    VK_RMENU: rechte Alt-Taste

Weitere Konstanten für Sondertasten (Windows-Taste, Microsoft-Tastatur-eigene Tasten) finden sich in der Hilfe unter dem Stichwort „Virtual-Key Codes“.

Die oben stehende Funktion basiert auf dem Aufruf von GetKeyState. Je nach
Status der Taste werden die Bits des Ergebnisstyps gesetzt. GetKeyState setzt
das unterste Bit, wenn die Taste gedrückt wurde. Dies würde einem Vergleich mit
1 entsprechen. Wird die Taste gedrückt gehalten ("toggled"), wird das oberste
Bit gesetzt. Da der Rückgabewert unter Delphi ein vorzeichenbehafteter
Integer-Typ ist, genügt in der Funktion ein Vergleich auf kleiner 0 (siehe
oben). Wurde die Taste nicht gedrückt, ist der Rückgabewert 0.
*}

begin
	Result := GetKeyState (iKey) < 0;
end; { KeyPressed }

(* ---- *)

{$IFNDEF VER70}
function MinimizePathName (const Wnd: HWND; const Filename: String) : String;
{ func to shorten the long path name with an ellipses '...' to fit
  in whatever control is passed to the Wnd parameter.
  Usage: Panel1.Caption := MinimizePathName(Panel1.Handle, DirectoryOutline1.Directory)
  This will shorten the path if necessary to fit in Panel1.
  Source: http://www.dts.nl/Helpdesk/delphi/delphi_vragen/2/d222200110.htm }


	(* ---- *)

    procedure CutFirstDirectory (var S: String);

    var
	    Root: Boolean;
    	P: Integer;

    begin
    	if S = '\' then
	    	S := ''
	    else
    	begin
		    if (S [1] = '\') then
		    begin
			    Root := True;
			    Delete (S, 1, 1);
		    end { if }
		    else Root := False;

	        if S [1] = '.' then
    		    Delete (S, 1, 4);

	        P := Pos ('\', S);

	        if (P <> 0) then
	        begin
	    	    Delete (S, 1, P);
				S := '...\' + S;
	        end { if }
    	    else S := '';

	        if Root then
    		    S := '\' + S;
        end; { else }
    end; { CutFirstDirectory }

    (* ---- *)

{$IFDEF 16BIT}
	function GetTextWidth (const DC: HDC; sText: String) : Integer;

	var
		Extent : WinTypes.TSize;

	begin
		sText [Length (sText) + 1] := #0;

		if WinProcs.GetTextExtentPoint(DC, @sText [1], Length (sText),
                                       Extent) then
			Result := Extent.cX
		else Result := 0;
	end; { GetTextWidth }
{$ELSE}
	function GetTextWidth (const DC: HDC; const Text: String) : Integer;

	var
		Extent: TSize;

	begin
{$IFDEF CLR}
		if (GetTextExtentPoint(DC, Text, Length(Text), Extent)) then
{$ELSE}
		if (GetTextExtentPoint(DC, PChar(Text), Length(Text), Extent{%H-})) then
{$ENDIF}
			Result := Extent.cX
		else Result := 0;
	end; { GetTextWidth }
{$ENDIF}

    (* ---- *)

var
    Drive, Dir, Name: String;
    R: TRect;
    DC: HDC;
    MaxLen: Integer;
    OldFont, Font: HFONT;

begin { MinimizePathName }
    Result := FileName;

    if (Wnd = 0) then
	    Exit;

    DC := GetDC (Wnd);

    if (DC = 0) then
    	Exit;

    Font := HFONT (SendMessage (Wnd, WM_GETFONT, 0, 0));

    OldFont := SelectObject (DC, Font);

    try
	    GetWindowRect (Wnd, R{%H-});
    	MaxLen := R.Right - R.Left;

		Dir := SysUtils.ExtractFilePath (Result);
    	Name := SysUtils.ExtractFileName (Result);

        if (Length (Dir) >= 2) and (Dir [2] = ':') then
        begin
	        Drive := Copy (Dir, 1, 2);
    	    Delete (Dir, 1, 2);
        end { if }
		else Drive := '';

        while ((Dir <> '') or (Drive <> '')) and
        	  (GetTextWidth (DC, Result) > MaxLen) do
        begin
            if (Dir = '\...\') then
            begin
	            Drive := '';
    	        Dir := '...\';
            end { if }
            else
                if (Dir = '') then
	                Drive := ''
                else CutFirstDirectory (Dir);

            Result := Drive + Dir + Name;
        end; { while }

    finally
        SelectObject (DC, OldFont);
        ReleaseDC (Wnd, DC);
    end; { try / finally }
end; { MinimizePathName }
{$ENDIF}

(* ---- *)

{$IFDEF MsgBox}
function MsgBox (const hWindow: HWnd; const sMsg, sCaption: String;
				 const iType: Integer) : Integer;


{$IFDEF 16BIT}
var
	pchMsg, pchCaption : PChar;
{$ENDIF}

begin
{$IFDEF 16BIT}
	pchMsg := StrAlloc (Length (sMsg) + 1);
    pchCaption := StrAlloc (Length (sCaption) + 1);

    StrPCopy (pchMsg, sMsg);
    StrPCopy (pchCaption, sCaption);

	MsgBox := WinProcs.MessageBox (hWindow, pchMsg, pchCaption, iType);

	StrDispose (pchMsg);
    StrDispose (pchCaption);
{$ELSE}
    {$IFDEF CLR}
        Result := Windows.MessageBox (hWindow, sMsg, sCaption, iType);
    {$ELSE}
        Result := Windows.MessageBox (hWindow, PChar (sMsg), PChar (sCaption),
                                      iType);
    {$ENDIF}
{$ENDIF}
end; { MsgBox }
{$ENDIF}

(* ---- *)

procedure MyYield;
{ Kontrolle an andere Programme abgeben. Aufrufendes Programms sollte
  wm_Close abfangen. }

var
	Message : TMsg;

begin
	while (PeekMessage (Message{%H-}, 0, 0, 0, pm_Remove)) do
	begin { Nachricht an uns empfangen }
		if (Message.Message = wm_Quit) then { Programm schließen }
			Halt;

		TranslateMessage (Message);
		DispatchMessage (Message);
	end; { while }
end; { MyYield }

(* ---- *)

procedure OutputDebugStr (const sMsg: String);

{$IFDEF 16BIT}
var
	szMsg : array [0..255] of Char;
{$ENDIF}

begin
{$IFDEF 16BIT}
	StrPCopy (@szMsg [0], sMsg);
    OutputDebugString (@szMsg [0]);
{$ELSE}
    OutputDebugString (PChar (sMsg));
{$ENDIF}
end; { OutputDebugStr }

(* ---- *)

{$IFDEF SUPPORTS_OVERLOAD}
procedure OutputDebugStr (const sMsg: String; const Args: array of const);
begin
    OutputDebugStr (Format (sMsg, Args));
end; { OutputDebugStr }
{$ENDIF}

(* ---- *)

procedure ProcessMessages (const hCurWnd: HWnd
{$IFDEF DEFAULTPARAMS}
= 0
{$ENDIF}
);

var
	Message : TMsg;
	bTranslateAndDispatch : Boolean;

begin
	while (PeekMessage (Message{%H-}, 0, 0, 0, pm_Remove)) do
	begin { Nachricht an uns empfangen }
		if (Message.Message = wm_Quit) then
		begin { Programm schließen }
			PostQuitMessage (Message.wParam);
			exit;
		end; { if }

		if (hCurWnd = 0) then
			bTranslateAndDispatch := true
		else bTranslateAndDispatch := not (IsDialogMessage (hCurWnd, Message));

		if (bTranslateAndDispatch) then
		begin
			TranslateMessage (Message);
			DispatchMessage (Message);
		end; { if }
	end; { while }
end; { ProcessMessages }

(* ---- *)

function PixelsPerInch (hWindow: HWnd
{$IFDEF SUPPORTS_DEFAULTPARAMS}
    = 0
{$ENDIF}
                                           ) : Integer;
// http://delphiexamples.com/systeminfo/fontsize.html

var
    DC : HDC;

begin
    if (hWindow = 0) then
        hWindow := GetDesktopWindow;

    DC := GetDC (hWindow);

    try
        Result := GetDeviceCaps (DC, LOGPIXELSX);  // 96 = standard, 120 = large

    finally
        ReleaseDC (GetDesktopWindow, DC);
    end; { try / finally }
end; { PixelsPerInch }

(* ---- *)

function ShiftPressed : Boolean;
begin
    Result := KeyPressed (vk_Shift);
end; { ShiftPressed }

(* ---- *)

procedure ShowHorizontalScrollbar (const hListBox: HWnd; const iWidth: Integer);
{ Horizontale Scrollbar in der Listbox "hListBox" anzeigen. Die Breite der
  Scrollbar wird über den Parameter "iWidth" gesteuert. }

begin
	SendMessage (hListBox, LB_SetHorizontalExtent, iWidth, 0);
end; { ShowHorizontalScrollbar }

(* ---- *)

function TopMost (const hWindow: HWnd; const bTopMost: Boolean
{$IFDEF SUPPORTS_DEFAULTPARAMS}
	= true
{$ENDIF}
) : Boolean;
{ Fenster mit dem "TopMost"-Status versehen bzw. Status entfernen }

var
	hWndInsertAfter : HWND;

begin
    TopMost := false;

  	if not (IsWindow (hWindow)) then
    	exit;

	if (bTopMost) then
    	hWndInsertAfter := hwnd_TopMost
    else hWndInsertAfter := hwnd_NoTopMost;

{$IFDEF 16BIT}
	TopMost := SetWindowPos (hWindow, hWndInsertAfter, 0, 0, 0, 0,
							 swp_NoSize or swp_NoMove);
{$ELSE}
	SetWindowPos (hWindow, hWndInsertAfter, 0, 0, 0, 0,
				  swp_NoSize or swp_NoMove);
	TopMost := true;
{$ENDIF}
end; { TopMost }

(* ---- *)

(*
Fm: Pat Ritchey (TeamB) 70007,4660
Here's a function that is called in the same way as WinExec but will wait
for the called task to terminate.  The only difference is that if the
function is succesful it will return zero.
*)

{$IFDEF 16BIT}
function WinExecAndWait(szPath: PChar; wVisibility: Word): Word;

var
   hInstanceID : THandle;
   pToMsg : PMsg;
   wResult : word;

begin
	hInstanceID := WinExec (szPath, wVisibility);

	if (hInstanceID < 32) then
		{ a value less than 32 indicates an Exec error }
		WinExecAndWait := hInstanceID
	else
	begin
		WinExecAndWait := 0;

		New (pToMsg);

		repeat
			while (PeekMessage (pToMsg^, 0, 0, 0, pm_Remove)) do
			begin { Nachricht an uns empfangen }
				if (pToMsg^.Message = wm_Quit) then
				begin { Programm schließen }
					PostQuitMessage (pToMsg^.wParam);
					Dispose (pToMsg);
					exit;
				end; { if }

				TranslateMessage (pToMsg^);
				DispatchMessage (pToMsg^);
			end; { while }
		until (GetModuleUsage (hInstanceID) = 0);

		Dispose (pToMsg);
	end; { else }
end; { WinExecAndWait }
{$ENDIF}

(* ---- *)

function YieldControl (const hThisWnd: hWnd) : Boolean;
{ Kontrolle an andere Programme abgeben. Aufrufendes Programms sollte
  wm_Close abfangen.
  -> hThisWnd : Fenster-Handle des Fensters, das YieldControl aufruft.
  <- YieldControl : FALSE, wenn Message = wm_Quit. }

var
	Message : TMsg;

begin
{$IFDEF SUPPORTS_ASSERT}
	Assert (hThisWnd <> 0);
{$ENDIF}

	while (PeekMessage (Message{%H-}, 0, 0, 0, pm_Remove)) do
	begin { Nachricht an uns empfangen }
		if (Message.Message = wm_Quit) then
		begin { Programm schließen }
			PostQuitMessage (Message.wParam);
			YieldControl := false;
			exit;
		end; { if }

		if not (IsDialogMessage (hThisWnd, Message)) then
		begin
			TranslateMessage (Message);
			DispatchMessage (Message);
		end; { if }
	end; { while }

    YieldControl := true;
end; { YieldControl }

(* ---- *)

end.
