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

(**
When reading from the registry with the low-level classic functions you must be
able to handle strings with and without a zero-terminator.

The easy way to do this is to secretly allocate one character extra that you
don't tell the API about when reading and then append the '\0' to the end of
however many characters it read.

Newer functions like RegGetValue handle it for you.

RegEnumValue returns different buffer size in Vista compatibility mode
https://stackoverflow.com/questions/47157809/regenumvalue-returns-different-buffer-size-in-vista-compatibility-mode

Beware of non-null-terminated registry strings
https://blogs.msdn.microsoft.com/oldnewthing/20040824-00/?p=38063
**)

unit RegistryApiU;

interface

uses Windows,
	 BaseTypesU;

type
	TRegEnumSubkeysProc = procedure (const HRootKey: HKEY; const sKey: String;
    							  const bDisableRedirection: Boolean) of Object;

function CheckRegApiResult (const lResult: LongInt) : Boolean;
                                       {$IFDEF SUPPORTS_INLINE} inline; {$ENDIF}

function GetRegApiErrorMsg (const lError: LongInt) : String;

function RegKeyClose (const HKey: HKEY) : Boolean;

function RegKeyCreate (const HRootKey: HKEY; const sKey: String;
					   out Key: HKEY;
                       const bDisableRedirection: Boolean = false) : Boolean;
                       												   overload;

function RegKeyCreate (const HRootKey: HKEY; const sKey: String;
                       const bDisableRedirection: Boolean = false) : Boolean;
                       												   overload;

function RegKeyDelete (const HRootKey: HKey; const sKey: String) : Boolean;
                                                                       overload;

function RegKeyDelete (const HRootKey: HKey; const sKey: String;
                       const bDisableRedirection: Boolean) : Boolean; overload;

function RegEnumSubkeys (const HRootKey: HKey; const sKey: String;
						 const RegEnumSubkeysProc: TRegEnumSubkeysProc;
                         const bDisableRedirection: Boolean = false) : Boolean;

function RegGetKeyNames (const HRootKey: HKey; const sKey: String;
						 out asKeys: TaString;
                         const bDisableRedirection: Boolean = false) : Boolean;

function RegKeyExists (const HRootKey: HKey; const sKey: String;
					   const bDisableRedirection: Boolean = false) : Boolean;

function RegKeyOpen (const HRootKey: HKEY; const sKey: String; out Key: HKEY;
                     const bDisableRedirection: Boolean = false) : Boolean;

function RegKeyOpenReadOnly (const HRootKey: HKEY; const sKey: String;
				  		     out Key: HKEY;
                          const bDisableRedirection: Boolean = false) : Boolean;

function RegReadInt (const HRootKey: HKey; const sKey, sName: String;
                     const iDefaultResult: Integer = 0;
                     const bDisableRedirection: Boolean = false) : Integer;

function RegReadMulti_SZ (const HRootKey: HKey; const sKey, sName: String;
					 	  out asResult: TaString;
                     	  const bDisableRedirection: Boolean = false) : Boolean;
                          											   overload;

function RegReadMulti_SZ (const HRootKey: HKey; const sKey, sName: String;
                     	  const bDisableRedirection: Boolean = false;
                          const chSeparator: Char = ',') : String; overload;

function RegReadStr (const HRootKey: HKey; const sKey, sName: String;
                     const sDefaultResult: String = '';
                     const bDisableRedirection: Boolean = false) : String;

function RegValueDelete (const HRootKey: HKey;
						 const sKey, sValue: String;
                         const bDisableRedirection: Boolean = false) : Boolean;

function RegValueExists (const HRootKey: HKey; const sKey, sName: String;
						 const bDisableRedirection: Boolean = false) : Boolean;

function RegValueGetType (const HRootKey: HKey; const sKey, sName: String;
                          out dwType: DWord;
                          const bDisableRedirection: Boolean = false) : Boolean;
                          											   overload;
function RegValueGetType (const HRootKey: HKey; const sKey, sName: String;
						  const bDisableRedirection: Boolean = false) : DWord;
                          											   overload;

function RegWriteBinary (const HRootKey: HKey; const sKey: String;
					  	 const bCreateKey: Boolean; const sName: String;
                      	 const pData: Pointer; const dwBufSize: DWord;
                      	 const bDisableRedirection: Boolean = false) : Boolean;

function RegWriteDWord (const HRootKey: HKey; const sKey: String;
					    const bCreateKey: Boolean; const sName: String;
                        const dwValue: DWord;
                        const bDisableRedirection: Boolean = false) : Boolean;

function RegWriteMulti_SZ (const HRootKey: HKey; const sKey: String;
					  	   const bCreateKey: Boolean; const sName: String;
                           const asValues: TaString;
                      	  const bDisableRedirection: Boolean = false) : Boolean;

function RegWriteRegExpandSZ (const HRootKey: HKey; const sKey: String;
					  		  const bCreateKey: Boolean;
                              const sName, sValue: String;
                      	  const bDisableRedirection: Boolean = false) : Boolean;

function RegWriteStr (const HRootKey: HKey; const sKey: String;
					  const bCreateKey: Boolean;
                      const sName, sValue: String;
                      const bDisableRedirection: Boolean = false) : Boolean;

implementation

uses SysUtils,
	 RegistryHelperU, VerifyU, Win32ToolsU, Server2003_ImportU;

(* ---- *)

function GetRegApiErrorMsg (const lError: LongInt) : String;

const
    cBufSize = $FF;

var
    dwLen : DWord;

begin
    SetLength (Result{%H-}, cBufSize);

    dwLen := FormatMessage (FORMAT_MESSAGE_FROM_SYSTEM, NIL,
                            lError, 0, PChar (Result), cBufSize,
                            NIL);

    if (dwLen > 0) then
    begin
        SetLength (Result, dwLen);
        Result := TrimRight (Result);
    end { if }
    else Result := '';
end; { GetRegApiErrorMsg }

(* ---- *)

function CheckRegApiResult (const lResult: LongInt) : Boolean;
begin
    if (lResult = Error_Success) then
        Result := true
    else
    begin
        Result := false;
        SetLastError (DWord (lResult));
    end; { if }
end; { CheckRegApiResult }

(* ---- *)

function RegKeyClose (const HKey: HKEY) : Boolean;
begin
	Assert (HKey <> 0);

    Result := RegCloseKey (HKey) = Error_Success;
    VerifyApi (Result);
end; { RegKeyClose }

(* ---- *)

function RegKeyCreate (const HRootKey: HKEY; const sKey: String;
					   out Key: HKEY;
                       const bDisableRedirection: Boolean = false) : Boolean;

var
	dwDisposition : DWord;
    lResult : LongInt;

begin
	Assert (sKey <> '');

    lResult := RegCreateKeyEx (HRootKey, PChar (sKey), 0, NIL,
    						   REG_OPTION_NON_VOLATILE,
                               GetSamDesired (bDisableRedirection){%H-},
                               NIL, Key{%H-},
                               PDWord (@dwDisposition));

    Result := CheckRegApiResult (lResult);
end; { RegKeyCreate }

(* ---- *)

function RegKeyCreate (const HRootKey: HKEY; const sKey: String;
                       const bDisableRedirection: Boolean = false) : Boolean;

var
	Key : HKey;

begin
    Result := RegKeyCreate (HRootKey, sKey, Key, bDisableRedirection);
end; { RegKeyCreate }

(* ---- *)

function RegKeyCreateVolatile (const HRootKey: HKEY; const sKey: String;
					   		   out Key: HKEY;
                          const bDisableRedirection: Boolean = false) : Boolean;

var
	dwDisposition : DWord;
    lResult : LongInt;

begin
	Assert (sKey <> '');

    lResult := RegCreateKeyEx (HRootKey, PChar (sKey), 0, NIL,
    						   REG_OPTION_VOLATILE,
                               GetSamDesired (bDisableRedirection){%H-},
                               NIL, Key{%H-},
                               PDWord (@dwDisposition));

    Result := CheckRegApiResult (lResult);
end; { RegKeyCreateVolatile }

(* ---- *)

function RegKeyDelete (const HRootKey: HKey; const sKey: String) : Boolean;
{ Löscht nur den aktuellen Schlüssel, aber keine Kind-Schlüssel!
  "RegKeyDelete" unterstützt keine "Registry-Redirection" unter Wow64! }

var
	Key: HKEY;
    lResult : LongInt;

begin
	Assert (sKey <> '');

    if (RegKeyOpen (HRootKey, sKey, Key)) then
    begin
        lResult := RegDeleteKey (Key, PChar (sKey));

        Result := CheckRegApiResult (lResult);

	    RegKeyClose (Key);
    end { if }
    else Result := false;
end; { RegKeyDelete }

(* ---- *)

function RegKeyDelete (const HRootKey: HKey; const sKey: String;
                       const bDisableRedirection: Boolean) : Boolean; overload;

var
	Key: HKEY;
    lResult : LongInt;

begin
	Assert (sKey <> '');

    if (Assigned (RegDeleteKeyEx)) then
    begin
        if (RegKeyOpen (HRootKey, sKey, Key)) then
        begin
            lResult := RegDeleteKeyEx (Key, PChar (sKey),
                                       GetSamDesired (bDisableRedirection){%H-},
                                       0);

            Result := CheckRegApiResult (lResult);

            RegKeyClose (Key);
        end { if }
        else Result := false;
    end { if }
    else Result := RegKeyDelete (HRootKey, sKey);
end; { RegKeyDelete }

(* ---- *)

function RegEnumSubkeys (const HRootKey: HKey; const sKey: String;
						 const RegEnumSubkeysProc: TRegEnumSubkeysProc;
                         const bDisableRedirection: Boolean = false) : Boolean;

var
	Key: HKEY;
    dwSubKeys, dwMaxSubKeyLen, dwcbName : DWord;
    pszSubKey : PChar;
    iIndex : Integer;
    lRetVal : LongInt;
    FileTime : TFileTime;

begin
	Assert (sKey <> '');
    Assert (Assigned (RegEnumSubkeysProc));

    Result := false;

    if (RegKeyOpenReadOnly (HRootKey, sKey, Key, bDisableRedirection)) then
    begin
        lRetVal := RegQueryInfoKey (Key, NIL, NIL, NIL, PDWord (@dwSubKeys),
                                    PDWord (@dwMaxSubKeyLen),
        					        NIL, NIL, NIL, NIL, NIL, NIL);

        if (lRetVal = Error_Success) then
        begin
            if (dwSubKeys > 0) then
            begin
            	Inc (dwMaxSubKeyLen);
            	pszSubKey := StrAlloc (dwMaxSubKeyLen);

                for iIndex := 0 to dwSubKeys - 1 do
                begin
                	dwcbName := dwMaxSubKeyLen;
                    lRetVal := RegEnumKeyEx (Key, iIndex, pszSubKey, dwcbName,
                                             NIL, NIL, NIL, @FileTime);

                    if (lRetVal <> Error_Success) then
                    	Break;

                    RegEnumSubkeysProc (HRootKey, sKey + '\' + pszSubkey,
                    					bDisableRedirection);
                end; { for }

                StrDispose (pszSubKey);
            end; { if }
        end; { if }

        Result := CheckRegApiResult (lRetVal);

	    RegKeyClose (Key);
    end; { if }
end; { RegEnumSubkeys }

(* ---- *)

function RegGetKeyNames (const HRootKey: HKey; const sKey: String;
						 out asKeys: TaString;
                         const bDisableRedirection: Boolean = false) : Boolean;
{ Die in "asKeys" gespeicherten Unterschlüssel sind nicht alphabetisch
  sortiert! }

var
	Key: HKEY;
    dwSubKeys, dwMaxSubKeyLen, dwcbName : DWord;
    pszSubKey : PChar;
    iIndex : Integer;
    lRetVal : LongInt;
    FileTime : TFileTime;

begin
	Assert (sKey <> '');

    Result := false;

    if (RegKeyOpenReadOnly (HRootKey, sKey, Key, bDisableRedirection)) then
    begin
        if (RegQueryInfoKey (Key, NIL, NIL, NIL, PDWord (@dwSubKeys),
                             PDWord (@dwMaxSubKeyLen),
        					 NIL, NIL, NIL, NIL, NIL, NIL) = Error_Success) then
        begin
        	SetLength (asKeys{%H-}, dwSubKeys);

            lRetVal := Error_Success;

            if (dwSubKeys > 0) then
            begin
            	Inc (dwMaxSubKeyLen);
            	pszSubKey := StrAlloc (dwMaxSubKeyLen);

                for iIndex := 0 to dwSubKeys - 1 do
                begin
                	dwcbName := dwMaxSubKeyLen;
                    lRetVal := RegEnumKeyEx (Key, iIndex, pszSubKey, dwcbName,
                                             NIL, NIL, NIL, @FileTime);

                    if (lRetVal <> Error_Success) then
                    	Break;

                    asKeys [iIndex] := pszSubKey;
                end; { for }

                StrDispose (pszSubKey);
            end; { if }

            Result := lRetVal = Error_Success;
        end; { if }

	    RegKeyClose (Key);
    end; { if }
end; { RegGetKeyNames }

(* ---- *)

function RegKeyExists (const HRootKey: HKey; const sKey: String;
					   const bDisableRedirection: Boolean = false) : Boolean;
{ Test, ob ein Schlüssel existiert.
  ->> HRootKey : Wurzelschlüssel, siehe REGEDIT;
  ->> sKey : Name des Schüssels, dessen Existenz überprüft wird;
  <<- Result : True, wenn der Schlüssel existiert. }

var
	Key: HKEY;

begin
	Assert (sKey <> '');

    Result := RegKeyOpenReadOnly (HRootKey, sKey, Key, bDisableRedirection);

    if (Result) then
	    RegKeyClose (Key);
end; { RegKeyExists }

(* ---- *)

function RegKeyOpen (const HRootKey: HKEY; const sKey: String; out Key: HKEY;
                     const bDisableRedirection: Boolean = false) : Boolean;

var
    lResult : LongInt;

begin
	Assert (sKey <> '');

    lResult := RegOpenKeyEx (HRootKey, PChar (sKey), 0,
    						 GetSamDesired (bDisableRedirection){%H-}, Key{%H-});

    Result := CheckRegApiResult (lResult);
end; { RegKeyOpen }

(* ---- *)

function RegKeyOpenReadOnly (const HRootKey: HKEY; const sKey: String;
				  		     out Key: HKEY;
                          const bDisableRedirection: Boolean = false) : Boolean;

var
    lResult : LongInt;

begin
	Assert (sKey <> '');

    lResult := RegOpenKeyEx (HRootKey, PChar (sKey), 0,
    						 GetSamDesired (bDisableRedirection, true){%H-},
                             Key{%H-});

    Result := CheckRegApiResult (lResult);
end; { RegKeyOpenReadOnly }

(* ---- *)

function RegReadInt (const HRootKey: HKey; const sKey, sName: String;
                     const iDefaultResult: Integer = 0;
                     const bDisableRedirection: Boolean = false) : Integer;

var
	Key: HKEY;
    dwData, dwType, dwLen : DWord;
    lResult : LongInt;

begin
	Assert (sKey <> '');
    Assert (sName <> '');

    Result := iDefaultResult;

    if not (RegKeyOpenReadOnly (HRootKey, sKey, Key, bDisableRedirection)) then
    	exit;

    dwLen := SizeOf (DWord);
    dwData := 0;

    lResult := RegQueryValueEx (Key, PChar (sName), NIL, PDWord (@dwType),
                                PByte (@dwData), PDWord (@dwLen));

    if (lResult = Error_Success) then
    begin
    	if (dwType = REG_DWORD) then
        	Result := Integer (dwData);
    end { if }
    else SetLastError (lResult);

    RegKeyClose (Key);
end; { RegReadInt }

(* ---- *)

function RegReadMulti_SZ (const HRootKey: HKey; const sKey, sName: String;
					 	  out asResult: TaString;
                     	  const bDisableRedirection: Boolean = false) : Boolean;

var
	Key: HKEY;
    dwType, dwLen : DWord;
    sMulti_SZ : String {$IFDEF FPC} = '' {$ENDIF};
    lResult : LongInt;

begin { RegReadMulti_SZ }
	Assert (sKey <> '');
	Assert (sName <> '');

    Result := false;

(**
    if (Assigned (RegGetValue)) and (bDisableRedirection = false) then
    begin
    	dwLen := 0;

        if (RegGetValue (HRootKey, PChar (sKey), PChar (sName),
        				 RRF_RT_REG_MULTI_SZ, PDWord (@dwType), NIL,
                         PDWord (@dwLen)) = Error_Success) then
        begin
			SetLength (sMulti_SZ, Succ (dwLen div SizeOf (Char)));

            Result := RegGetValue (HRootKey, PChar (sKey), PChar (sName),
        				 		   RRF_RT_REG_MULTI_SZ, PDWord (@dwType),
                                   PChar (sMulti_SZ),
                                   PDWord (@dwLen)) = Error_Success;
        end; { if }
    end { if }
    else
**)
    begin
        if not (RegKeyOpenReadOnly (HRootKey, sKey, Key,
        							bDisableRedirection)) then
            exit;

        dwLen := 0;

        lResult := RegQueryValueEx (Key, PChar (sName), NIL, PDWord (@dwType),
                                    NIL, PDWord (@dwLen));

        if (lResult = Error_Success) then
            if (dwType = REG_MULTI_SZ) then
            begin  // "dwLen" beinhaltet abschließendes "#0#0"
                SetLength (sMulti_SZ, dwLen div SizeOf (Char));

                lResult := RegQueryValueEx (Key, PChar (sName), NIL,
                						    PDWord (@dwType),
                                            PByte (PChar (sMulti_SZ)),
                                            PDWord (@dwLen));
            end; { if }

        Result := CheckRegApiResult (lResult);

        RegKeyClose (Key);
    end; { else }

    if (Result) then
        asResult := ConvertWindowsStringArray (sMulti_SZ);
end; { RegReadMulti_SZ }

(* ---- *)

function RegReadMulti_SZ (const HRootKey: HKey; const sKey, sName: String;
                     	  const bDisableRedirection: Boolean = false;
                          const chSeparator: Char = ',') : String;

var
	asResult : TaString;
    iIndex : Integer;

begin
	Result := '';

	if (RegReadMulti_SZ (HRootKey, sKey, sName, asResult,
    					 bDisableRedirection)) then
    	if (Length (asResult) > 0) then
        	for iIndex := 0 to Length (asResult) - 1 do
            	if (iIndex > 0) then
                	Result := Result + chSeparator + asResult [iIndex]
                else Result := asResult [iIndex];
end; { RegReadMulti_SZ }

(* ---- *)

function RegReadStr (const HRootKey: HKey; const sKey, sName: String;
                     const sDefaultResult: String = '';
                     const bDisableRedirection: Boolean = false) : String;
{ "sName" = '' -> Default Value }

var
	Key: HKEY;
    dwType, dwLen : DWord;
    lResult : LongInt;

begin
	Assert (sKey <> '');

    Result := '';

(**
    // "RegGetValue" does not seem to be able to handle registry redirection
    if (Assigned (RegGetValue)) and (bDisableRedirection = false) then
    begin
    	dwLen := 0;

        lResult := RegGetValue (HRootKey, PChar (sKey), PChar (sName),
        						RRF_RT_ANY or RRF_NOEXPAND, PDWord (@dwType),
                                NIL, PDWord (@dwLen));

        if (lResult = Error_Success) and
           ((dwType = REG_EXPAND_SZ) or (dwType = REG_SZ)) then
        begin
			SetLength (Result, Pred (dwLen div SizeOf (Char)));

            lResult := RegGetValue (HRootKey, PChar (sKey), PChar (sName),
        				 		 	RRF_RT_ANY or RRF_NOEXPAND, PDWord (@dwType),
                                    PChar (Result), PDWord (@dwLen));
        end; { if }
    end { if }
    else
**)
    begin
        if not (RegKeyOpenReadOnly (HRootKey, sKey, Key,
        							bDisableRedirection)) then
        begin
            Result := sDefaultResult;
            exit;
        end; { if }

        dwLen := 0;

        lResult := RegQueryValueEx (Key, PChar (sName), NIL, PDWord (@dwType),
                             		NIL, PDWord (@dwLen));

        if (lResult = Error_Success) and
           ((dwType = REG_SZ) or (dwType = REG_EXPAND_SZ)) then
        begin
            // Allocate an extra character at the end of the string to deal
            // with string values that are not 0 terminated in the registry
            SetLength (Result, Succ (dwLen div SizeOf (Char)));

            lResult := RegQueryValueEx (Key, PChar (sName), NIL,
            							PDWord (@dwType),
                                        PByte (PChar (Result)), PDWord (@dwLen))
        end; { if }

        RegKeyClose (Key);
    end; { else }

    if (lResult = Error_Success) then
        SetLength (Result, lstrlen (PChar (Result)))
    else Result := sDefaultResult;
end; { RegReadStr }

(* ---- *)

function RegValueDelete (const HRootKey: HKey;
						 const sKey, sValue: String;
                         const bDisableRedirection: Boolean = false) : Boolean;

var
	Key: HKEY;
    lResult : LongInt;

begin
	Assert (sKey <> '');
    Assert (sValue <> '');

    Result := false;

    if (RegKeyOpen (HRootKey, sKey, Key, bDisableRedirection)) then
    begin
        lResult := RegDeleteValue (Key, PChar (sValue));

        Result := CheckRegApiResult (lResult);

	    RegKeyClose (Key);
    end; { if }
end; { RegValueDelete }

(* ---- *)

function RegValueExists (const HRootKey: HKey; const sKey, sName: String;
						 const bDisableRedirection: Boolean = false) : Boolean;
{ Ermitteln, ob ein Wert in der Registry existiert.
  ->> HRootKey : Wurzelschlüssel, siehe REGEDIT;
  ->> sKey : Name des Schüssel, in dem sich der Wert befindet;
  ->> sName : Name des Werts, der gesucht wird;
  <<- Result : TRUE, wenn der Wert existiert, sonst FALSE. }

var
	Key: HKEY;
    lResult : LongInt;

begin
	Assert (sKey <> '');
    Assert (sName <> '');

	Result := false;

    if not (RegKeyOpenReadOnly (HRootKey, sKey, Key, bDisableRedirection)) then
    	exit;

    lResult := RegQueryValueEx (Key, PChar (sName), NIL, NIL, NIL, NIL);

    Result := CheckRegApiResult (lResult);

    RegKeyClose (Key);
end; { RegValueExists }

(* ---- *)

function RegValueGetType (const HRootKey: HKey; const sKey, sName: String;
                          out dwType: DWord;
                          const bDisableRedirection: Boolean = false) : Boolean;
{ Den Typ eines Wertes ermitteln.
  ->> HRootKey : Wurzelschlüssel, siehe REGEDIT;
  ->> sKey : Name des Schüssel, in dem sich der Wert befindet;
  ->> sName : Name des Werts, der gesucht wird;
  <<- dwType : der Typ oder "REG_NONE" im Fehlerfall;
  <<- Result : TRUE, wenn der Wert ermittelt werden konnte, sonst FALSE. }

var
	Key: HKEY;
    lResult : LongInt;

begin
	Assert (sKey <> '');
    Assert (sName <> '');

	Result := false;

    if not (RegKeyOpenReadOnly (HRootKey, sKey, Key, bDisableRedirection)) then
    	exit;

    lResult := RegQueryValueEx (Key, PChar (sName), NIL, PDWord (@dwType), NIL,
    					        NIL);

    Result := CheckRegApiResult (lResult);

    if not (Result) then
        dwType := REG_NONE;

    RegKeyClose (Key);
end; { RegValueGetType }

(* ---- *)

function RegValueGetType (const HRootKey: HKey; const sKey, sName: String;
						  const bDisableRedirection: Boolean = false) : DWord;
                          											   overload;
{ Den Typ eines Wertes ermitteln.
  ->> HRootKey : Wurzelschlüssel, siehe REGEDIT;
  ->> sKey : Name des Schüssel, in dem sich der Wert befindet;
  ->> sName : Name des Werts, der gesucht wird;
  <<- Result : der Typ oder "REG_NONE" im Fehlerfall. }

begin
	Assert (sKey <> '');
    Assert (sName <> '');

	RegValueGetType (HRootKey, sKey, sName, Result, bDisableRedirection);
end; { RegValueGetType }

(* ---- *)

function RegWriteBinary (const HRootKey: HKey; const sKey: String;
					  	 const bCreateKey: Boolean; const sName: String;
                      	 const pData: Pointer; const dwBufSize: DWord;
                      	 const bDisableRedirection: Boolean = false) : Boolean;
{ Integer-Wert in die Registry schreiben.
  ->> HRootKey : Wurzelschlüssel, siehe REGEDIT;
  ->> sKey : Name des Schüssel, in dem sich der Wert befindet;
  ->> bCreateKey : Soll der Schlüssel angelegt werden, wenn es ihn nicht gibt?
  ->> sName : Name des Werts, der geschrieben werden soll;
  ->> dwValue : Zu schreibender Wert;
  <<- Result : TRUE, wenn OK, sonst FALSE }

var
	Key: HKEY;
    lResult : LongInt;

begin
	Assert (sKey <> '');
    Assert (sName <> '');

	Result := false;

    if (bCreateKey) then
    begin
    	if not (RegKeyCreate (HRootKey, sKey, Key, bDisableRedirection)) then
        	exit;
    end { if }
    else
	    if not (RegKeyOpen (HRootKey, sKey, Key, bDisableRedirection)) then
    		exit;

    lResult := RegSetValueEx (Key, PChar (sName), 0, REG_Binary, pData,
    						  dwBufSize);

    Result := CheckRegApiResult (lResult);

    RegKeyClose (Key);
end; { RegWriteBinary }

(* ---- *)

function RegWriteDWord (const HRootKey: HKey; const sKey: String;
					    const bCreateKey: Boolean; const sName: String;
                        const dwValue: DWord;
                        const bDisableRedirection: Boolean = false) : Boolean;
{ DWord (Cardinal)-Wert in die Registry schreiben.
  ->> HRootKey : Wurzelschlüssel, siehe REGEDIT;
  ->> sKey : Name des Schüssel, in dem sich der Wert befindet;
  ->> bCreateKey : Soll der Schlüssel angelegt werden, wenn es ihn nicht gibt?
  ->> sName : Name des Werts, der geschrieben werden soll;
  ->> dwValue : Zu schreibender Wert;
  <<- Result : TRUE, wenn OK, sonst FALSE }

var
	Key: HKEY;
    lResult : LongInt;

begin
	Assert (sKey <> '');
    Assert (sName <> '');

	Result := false;

    if (bCreateKey) then
    begin
    	if not (RegKeyCreate (HRootKey, sKey, Key, bDisableRedirection)) then
        	exit;
    end { if }
    else
	    if not (RegKeyOpen (HRootKey, sKey, Key, bDisableRedirection)) then
    		exit;

    lResult := RegSetValueEx (Key, PChar (sName), 0, REG_DWORD, @dwValue,
    						  SizeOf (DWord));

    Result := CheckRegApiResult (lResult);

    RegKeyClose (Key);
end; { RegWriteDWord }

(* ---- *)

function RegWriteMulti_SZ (const HRootKey: HKey; const sKey: String;
					  	   const bCreateKey: Boolean; const sName: String;
                           const asValues: TaString;
                      	  const bDisableRedirection: Boolean = false) : Boolean;

var
	Key: HKEY;
    dwLen : DWord;
    iIndex : Integer;
    sMulti_SZ : String;
    lResult : LongInt;

begin
	Assert (sKey <> '');
    Assert (sName <> '');
    Assert (Length (asValues) > 0);

	Result := false;

    if (bCreateKey) then
    begin
    	if not (RegKeyCreate (HRootKey, sKey, Key, bDisableRedirection)) then
        	exit;
    end { if }
    else
	    if not (RegKeyOpen (HRootKey, sKey, Key, bDisableRedirection)) then
    		exit;

    if (Length (asValues) > 0) then
    begin
	    for iIndex := 0 to High (asValues) do
            sMulti_SZ := {%H-}sMulti_SZ + asValues [iIndex] {%H-}+ #0;

        sMulti_SZ := sMulti_SZ + #0;
    end { if }
    else sMulti_SZ := #0#0;  // Abschluss durch "#0#0"';

    dwLen := Length (sMulti_SZ) * SizeOf (Char); { #0 am Ende mitspeichern }

    lResult := RegSetValueEx (Key, PChar (sName), 0, REG_MULTI_SZ,
    						 PChar (sMulti_SZ), dwLen);

    Result := CheckRegApiResult (lResult);

    RegKeyClose (Key);
end; { RegWriteMulti_SZ }

(* ---- *)

function RegWriteRegExpandSZ (const HRootKey: HKey; const sKey: String;
					  		  const bCreateKey: Boolean;
                              const sName, sValue: String;
                      	  const bDisableRedirection: Boolean = false) : Boolean;
{ Zeichenkette in die Registry schreiben.
  ->> HRootKey : Wurzelschlüssel, siehe REGEDIT;
  ->> sKey : Name des Schüssel, in dem sich der Wert befindet;
  ->> bCreateKey : Soll der Schlüssel angelegt werden, wenn es ihn nicht gibt?
  ->> sName : Name des Werts, der geschrieben werden soll. Ist "sName" leer,
  			   dann wird der Default-Wert geschrieben;
  ->> sValue : Zu schreibender Wert;
  <<- Result : TRUE, wenn OK, sonst FALSE }

var
	Key: HKEY;
    dwLen : DWord;
    lResult : LongInt;

begin
	Assert (sKey <> '');
    Assert (sName <> '');

	Result := false;

    if (bCreateKey) then
    begin
    	if not (RegKeyCreate (HRootKey, sKey, Key, bDisableRedirection)) then
        	exit;
    end { if }
    else
	    if not (RegKeyOpen (HRootKey, sKey, Key, bDisableRedirection)) then
    		exit;

    dwLen := (Length (sValue) + 1) * SizeOf (Char); { #0 am Ende mitspeichern }

    lResult := RegSetValueEx (Key, PChar (sName), 0, REG_EXPAND_SZ,
    						  PChar (sValue), dwLen);

    Result := CheckRegApiResult (lResult);

    RegKeyClose (Key);
end; { RegWriteRegExpandSZ }

(* ---- *)

function RegWriteStr (const HRootKey: HKey; const sKey: String;
					  const bCreateKey: Boolean;
                      const sName, sValue: String;
                      const bDisableRedirection: Boolean = false) : Boolean;
{ Zeichenkette in die Registry schreiben.
  ->> HRootKey : Wurzelschlüssel, siehe REGEDIT;
  ->> sKey : Name des Schüssel, in dem sich der Wert befindet;
  ->> bCreateKey : Soll der Schlüssel angelegt werden, wenn es ihn nicht gibt?
  ->> sName : Name des Werts, der geschrieben werden soll. Ist "sName" leer,
  			   dann wird der Default-Wert geschrieben;
  ->> sValue : Zu schreibender Wert;
  <<- Result : TRUE, wenn OK, sonst FALSE }

var
	Key: HKEY;
    dwLen : DWord;
    lResult : LongInt;

begin
	Assert (sKey <> '');
    Assert (sName <> '');

	Result := false;

    if (bCreateKey) then
    begin
    	if not (RegKeyCreate (HRootKey, sKey, Key, bDisableRedirection)) then
        	exit;
    end { if }
    else
	    if not (RegKeyOpen (HRootKey, sKey, Key, bDisableRedirection)) then
    		exit;

    dwLen := (Length (sValue) + 1) * SizeOf (Char); { #0 am Ende mitspeichern }

    lResult := RegSetValueEx (Key, PChar (sName), 0, REG_SZ, PChar (sValue),
    				   		  dwLen);

    Result := CheckRegApiResult (lResult);

    RegKeyClose (Key);
end; { RegWriteStr }

(* ---- *)

end.
