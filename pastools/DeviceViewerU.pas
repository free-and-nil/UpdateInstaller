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
{$ENDIF FPC}

{$WARN SYMBOL_DEPRECATED OFF}
{$WARN SYMBOL_PLATFORM OFF}

unit DeviceViewerU;

interface

uses Classes, Windows, Contnrs,
{$IFDEF IMAGE_LIST}
     Controls,
{$ENDIF}
	 SetupApi_ImportU, BaseTypesU;

const
    cCaption = 'Caption';
    cDescription = 'Description';
    cManufacturer = 'Manufacturer';
    cDriverVer = 'DriverVer';
    cDriverDate = 'DriverDate';
    cDriverProvider = 'Driver Provider';
    cClass = 'Class';
    cDevicePresent = 'Device present';
    cDeviceStarted = 'Device started';
    cDeviceDisabled = 'Device disabled';
    cDeviceHidden = 'Device hidden';
    cCount = 'Count';
    cHardware_ID = 'Hardware ID';
    cPCI_ID_Prefix = 'PCI_ID_';

type
	TDeviceClass = class;

	TDevice = class
      private
        FCaption,
        FDescription,
        FDriverKey,
        FManufacturer,
        FServiceName,
        FDriverVer,
        FDriverProvider : String;
        FHardware_IDs : TaString;
        FDeviceDisabled, FDeviceHidden, FDevicePresent, FDeviceStarted : Boolean;

{$IFDEF IMAGE_LIST}
        FImageIndex : Integer;
{$ENDIF}
        ftDriverDate : TFileTime;
        DeviceClass : TDeviceClass;

        iClassIndex : Integer;

        function GetClassGuid : String;
        function GetClassDescription : String;
        function GetDriverDateAsDateTime : TDateTime;
        function GetDriverDateAsStr : String;

        function GetDevNodeStatus (const dwDevInst: DWord) : Boolean;
        procedure GetDriverInfo (const hDeviceInfo: HDEVINFO;
        					     var DevInfoData: TSPDevInfoData);
        function GetPropertiesLegacy (const hDeviceInfo: HDEVINFO;
        					          var DevInfoData: TSPDevInfoData;
                                      var sDeviceClass: String) : Boolean;
        function GetPropertiesVista (const hDeviceInfo: HDEVINFO;
        					         var DevInfoData: TSPDevInfoData;
                                     var sClassGuid: String) : Boolean;
        procedure SetDeviceClass (const DeviceClass: TDeviceClass);

      public
      	constructor Create;

        function GetDriverDate (out DateTime: TDateTime) : Boolean;
        function GetHardware_IDs : String; overload;
        function GetHardware_IDs (out iCount: Integer;
                                  const bIniFormat: Boolean = false) : String;
                                                                       overload;

        property Caption : String read FCaption;
        property ClassGuid : String read GetClassGuid;
        property ClassDescription : String read GetClassDescription;
        property Description : String read FDescription;
        property DeviceDisabled : Boolean read FDeviceDisabled;
        property DeviceHidden : Boolean read FDeviceHidden;
        property DevicePresent : Boolean read FDevicePresent;
        property DeviceStarted : Boolean read FDeviceStarted;
        property DriverDate : TDateTime read GetDriverDateAsDateTime;
        property DriverDateStr : String read GetDriverDateAsStr;
        property DriverKey : String read FDriverKey;
        property DriverProvider : String read FDriverProvider;
        property DriverVer : String read FDriverVer;
        property Hardware_IDs : TaString read FHardware_IDs;
{$IFDEF IMAGE_LIST}
        property ImageIndex : Integer read FImageIndex;
{$ENDIF}
        property Manufacturer : String read FManufacturer;
        property ServiceName : String read FServiceName;
    end; { TDevice }

    TDeviceClassEnumerator = class
      private
        FIndex: Integer;
        FDeviceClass : TDeviceClass;

      public
        constructor Create (const DeviceClass : TDeviceClass);
        function GetCurrent : TDevice;
        function MoveNext : Boolean;
        property Current : TDevice read GetCurrent;
    end; { TDeviceClassEnumerator }

	TDeviceClass = class
      private
    	FDescription : String;
        FClassGuid : TGuid;

{$IFDEF IMAGE_LIST}
        FImageIndex : Integer;
        pClassImageListData : PSPClassImageListData;
{$ENDIF}
      	DeviceList : TObjectList;

        function AddDevice (const Device: TDevice) : Integer;
        function GetDevice (iIndex: Integer) : TDevice;
        function GetDeviceCount : Integer;
        function GetGuid : String;
{$IFDEF IMAGE_LIST}
        procedure SetClassImageListData (const pCILD: PSPClassImageListData);
{$ENDIF}

      public
        constructor Create (const sDescription: String; const Guid: TGuid);
        destructor Destroy; override;

        function GetEnumerator : TDeviceClassEnumerator;
        function HiddenDevices : Integer;
        function NonPresentDevices : Integer;

        property Description : String read FDescription;
        property DeviceCount : Integer read GetDeviceCount;
        property Devices [Index: Integer] : TDevice read GetDevice; default;
        property ClassGuid : String read GetGuid;
        property GuidGuid : TGuid read FClassGuid;
{$IFDEF IMAGE_LIST}
        property ImageIndex : Integer read FImageIndex;
{$ENDIF}
    end; { TDeviceClass }

    TDevices = class;

    TDevicesEnumerator = class
      private
        FIndex: Integer;
        FDevices : TDevices;

      public
        constructor Create (const Devices : TDevices);
        function GetCurrent : TDevice;
        function MoveNext : Boolean;
        property Current : TDevice read GetCurrent;
    end; { TDevicesEnumerator }

    TDevices = class
      private
      	FClasses : TObjectList;
        FDevices : TObjectList;
        FIncludeNonpresentDevices : Boolean;
        FUserLangLCID : LANGID;
        FUseVistaFunctions : Boolean;

{$IFDEF IMAGE_LIST}
        ClassImageList : TImageList;
        CIL_Data : TSPClassImageListData;

    {$IFDEF FPC}
        procedure CopyImagesFromSysImageListToImageList;
    {$ENDIF}
{$ENDIF}

        function GetClassCount : Integer;
        function GetClass (iIndex: Integer) : TDeviceClass;
        function GetDeviceCount : Integer;
        function GetDevice (iIndex: Integer) : TDevice;
{$IFDEF IMAGE_LIST}
        procedure InitImageList;
{$ENDIF}
        procedure LoadSavedTextList (const sFileName: String;
                                     out bHidden, bNonPresent: Boolean);

      public
        constructor Create (const bIncludeNonpresentDevices: Boolean = false;
        					const bRefreshNow: Boolean = true
{$IFDEF IMAGE_LIST}
        					; const ImageList: TImageList = NIL
{$ENDIF}
                                                                    );
        constructor CreateFromFile (const sFileName: String;
                                    out bHidden, bNonPresent: Boolean);
        destructor Destroy; override;

        function GetEnumerator : TDevicesEnumerator;
        procedure Refresh (const bIncludeNonpresentDevices: Boolean);
        procedure Hardware_IDs_ToLower;

        property ClassCount : Integer read GetClassCount;
        property Classes [iIndex: Integer] : TDeviceClass read GetClass;
      	property Count : Integer read GetDeviceCount;
        property Devices [iIndex: Integer] : TDevice read GetDevice; default;
        property IncludeNonpresentDevices : Boolean
                                                 read FIncludeNonpresentDevices;
        property UserLangLCID : LANGID read FUserLangLCID;
        property UseVistaFunctions : Boolean read FUseVistaFunctions
                                             write FUseVistaFunctions;
    end; { TDevices }

implementation

uses SysUtils,
{$IFDEF FPC}
     CommCtrl, JwaWinError,
  {$IFNDEF CONSOLEAPP}
     LazarusShellImageListU,
  {$ENDIF}
{$ENDIF}
     Regstr_ImportU, VerifyU, Delphi_T, CfgMgr32_ImportU, Cfg_ImportU,
     Win32ToolsU, WinTools;

const
{$IFNDEF FPC}
    // MessageId: ERROR_NOT_FOUND
    // MessageText: Element not found.
    ERROR_NOT_FOUND = DWord (1168);
{$ENDIF}

    cUnknownClass = '?';
    cUnknownDevice = 'Unknown device #%d';

var
    iUnknownCount : Integer;

(* ---- *)

function ClassesCompare (Item1, Item2: Pointer) : Integer;
begin
	Result := StrComp (PChar (TDeviceClass (Item1).Description),
                       PChar (TDeviceClass (Item2).Description));
end; { ClassesCompare }

(* ---- *)

function DevicesCompare (Item1, Item2: Pointer) : Integer;

var
    Device1, Device2 : TDevice;

begin
    Device1 := TDevice (Item1);
    Device2 := TDevice (Item2);

	Result := StrComp (PChar (Device1.FCaption), PChar (Device2.FCaption));

    if (Result = 0) then
        if (Device1.FDriverKey <> '') and (Device2.FDriverKey <> '') then
            Result := StrComp (PChar (Device1.FDriverKey),
                               PChar (Device2.FDriverKey));
end; { DevicesCompare }

(* ---- *)

{$IFDEF IMAGE_LIST}
function GetDeviceImageIndex (const CIL_Data: TSPClassImageListData;
							  const DeviceGUID: TGUID) : Integer;
begin
	if not (SetupDiGetClassImageIndex (@CIL_Data, @DeviceGUID,
    								   Result{%H-})) then
  		Result := (-1);
end; { GetDeviceImageIndex }
{$ENDIF}

(* ---- *)

function TDevice.GetClassGuid : String;
begin
    if (DeviceClass.Description <> cUnknownClass) then
        Result := GuidToString (DeviceClass.FClassGuid)
    else Result := cUnknownClass;
end; { TDevice.GetClassGuid }

(* ---- *)

function TDevice.GetClassDescription : String;
begin
    Result := DeviceClass.Description;
end; { TDevice.GetClassDescription }

(* ---- *)

function TDevice.GetDriverDateAsDateTime : TDateTime;
begin
    if not (GetDriverDate (Result)) then
        Result := 0.0;
end; {TDevice.GetDriverDateAsDateTime }

(* ---- *)

function TDevice.GetDriverDateAsStr : String;

var
    DateTime : TDateTime;

begin
    if (GetDriverDate (DateTime)) then
        Result := DateToStr (DateTime)
    else Result := '';
end; { TDevice.GetDriverDateAsStr }

(* ---- *)

function TDevice.GetDevNodeStatus (const dwDevInst: DWord) : Boolean;

var
    ulStatus, ulProblemNumber : ULONG;
    CR : CONFIGRET;

begin
    ulStatus := 0;
    ulProblemNumber := 0;

    CR := CM_Get_DevNode_Status (ulStatus, ulProblemNumber, dwDevInst, 0);

    if (CR = CR_SUCCESS) then
    begin
        Result := true;
        FDevicePresent := true;
        FDeviceHidden := ulStatus and DN_NO_SHOW_IN_DM <> 0;
        FDeviceStarted := ulStatus and DN_STARTED <> 0;
    end { if }
    else
        if (CR = CR_NO_SUCH_DEVINST) or (CR = CR_NO_SUCH_VALUE) then
        begin
            Result := true;
            FDevicePresent := false;
        end { if }
        else Result := false;
end; { TDevice.GetDevNodeStatus }

(* ---- *)

procedure TDevice.GetDriverInfo (const hDeviceInfo: HDEVINFO;
								 var DevInfoData: TSPDevInfoData);

	(* ---- *)

    function RegReadStr (const hRegKey: HKEY; const sValueName: String;
    					 out sValue: String) : Boolean;

    var
		dwLen, dwType : DWord;

    begin
    	Result := false;
        dwLen := 0;

        if (RegQueryValueEx (hRegKey, PChar (sValueName), NIL, @dwType,
                             NIL, @dwLen) = Error_Success) then
            if (dwType = REG_SZ) or (dwType = REG_EXPAND_SZ) then
            begin
                SetLength (sValue{%H-}, Pred (dwLen div SizeOf (Char)));

                if (RegQueryValueEx (hRegKey, PChar (sValueName), NIL, @dwType,
                					 PByte (PChar (sValue)),
                                     @dwLen) = Error_Success) then
                	Result := true
                else sValue := '';
            end; { if }
    end; { RegReadStr }

	(* ---- *)

    function RegReadFileTime (const hRegKey: HKEY; const sValueName: String;
    					   	  out FileTime: TFileTime) : Boolean;

    var
		dwLen, dwType : DWord;

    begin
    	Result := false;
        dwLen := 0;

        if (RegQueryValueEx (hRegKey, PChar (sValueName), NIL, @dwType,
                             NIL, @dwLen) = Error_Success) then
            if (dwType = REG_BINARY) and (dwLen = 8) then
                if (RegQueryValueEx (hRegKey, PChar (sValueName), NIL, @dwType,
                					 PByte (@FileTime),
                                     @dwLen) = Error_Success) then
                	Result := true
                else FillChar (FileTime, SizeOf (TFileTime), #0);
    end; { RegReadFileTime }

    (* ---- *)

const
	cDriverVersion = 'DriverVersion';
    cProviderName = 'ProviderName';
//	cDriverDate = 'DriverDate';
    cDriverDateData = 'DriverDateData';

var
	hRegKey : HKEY;

begin { TDevice.GetDriverInfo }
    hRegKey := SetupDiOpenDevRegKey (hDeviceInfo, DevInfoData, DICS_FLAG_GLOBAL,
    								 0, DIREG_DRV, KEY_READ);

    if (hRegKey <> INVALID_HANDLE_VALUE) then
    begin
        if (RegReadStr (hRegKey, cDriverVersion, FDriverVer)) then
        begin
			RegReadFileTime (hRegKey, cDriverDateData, ftDriverDate);
            RegReadStr (hRegKey, cProviderName, FDriverProvider);
        end; { if }

        VerifyApi (RegCloseKey (hRegKey) = ERROR_SUCCESS);
    end; { if }
end; { TDevice.GetDriverInfo }

(* ---- *)

function TDevice.GetPropertiesLegacy (const hDeviceInfo: HDEVINFO;
								      var DevInfoData: TSPDevInfoData;
                                      var sDeviceClass: String) : Boolean;

	(* ---- *)

    function GetProperty (const dwProperty: DWord;
    					  var sResult: String) : Boolean; overload;

    var
        dwRequiredSize, dwRegDataType, dwBufSize : DWord;
        iLen : Integer;

    begin
        Result := false;

        dwRequiredSize := 0;
        dwRegDataType := 0;

        SetupDiGetDeviceRegistryProperty (hDeviceInfo, DevInfoData, dwProperty,
        								  dwRegDataType, NIL, 0, dwRequiredSize);

        if (GetLastError <> ERROR_INSUFFICIENT_BUFFER) then
            exit;

        dwBufSize := dwRequiredSize;
        iLen := Pred (dwBufSize div SizeOf (Char));
        SetLength (sResult, iLen);

        if (iLen = 0) or
           (SetupDiGetDeviceRegistryProperty (hDeviceInfo, DevInfoData,
                                              dwProperty, dwRegDataType,
                                              PByte (PChar (sResult)),
                                              dwBufSize,
                                              dwRequiredSize)) then
            Result := true
        else sResult := '';
    end; { GetProperty }

	(* ---- *)

    function GetProperty (const dwProperty: DWord;
    					  var dwResult: DWord) : Boolean; overload;

    var
        dwRequiredSize, dwRegDataType : DWord;

    begin
        dwRequiredSize := 0;
        dwRegDataType := 0;

        Result := SetupDiGetDeviceRegistryProperty (hDeviceInfo, DevInfoData,
        											dwProperty, dwRegDataType,
                                                    PByte (@dwResult),
                                                    SizeOf (DWord),
                                                    dwRequiredSize);
	end; { GetProperty }

	(* ---- *)

    function GetPropertyWide (const dwProperty: DWord;
    					      var sResult: String) : Boolean;

    var
        dwRequiredSize, dwRegDataType, dwBufSize : DWord;
        sWideResult : WideString;
        iLen : Integer;

    begin
        Result := false;

        dwRequiredSize := 0;
        dwRegDataType := 0;

        SetupDiGetDeviceRegistryPropertyW (hDeviceInfo, DevInfoData, dwProperty,
        								   dwRegDataType, NIL, 0,
                                           dwRequiredSize);

        if (GetLastError <> ERROR_INSUFFICIENT_BUFFER) then
            exit;

        dwBufSize := dwRequiredSize;
        iLen := Pred (dwBufSize div SizeOf (WideChar));
        SetLength (sWideResult{%H-}, iLen);

        if (iLen = 0) or
           (SetupDiGetDeviceRegistryPropertyW (hDeviceInfo, DevInfoData,
                                               dwProperty, dwRegDataType,
                                               PByte (PWideChar (sWideResult)),
                                               dwBufSize,
                                               dwRequiredSize)) then
        begin
            Result := true;
            sResult := String (sWideResult);
        end { if }
        else sResult := '';
    end; { GetPropertyWide }

    (* ---- *)

var
	dwResult : DWord;
    sHardware_IDs : String;
    bUnknownDevice : Boolean;

begin { TDevice.GetPropertiesLegacy }
    Result := false;
    bUnknownDevice := false;
    sDeviceClass := '';

    if not (GetProperty (SPDRP_CLASSGUID, sDeviceClass)) then
        if (GetLastError = ERROR_INVALID_DATA) then
            sDeviceClass := cUnknownClass
        else exit;

    if (Win32MajorVersion >= 5) then
    begin
	    GetPropertyWide (SPDRP_DEVICEDESC, FDescription);
	    GetPropertyWide (SPDRP_FRIENDLYNAME, FCaption);
    end { if }
    else
    begin
	    GetProperty (SPDRP_DEVICEDESC, FDescription);
	    GetProperty (SPDRP_FRIENDLYNAME, FCaption);
    end; { else }

    if (FCaption = '') then
        if (FDescription <> '') then
            FCaption := FDescription
        else
        begin
            bUnknownDevice := true;
            Inc (iUnknownCount);
            FCaption := Format (cUnknownDevice, [iUnknownCount]);
        end; { else }

    if (GetProperty (SPDRP_HARDWAREID, sHardware_IDs{%H-})) then  // RegMulti_SZ
        FHardware_IDs := ConvertWindowsStringArray (sHardware_IDs)
    else
        if (bUnknownDevice) then
        begin
            Dec (iUnknownCount);
            exit;
        end; { if }

    GetDevNodeStatus (DevInfoData.DevInst);

    if (GetProperty (SPDRP_CONFIGFLAGS, dwResult{%H-})) then
        FDeviceDisabled := (dwResult and CONFIGFLAG_DISABLED) <> 0;

    if (sDeviceClass <> cUnknownClass) then
    begin
        GetProperty (SPDRP_DRIVER, FDriverKey);
        GetProperty (SPDRP_MFG, FManufacturer);
        GetProperty (SPDRP_SERVICE, FServiceName);
        GetDriverInfo (hDeviceInfo, DevInfoData);
    end; { if }

    Result := true;
end; { TDevice.GetPropertiesLegacy }

(* ---- *)

function TDevice.GetPropertiesVista (const hDeviceInfo: HDEVINFO;
                                     var DevInfoData: TSPDevInfoData;
                                     var sClassGuid: String) : Boolean;

	(* ---- *)

    function GetProperty (const PropertyKey: TDevPropKey;
    					  out sResult: String) : Boolean; overload;

    var
        dwRequiredSize : DWord;
        sWideResult : WideString;
        PropertyType : TDevPropType;
        iLen : Integer;

    begin
        Result := false;
        dwRequiredSize := 0;

        if not (SetupDiGetDevicePropertyW (hDeviceInfo, @DevInfoData,
                                           @PropertyKey, @PropertyType, NIL, 0,
                                           @dwRequiredSize, 0)) then
            if (GetLastError <> ERROR_INSUFFICIENT_BUFFER) then
            begin
    {$IFDEF DEBUG}
                OutputDebugStr ('GetLastError <> ERROR_INSUFFICIENT_BUFFER');
    {$ENDIF}

                exit;
            end;

        iLen := Pred (dwRequiredSize div SizeOf (WideChar));
        SetLength (sWideResult{%H-}, iLen);

        if (iLen = 0) or
           (SetupDiGetDevicePropertyW (hDeviceInfo, @DevInfoData, @PropertyKey,
                                       @PropertyType,
                                       PByte (PWideChar (sWideResult)),
                                       dwRequiredSize, NIL, 0)) then
        begin
        	Result := true;
            sResult := String (sWideResult);
        end { if }
        else sResult := '';
    end; { GetProperty }

	(* ---- *)

    function GetProperty (const PropertyKey: TDevPropKey;
    					  var dwResult: DWord) : Boolean; overload;

    var
        PropertyType : TDevPropType;

    begin
        Result := SetupDiGetDevicePropertyW (hDeviceInfo, @DevInfoData,
                                             @PropertyKey, @PropertyType,
                                             PByte (@dwResult), SizeOf (DWord),
                                             NIL, 0);
    end; { GetProperty }

	(* ---- *)

    function GetClassGuid (const PropertyKey: TDevPropKey;
    					   var sGuid: String) : Boolean;

    var
        PropertyType : TDevPropType;
        Guid : TGuid;

    begin
        if (SetupDiGetDevicePropertyW (hDeviceInfo, @DevInfoData, @PropertyKey,
                                       @PropertyType, PByte (@Guid),
                                       SizeOf (TGuid), NIL, 0)) then
        begin
            Result := true;
            sGuid := GUIDToString (Guid);
        end { if }
        else Result := false;
    end; { GetProperty }

	(* ---- *)

var
    bUnknownDevice : Boolean;
    sHardware_IDs : String;
    dwResult : DWord;

begin { TDevice.GetPropertiesVista }
    Result := false;
    bUnknownDevice := false;
    sClassGuid := '';

    if not (GetClassGuid (DEVPKEY_Device_ClassGuid, sClassGuid)) then
        if (GetLastError = ERROR_NOT_FOUND) then  // Element not found
            sClassGuid := cUnknownClass
        else
        begin
{$IFDEF DEBUG}
            OutputDebugStr (GetLastErrorMsg);
{$ENDIF}
            exit;
        end; { else }

	if (GetProperty (DEVPKEY_Device_DeviceDesc, FDescription)) then
    begin
        if not (GetProperty (DEVPKEY_Device_FriendlyName, FCaption)) then
            FCaption := FDescription;
    end { if }
    else
    begin
        bUnknownDevice := true;
        Inc (iUnknownCount);
        FCaption := Format (cUnknownDevice, [iUnknownCount]);
    end; { else }

    if (GetProperty (DEVPKEY_Device_HardwareIds, sHardware_IDs)) then
        // RegMulti_SZ
        FHardware_IDs := ConvertWindowsStringArray (sHardware_IDs)
    else
        if (bUnknownDevice) then
        begin
            Dec (iUnknownCount);
            exit;
        end; { if }

    GetDevNodeStatus (DevInfoData.DevInst);

    if (GetProperty (DEVPKEY_Device_ConfigFlags, dwResult{%H-})) then
        FDeviceDisabled := (dwResult and CONFIGFLAG_DISABLED) <> 0;

    if (sClassGuid <> cUnknownClass) then
    begin
        GetProperty (DEVPKEY_Device_Driver, FDriverKey);
        GetProperty (DEVPKEY_Device_Manufacturer, FManufacturer);
        GetProperty (DEVPKEY_Device_Service, FServiceName);
        GetDriverInfo (hDeviceInfo, DevInfoData);
    end; { if }

    Result := true;
end; { TDevice.GetPropertiesVista }

(* ---- *)

procedure TDevice.SetDeviceClass (const DeviceClass: TDeviceClass);
begin
    Assert (DeviceClass <> NIL);

    Self.DeviceClass := DeviceClass;
end; { TDevice.SetDeviceClass }

(* ---- *)

constructor TDevice.Create;
begin
	inherited Create;

    iClassIndex := (-1);
    FDeviceHidden := true;

{$IFDEF IMAGE_LIST}
    FImageIndex := (-1);
{$ENDIF}
end; { TDevice.Create }

(* ---- *)

function TDevice.GetDriverDate (out DateTime: TDateTime) : Boolean;

var
    LocalFileTime : TFileTime;
    SystemTime : TSystemTime;

begin
	Result := false;

    with ftDriverDate do
    	if (dwLowDateTime = 0) and (dwHighDateTime = 0) then
    		exit;

    if (FileTimeToLocalFileTime (ftDriverDate, LocalFileTime{%H-})) then
    	if (FileTimeToSystemTime (LocalFileTime, SystemTime{%H-})) then
        begin
        	Result := true;
        	DateTime := SystemTimeToDateTime (SystemTime);
        end; { if }
end; { TDevice.GetDriverDate }

(* ---- *)

function TDevice.GetHardware_IDs : String;

var
    iCount : Integer;

begin
    Result := GetHardware_IDs (iCount, false);
end; { TDevice.GetHardware_IDs }

(* ---- *)

function TDevice.GetHardware_IDs (out iCount: Integer;
                                  const bIniFormat: Boolean = false) : String;

var
    iIndex : Integer;

begin
    with TStringList.Create do
        try
            iCount := Length (FHardware_IDs);

            for iIndex := 0 to High (FHardware_IDs) do
                if (bIniFormat) then
                    Add (Format (cPCI_ID_Prefix + '%d=%s',
                                 [iIndex + 1, FHardware_IDs [iIndex]]))
                else Add (FHardware_IDs [iIndex]);

            Result := Text;

        finally
            Free;
        end; { try / finally }
end; { TDevice.GetHardware_IDs }

constructor TDeviceClassEnumerator.Create (const DeviceClass: TDeviceClass);
begin
    Assert (DeviceClass <> NIL);

    inherited Create;

    FIndex := (-1);
    FDeviceClass := DeviceClass;
end; { TDeviceClassEnumerator.Create }

(* ---- *)

function TDeviceClassEnumerator.GetCurrent : TDevice;
begin
	Result := FDeviceClass [FIndex];
end; { TDeviceClassEnumerator.GetCurrent }

(* ---- *)

function TDeviceClassEnumerator.MoveNext : Boolean;
begin
	Result := FIndex < (FDeviceClass.DeviceCount - 1);

    if (Result) then
    	Inc (FIndex);
end; { TDeviceClassEnumerator.MoveNext }

(* ---- *)

constructor TDeviceClass.Create (const sDescription: String; const Guid: TGuid);
begin
    Assert (sDescription <> '');

	inherited Create;

    DeviceList := TObjectList.Create;
    DeviceList.OwnsObjects := false;

    FDescription := sDescription;
    FClassGuid := Guid;

{$IFDEF IMAGE_LIST}
    FImageIndex := (-1);
{$ENDIF}
end; { TDeviceClass.Create }

(* ---- *)

destructor TDeviceClass.Destroy;
begin
    DeviceList.Free;

	inherited;
end; { TDeviceClass.Destroy }

(* ---- *)

function TDeviceClass.GetEnumerator : TDeviceClassEnumerator;
begin
	Result := TDeviceClassEnumerator.Create (Self);
end; { TDeviceClass.GetEnumerator }

(* ---- *)

function TDeviceClass.HiddenDevices : Integer;

var
    iIndex : Integer;

begin
    Result := 0;

    for iIndex := 0 to DeviceList.Count - 1 do
        if (TDevice (DeviceList [iIndex]).FDeviceHidden) then
            Inc (Result);
end; { TDeviceClass.HiddenDevices }

(* ---- *)

function TDeviceClass.NonPresentDevices : Integer;

var
    iIndex : Integer;

begin
    Result := 0;

    for iIndex := 0 to DeviceList.Count - 1 do
        if (TDevice (DeviceList [iIndex]).FDevicePresent = false) then
            Inc (Result);
end; { TDeviceClass.NonPresentDevices }

(* ---- *)

function TDeviceClass.AddDevice (const Device: TDevice) : Integer;
begin
    Assert (Device <> NIL);

    Result := DeviceList.Add (Device);
end; { TDeviceClass.AddDevice }

(* ---- *)

function TDeviceClass.GetDevice (iIndex: Integer) : TDevice;
begin
	Result := DeviceList [iIndex] as TDevice;
end; { TDeviceClass.GetDevice }

(* ---- *)

function TDeviceClass.GetDeviceCount : Integer;
begin
    Result := DeviceList.Count;
end; { TDeviceClass.GetDeviceCount }

(* ---- *)

function TDeviceClass.GetGuid : String;
begin
	Result := GUIDToString (FClassGuid);
end; { TDeviceClass.GetGuid }

(* ---- *)

{$IFDEF IMAGE_LIST}
procedure TDeviceClass.SetClassImageListData (const pCILD: PSPClassImageListData);
begin
    Assert (Assigned (pCILD));

    pClassImageListData := pCILD;
end; { TDeviceClass.SetClassImageListData }
{$ENDIF}

(* ---- *)

constructor TDevices.Create (const bIncludeNonpresentDevices: Boolean = false;
                			 const bRefreshNow: Boolean = true
{$IFDEF IMAGE_LIST}
        					 ; const ImageList: TImageList = NIL
{$ENDIF}
                             );
begin
	inherited Create;


{$IFDEF IMAGE_LIST}
    if (Assigned (ImageList)) then
    begin
	    ClassImageList := ImageList;
        InitImageList;
    end; { if }
{$ENDIF}

    FClasses := TObjectList.Create (true);
    FDevices := TObjectList.Create (true);

    FIncludeNonpresentDevices := bIncludeNonpresentDevices;
	FUserLangLCID := GetUserDefaultLangID;

    FUseVistaFunctions := Win32MajorVersion >= 6;

    if (bRefreshNow) then
    	Refresh (bIncludeNonpresentDevices);
end; { TDevices.Create }

(* ---- *)

constructor TDevices.CreateFromFile (const sFileName: String;
                                     out bHidden, bNonPresent: Boolean);
begin
    Assert (sFileName <> '');

    Create (false, false);

    if (LowerCase (ExtractFileExt (sFileName)) = cIniExt) then
        raise Exception.Create ('Functionality not implemented')
    else LoadSavedTextList (sFileName, bHidden, bNonPresent);
end; { TDevices.CreateFromFile }

(* ---- *)

destructor TDevices.Destroy;
begin
{$IFDEF IMAGE_LIST}
    if (Assigned (ClassImageList)) then
    	VerifyApi (SetupDiDestroyClassImageList (CIL_Data));
{$ENDIF}

    FClasses.Free;
    FDevices.Free;

  	inherited;
end; { TDevices.Destroy }

(* ---- *)

constructor TDevicesEnumerator.Create (const Devices: TDevices);
begin
    Assert (Devices <> NIL);

    inherited Create;

    FIndex := (-1);
    FDevices := Devices;
end; { TDevicesEnumerator.Create }

(* ---- *)

function TDevicesEnumerator.GetCurrent : TDevice;
begin
	Result := FDevices [FIndex] as TDevice;
end; { TDevicesEnumerator.GetCurrent }

(* ---- *)

function TDevicesEnumerator.MoveNext : Boolean;
begin
    Result := FIndex < (FDevices.Count - 1);

    if (Result) then
    	Inc (FIndex);
end; { TDevicesEnumerator.MoveNext }

(* ---- *)

{$IFDEF IMAGE_LIST}
    {$IFDEF FPC}
procedure TDevices.CopyImagesFromSysImageListToImageList;

var
    SIL : TShellImageList;
    iIndex, iCount : Integer;

begin
    SIL := TShellImageList.Create (NIL, isSmall);

    try
        iCount := ImageList_GetImageCount (CIL_Data.ImageList);

        if (iCount > 0) then
        begin
            for iIndex := 0 to iCount do
                SIL.AddShellIcon (ImageList_GetIcon (CIL_Data.ImageList,
                                                     iIndex, ILD_NORMAL),
                                  IntToStr (iIndex));

            ClassImageList.Assign (SIL);
        end; { if }

    finally
        SIL.Free;
    end; { try / finally }
end; { TDevices.CopyImagesFromSysImageListToImageList }
    {$ENDIF FPC}
{$ENDIF IMAGE_LIST}

(* ---- *)

function TDevices.GetClass (iIndex: Integer) : TDeviceClass;
begin
	Result := FClasses [iIndex] as TDeviceClass;
end; { TDevices.GetClass }

(* ---- *)

function TDevices.GetClassCount : Integer;
begin
    Result := FClasses.Count;
end; { TDevices.GetClassCount }

(* ---- *)

function TDevices.GetDeviceCount : Integer;
begin
	Result := FDevices.Count;
end; { TDevices.GetDeviceCount }

(* ---- *)

function TDevices.GetDevice (iIndex: Integer) : TDevice;
begin
	Result := FDevices [iIndex] as TDevice;
end; { TDevices.GetDevice }

(* ---- *)

{$IFDEF IMAGE_LIST}
procedure TDevices.InitImageList;
begin
    CIL_Data.cbSize := SizeOf (TSPClassImageListData);
    Win32Check (SetupDiGetClassImageList (CIL_Data));

{$IFNDEF FPC}
    ClassImageList.Handle := CIL_Data.ImageList;
{$ENDIF}
end; { TDevices.InitImageList }
{$ENDIF}

(* ---- *)

procedure TDevices.LoadSavedTextList (const sFileName: String;
                                      out bHidden, bNonPresent: Boolean);

    (* ---- *)

    function GetDeviceProperty (const sLine: String; const sName: String;
                                var sValue: String) : Boolean;

    var
        iPos : Integer;

    begin
        Result := Pos (sName + ' =', sLine) = 1;

        if (Result) then
        begin
            iPos := Length (sName) + 3;

            if (Length (sLine) > iPos) then
                sValue := Copy (sLine, Succ (iPos), Length (sLine) - iPos)
            else sValue := '';
        end; { if }
    end; { GetDeviceProperty }

    (* ---- *)

    function GetHardware_ID (const sLine: String; var sValue: String) : Boolean;

    var
        iPos : Integer;

    begin
        Result := Pos (cHardware_ID + ':', sLine) = 1;

        if (Result) then
        begin
            iPos := Length (cHardware_ID) + 2;

            if (Length (sLine) > iPos) then
                sValue := Copy (sLine, iPos + 3, Length (sLine) - (iPos + 2))
            else sValue := '';
        end; { if }
    end; { GetHardware_ID }

    (* ---- *)

    function GetHardware_IDS (const TextList: TStrings; iIndex: Integer;
                              var Hardware_IDs: TaString) : Boolean;

    var
        iDevice : Integer;

    begin
        Result := Pos (cHardware_ID + 's:', TextList [iIndex]) = 1;

        if (Result) then
        begin
            Inc (iIndex);
            iDevice := 0;

            while ((iIndex < TextList.Count) and (TextList [iIndex] <> '') and
                   (TextList [iIndex][1] <> '=') and (TextList [iIndex][1] <> '-')) do
            begin
                Inc (iDevice);

                SetLength (Hardware_IDs, iDevice);
                Hardware_IDs [iDevice - 1] := TextList [iIndex];

                Inc (iIndex);
            end; { while }
        end; { if }
    end; { GetHardware_IDS }

    (* ---- *)

    function AddDevices (const TextList: TStrings;
                         const DeviceClass: TDeviceClass;
                         iIndex: Integer) : Integer;

        (* ---- *)

        function AddDevice (var iIndex: Integer; out Device: TDevice) : Boolean;

        var
            sValue : String;

        begin
            Device := TDevice.Create;

            if (GetDeviceProperty (TextList [iIndex],
            					   cCaption, sValue{%H-})) then
            begin
                Device.FCaption := sValue;

                if (GetDeviceProperty (TextList [iIndex + 1], cDescription,
                                       sValue)) then
                begin
                    Device.FDescription := sValue;

                    if (GetDeviceProperty (TextList [iIndex + 2], cManufacturer,
                                           sValue)) then
                        Device.FManufacturer := sValue;

                    if (GetDeviceProperty (TextList [iIndex + 3], cDriverVer,
                                           sValue)) then
                        Device.FDriverVer := sValue;

                    // Skip #4 driver date

                    if (GetDeviceProperty (TextList [iIndex + 5],
                                           cDriverProvider, sValue)) then
                        Device.FDriverProvider := sValue;

                    // Skip #6 class

                    if (GetDeviceProperty (TextList [iIndex + 7],
                                           cDevicePresent, sValue)) then
                        if (StrToBoolDef (sValue, false)) then
                            Device.FDevicePresent := true
                        else bNonPresent := true;

                    if (GetDeviceProperty (TextList [iIndex + 8],
                                           cDeviceStarted, sValue)) then
                        Device.FDeviceStarted := StrToBoolDef (sValue, false);

                    if (GetDeviceProperty (TextList [iIndex + 9],
                                           cDeviceDisabled, sValue)) then
                        Device.FDeviceDisabled := StrToBoolDef (sValue, false);

                    if (GetDeviceProperty (TextList [iIndex + 10],
                                           cDeviceHidden, sValue)) then
                    begin
                        // "Device.FDeviceHidden" defaults to "true"
                        Device.FDeviceHidden := StrToBoolDef (sValue, false);

                        if (Device.FDeviceHidden) then
                            bHidden := true;
                    end; { if }

                    if (GetHardware_ID (TextList [iIndex + 11], sValue)) then
                    begin
                        if (sValue <> '') then
                        begin
                            SetLength (Device.FHardware_IDs, 1);
                            Device.FHardware_IDs [0] := sValue;
                        end; { if }
                    end { if }
                    else
                        if ((iIndex + 12) < TextList.Count) then
                            GetHardware_IDS (TextList, iIndex + 12,
                                             Device.FHardware_IDs);

                    Inc (iIndex, 11);

                    Result := true;
                    exit;
                end; { if }
            end; { if }

            Result := false;
            Device.Free;
        end; { AddDevice }

        (* ---- *)

    var
        Device : TDevice;
        bAddDevice, bNextDevice : Boolean;

    begin
        Result := 0;

        bNextDevice := true;

        while (bNextDevice) do
        begin
            if (AddDevice (iIndex, Device)) then
            begin
                Inc (Result);
                Device.DeviceClass := DeviceClass;
                DeviceClass.DeviceList.Add (Device);
                FDevices.Add (Device);
            end { if }
            else Break;

            bAddDevice := false;

            while (true) do
            begin
                Inc (iIndex);

                if (iIndex = TextList.Count) then
                begin
                    bNextDevice := false;
                    Break;
                end; { if }

                if (bAddDevice) then
                    Break;

                if (TextList [iIndex][1] = '-') then
                    bAddDevice := true
                else if (TextList [iIndex][1] = '=') then
                begin
                    bNextDevice := false;
                    Break;
                end; { else if }
            end; { while }
        end; { while }
    end; { AddDevices }

    (* ---- *)

    function CheckForClass (const TextList: TStrings;
                            var iMainIndex: Integer) : Boolean;

    var
        iPos1, iPos2, iLen : Integer;
        DeviceClass : TDeviceClass;
        sDesc, sGuid : String;

    begin
        Result := false;

        if (Pos (cClass + ' = ', TextList [iMainIndex]) <> 1) then
            exit;

        iPos2 := Pos ('[{', TextList [iMainIndex]);
        iLen := Length (TextList [iMainIndex]);

        if (iPos2 = 0) or
           (TextList [iMainIndex][iLen] <> ']') then
            exit;

        iPos1 := Length (cClass) + 4;
        sDesc := TrimRight (Copy (TextList [iMainIndex], iPos1, iPos2 - iPos1));
        sGuid := Copy (TextList [iMainIndex], iPos2 + 1, Pred (iLen) - iPos2);

        DeviceClass := TDeviceClass.Create (sDesc, StringToGUID (sGuid));
        FClasses.Add (DeviceClass);

        Inc (iMainIndex, 2);

        if (iMainIndex < TextList.Count) then
            Result := AddDevices (TextList, DeviceClass, iMainIndex) > 0;
    end; { CheckForClass }

    (* ---- *)

var
    TextList : TStringList;
    iIndex : Integer;
    bCheckForClass : Boolean;

begin { TDevices.LoadSavedTextList }
    Assert (sFileName <> '');

    bHidden := false;
    bNonPresent := false;

    TextList := TStringList.Create;

    try
        TextList.LoadFromFile (sFileName);

        bCheckForClass := false;

        iIndex := 0;

        while (iIndex < TextList.Count) do
        begin
            if (TextList [iIndex] <> '') then
                if (bCheckForClass) then
                begin
                    bCheckForClass := false;

                    if (CheckForClass (TextList, iIndex)) then
                        Inc (iIndex)
                    else Break;
                end { if }
                else if (TextList [iIndex][1] = '=') then
                    bCheckForClass := true;

            Inc (iIndex);
        end; { while }

    finally
        TextList.Free;
    end; { try / finally }
end; { LoadSavedTextList }

(* ---- *)

function TDevices.GetEnumerator : TDevicesEnumerator;
begin
	Result := TDevicesEnumerator.Create (Self);
end; { TDevices.GetEnumerator }

(* ---- *)

procedure TDevices.Hardware_IDs_ToLower;

var
    iIndex, iID : Integer;

begin
    for iIndex := 0 to FDevices.Count - 1 do
        with TDevice (FDevices [iIndex]) do
            for iID := 0 to High (FHardware_IDs) do
                FHardware_IDs [iID] := LowerCase (FHardware_IDs [iID]);
end; { TDevices.Hardware_IDs_ToLower }

(* ---- *)

procedure TDevices.Refresh (const bIncludeNonpresentDevices: Boolean);

    (* ---- *)

    function CreateDeviceClass (const sGuid: String;
                                out DeviceClass: TDeviceClass) : Boolean;

        (* ---- *)

        function GetClassDescription (Guid: TGuid) : String;

        const
            cLen = MAX_PATH;

        var
            dwReqiredSize : DWord;

        begin
            SetLength (Result{%H-}, cLen);

            if (SetupDiGetClassDescription (Guid, PChar (Result), cLen,
                                            dwReqiredSize{%H-})) then
                SetLength (Result, dwReqiredSize - 1)
            else Result := '';
        end; { GetClassDescription }

        (* ---- *)

    var
        Guid : TGuid;
        sDescription : String;

    begin { CreateDeviceClass }
        if (sGuid <> cUnknownClass) then
        begin
            Guid := StringToGUID (sGuid);
            sDescription := GetClassDescription (Guid);
        end { if }
        else
        begin
            FillChar (Guid{%H-}, SizeOf (TGuid), #0);
            sDescription := '?';
        end; { else }

        DeviceClass := TDeviceClass.Create (sDescription, Guid);

        Result := true;
    end; { CreateDeviceClass }

    (* ---- *)

    procedure GetDeviceProperties (const Device: TDevice;
                                   const hDevInfo: THDevInfo;
                                   DevInfoData: TSPDevInfoData;
                                   const ClassNameList: TStringList);

    var
        iClass : Integer;
        DevClass : TDeviceClass;
        sClassGuid : String;
        bResult : Boolean;

    begin
        if (Win32MajorVersion >= 6) and (FUseVistaFunctions) then
            bResult := Device.GetPropertiesVista (hDevInfo, DevInfoData,
                                                  sClassGuid{%H-})
        else bResult := Device.GetPropertiesLegacy (hDevInfo, DevInfoData,
                                                    sClassGuid{%H-});

        if (bResult) then
        begin
            if (ClassNameList.Find (sClassGuid, iClass)) then
                DevClass := TDeviceClass (ClassNameList.Objects [iClass])
            else
                if (CreateDeviceClass (sClassGuid, DevClass)) then
                begin
                    ClassNameList.AddObject (sClassGuid, DevClass);
{$IFDEF IMAGE_LIST}
                    DevClass.SetClassImageListData (@CIL_Data);
                    DevClass.FImageIndex := GetDeviceImageIndex (CIL_Data,
                                                            DevClass.FClassGuid)
{$ENDIF}
                end { if }
                else
                begin
                    Device.Free;
                    exit;
                end; { else }

            Device.SetDeviceClass (DevClass);

{$IFDEF IMAGE_LIST}
            if (Assigned (DevClass.pClassImageListData)) then
                Device.FImageIndex := DevClass.FImageIndex;
{$ENDIF}

            FDevices.Add (Device);
        end { if }
        else Device.Free;
    end; { GetDeviceProperties }

    (* ---- *)

var
    ClassNameList : TStringList;
    hDevInfo : THDevInfo;
    DevInfoData : TSPDevInfoData;
    iDevice, iClass : Integer;
    dwFlags : DWord;
    Device : TDevice;

begin { TDevices.Refresh }
    FDevices.Clear;
    FClasses.Clear;

    iUnknownCount := 0;
    FIncludeNonpresentDevices := bIncludeNonpresentDevices;

    ClassNameList := CreateSortedStringList (dupIgnore);

    try
        if (bIncludeNonpresentDevices) then
            dwFlags := DIGCF_ALLCLASSES
        else dwFlags := DIGCF_ALLCLASSES or DIGCF_PRESENT;

        // Create a HDEVINFO with all present devices.
        hDevInfo := SetupDiGetClassDevs (NIL, NIL, 0, dwFlags);

        if (hDevInfo = INVALID_HANDLE_VALUE) then
            RaiseLastOSError;

        iDevice := 0;

        FillChar (DevInfoData{%H-}, SizeOf (TSPDevInfoData), #0);
        DevInfoData.cbSize := SizeOf (TSPDevInfoData);

        while (SetupDiEnumDeviceInfo (hDevInfo, iDevice, DevInfoData)) do
        begin
            Device := TDevice.Create;

{$IFDEF DEBUG}
  			OutputDebugStr ('Device = %d', [iDevice]);
{$ENDIF}

            GetDeviceProperties (Device, hDevInfo, DevInfoData, ClassNameList);

            Inc (iDevice);
        end; { while }

{$IFDEF DEBUG}
        if (ClassNameList.Count = 0) then
            RaiseLastWin32Error;
{$ENDIF}

        if (hDevInfo <> INVALID_HANDLE_VALUE) then
            VerifyApi (SetupDiDestroyDeviceInfoList (hDevInfo));

        for iClass := 0 to ClassNameList.Count - 1 do
            FClasses.Add (ClassNameList.Objects [iClass]);

        FClasses.Sort (ClassesCompare);
        FDevices.Sort (DevicesCompare);

        for iDevice := 0 to FDevices.Count - 1 do
        begin
            Device := TDevice (FDevices [iDevice]);
            Device.DeviceClass.AddDevice (Device);
        end; { for }

{$IFDEF IMAGE_LIST}
    {$IFDEF FPC}
        if (Assigned (ClassImageList)) then
            CopyImagesFromSysImageListToImageList;
    {$ENDIF}
{$ENDIF}

    finally
        ClassNameList.Free;
    end; { try / finally }
end; { TDevices.Refresh}

(* ---- *)

end.

