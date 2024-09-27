// Copyright (c) 2023 Olaf Hess
// All rights reserved.

// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at https://mozilla.org/MPL/2.0/.

{$I .\switches.inc}

{$IFDEF FPC}
	{$MODE DELPHI}
{$ENDIF}

{$WARN SYMBOL_PLATFORM OFF}

(**
    TPackage.CheckBooleanTag :
        "CheckForSameAction": Multiple driver versions for the same device
                              (X240, Integrated Camera Driver)

    TPackage.CheckDriverTag :
        Perform installation only if device is present (P50, Intel SGX device)

    TPackage.CheckFileVersionTag : "FInstallationRequired := not Result"
        T460 Win81: check for different files
**)

unit PackageClassU;

interface

uses Windows,
     XmlBaseClassesU, GlobalsU,
     OmniXML,
     DeviceViewerU;

type
    TBoolCondition = (bcAnd, bcOr, bcNot);
    TCompareOp = (coEqu, coGtrEqu, coLessEqu);
    TCompareResult = (crSame, crNewer, crOlder);
    TInstallType = (itCmd, itInf);
    TPackageType = (ptNoPackageType, ptApplication, ptDriver, ptSystemBios,
                    ptFirmware, ptUnknown);
    TProcessingStep = (psDetectInstall, psDependencies);
    TSkippedReason = (srNotChecked, srPrerequisites, srDetectInstall,
                      srNotSkipped);

    TPackage = class (TXmlBaseClass)
      private
        FCompatibilityCheck : Boolean;
        FCompatibilityMsg : String;
        FDescription : String;
        FDriverPresent : Boolean;
        FExtractCommand : String;
        FID : String;
        FIsInstalled : Boolean;
        FInstallCmd : String;
        FInstallResult : TCheckResult;
        FInstallResultValues : DWord;
        FInstallSuccess : TaDWord;
        FInstallType : TInstallType;
        FInstalledDriverProvider : String;
        FInstalledVersion : String;
        FLocalPath : String;
        FMicrosoftDriver : Boolean;
        FNoArchive : Boolean;
        FPackageType : TPackageType;
        FPackageVersion : String;
        FPackageXml : String;
        FRebootRequired : Boolean;
        FRebootType : Integer;
        FSkippedReason : TSkippedReason;
        FTargetPresent : TCheckResult;
        FXmlError : Boolean;

        Devices : TDevices;
        ProcessingStep : TProcessingStep;

        function Check_OS_Tag (const ParentNode: IXMLNode) : Boolean;
        function CheckBiosTag (const ParentNode: IXMLNode) : Boolean;
        function CheckBooleanTag (const ParentNode: IXMLNode;
                                  const Condition: TBoolCondition;
                                  const sBooleanTag: String) : Boolean;
        function CheckConditions (const Node: IXMLNode;
                                  const sNodeName: String) : Boolean;
        function CheckCpuAddressWidthTag (const ParentNode: IXMLNode) : Boolean;
        function CheckDriverTag (const ParentNode: IXMLNode) : Boolean;
        function CheckEmbeddedControllerVersionTag (const ParentNode: IXMLNode) : Boolean;
        function CheckExternalDetectionTag (const ParentNode: IXMLNode) : Boolean;
        function CheckFileVersionTag (const ParentNode: IXMLNode) : Boolean;
        function CheckFirmwareTag (const ParentNode: IXMLNode) : Boolean;
        function CheckPnP_ID_Tag (const ParentNode: IXMLNode) : Boolean;
        function CheckRegistryKeyTag (const ParentNode: IXMLNode) : Boolean;
        function CheckRegistryKeyValueTag (const ParentNode: IXMLNode) : Boolean;
        function CheckWindowsBuildVersionTag (const ParentNode: IXMLNode) : Boolean;
        function DetectDependencies (const ParentNode: IXMLNode) : Boolean;
        procedure DetectInstall (const ParentNode: IXMLNode);
        procedure DisplayComparissionResult (const {%H-}bResult: Boolean;
                                             const CompareOp: TCompareOp);
        function ExpandPathVariables (const sPath: String) : String;
        function FindDevice (const sDevice_ID: String;
                             var FoundDevice: TDevice) : Boolean;

        function GetHardwareIdTagValue (const ParentNode: IXMLNode;
                                        var sVersion: String;
                                        var bDevicePresent,
                                            bForceInstall: Boolean) : Boolean;
        function GetPackagePath : String;
        function GetServiceTagValue (const ParentNode: IXMLNode;
                                     out sVersion: String) : Boolean;
        function GetVersionStr (const FirstNode: IXMLNode;
                                var sVersion: String;
                                var CompareOp: TCompareOp) : Boolean; overload;
        function GetVersionStr (const FirstNode: IXMLNode; const sAttr: String;
                                var sVersion, sAttrValue: String;
                                var CompareOp: TCompareOp) : Boolean; overload;
        function IsMicrosoftDriver (const sDriverProvider: String) : Boolean;
        function ProcessDependencies : Boolean;
        function ProcessDetectInstall : Boolean;
        procedure RetrievePackageInfo;
        function RunExternalProgram (sCmd: String;
                                     out dwExitCode: DWord) : Boolean;
        procedure SetCmdType (const sCmdType: String);
        procedure SetPackageType (const sType: String);

      public
        constructor Create;
        destructor Destroy; override;

        function GetPackageType : String;
        function ProcessPackageXml (const sPackageXml: String;
                                    const Devices: TDevices = NIL) : Boolean;

        property CompatibilityCheck : Boolean read FCompatibilityCheck
                                              write FCompatibilityCheck;
        property CompatibilityMsg : String read FCompatibilityMsg
                                           write FCompatibilityMsg;
        property Description : String read FDescription write FDescription;
        property DriverPresent : Boolean read FDriverPresent;
        property ExtractCommand : String read FExtractCommand;
        property ID : String read FID write FID;
        property IsInstalled : Boolean read FIsInstalled write FIsInstalled;
        property InstallCmd : String read FInstallCmd;
        property InstalledDriverProvider : String read FInstalledDriverProvider;
        property InstalledVersion : String read FInstalledVersion;
        property InstallResult : TCheckResult read FInstallResult
                                              write FInstallResult;
        property InstallResultValues : DWord read FInstallResultValues
                                             write FInstallResultValues;
        property InstallSuccess : TaDWord read FInstallSuccess;
        property InstallType : TInstallType read FInstallType;
        property LocalPath : String read FLocalPath write FLocalPath;
        property MicrosoftDriver : Boolean read FMicrosoftDriver;
        property NoArchive : Boolean read FNoArchive;
        property PackageVersion : String read FPackageVersion
                                         write FPackageVersion;
        property PackageType : TPackageType read FPackageType;
        property PackageXml : String read FPackageXml;
        property RebootRequired : Boolean read FRebootRequired
                                          write FRebootRequired;
        property RebootType : Integer read FRebootType;
        property SkippedReason : TSkippedReason read FSkippedReason;
        property TargetPresent : TCheckResult read FTargetPresent;
        property XmlError : Boolean read FXmlError write FXmlError;
    end; { TPackage }

implementation

uses SysUtils, Classes,
     GetPackageVerU,
     LogToTempU, Win32ToolsU, PasTools, ServicesU, Wow64U, RegistryApiU,
     RegistryHelperU;

ResourceString
    cAddressWidthErrMsg = 'Unknown "%s": %s';
    cAddressWidthFoundMsg = 'Address width "%s" successfully checked';
    cAddressWidthNotFoundMsg = 'Condition address width "%s" not fulfilled';
    cAttributeNotFoundMsg = '"%s": attribute "%s" not found';
    cAttributeUnsupportedMsg = 'Attribute "%s" not supported';
    cBiosUpdateNecessaryMsg = 'BIOS update necessary';
    cBiosVerFoundMsg = 'Matching BIOS version "%s" found';
    cBiosVerNotFoundMsg = 'No matching BIOS version found';
    cBooleanConditionCheck = 'Checking "%s" condition';
    cBooleanConditionMetMsg = 'Boolean condition "%s" fulfilled';
    cBooleanConditionNotMetMsg = 'Boolean condition "%s" not fulfilled';
    cCheckForConditionMissingMsg = 'Check for condition "%s" missing';
    cCheckingTag = 'Checking "%s" tag ...';
    cCompareErrMsg = 'Error comparing "%s" with "%s"';
    cComparingMsg = 'Comparing "%s" with desired value "%s"';
    cEmbeddedControllerVersionErrorMsg =
                             'Error retrieving the embedded controller version';
    cCompareEqualMsg = 'equal';
    cCompareGreaterOrEqualMsg = 'greater or equal';
    cCompareLessOrEqualMsg = 'less or equal';
    cCompareVersionResultMsg = 'Version compare result (%s) = %s';
    cCompatibilityWarningMsg = 'Compatibility warning: ';
    cDependenciesCheckSuccessMsg = 'Dependencies check successfully performed';
    cDependenciesCheckFailureMsg =
                     'Dependencies check failed; skipping package installation';
    cDependenciesDetectionUndefinedMsg =
                                    'Dependencies not defined for this package';
    cDeviceFoundMsg = 'Device "%s" found (device present = %s)';
    cDeviceNotPresentMsg = 'Device not present; skipping package installation';
    cDriverCheckSuccessMsg =
                       'Check for driver condition "%s" successfully performed';
    cDriverCheckErrorMsg = 'Failed to check for driver condition "%s"';
    cDriverVerUndefinedMsg = 'Driver version undefined';
    cEmbbeddedControllerCurrentMsg =
    						    'Matching embedded controller version %s found';
    cEmbbeddedControllerDifferentMsg =
                    '%s embedded controller version %s found (expected is v%s)';
    cEmptyValueErrMsg = 'No value for "%s" defined';
    cExternalDetectionSuccessMsg =
                            'External detection return value matches condition';
    cExternalDetectionFailureMsg =
                     'External detection return value does not match condition';
    cFileNotFoundMsg = 'File "%s" not found';
    cFileCheckingVersionMsg = 'Checking file version of "%s"';
    cFileNoVersionInfoMsg = 'The file "%s" does not contain version information';
    cFirmwareCheckingVersionMsg = 'Checking firmware version of "%s"';
    cFirmwareFoundMsg = 'Firmware "%s" found';
    cFirmwareNotFoundMsg = 'Matching firmware not found';
    cFirmwareLocalVerMsg = 'Installed firmware version = "%s" (%s)';
    cFirmwarePackageVerMsg = 'Firmware version from package = "%s" (%s)';
    cGenericDriverFoundMsg =
    				   'Generic "%s" driver v%s found; ignoring driver version';
    cGetValueErrMsg = 'Unable to retieve the "%s" value';
    cGetVersionErrMsg = 'Unable to retrieve the version information';
    cHardwareID_FoundMsg = 'Match for hardware ID found: "%s"';
    cIgnoringTagMsg = 'Ignoring the "%s" tag';
    cInstallationDetectionUndefinedMsg =
             #13#10'*** No installation detection defined for this package ***';
    cInstallationRequiredMsg = #13#10'The package must be installed';
    cInstallationNotRequiredMsg = #13#10'Package installation not necessary';
    cInstalledDriverVerMsg = 'Installed driver version = %s';
    cInstallTypeUnknownMsg = 'Unknown install type "%s"';
    cNewerMsg = 'Newer';
    cNodeNotFoundMsg = 'XML node "%s" not found';
    cNodeProcessingErrMsg = 'Error processing node "%s"';
    cNotCheckingBiosTagMsg = 'Option "/IgnoreSystemCompatibility" used: not ' +
                             'checking BIOS compatibility';
    cOlderMsg = 'Older';
    cPackageCheckingDependencies =
                                  #13#10'*** Checking package dependencies ***';
    cPackageCheckingInstallStatusMsg =
                             #13#10'*** Checking for existing installation ***';
    cPackageTypeDriver = 'Driver';
    cPackageTypeFirmware = 'Firmware';
    cPackageTypeMsg = ' (package type = %s)';
    cPackageTypeApplication = 'Application';
    cPackageTypeSystemBios = 'SystemBios';
    cPackageTypeUndefinedMsg = 'No "PackageType" defined';
    cPackageVerErrMsg = 'Unable to retrieve the package version from "%s"';
    cProcessingMsg = #13#10'> Package "%s" v%s';
    cOS_ConditionMetMsg = 'OS condition "%s" fulfilled';
    cOS_ConditionNotMetMsg = 'No OS condition fulfilled';
    cReadVersionErrMsg = 'Unable to read the value "%s" from "%s"';
    cRegKeyUnknownMsg = 'Unable to process the registry key "%s"';
(**
    cRegProcessingErrMsg =
               'Unable to evaluate the "%s" section; no recognized value found';
**)
    cRegKeyNotFoundMsg = 'Registry key "%s" not found';
    cRegValueNotFoundMsg = 'Registry value "%s", key "%s" not found';
    cReplacingGenericDriverMsg =
                  'Replacing "%s" driver v%s with manufacturer provided driver';
    cReturnCodeErrMsg = '"%s": "%s" is not valid return code';
(**
    cSameMsg = 'Same';
**)
    cService_FoundMsg = 'Service "%s" found';
    cSkippingBiosUpdatePackageMsg = 'Skipping system BIOS update packages';
    cSkippingFirmwarePackageMsg = 'Skipping firmware package';
    cSkippingNonBiosUpdatePackageMsg = 'Skipping non system BIOS package';
    cSkippingNonFirmwarePackageMsg = 'Skipping non firmware package';
    cUnknowDriverTagErrMsg = 'Unknown "_Driver" subtag "%s"';
    cUnknownVariableMsg = 'The path "%s" contains the unknown variable "%s"';
    cUnsupportedPackageVerMsg = 'Unsupported package version %d';
    cValueNotFoundMsg = 'Value "%s" not found';
    cVersionNotFoundMsg = 'Unable to retrieve the "version" attribute';
    cVersionStringChanged = 'Version string changed from "%s" to "%s"';
    cVersionUnequalMsg =
              'The installed version %s does not match the expected version %s';
    cWindowsBuildVerSuccessMsg = 'Windows build version "%s" successfully checked';
    cWindowsBuildVerFailureMsg = 'Condition Windows build version "%s" not fulfilled';
    cWrongHeaderMsg = 'Package "%s": "Package" node missing';

const
    c_OS = '_os';
    cAddressWidth = 'AddressWidth';
    cBios = '_bios';
    cBool_And = 'and';
    cBool_Not = 'not';
    cBool_Or = 'or';
    cCmdLine = 'Cmdline';
    cCmdType_Cmd = 'cmd';
    cCmdType_Inf = 'inf';
    cCoreq = '_coreq';
    cCpuAddressWidth = '_cpuaddresswidth';
    cDataSection = '#cdata-section';
    cDate = 'Date';
    cDependencies = 'Dependencies';
    cDesc = 'Desc';
    cDriver = '_driver';
    cDetectInstall = 'DetectInstall';
    cEmbeddedControllerVersion = '_embeddedcontrollerversion';
    cExternalDetection = '_externaldetection';
    cExtractCommand = 'ExtractCommand';
    cFile = 'File';
    cFileVersion = '_fileversion';
    cFirmWare = '_firmware';
//    cGenericAdapter = 'genericadapter';
    cHardwareID = 'HardwareID';
    cHardwareIDs = cHardwareID + 's';
    cHex2Dec = 'hex2dec';
//    cID = 'id';
    cInfFileNode = '/INFCmd/INFfile';
    cInstall = 'Install';
    cKey = 'Key';
    cKeyName = 'KeyName';
    cLevel = 'Level';
    cMicrosoft = 'microsoft';
    cOS = 'OS';
    cPackage = 'Package';
    cPackageType = 'PackageType';
    cPnP_ID = '_pnpid';
    cReboot = 'Reboot';
    cRC = 'rc';
    cRegistryKey = '_registrykey';
    cRegistryKeyValue = '_registrykeyvalue';
    cRootNode = cPackage + '/';
    cServiceName = 'ServiceName';
    cTitle = 'Title';
    cType = 'type';
    cVersion = 'Version';
    cVersionUndefined = '0.0.0.0';
    cWindowsBuildVersion = '_windowsbuildversion';

(* ---- *)

function GetCompareOp (const sCompare: String;
                       out CompareOp: TCompareOp) : String;

var
    iPos : Integer;

begin
    Assert (sCompare <> '');

    Result := Trim (sCompare);

    iPos := Pos ('^', Result);

    if (iPos = 0) then
        CompareOp := coEqu
    else
    begin
        if (iPos = 1) then
            CompareOp := coLessEqu
        else CompareOp := coGtrEqu;

        Delete (Result, iPos, 1);
    end; { else }
end; { GetCompareOp }

(* ---- *)

function CompareNumbers (const iNo1, iNo2: Integer;
                         const CompareOp: TCompareOp) : Boolean;
begin
    case CompareOp of
        coGtrEqu : Result := (iNo1 >= iNo2);
        coLessEqu : Result := (iNo1 <= iNo2);
        else Result := iNo1 = iNo2;  // coEqu
    end; { case CompareOp of }
end; { CompareNumbers }

(* ---- *)

function CompareVersionStr (const sVer1, sVer2: String;
                            const CompareOp: TCompareOp) : Boolean;

var
    iCompare : Integer;

begin
    iCompare := CompareVersions (sVer1, sVer2);

    case CompareOp of
        coGtrEqu : Result := (iCompare >= 0);
        coLessEqu : Result := (iCompare <= 0);
        else Result := iCompare = 0;  // coEqu
    end; { case CompareOp of }
end; { CompareVersionStr }

(* ---- *)

function FillReturnCodeArray (const sReturnCode: String;
                              out aCodes: TaDWord) : Boolean;

var
    iIndex, iResult : Integer;

begin
    if (Pos (',', sReturnCode) = 0) then
    begin
        SetLength (aCodes{%H-}, 1);

        if (Str2Int (sReturnCode, iResult{%H-})) then
        begin
            Result := true;
            aCodes [0] := DWord (iResult);
        end { if }
        else Result := false;
    end { if }
    else
    begin
        Result := true;

        with TStringList.Create do
            try
                CommaText := sReturnCode;

                if (Count > 0) then
                begin
                    SetLength (aCodes, Count);

                    for iIndex := 0 to Count - 1 do
                    begin
                        if (Str2Int (Strings [iIndex], iResult)) then
                            aCodes [iIndex] := DWord (iResult)
                        else
                        begin
                            Result := false;
                            Break;
                        end; { else }
                    end; { for }
                end { if }
                else Result := false;

            finally
                Free;
            end; { try / finally }
    end; { else }
end; { FillReturnCodeArray }

(* ---- *)

function Format_PnP_ID (const sID: String) : String;

var
    iPos : Integer;

begin
    if (sID <> '') then
    begin
        iPos := Pos ('&subsys_', sID);

        if (iPos > 0) then
        begin
            Result := Copy (sID, 1, iPos);
            exit;
        end; { if }
    end; { if }

    Result := sID;
end; { Format_PnP_ID }

(* ---- *)

function MatchHardware_ID (const sID: String; const Device: TDevice) : Boolean;

var
    iIndex : Integer;

begin
    Assert (sID <> '');

    for iIndex := 0 to High (Device.Hardware_IDs) do
    begin
        if (VeryVerboseLogging) then
            LogVerbose (cComparingMsg, [sID, Device.Hardware_IDs [iIndex]]);

        if (Pos (sID, Device.Hardware_IDs [iIndex]) > 0) then
        begin
            Result := true;
            exit;
        end; { if }
    end; { for }

    Result := false;
end; { MatchHardware_ID }

(* ---- *)

function TPackage.Check_OS_Tag (const ParentNode: IXMLNode) : Boolean;
{ Returns "true" if OS condition is met }

var
    Node : IXMLNode;
    sOS, sValue : String;
    iPos : Integer;

begin
    Assert (ParentNode <> NIL);

    if (IgnoreWindowsCompatibility) then
    begin
        Result := true;
        LogVerbose (cIgnoringTagMsg, [c_OS]);
        exit;
    end { if }
    else Result := false;

    if not (ParentNode.HasChildNodes) then
        raise EXMLException.CreateFmt (cNodeNotFoundMsg, [cOS]);

    LogVerbose (cCheckingTag, [ParentNode.NodeName]);

    sOS := 'WIN' + Get_OS_Ver;

    Node := ParentNode.FirstChild;

    while (Node <> NIL) do
    begin
        if (Node.NodeName = cOS) and (Node.HasChildNodes) then
        begin
            sValue := Node.FirstChild.NodeValue;

            if (VeryVerboseLogging) then
                LogVerbose (cComparingMsg, [sOS, sValue]);

            iPos := Pos ('.', sValue);

            if (iPos > 0) then
                SetLength (sValue, Pred (iPos));

            if (sOS = sValue) then
            begin
                Result := true;
                LogVerbose (cOS_ConditionMetMsg, [sOS]);
                Break;
            end; { if }
        end; { if }

        Node := Node.NextSibling;
    end; { while }

    if not (Result) then
        LogVerbose (cOS_ConditionNotMetMsg);
end; { TPackage.Check_OS_Tag }

(* ---- *)

function TPackage.CheckBiosTag (const ParentNode: IXMLNode) : Boolean;
{ Returns "true" if condition is met }

var
    Node : IXMLNode;
    sVersion : String;

begin { TPackage.CheckBiosTag }
    Assert (ParentNode <> NIL);

    if (IgnoreSystemCompatibility) and (FCompatibilityCheck = false) then
    begin
        Result := true;
        LogVerbose (cNotCheckingBiosTagMsg);
        exit;
    end; { if }

    Result := false;

    if not (GetFirstNamedNode (ParentNode.FirstChild, Node)) then
        raise EXMLException.CreateFmt (cNodeNotFoundMsg, [cBios]);

    LogVerbose (cCheckingTag, [ParentNode.NodeName]);

    repeat
        if (Node.NodeName = cLevel) and (Node.HasChildNodes) then
        begin
            sVersion := UpperCase (Node.FirstChild.NodeValue);

            if (VeryVerboseLogging) then
                LogVerbose (cComparingMsg, [BiosVersion, sVersion]);

            if (MatchString (sVersion, BiosVersion)) then
            begin
                Result := true;
                LogVerbose (cBiosVerFoundMsg, [Node.FirstChild.NodeValue]);
                Break;
            end; { if }
        end; { if }

        Node := Node.NextSibling;
    until (Node = NIL);

    if not (Result) then
        if (ProcessingStep = psDetectInstall) then
        begin
            FTargetPresent := crTrue;
            LogMsg (cBiosUpdateNecessaryMsg);
        end { if }
        else LogVerbose (cBiosVerNotFoundMsg);
end; { TPackage.CheckBiosTag }

(* ---- *)

function TPackage.CheckBooleanTag (const ParentNode: IXMLNode;
                                   const Condition: TBoolCondition;
                                   const sBooleanTag: String) : Boolean;

    (* ---- *)

    function CheckForSameAction (const sSiblingAction: String;
                                 ChildNode: IXMLNode) : Boolean;

    var
        sNodeName : String;

    begin
        Result := false;

        if (sSiblingAction = cBool_And) or (sSiblingAction = cBool_Not) or
           (sSiblingAction = cBool_Or) then
            exit;

        while (ChildNode <> NIL) do
        begin
            sNodeName := LowerCase (ChildNode.NodeName);

            if (sNodeName <> cHashText) then
                if (sNodeName = sSiblingAction) then
                begin
                    Result := true;
                    Break;
                end; { if }

            ChildNode := ChildNode.NextSibling;
        end; { while }
    end; { CheckForSameAction }

    (* ---- *)

var
    Node : IXMLNode;
    bResult : Boolean;
    sNodeName : String;

begin { TPackage.CheckBooleanTag }
    bResult := false;

    if not (ParentNode.HasChildNodes) then
    begin
        LogMsg (cInstallationDetectionUndefinedMsg);
        Result := false;
        exit;
    end; { if }

    Node := ParentNode.FirstChild;

    repeat
        sNodeName := LowerCase (Node.NodeName);

        if (sNodeName <> cHashText) then
        begin
            if (VeryVerboseLogging) then
                LogVerbose (cBooleanConditionCheck, [sBooleanTag]);

            bResult := CheckConditions (Node, sNodeName);

            if (Condition = bcOr) and (bResult) then
                Break
            else if (Condition = bcNot) then
            begin
            	if (ProcessingStep = psDependencies) then
                begin
                	if not (FMicrosoftDriver and bResult) then
                    	bResult := not bResult;
                end { if }
                else bResult := not bResult;

                if (bResult = false) then
                    Break;
            end { else if }
            else if (Condition = bcAnd) and (bResult = false) then
                if not (CheckForSameAction (sNodeName, Node.NextSibling)) then
                    Break;
        end; { if }

        Node := Node.NextSibling;
    until (Node = NIL);

    Result := bResult;

    if (Result) then
        LogVerbose (cBooleanConditionMetMsg, [sBooleanTag])
    else LogVerbose (cBooleanConditionNotMetMsg, [sBooleanTag]);
end; { TPackage.CheckBooleanTag }

(* ---- *)

function TPackage.CheckConditions (const Node: IXMLNode;
                                   const sNodeName: String) : Boolean;

begin
    if (sNodeName = c_OS) then
        Result := Check_OS_Tag (Node)
    else if (sNodeName = cBios) then
        Result := CheckBiosTag (Node)
    else if (sNodeName = cCpuAddressWidth) then
        Result := CheckCpuAddressWidthTag (Node)
    else if (sNodeName = cExternalDetection) then
        Result := CheckExternalDetectionTag (Node)
    else if (sNodeName = cDriver) then
        Result := CheckDriverTag (Node)
    else if (sNodeName = cEmbeddedControllerVersion) then
        Result := CheckEmbeddedControllerVersionTag (Node)
    else if (sNodeName = cFileVersion) then
        Result := CheckFileVersionTag (Node)
    else if (sNodeName = cFirmware) then
        Result := CheckFirmwareTag (Node)
    else if (sNodeName = cPnP_ID) then
        Result := CheckPnP_ID_Tag (Node)
    else if (sNodeName = cRegistryKey) then
        Result := CheckRegistryKeyTag (Node)
    else if (sNodeName = cRegistryKeyValue) then
        Result := CheckRegistryKeyValueTag (Node)
    else if (sNodeName = cWindowsBuildVersion) then
        Result := CheckWindowsBuildVersionTag (Node)
    else if (sNodeName = cBool_And) then
        Result := CheckBooleanTag (Node, bcAnd, UpperCase (sNodeName))
    else if (sNodeName = cBool_Not) then
        Result := CheckBooleanTag (Node, bcNot, UpperCase (sNodeName))
    else if (sNodeName = cBool_Or) then
        Result := CheckBooleanTag (Node, bcOr, UpperCase (sNodeName))
    else if (sNodeName = cCoreq) then
    begin
        Result := true;
        LogVerbose (cIgnoringTagMsg, [cCoreq]);
    end { else if }
    else
    begin
        LogMsg (cCheckForConditionMissingMsg, [sNodeName]);
        Result := false;
    end; { else }
end; { TPackage.CheckConditions }

(* ---- *)

function TPackage.CheckCpuAddressWidthTag (const ParentNode: IXMLNode) : Boolean;
{ Returns "true" if condition is met }

var
    sValue : String;
    iValue : Integer;

begin
    Assert (ParentNode <> NIL);

    if not (ParentNode.HasChildNodes) then
        raise EXMLException.CreateFmt (cNodeNotFoundMsg, [cCpuAddressWidth]);

    LogVerbose (cCheckingTag, [ParentNode.NodeName]);

    if (GetChildNodeValue (ParentNode, cAddressWidth, sValue)) then
    begin
        iValue := StrToIntDef (sValue, 0);

        case iValue of
            32 : Result := IsWindows_x64 = false;
            64 : Result := IsWindows_x64;
        	else raise Exception.CreateFmt (cAddressWidthErrMsg,
                                            [cAddressWidth, sValue]);
        end; { case iValue of }
    end { if }
    else raise EXMLException.CreateFmt (cNodeNotFoundMsg, [cAddressWidth]);

    if (Result) then
        LogVerbose (cAddressWidthFoundMsg, [sValue])
    else LogVerbose (cAddressWidthNotFoundMsg, [sValue]);
end; { TPackage.CheckCpuAddressWidthTag }

(* ---- *)

function TPackage.CheckDriverTag (const ParentNode: IXMLNode) : Boolean;
{ Returns "true" if a driver is installed }

    (* ---- *)

    function GetFileVersion (const Node: IXMLNode;
                             out sVersion: String) : Boolean;

    var
        sFile : String;

    begin
        Assert (Node <> NIL);

        if not (Node.HasChildNodes) then
            raise EXMLException.CreateFmt (cGetValueErrMsg, [cFile]);

        sFile := ExpandPathVariables (Node.FirstChild.NodeValue);

        sVersion := GetVersionInfo (GetSysNativePath (sFile));

        Result := sVersion <> '';
    end; { GetFileVersion }

    (* ---- *)

var
    Node : IXMLNode;
    sNodeName, sLastName, sVersion, sCurrentVer : String;
    CompareOp : TCompareOp;
    bForceInstall, bDevicePresent : Boolean;

begin { TPackage.CheckDriverTag }
    Assert (ParentNode <> NIL);

    Result := false;
    bForceInstall := false;
    bDevicePresent := false;

    if not (GetFirstNamedNode (ParentNode.FirstChild, Node)) then
        raise EXMLException.CreateFmt (cNodeNotFoundMsg, [cDriver]);

    LogVerbose (cCheckingTag, [ParentNode.NodeName]);

    GetVersionStr (Node, sVersion{%H-}, CompareOp{%H-});

    repeat
        sNodeName := Node.NodeName;

        if (sNodeName = cFile) then
        begin
            if not (GetFileVersion (Node, sCurrentVer)) then
                sLastName := cFile;
        end { if }
        else if (sNodeName = cHardwareID) then
        begin
            if not (GetHardwareIdTagValue (Node, sCurrentVer, bDevicePresent,
                                           bForceInstall)) then
                sLastName := cHardwareID;
        end { else }
        else if (sNodeName = cServiceName) then
        begin
            if not (GetServiceTagValue (Node, sCurrentVer)) then
                sLastName := cServiceName;
        end { else }
        else
            if not ((sNodeName = cHashText) or (sNodeName = cDate) or
                    (sNodeName = cVersion)) then
                raise EXMLException.CreateFmt (cUnknowDriverTagErrMsg,
                                               [sNodeName]);

        Node := Node.NextSibling;
    until (Node = NIL) or (sCurrentVer <> '');

    if (sCurrentVer <> '') then
    begin
        LogVerbose (cDriverCheckSuccessMsg, [sNodeName]);

        Result := CompareVersionStr (sCurrentVer, sVersion, CompareOp);

        if (ProcessingStep = psDependencies) then
        begin
            if (Result = false) then
            	if (FMicrosoftDriver) then
                begin
                	Result := true;
                	LogVerbose (cGenericDriverFoundMsg,
                    			[FInstalledDriverProvider, sCurrentVer]);
                end { if }
                else LogVerbose (cVersionUnequalMsg,
                				 [FInstalledVersion, sVersion]);
        end { if }
        else  // if (ProcessingStep = psDetectInstall) then
        begin
            FDriverPresent := sCurrentVer <> cVersionUndefined;

            if (Result) and (bForceInstall = false) then
                FIsInstalled := true;

            DisplayComparissionResult (FIsInstalled, CompareOp);

            if (FIsInstalled = false) and (bDevicePresent = false) then
            begin
                FTargetPresent := crFalse;
                LogVerbose (cDeviceNotPresentMsg);
            end; { if }
        end; { else }
    end { if }
    else
    begin
        LogVerbose (cDriverCheckErrorMsg, [sLastName]);  // sCurrentVer = ''

        if (bDevicePresent) then
            FTargetPresent := crTrue;
    end; { else }
end; { TPackage.CheckDriverTag }

(* ---- *)

function TPackage.CheckEmbeddedControllerVersionTag (
                                          const ParentNode: IXMLNode) : Boolean;

	(* ---- *)

    function CompareEmbeddedControllerVersion (const sVerFromXml: String;
                                               const CompareOp: TCompareOp;
                                               out sVerFromBios: String;
                                               out CR: TCompareResult) : Boolean;
    { Result = "true": "CompareOp" is true }

        (* ---- *)

        procedure RaiseException;
        begin
            raise EXMLException.CreateFmt (cReadVersionErrMsg,
                                           [cVersion, cEmbeddedControllerVersion])
        end; { RaiseException }

        (* ---- *)

    var
    	iPos : Integer;
        sMajor, sMinor : String;
        uMajor, uMinor, uBiosMajor, uBiosMinor : UInt;

    begin { CompareEmbeddedControllerVersion }
        Result := false;

        iPos := Pos ('.', sVerFromXml);

        if (iPos = 0) then
        	RaiseException;

        sMajor := Copy (sVerFromXml, 1, iPos - 1);
        sMinor := Copy (sVerFromXml, iPos + 1, Length (sVerFromXml) - iPos);

        if (Str2Int (sMajor, uMajor{%H-}) = false) or
           (Str2Int (sMinor, uMinor{%H-}) = false) then
            RaiseException;

        if (GetEmbeddedControllerVersion (uBiosMajor, uBiosMinor)) then
        begin
            sVerFromBios := Format ('%d.%.2d', [uBiosMajor, uBiosMinor]);

            if (VeryVerboseLogging) then
                LogVerbose (cComparingMsg, [sVerFromBios, sVerFromXml]);

            case CompareOp of
                coEqu : Result := (uBiosMajor = uMajor) and
                                  (uBiosMinor = uMinor);

                coGtrEqu :
                    Result := (uBiosMajor > uMajor) or
                              ((uBiosMajor = uMajor) and (uBiosMinor >= uMinor));

                coLessEqu :
                    Result := (uBiosMajor < uMajor) or
                              ((uBiosMajor = uMajor) and (uBiosMinor <= uMinor));
            end; { case CompareOp of }

            if (uBiosMajor < uMajor) or
               ((uBiosMajor = uMajor) and (uBiosMinor < uMinor)) then
                CR := crOlder
            else if (uBiosMajor > uMajor) or
                    ((uBiosMajor = uMajor) and (uBiosMinor > uMinor)) then
                CR := crNewer
            else CR := crSame;
        end; { if }
    end; { CompareEmbeddedControllerVersion }

    (* ---- *)

var
    Node : IXMLNode;
    sVerFromXml, sVerFromBios, sNodeValue : String;
    CompareOp : TCompareOp;
    CompareResult : TCompareResult;
    uMajor, uMinor : UInt;

begin { TPackage.CheckEmbeddedControllerVersionTag }
    Assert (ParentNode <> NIL);

    Result := false;

    LogVerbose (cCheckingTag, [ParentNode.NodeName]);

    if not (GetEmbeddedControllerVersion (uMajor, uMinor)) then
    begin
        LogMsg (cEmbeddedControllerVersionErrorMsg);
        exit;
    end;  { if }

    if (ParentNode.HasChildNodes = false) then
        raise EXMLException.CreateFmt (cEmptyValueErrMsg,
        							   [cEmbeddedControllerVersion]);

    if (GetFirstNamedNode (ParentNode.FirstChild, Node) = false) or
       (Node.NodeName <> cVersion) then
    	raise EXMLException.Create (cGetVersionErrMsg);

    if not (Node.HasChildNodes) then
    	raise EXMLException.Create (cGetVersionErrMsg);

    sNodeValue := Node.FirstChild.NodeValue;

    if (sNodeValue <> '') then
    begin
        sVerFromXml := GetCompareOp (sNodeValue, CompareOp);

        if (CompareEmbeddedControllerVersion (sVerFromXml, CompareOp,
                                              sVerFromBios,
                                              CompareResult)) then
            Result := true;

        if (CompareResult = crNewer) then
            LogMsg (cEmbbeddedControllerDifferentMsg,
                    [cNewerMsg, sVerFromBios, sVerFromXml])
        else if (CompareResult = crOlder) then
            LogMsg (cEmbbeddedControllerDifferentMsg,
                    [cOlderMsg, sVerFromBios, sVerFromXml])
        else LogVerbose (cEmbbeddedControllerCurrentMsg, [sVerFromXml]);

        if (ProcessingStep = psDetectInstall) and (Result) then
            FIsInstalled := true;
    end { if }
    else raise EXMLException.CreateFmt (cEmptyValueErrMsg, [cVersion]);
end; { TPackage.CheckEmbeddedControllerVersionTag }

(* ---- *)

function TPackage.CheckExternalDetectionTag (
                                          const ParentNode: IXMLNode) : Boolean;
{ Returns "true" if return value is met }

var
    sValue : String;
    aReturnCodes : TaDWord;
    dwReturnCode : DWord;

begin
    Assert (ParentNode <> NIL);

    Result := false;

    if not (GetAttrValue (ParentNode, cRC, sValue)) then
        raise EXMLException.CreateFmt (cReturnCodeErrMsg,
                                       [cExternalDetection, '?']);

    if not (FillReturnCodeArray (sValue, aReturnCodes)) then
        raise EXMLException.CreateFmt (cReturnCodeErrMsg,
                                       [cExternalDetection, sValue]);

    if (ParentNode.HasChildNodes = false) or
       (GetNodeValue (ParentNode.FirstChild, sValue) = false) then
        raise EXMLException.CreateFmt (cEmptyValueErrMsg, [cExternalDetection]);

    LogVerbose (cCheckingTag, [ParentNode.NodeName]);

    if (RunExternalProgram (sValue, dwReturnCode)) then
    begin
        if (ValueInArray (dwReturnCode, aReturnCodes)) then
        begin
            Result := true;
            FTargetPresent := crTrue;
            LogVerbose (cExternalDetectionSuccessMsg);
        end { if }
        else
        begin
            FTargetPresent := crFalse;
            LogVerbose (cExternalDetectionFailureMsg);
        end; { else }
    end; { if }
end; { TPackage.CheckExternalDetectionTag }

(* ---- *)

function TPackage.CheckFileVersionTag (const ParentNode: IXMLNode) : Boolean;
{ Returns "true" if condition is met }

    (* ---- *)

    function GetFileVersion (const sFileName: String;
                             out sVersion: String) : Boolean;

    var
        sFullFileName : String;

    begin
        Assert (sFileName <> '');

        Result := false;

        if (Pos ('%', sFileName) > 0) then
            sFullFileName := ExpandPathVariables (sFileName)
        else sFullFileName := sFileName;

        sFullFileName := GetSysNativePath (sFullFileName);

        if (VeryVerboseLogging) then
            LogVerbose (cFileCheckingVersionMsg, [sFullFileName]);

        if (FileExists (sFullFileName)) then
        begin
            sVersion := GetVersionInfo (sFullFileName);

            if (sVersion <> '') then
            begin
                Result := true;
                LogVerbose (cInstalledDriverVerMsg, [sVersion]);
            end { if }
            else LogVerbose (cFileNoVersionInfoMsg, [sFileName]);
        end { if }
        else LogVerbose (cFileNotFoundMsg, [sFileName]);
    end; { GetFileVersion }

    (* ---- *)

var
    Node : IXMLNode;
    sValue, sVersion : String;
    CompareOp : TCompareOp;

begin { TPackage.CheckFileVersionTag }
    Assert (ParentNode <> NIL);

    Result := false;

    if not (GetFirstNamedNode (ParentNode.FirstChild, Node)) then
        raise EXMLException.CreateFmt (cNodeNotFoundMsg, [cFileVersion]);

    if not (LowerCase (Node.NodeName) = LowerCase (cFile)) then
        raise EXMLException.CreateFmt (cValueNotFoundMsg, [cFile]);

    LogVerbose (cCheckingTag, [ParentNode.NodeName]);

    GetVersionStr (Node, sVersion{%H-}, CompareOp{%H-});

    if not (Node.HasChildNodes) then
        raise EXMLException.CreateFmt (cNodeProcessingErrMsg, [cFile]);

    if not (GetNodeValue (Node.FirstChild, sValue)) then
        raise EXMLException.CreateFmt (cGetValueErrMsg, [cFile]);

    if (GetFileVersion (sValue, FInstalledVersion)) then
    begin
        if (ProcessingStep = psDetectInstall) then
            FDriverPresent := true;

        if (VeryVerboseLogging) then
            LogVerbose (cComparingMsg, [FInstalledVersion, sVersion]);

        Result := CompareVersionStr (FInstalledVersion, sVersion, CompareOp);

        DisplayComparissionResult (Result, CompareOp);
    end; { if }

    if (ProcessingStep = psDetectInstall) and (Result) then
        FIsInstalled := true;
end; { TPackage.CheckFileVersionTag }

(* ---- *)

function TPackage.CheckFirmwareTag (const ParentNode: IXMLNode) : Boolean;

    (* ---- *)

    function FindDeviceAndGetFirmwareVersion (const sFirmware_ID: String;
                                              var sVersion: String) : Boolean;

    const
        cRevision = '&rev_';

    var
        Device : TDevice;
        iIndex, iPos : Integer;
        sID : String;

    begin
        Assert (sFirmware_ID <> '');

        Result := false;

        if (VeryVerboseLogging) then
            LogVerbose (cFirmwareCheckingVersionMsg, [sFirmware_ID]);

        if (FindDevice (LowerCase (sFirmware_ID), Device{%H-})) then
        begin
            LogMsg (cFirmwareFoundMsg, [sFirmware_ID]);

            for iIndex := 0 to High (Device.Hardware_IDs) do
            begin
                iPos := Pos (cRevision, Device.Hardware_IDs [iIndex]);

                if (iPos > 0) then
                begin
                    Result := true;
                    sID := Device.Hardware_IDs [iIndex];

                    sVersion := '$' + Copy (sID, iPos + Length (cRevision),
                                            Length (sID) -
                                            (Pred (iPos) + Length (cRevision)));
                    Break;
                end; { if }
            end; { for }
        end; { if }
    end; { FindDeviceAndGetFirmwareVersion }

    (* ---- *)

    function VersionToStr (const sInput: String; var sOutput: String) : Boolean;

    var
        iVersion: Integer;
        IntRec : LongRec;

    begin
        iVersion := StrToInt64Def (sInput, 0);
        Result := iVersion <> 0;

        if (Result) then
        begin
            IntRec := LongRec (iVersion);

            with IntRec do
                sOutput := Format ('%d.%d.%d.%d',
                                   [Bytes [3], Bytes [2], Bytes [1], Bytes [0]])
        end { if }
        else sOutput := '0.0.0.0';
    end; { VersionToStr }

    (* ---- *)

var
    Node : IXMLNode;
    sValue, sPackageVer, sDeviceVer, sAttrValue : String;
    CompareOp : TCompareOp;
    bHexStr, bConvert : Boolean;

begin { TPackage.CheckFirmwareTag }
    Assert (ParentNode <> NIL);

    Result := false;

    if not (GetFirstNamedNode (ParentNode.FirstChild, Node)) then
        raise EXMLException.CreateFmt (cNodeNotFoundMsg, [cFirmWare]);

    if not (LowerCase (Node.NodeName) = LowerCase (cHardwareIDs)) then
        raise EXMLException.CreateFmt (cValueNotFoundMsg, [cHardwareIDs]);

    LogVerbose (cCheckingTag, [ParentNode.NodeName]);

    GetVersionStr (Node, cHex2Dec, sPackageVer{%H-}, sAttrValue{%H-},
                   CompareOp{%H-});

    if (sAttrValue <> '') then
        bHexStr := StrToBoolDef (sAttrValue, false)
    else bHexStr := false;

    if not (Node.HasChildNodes) then
        raise EXMLException.CreateFmt (cNodeProcessingErrMsg, [cHardwareIDs]);

    sValue := '';
    Node := ParentNode.FirstChild;

    while (Node <> NIL) do
    begin  // ThinkPad X13 Yoga Gen. 2 -> Multiple firmware values
        if (Node.NodeName = cHardwareIDs) and (Node.HasChildNodes) then
        begin
            sValue := Node.FirstChild.NodeValue;

            if (sValue = '') then
                raise EXMLException.CreateFmt (cGetValueErrMsg, [cHardwareIDs]);

            if (FindDeviceAndGetFirmwareVersion (sValue, sDeviceVer{%H-})) then
            begin
                bConvert := VersionToStr (sDeviceVer, FInstalledVersion);
                LogMsg (cFirmwareLocalVerMsg,
                        [sDeviceVer, FInstalledVersion]);

                if (ProcessingStep = psDependencies) then
                begin
                    Result := true;
                    FTargetPresent := crTrue;
                    Break;
                end { if }
                else
                begin
                    if (bConvert) and
                       (VersionToStr (iif (bHexStr, '$' + sPackageVer,
                                           sPackageVer), FPackageVersion)) then
                    begin
                        LogMsg (cFirmwarePackageVerMsg,
                                [sPackageVer, FPackageVersion]);

                        Result := CompareVersionStr (FInstalledVersion,
                                                     FPackageVersion,
                                                     CompareOp);
                        DisplayComparissionResult (Result, CompareOp);

                        if (Result) then
                        begin
                            FIsInstalled := true;
                            Break;
                        end; { if }
                    end { if }
                    else LogMsg (cCompareErrMsg, [sDeviceVer, sPackageVer]);
                end; { else }
            end; { if }
        end; { if }

        Node := Node.NextSibling;
    end; { while }

    if (Result = false) and (sValue = '') then
        LogMsg (cFirmwareNotFoundMsg);
end; { TPackage.CheckFirmwareTag }

(* ---- *)

function TPackage.CheckPnP_ID_Tag (const ParentNode: IXMLNode) : Boolean;
{ Returns "true" if ID is found }

var
    Node : IXMLNode;
    Device : TDevice;

begin { TPackage.CheckPnP_ID_Tag }
    Assert (ParentNode <> NIL);

    Result := false;

    if not (GetFirstNamedNode (ParentNode.FirstChild, Node)) then
        raise EXMLException.CreateFmt (cNodeNotFoundMsg, [cPnP_ID]);

    LogVerbose (cCheckingTag, [ParentNode.NodeName]);

    repeat
        if (Node.NodeType = CDATA_SECTION_NODE) then
            if (FindDevice (LowerCase (Node.NodeValue), Device{%H-})) then
            begin
                Result := true;

                if (DebugMode) then
                    with Device do
                        LogVerbose (cHardwareID_FoundMsg,
                                    [ClassDescription + ' / ' + Caption])
                else LogVerbose (cHardwareID_FoundMsg, [Device.Caption]);

                Break;
            end; { if }

        Node := Node.NextSibling;
    until (Node = NIL) or (Result);
end; { TPackage.CheckPnP_ID_Tag }

(* ---- *)

function TPackage.CheckRegistryKeyTag (const ParentNode: IXMLNode) : Boolean;
{ Returns "true" if key is found }

var
    sKey : String;
    hRegKey : HKEY;

begin
    Assert (ParentNode <> NIL);

    Result := false;

    if not (GetChildNodeValue (ParentNode, cKey, sKey)) then
        raise EXMLException.CreateFmt (cNodeNotFoundMsg, [cKey]);

    if not (SetRegKey (sKey, hRegKey)) then
        raise Exception.CreateFmt (cRegKeyUnknownMsg, [sKey]);

    if (RegKeyExists (hRegKey, sKey, IsWow64)) then
    begin
        Result := true;

        if (ProcessingStep = psDetectInstall) then
            FDriverPresent := true;
    end { if }
    else LogMsg (cRegKeyNotFoundMsg, [sKey]);
end; { TPackage.CheckRegistryKeyTag }

(* ---- *)

function TPackage.CheckRegistryKeyValueTag (const ParentNode: IXMLNode) : Boolean;
{ Returns "true" if condition is met }

    (* ---- *)

    function CheckLevel (const hRegKey: HKey;
                         const sKey, sKeyName, sLevel: String) : Boolean;

    var
        sRegValue : String;

    begin
        Result := false;

        sRegValue := RegReadStr (hRegKey, sKey, sKeyName, '', IsWow64);

        if (sRegValue <> '') then
            if (Pos ('*', sLevel) > 0) then
                Result := MatchString (LowerCase (sLevel), LowerCase (sRegValue))
            else Result := Pos (LowerCase (sLevel), LowerCase (sRegValue)) = 1;
    end; { CheckLevel }

    (* ---- *)

    function CheckVersionStr (const sVersion: String) : String;
    { Check that the version string has the format "0.0.0.0" }

    var
    	iIndex, iDots, iLastPos : Integer;

    begin
        Assert (sVersion <> '');

    	iDots := 0;
        iLastPos := 0;

        for iIndex := 1 to Length (sVersion) do
        	if (sVersion [iIndex] = '.') then
            begin
            	Inc (iDots);
                iLastPos := iIndex;
            end; { if }

        if (iDots >= 3) then
        begin
			if (iLastPos < Length (sVersion)) then
            	Result := sVersion
            else Result := sVersion + '0';
        end { if }
        else
        begin
        	Result := sVersion;

        	for iIndex := Succ (iDots) to 3 do
            	Result := Result + '.0';

			LogMsg (cVersionStringChanged, [sVersion, Result]);
        end; { else }
    end; { CheckVersionStr }

    (* ---- *)

const
    cREG_SZ = 'REG_SZ';

var
    sKey, sValueName, sValue, sVersion : String;
    hRegKey : HKEY;
    CompareOp : TCompareOp;

begin { TPackage.CheckRegistryKeyValueTag }
    Assert (ParentNode <> NIL);

    Result := false;

    if not (GetAttrValue (ParentNode, cType, sValue)) then
        raise EXMLException.CreateFmt (cAttributeNotFoundMsg,
                                       [cRegistryKeyValue, cType]);

    if (UpperCase (sValue) <> cReg_SZ) then
        raise EXMLException.CreateFmt (cAttributeUnsupportedMsg, [cReg_SZ]);

    if not (GetChildNodeValue (ParentNode, cKey, sKey)) then
        raise EXMLException.CreateFmt (cNodeNotFoundMsg, [cKey]);

    if not (GetChildNodeValue (ParentNode, cKeyName, sValueName)) then
        raise EXMLException.CreateFmt (cNodeNotFoundMsg, [cKeyName]);

    if not (SetRegKey (sKey, hRegKey)) then
        raise Exception.CreateFmt (cRegKeyUnknownMsg, [sKey]);

    LogVerbose (cCheckingTag, [ParentNode.NodeName]);

    if (RegValueExists (hRegKey, sKey, sValueName, IsWow64)) then
    begin
        if (ProcessingStep = psDetectInstall) then
            FDriverPresent := true;

        sVersion := CheckVersionStr (RegReadStr (hRegKey, sKey, sValueName,
                                				 cVersionUndefined, IsWow64));

        GetVersionStr (ParentNode.FirstChild, sValue, CompareOp{%H-});

        sValue := CheckVersionStr (sValue);

        Result := CompareVersionStr (sVersion, sValue, CompareOp);

        DisplayComparissionResult (Result, CompareOp);

(**
        // X260 Win7: KeyValue
        else if (GetChildNodeValue (ParentNode, cLevel, sValue)) then
            if (CheckLevel (hRegKey, sKey, sValueName, sValue) = false) then
                FIsInstalled := false
        else raise Exception.CreateFmt (cRegProcessingErrMsg,
                                        [cRegistryKeyValue])
**)
    end { if }
    else LogMsg (cRegValueNotFoundMsg, [sValueName, sKey]);

    if (ProcessingStep = psDetectInstall) and (Result) then
        FIsInstalled := true;
end; { TPackage.CheckRegistryKeyValueTag }

(* ---- *)

function TPackage.CheckWindowsBuildVersionTag (
										  const ParentNode: IXMLNode) : Boolean;
{ Returns "true" if condition is met }

const
	cKey = 'SOFTWARE\Microsoft\Windows NT\CurrentVersion';
    cCurrentBuildNo = 'CurrentBuildNumber';

var
    sValue, sBuildNo : String;
    iCurBuildNo : Integer;
    CompareOp : TCompareOp;

begin
    Assert (ParentNode <> NIL);

    if not (ParentNode.HasChildNodes) then
        raise EXMLException.CreateFmt (cNodeNotFoundMsg, [cWindowsBuildVersion]);

    LogVerbose (cCheckingTag, [ParentNode.NodeName]);

    if (GetChildNodeValue (ParentNode, cVersion, sValue)) then
    begin
        iCurBuildNo := StrToIntDef (RegReadStr (HKey_Local_Machine, cKey,
        										cCurrentBuildNo), 0);
        sBuildNo := GetCompareOp (sValue, CompareOp);

        Result := CompareNumbers (iCurBuildNo, StrToIntDef (sBuildNo, 0),
        						  CompareOp);
    end { if }
    else raise EXMLException.CreateFmt (cNodeNotFoundMsg, [cVersion]);

    if (Result) then
        LogVerbose (cWindowsBuildVerSuccessMsg, [sValue])
    else LogVerbose (cWindowsBuildVerFailureMsg, [sValue]);
end; { TPackage.CheckWindowsBuildVersionTag }

(* ---- *)

function TPackage.DetectDependencies (const ParentNode: IXMLNode) : Boolean;

var
    Node : IXMLNode;

begin
    Assert (ParentNode <> NIL);

    if not (GetFirstNamedNode (ParentNode, Node)) then
        raise EXMLException.CreateFmt (cNodeProcessingErrMsg, [cDetectInstall]);

    if not (Node.HasChildNodes) then
        raise EXMLException.CreateFmt (cNodeProcessingErrMsg, [cDetectInstall]);

    LogMsg (cPackageCheckingDependencies);
    FSkippedReason := srPrerequisites;

    Result := CheckConditions (Node, LowerCase (Node.NodeName));

    if (Result) then
    begin
        FTargetPresent := crTrue;
        LogMsg (cDependenciesCheckSuccessMsg);
    end { if }
    else LogMsg (cDependenciesCheckFailureMsg);
end; { TPackage.DetectDependencies }

(* ---- *)

procedure TPackage.DetectInstall (const ParentNode: IXMLNode);

var
    Node : IXMLNode;

begin
    Assert (ParentNode <> NIL);

    if not (GetFirstNamedNode (ParentNode, Node)) then
        raise EXMLException.CreateFmt (cNodeProcessingErrMsg, [cDetectInstall]);

    if not (Node.HasChildNodes) then
        raise EXMLException.CreateFmt (cNodeProcessingErrMsg, [cDetectInstall]);

    LogMsg (cPackageCheckingInstallStatusMsg);

    CheckConditions (Node, LowerCase (Node.NodeName));

    if (FIsInstalled) then
        FSkippedReason := srDetectInstall
    else FSkippedReason := srNotSkipped;
end; { TPackage.DetectInstall }

(* ---- *)

procedure TPackage.DisplayComparissionResult (const bResult: Boolean;
                                              const CompareOp: TCompareOp);

var
    sType : String;

begin
    case CompareOp of
        coEqu : sType := cCompareEqualMsg;
        coGtrEqu : sType := cCompareGreaterOrEqualMsg;
        coLessEqu : sType := cCompareLessOrEqualMsg;
    end; { case CompareOp of }

    LogVerbose (cCompareVersionResultMsg, [sType, BoolToStr (bResult, true)]);
end; { TPackage.DisplayComparissionResult }

(* ---- *)

function TPackage.ExpandPathVariables (const sPath: String) : String;

    (* ---- *)

    function GetProgramFilesDir : String;
    begin
        if (IsWow64) then
            Result := GetEnvironmentVar ('ProgramW6432')
        else Result := GetEnvironmentVar ('ProgramFiles');
    end; { GetProgramFilesDir }

    (* ---- *)

const
    asVars : array [1..4] of String =
        ('programfiles', 'programfiles(x86)', 'windows', 'packagepath');

var
    iIndex, iPos, iLen : Integer;
    sValue : String;

begin { TPackage.ExpandPathVariables }
    Assert (sPath <> '');

    Result := sPath;

    while (Pos ('%', Result) > 0) do
        if (FindVariable (Result, '%', iPos{%H-}, iLen{%H-}, sValue{%H-})) then
        begin
            sValue := LowerCase (sValue);

            for iIndex := 1 to 4 do
                if (sValue = asVars [iIndex]) then
                begin
                    Delete (Result, iPos, iLen);

                    case iIndex of
                        1 : sValue := GetProgramFilesDir;
                        2 : sValue := GetEnvironmentVar ('ProgramFiles(x86)');
                        3 : sValue := GetWinDir;
                        4 : sValue := GetPackagePath;
                    end; { case iIndex of }

                    Insert (sValue, Result, iPos);

                    Break;
                end; { if }
        end { if }
        else raise Exception.CreateFmt (cUnknownVariableMsg, [sPath, sValue]);
end; { TPackage.ExpandPathVariables }

(* ---- *)

function TPackage.FindDevice (const sDevice_ID: String;
                              var FoundDevice: TDevice) : Boolean;
// "sDevice_ID" must be in "LowerCase"

var
    Device : TDevice;

begin
    for Device in Devices do
        if (MatchHardware_ID (sDevice_ID, Device)) then
        begin
            Result := true;
            FoundDevice := Device;
            exit;
        end; { if }

    Result := false;
end; { TPackage.FindDevice }

(* ---- *)

function TPackage.GetHardwareIdTagValue (const ParentNode: IXMLNode;
                                         var sVersion: String;
                                         var bDevicePresent,
                                             bForceInstall: Boolean) : Boolean;

    (* ---- *)

    procedure ProcessDeviceInfo (const Device: TDevice);
    begin
        bDevicePresent := Device.DevicePresent;

        LogVerbose (cDeviceFoundMsg, [Device.Caption,
                                      BoolToStr (bDevicePresent, true)]);

        if (Device.DriverVer <> '') then
        begin
            sVersion := Device.DriverVer;
            FInstalledDriverProvider := Device.DriverProvider;

            if (IsMicrosoftDriver (FInstalledDriverProvider)) then
            begin
            	FMicrosoftDriver := true;
                bForceInstall := true;
                LogVerbose (cReplacingGenericDriverMsg,
                            [Device.Manufacturer, sVersion]);
            end { if }
            else LogVerbose (cInstalledDriverVerMsg, [sVersion]);
        end { if }
        else
        begin
            sVersion := cVersionUndefined;
            LogVerbose (cDriverVerUndefinedMsg);
        end; { else }
    end; { ProcessDeviceInfo }

    (* ---- *)

var
    Node : IXMLNode;
    Device, MatchingDevice : TDevice;
    sHardwareID : String;

begin { TPackage.GetHardwareIdTagValue }
    Assert (ParentNode <> NIL);

    Result := false;

    if not (GetFirstNamedNode (ParentNode.FirstChild, Node)) then
        raise EXMLException.CreateFmt (cNodeNotFoundMsg, [cDriver]);

    if not (Node.NodeName = cDataSection) then
        raise EXMLException.CreateFmt (cValueNotFoundMsg, [cDriver]);

    LogVerbose (cCheckingTag, [ParentNode.NodeName]);

    sHardwareID := Format_PnP_ID (LowerCase (Node.NodeValue));

    MatchingDevice := NIL;

    for Device in Devices do
        if (MatchHardware_ID (sHardwareID, Device)) then
        begin  // Special handling for HID devices (X1 Tablet Gen. 2)
            MatchingDevice := Device;

            if (Device.DriverVer <> '') and
               (IsMicrosoftDriver (Device.DriverProvider) = false) then
                Break;
        end; { if }

    if (MatchingDevice <> NIL) then
    begin
        Result := true;
        ProcessDeviceInfo (MatchingDevice);
    end; { if }
end; { TPackage.GetHardwareIdTagValue }

(* ---- *)

function TPackage.GetPackagePath : String;
begin
    Result := ExtractFileDir (FileName);
end; { TPackage.GetPackagePath }

(* ---- *)

function TPackage.GetServiceTagValue (const ParentNode: IXMLNode;
                                      out sVersion: String) : Boolean;

var
    Node : IXMLNode;
    Services : TServices;
    Service : TService;
    sService, sExePath : String;

begin
    Result := false;

    if (ParentNode.HasChildNodes) then
        Node := ParentNode.FirstChild
    else exit;

    if not (Node.NodeName = cHashText) then
        raise EXMLException.CreateFmt (cValueNotFoundMsg, [cServiceName]);

    sService := LowerCase (Node.NodeValue);

    if (sService = '') then
        raise EXMLException.CreateFmt (cEmptyValueErrMsg, [cServiceName]);

    LogVerbose (cCheckingTag, [ParentNode.NodeName]);

    Services := TServices.Create (true);

    try
        for Service in Services do
            if (sService = LowerCase (Service.Name)) then
            begin
                sExePath := GetSysNativePath (Service.GetFullPath (true));
                Break;
            end; { if }

    finally
        Services.Free;
    end; { try / finally }

    if (sExePath <> '') and (FileExists (sExePath)) then
    begin
        Result := true;
        LogVerbose (cService_FoundMsg, [sService]);
        sVersion := GetVersionInfo (sExePath);

        if (FInstalledVersion <> '') then
            LogVerbose (cInstalledDriverVerMsg, [sVersion])
        else
        begin
            FInstalledVersion := cVersionUndefined;
            LogVerbose (cFileNoVersionInfoMsg, [sExePath]);
        end; { else }
    end; { if }
end; { TPackage.GetServiceTagValue }

(* ---- *)

function TPackage.GetVersionStr (const FirstNode: IXMLNode;
                                 var sVersion: String;
                                 var CompareOp: TCompareOp) : Boolean;

var
    sAttr : String;

begin
    Result := GetVersionStr (FirstNode, '', sVersion, sAttr{%H-}, CompareOp);
end; { TPackage.GetVersionStr }

(* ---- *)

function TPackage.GetVersionStr (const FirstNode: IXMLNode; const sAttr: String;
                                 var sVersion, sAttrValue: String;
                                 var CompareOp: TCompareOp) : Boolean;

var
    Node : IXMLNode;

begin
    Assert (FirstNode <> NIL);

    Result := false;

    Node := FirstNode;

    repeat
        if (Node.NodeName = cVersion) then
        begin
            if (sAttr <> '') then
                if not (GetAttrValue (Node, sAttr, sAttrValue)) then
                begin
                    sAttrValue := '';
                    LogMsg (cAttributeNotFoundMsg, [cVersion, cHex2Dec]);
                end; { if }

            if (Node.HasChildNodes) then
            begin
                sVersion := GetCompareOp (Node.FirstChild.NodeValue, CompareOp);
                Result := sVersion <> '';
                Break;
            end; { if }
        end; { if }

        Node := Node.NextSibling;
    until (Node = NIL);

    if not (Result) then
        raise EXMLException.Create (cGetVersionErrMsg);
end; { TPackage.GetVersionStr }

(* ---- *)

function TPackage.IsMicrosoftDriver (const sDriverProvider: String) : Boolean;
begin
    Assert (sDriverProvider <> '');

    Result := Pos (cMicrosoft, LowerCase (sDriverProvider)) = 1;
end; { TPackage.IsMicrosoftDriver }

(* ---- *)

function TPackage.ProcessDependencies : Boolean;

var
    XmlNode : IXMLNode;

begin
    ProcessingStep := psDependencies;

    XmlNode := XmlDoc.SelectSingleNode (cRootNode + cDependencies);

    if (XmlNode = NIL) then
        raise EXMLException.CreateFmt (cNodeNotFoundMsg, [cDependencies]);

    if (XmlNode.HasChildNodes) then
        Result := DetectDependencies (XmlNode.FirstChild)
    else
    begin
        Result := false;
        LogMsg (cDependenciesDetectionUndefinedMsg);
    end; { else }
end; { TPackage.ProcessDependencies }

(* ---- *)

function TPackage.ProcessDetectInstall : Boolean;

var
    XmlNode : IXMLNode;

begin
    ProcessingStep := psDetectInstall;

    XmlNode := XmlDoc.SelectSingleNode (cRootNode + cDetectInstall);

    if (XmlNode = NIL) then
        raise EXMLException.CreateFmt (cNodeNotFoundMsg, [cDetectInstall]);

    Result := XmlNode.HasChildNodes;

    if (Result) then
        DetectInstall (XmlNode.FirstChild)
    else LogMsg (cInstallationDetectionUndefinedMsg);
end; { TPackage.ProcessDetectInstall }

(* ---- *)

procedure TPackage.RetrievePackageInfo;

    (* ---- *)

    procedure GetExtractCmd;

    var
        Node : IXMLNode;

    begin
        Node := XmlDoc.SelectSingleNode (cRootNode + cExtractCommand);

        if (Node <> NIL) and (Node.HasChildNodes) then
        begin
            if not (GetNodeValue (Node.FirstChild, FExtractCommand)) then
                raise EXMLException.CreateFmt (cGetValueErrMsg,
                                               [cExtractCommand]);
        end { if }
        else FNoArchive := true;  // "ExtractCommand" not always defined
    end; { GetExtractCmd }

    (* ---- *)

    procedure GetInstallCmd_Cmd;

    var
        Node : IXMLNode;

    begin
        Node := XmlDoc.SelectSingleNode (cRootNode + cInstall + '/' + cCmdline);

        if (Node = NIL) then
            raise EXMLException.CreateFmt (cNodeNotFoundMsg, [cInfFileNode]);

        if (Node.HasChildNodes = false) or
           (GetNodeValue (Node.FirstChild, FInstallCmd) = false) then
            raise EXMLException.CreateFmt (cValueNotFoundMsg, [cInfFileNode]);
    end; { GetInstallCmd_Cmd }

    (* ---- *)

    procedure GetInstallCmd_Inf;

    var
        Node : IXMLNode;

    begin
        Node := XmlDoc.SelectSingleNode (cRootNode + cInstall + cInfFileNode);

        if (Node = NIL) then
            raise EXMLException.CreateFmt (cNodeNotFoundMsg, [cInfFileNode]);

        if (Node.HasChildNodes = false) or
           (GetNodeValue (Node.FirstChild, FInstallCmd) = false) then
            raise EXMLException.CreateFmt (cValueNotFoundMsg, [cInfFileNode]);
    end; { GetInstallCmd_Inf }

    (* ---- *)

    procedure GetInstallationCmd;

    var
        sValue : String;
        Node : IXMLNode;

    begin
        Node := XmlDoc.SelectSingleNode (cRootNode + cInstall);

        if (Node = NIL) then
            raise EXMLException.CreateFmt (cNodeNotFoundMsg, [cInstall]);

        if (GetAttrValue (Node, cRC, sValue)) then
        begin
            if not (FillReturnCodeArray (sValue, FInstallSuccess)) then
                raise EXMLException.CreateFmt (cReturnCodeErrMsg,
                                               [cInstall, sValue]);
        end { if }
        else raise EXMLException.CreateFmt (cAttributeNotFoundMsg,
                                            [cInstall, cRC]);

        if (GetAttrValue (Node, cType, sValue)) then
            SetCmdType (sValue)
        else raise EXMLException.CreateFmt (cAttributeNotFoundMsg,
                                            [cInstall, cType]);

        case FInstallType of
            itCmd : GetInstallCmd_Cmd;
            itInf : GetInstallCmd_Inf;
        end; { case FInstallType of }
    end; { GetInstallationCmd }

    (* ---- *)

    procedure GetRebootType;

    var
        sValue : String;
        Node : IXMLNode;

    begin
        Node := XmlDoc.SelectSingleNode (cRootNode + cReboot);

        if (Node <> NIL) and (GetAttrValue (Node, cType, sValue)) then
            if not (Str2Int (sValue, FRebootType)) then
                raise EXMLException.CreateFmt (cGetValueErrMsg, [cReboot]);
    end; { GetRebootType }

    (* ---- *)

var
    XmlNode : IXMLNode;
    sValue : String;

begin { TPackage.RetrievePackageInfo }
    XmlNode := XmlDoc.DocumentElement;

    if not (XmlNode.NodeName = cPackage) then
        raise EXMLException.Create (cWrongHeaderMsg);

    if not (GetAttrValue (XmlNode, LowerCase (cVersion), FPackageVersion)) then
        raise EXMLException.Create (cVersionNotFoundMsg);

    XmlNode := SetNode (cRootNode + cTitle + '/' + cDesc);

    if (XmlNode = NIL) or (XmlNode.HasChildNodes = false) then
        raise EXMLException.CreateFmt (cNodeNotFoundMsg, [cTitle]);

    if not (GetNodeValue (XmlNode.FirstChild, sValue)) then
        raise EXMLException.CreateFmt (cGetValueErrMsg, [cDesc]);

    if (sValue <> FDescription) then
        FDescription := sValue;

    XmlNode := XmlDoc.SelectSingleNode (cRootNode + cPackageType);

    if (XmlNode <> NIL) and (GetAttrValue (XmlNode, cType, sValue)) then
        SetPackageType (sValue)
    else SetPackageType ('');

    if (Action <> aCheck) then
        GetExtractCmd;

    if (Action = aInstall) then
        GetRebootType;

    GetInstallationCmd;
end; { TPackage.RetrievePackageInfo }

(* ---- *)

function TPackage.RunExternalProgram (sCmd: String;
                                      out dwExitCode: DWord) : Boolean;
begin
    Assert (sCmd <> '');

    Result := false;

    if (Pos ('%', sCmd) = 1) then
        sCmd := ExpandPathVariables (sCmd)
    else sCmd := GetPackagePath + '\' + sCmd;

    if (Pos ('%', sCmd) > 0) then
        repeat
            sCmd := ExpandPathVariables (sCmd);
        until (Pos ('%', sCmd) = 0);

    if (WinExecAndWait (sCmd, dwExitCode)) then
    begin
        Result := true;
        LogVerbose (cProcessReturnCodeMsg, [TimeToStr (Now), dwExitCode]);
    end { if }
    else LogVerbose (cProcessExecutionErrMsg, [GetLastErrorMsg]);
end; { TPackage.RunExternalProgram }

(* ---- *)

procedure TPackage.SetCmdType (const sCmdType: String);
begin
    if (sCmdType = cCmdType_Cmd) then
        FInstallType := itCmd
    else if (sCmdType = cCmdType_Inf) then
        FInstallType := itInf
    else raise EXMLException.CreateFmt (cInstallTypeUnknownMsg, [sCmdType]);
end; { TPackage.SetCmdType }

(* ---- *)

procedure TPackage.SetPackageType (const sType: String);
begin
    if (sType = '') then
        FPackageType := ptNoPackageType
    else
        if (Length (sType) = 1) then
            case sType [1] of
                '1' : FPackageType := ptApplication;
                '2' : FPackageType := ptDriver;
                '3' : FPackageType := ptSystemBios;
                '4' : FPackageType := ptFirmware;
                else FPackageType  := ptUnknown;
            end { case of }
        else FPackageType := ptUnknown;
end; { TPackage.SetPackageType }

(* ---- *)

constructor TPackage.Create;
begin
    inherited Create;

    FRebootType := (-1);
end; { TPackage.Create }

(* ---- *)

destructor TPackage.Destroy;
begin
    inherited;
end; { TPackage.Destroy }

(* ---- *)

function TPackage.GetPackageType : String;
begin
    case FPackageType of
        ptApplication : Result := cPackageTypeApplication;
        ptDriver : Result := cPackageTypeDriver;
        ptSystemBios : Result := cPackageTypeSystemBios;
        ptFirmware : Result := cPackageTypeFirmware;
    	else Result := cPackageTypeUndefinedMsg;
    end; { case FPackageType of }
end; { TPackage.GetPackageType }

(* ---- *)

function TPackage.ProcessPackageXml (const sPackageXml: String;
                                     const Devices: TDevices = NIL) : Boolean;

    (* ---- *)

    procedure DisplayPackageDescription;
    begin
        LogMsg (cProcessingMsg +
                iif (VerboseLogging,
                     Format (cPackageTypeMsg, [GetPackageType]), ''),
                [FDescription, FPackageVersion]);
    end; { DisplayPackageDescription }

    (* ---- *)

const
    cPackageVer = 0230;

var
    iVersion : Integer;
    sExitMsg : String;

begin { TPackage.ProcessPackageXml }
    Assert (sPackageXml <> '');

    Result := false;

    if not (Get_Package_Version (sPackageXml, iVersion)) then
        raise Exception.CreateFmt (cPackageVerErrMsg, [sPackageXml]);

    if (iVersion <> cPackageVer) then
        raise Exception.CreateFmt (cUnsupportedPackageVerMsg, [iVersion]);

    FPackageXml := sPackageXml;

    if (Devices <> NIL) then
        Self.Devices := Devices;

    if not (LoadXml (sPackageXml)) then
        exit;

    RetrievePackageInfo;

    if (Action = aUnpack) then
    begin
        Result := true;
        DisplayPackageDescription;
    end { if }
    else
    begin
        if (Action = aInstall) or
           ((Action = aCheck) and (CheckAll = false)) then
            if (FPackageType = ptFirmware) and (UpdateFirmware = false) then
            begin
                if (ListAll) then
                    sExitMsg := cSkippingFirmwarePackageMsg
                else exit;
            end { if }
            else if (FPackageType <> ptFirmware) and (UpdateFirmware) then
            begin
                if (ListAll) then
                    sExitMsg := cSkippingNonFirmwarePackageMsg
                else exit;
            end { if }
            else if (FPackageType = ptSystemBios) and (UpdateBios = false) then
            begin
                if (ListAll) then
                    sExitMsg := cSkippingBiosUpdatePackageMsg
                else exit;
            end { if }
            else if (FPackageType <> ptSystemBios) and (UpdateBios) then
            begin
                if (ListAll) then
                    sExitMsg := cSkippingNonBiosUpdatePackageMsg
                else exit;
            end; { if }

        DisplayPackageDescription;

        if (sExitMsg <> '') then
        begin
            LogMsg (sExitMsg);
            exit;
        end; { if }

        LogVerbose (FPackageXml);

        if (GetIniFileName <> '') and (Action = aInstall) then
			FTargetPresent := crTrue
        else
        begin
            if not (FCompatibilityCheck) then
                LogMsg (cCompatibilityWarningMsg + FCompatibilityMsg);

            if (ProcessDependencies) then
            begin
                ProcessDetectInstall;

                if (FIsInstalled) then
                begin
                    if (FTargetPresent = crTrue) then
                        LogMsg (cInstallationNotRequiredMsg);
                end { if }
                else LogMsg (cInstallationRequiredMsg)
            end; { if }
        end; { else }

        Result := true;
    end; { else }

    LogMsg ('');

    if (Devices <> NIL) then
        Self.Devices := NIL;
end; { TPackage.ProcessPackageXml }

(* ---- *)

end.

