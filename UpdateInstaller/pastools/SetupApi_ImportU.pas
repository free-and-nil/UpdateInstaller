// Needed declarations copied from "SetupApi.pas" by

{ Portions created by Microsoft are                                }
{ Copyright (C) 1995-1999 Microsoft Corporation.                   }
{ All Rights Reserved.                                             }
{                                                                  }
{ The original file is: setupapi.h, released March 1999.           }
{ The original Pascal code is: SetupApi.pas, released 29 Jan 2000. }
{ The initial developer of the Pascal code is Robert Marquardt     }
{ (robert_marquardt att gmx dott de)                               }
{                                                                  }
{ Portions created by Robert Marquardt are                         }
{ Copyright (C) 1999 Robert Marquardt.                             }
{                                                                  }
{ Contributor(s): Marcel van Brakel (brakelm att bart dott nl)     }
{                                                                  }
{ Obtained through:                                                }
{ Joint Endeavour of Delphi Innovators (Project JEDI)              }
{                                                                  }

{$I ..\switches.inc}

{$MINENUMSIZE 4}

{$IFDEF FPC}
	{$MODE DELPHI}

    {$IFDEF CPUX64}
        // https://forum.lazarus.freepascal.org/index.php?topic=13157.0
        {$PACKRECORDS 8}
    {$ENDIF CPUX64}
{$ENDIF}

unit SetupApi_ImportU;

interface

uses Windows, CommCtrl;

type
    DevPropKey = packed record
      fmtid : TGUID ;
      pid : ULONG;
    end; { DevPropKey }
    PTDevPropKey = ^TDevPropKey;
    TDevPropKey = DevPropKey;

    PTaDevPropKey = ^TaDevPropKey;
    TaDevPropKey = packed array [0..0] of TDevPropKey;

const
    //
    // DEVPKEY_NAME
    // Common DEVPKEY used to retrieve the display name for an object.
    //
    DEVPKEY_NAME : TDevPropKey = (fmtid: '{B725F130-47EF-101A-A5F1-02608C9EEBAC}'; pid: ULong (10));
    //
    // Device properties
    // These DEVPKEYs correspond to the SetupAPI SPDRP_XXX device properties.
    //
    DEVPKEY_Device_DeviceDesc : TDevPropKey = (fmtid: '{A45C254E-DF1C-4EFD-8020-67D146A850E0}'; pid: ULong (2));
    DEVPKEY_Device_HardwareIds : TDevPropKey = (fmtid: '{A45C254E-DF1C-4EFD-8020-67D146A850E0}'; pid: ULong (3));
    DEVPKEY_Device_CompatibleIds : TDevPropKey = (fmtid: '{A45C254E-DF1C-4EFD-8020-67D146A850E0}'; pid: ULong (4));
    DEVPKEY_Device_Service : TDevPropKey = (fmtid: '{A45C254E-DF1C-4EFD-8020-67D146A850E0}'; pid: ULong (6));
    DEVPKEY_Device_Class : TDevPropKey = (fmtid: '{A45C254E-DF1C-4EFD-8020-67D146A850E0}'; pid: ULong (9));
    DEVPKEY_Device_ClassGuid : TDevPropKey = (fmtid: '{A45C254E-DF1C-4EFD-8020-67D146A850E0}'; pid: ULong (10));
    DEVPKEY_Device_Driver : TDevPropKey = (fmtid: '{A45C254E-DF1C-4EFD-8020-67D146A850E0}'; pid: ULong (11));
    DEVPKEY_Device_ConfigFlags : TDevPropKey = (fmtid: '{A45C254E-DF1C-4EFD-8020-67D146A850E0}'; pid: ULong (12));
    DEVPKEY_Device_Manufacturer : TDevPropKey = (fmtid: '{A45C254E-DF1C-4EFD-8020-67D146A850E0}'; pid: ULong (13));
    DEVPKEY_Device_FriendlyName : TDevPropKey = (fmtid: '{A45C254E-DF1C-4EFD-8020-67D146A850E0}'; pid: ULong (14));
    DEVPKEY_Device_LocationInfo : TDevPropKey = (fmtid: '{A45C254E-DF1C-4EFD-8020-67D146A850E0}'; pid: ULong (15));
    DEVPKEY_Device_PDOName : TDevPropKey = (fmtid: '{A45C254E-DF1C-4EFD-8020-67D146A850E0}'; pid: ULong (16));
    DEVPKEY_Device_Capabilities : TDevPropKey = (fmtid: '{A45C254E-DF1C-4EFD-8020-67D146A850E0}'; pid: ULong (17));
    DEVPKEY_Device_UINumber : TDevPropKey = (fmtid: '{A45C254E-DF1C-4EFD-8020-67D146A850E0}'; pid: ULong (18));
    DEVPKEY_Device_UpperFilters : TDevPropKey = (fmtid: '{A45C254E-DF1C-4EFD-8020-67D146A850E0}'; pid: ULong (19));
    DEVPKEY_Device_LowerFilters : TDevPropKey = (fmtid: '{A45C254E-DF1C-4EFD-8020-67D146A850E0}'; pid: ULong (20));
    DEVPKEY_Device_BusTypeGuid : TDevPropKey = (fmtid: '{A45C254E-DF1C-4EFD-8020-67D146A850E0}'; pid: ULong (21));
    DEVPKEY_Device_LegacyBusType : TDevPropKey = (fmtid: '{A45C254E-DF1C-4EFD-8020-67D146A850E0}'; pid: ULong (22));
    DEVPKEY_Device_BusNumber : TDevPropKey = (fmtid: '{A45C254E-DF1C-4EFD-8020-67D146A850E0}'; pid: ULong (23));
    DEVPKEY_Device_EnumeratorName : TDevPropKey = (fmtid: '{A45C254E-DF1C-4EFD-8020-67D146A850E0}'; pid: ULong (24));
    DEVPKEY_Device_Security : TDevPropKey = (fmtid: '{A45C254E-DF1C-4EFD-8020-67D146A850E0}'; pid: ULong (25));
    DEVPKEY_Device_SecuritySDS : TDevPropKey = (fmtid: '{A45C254E-DF1C-4EFD-8020-67D146A850E0}'; pid: ULong (26));
    DEVPKEY_Device_DevType : TDevPropKey = (fmtid: '{A45C254E-DF1C-4EFD-8020-67D146A850E0}'; pid: ULong (27));
    DEVPKEY_Device_Exclusive : TDevPropKey = (fmtid: '{A45C254E-DF1C-4EFD-8020-67D146A850E0}'; pid: ULong (28));
    DEVPKEY_Device_Characteristics : TDevPropKey = (fmtid: '{A45C254E-DF1C-4EFD-8020-67D146A850E0}'; pid: ULong (29));
    DEVPKEY_Device_Address : TDevPropKey = (fmtid: '{A45C254E-DF1C-4EFD-8020-67D146A850E0}'; pid: ULong (30));
    DEVPKEY_Device_UINumberDescFormat : TDevPropKey = (fmtid: '{A45C254E-DF1C-4EFD-8020-67D146A850E0}'; pid: ULong (31));
    DEVPKEY_Device_PowerData : TDevPropKey = (fmtid: '{A45C254E-DF1C-4EFD-8020-67D146A850E0}'; pid: ULong (32));
    DEVPKEY_Device_RemovalPolicy : TDevPropKey = (fmtid: '{A45C254E-DF1C-4EFD-8020-67D146A850E0}'; pid: ULong (33));
    DEVPKEY_Device_RemovalPolicyDefault : TDevPropKey = (fmtid: '{A45C254E-DF1C-4EFD-8020-67D146A850E0}'; pid: ULong (34));
    DEVPKEY_Device_RemovalPolicyOverride : TDevPropKey = (fmtid: '{A45C254E-DF1C-4EFD-8020-67D146A850E0}'; pid: ULong (35));
    DEVPKEY_Device_InstallState : TDevPropKey = (fmtid: '{A45C254E-DF1C-4EFD-8020-67D146A850E0}'; pid: ULong (36));
    DEVPKEY_Device_LocationPaths : TDevPropKey = (fmtid: '{A45C254E-DF1C-4EFD-8020-67D146A850E0}'; pid: ULong (37));
    DEVPKEY_Device_BaseContainerId : TDevPropKey = (fmtid: '{A45C254E-DF1C-4EFD-8020-67D146A850E0}'; pid: ULong (38));
    //
    // Device and Device Interface property
    // Common DEVPKEY used to retrieve the device instance id associated with devices and device interfaces.
    //
    DEVPKEY_Device_InstanceId : TDevPropKey = (fmtid: '{78C34FC8-104A-4ACA-9EA4-524D52996E57}'; pid: ULong (256));
    //
    // Device properties
    // These DEVPKEYs correspond to a device's status and problem code.
    //
    DEVPKEY_Device_DevNodeStatus : TDevPropKey = (fmtid: '{4340A6C5-93FA-4706-972C-7B648008A5A7}'; pid: ULong (2));
    DEVPKEY_Device_ProblemCode : TDevPropKey = (fmtid: '{4340A6C5-93FA-4706-972C-7B648008A5A7}'; pid: ULong (3));
    //
    // Device properties
    // These DEVPKEYs correspond to a device's relations.
    //
    DEVPKEY_Device_EjectionRelations : TDevPropKey = (fmtid: '{4340A6C5-93FA-4706-972C-7B648008A5A7}'; pid: ULong (4));
    DEVPKEY_Device_RemovalRelations : TDevPropKey = (fmtid: '{4340A6C5-93FA-4706-972C-7B648008A5A7}'; pid: ULong (5));
    DEVPKEY_Device_PowerRelations : TDevPropKey = (fmtid: '{4340A6C5-93FA-4706-972C-7B648008A5A7}'; pid: ULong (6));
    DEVPKEY_Device_BusRelations : TDevPropKey = (fmtid: '{4340A6C5-93FA-4706-972C-7B648008A5A7}'; pid: ULong (7));
    DEVPKEY_Device_Parent : TDevPropKey = (fmtid: '{4340A6C5-93FA-4706-972C-7B648008A5A7}'; pid: ULong (8));
    DEVPKEY_Device_Children : TDevPropKey = (fmtid: '{4340A6C5-93FA-4706-972C-7B648008A5A7}'; pid: ULong (9));
    DEVPKEY_Device_Siblings : TDevPropKey = (fmtid: '{4340A6C5-93FA-4706-972C-7B648008A5A7}'; pid: ULong (10));
    DEVPKEY_Device_TransportRelations : TDevPropKey = (fmtid: '{4340A6C5-93FA-4706-972C-7B648008A5A7}'; pid: ULong (11));
    //
    // Device property
    // This DEVPKEY corresponds to a the status code that resulted in a device to be in a problem state.
    //
    DEVPKEY_Device_ProblemStatus : TDevPropKey = (fmtid: '{4340A6C5-93FA-4706-972C-7B648008A5A7}'; pid: ULong (12));
    //
    // Device properties
    // These DEVPKEYs are set for the corresponding types of root-enumerated devices.
    //
    DEVPKEY_Device_Reported : TDevPropKey = (fmtid: '{80497100-8C73-48B9-AAD9-CE387E19C56E}'; pid: ULong (2));
    DEVPKEY_Device_Legacy : TDevPropKey = (fmtid: '{80497100-8C73-48B9-AAD9-CE387E19C56E}'; pid: ULong (3));
    //
    // Device Container Id
    //
    DEVPKEY_Device_ContainerId : TDevPropKey = (fmtid: '{8C7ED206-3F8A-4827-B3AB-AE9E1FAEFC6C}'; pid: ULong (2));
    DEVPKEY_Device_InLocalMachineContainer : TDevPropKey = (fmtid: '{8C7ED206-3F8A-4827-B3AB-AE9E1FAEFC6C}'; pid: ULong (4));
    //
    // Device property
    // This DEVPKEY correspond to a device's model.
    //
    DEVPKEY_Device_Model : TDevPropKey = (fmtid: '{78C34FC8-104A-4ACA-9EA4-524D52996E57}'; pid: ULong (39));
    //
    // Device Experience related Keys
    //
    DEVPKEY_Device_ModelId : TDevPropKey = (fmtid: '{80D81EA6-7473-4B0C-8216-EFC11A2C4C8B}'; pid: ULong (2));
    DEVPKEY_Device_FriendlyNameAttributes : TDevPropKey = (fmtid: '{80D81EA6-7473-4B0C-8216-EFC11A2C4C8B}'; pid: ULong (3));
    DEVPKEY_Device_ManufacturerAttributes : TDevPropKey = (fmtid: '{80D81EA6-7473-4B0C-8216-EFC11A2C4C8B}'; pid: ULong (4));
    DEVPKEY_Device_PresenceNotForDevice : TDevPropKey = (fmtid: '{80D81EA6-7473-4B0C-8216-EFC11A2C4C8B}'; pid: ULong (5));
    DEVPKEY_Device_SignalStrength : TDevPropKey = (fmtid: '{80D81EA6-7473-4B0C-8216-EFC11A2C4C8B}'; pid: ULong (6));
    DEVPKEY_Device_IsAssociateableByUserAction : TDevPropKey = (fmtid: '{80D81EA6-7473-4B0C-8216-EFC11A2C4C8B}'; pid: ULong (7));
    DEVPKEY_Device_ShowInUninstallUI : TDevPropKey = (fmtid: '{80D81EA6-7473-4B0C-8216-EFC11A2C4C8B}'; pid: ULong (8));
    DEVPKEY_Device_Numa_Proximity_Domain : TDevPropKey = (fmtid: '{540B947E-8B40-45BC-A8A2-6A0B894CBDA2}'; pid: ULong (1));
    DEVPKEY_Device_DHP_Rebalance_Policy : TDevPropKey = (fmtid: '{540B947E-8B40-45BC-A8A2-6A0B894CBDA2}'; pid: ULong (2));
    DEVPKEY_Device_Numa_Node : TDevPropKey = (fmtid: '{540B947E-8B40-45BC-A8A2-6A0B894CBDA2}'; pid: ULong (3));
    DEVPKEY_Device_BusReportedDeviceDesc : TDevPropKey = (fmtid: '{540B947E-8B40-45BC-A8A2-6A0B894CBDA2}'; pid: ULong (4));
    DEVPKEY_Device_IsPresent : TDevPropKey = (fmtid: '{540B947E-8B40-45BC-A8A2-6A0B894CBDA2}'; pid: ULong (5));
    DEVPKEY_Device_HasProblem : TDevPropKey = (fmtid: '{540B947E-8B40-45BC-A8A2-6A0B894CBDA2}'; pid: ULong (6));
    DEVPKEY_Device_ConfigurationId : TDevPropKey = (fmtid: '{540B947E-8B40-45BC-A8A2-6A0B894CBDA2}'; pid: ULong (7));
    DEVPKEY_Device_ReportedDeviceIdsHash : TDevPropKey = (fmtid: '{540B947E-8B40-45BC-A8A2-6A0B894CBDA2}'; pid: ULong (8));
    DEVPKEY_Device_PhysicalDeviceLocation : TDevPropKey = (fmtid: '{540B947E-8B40-45BC-A8A2-6A0B894CBDA2}'; pid: ULong (9));
    DEVPKEY_Device_BiosDeviceName : TDevPropKey = (fmtid: '{540B947E-8B40-45BC-A8A2-6A0B894CBDA2}'; pid: ULong (10));
    DEVPKEY_Device_DriverProblemDesc : TDevPropKey = (fmtid: '{540B947E-8B40-45BC-A8A2-6A0B894CBDA2}'; pid: ULong (11));
    DEVPKEY_Device_DebuggerSafe : TDevPropKey = (fmtid: '{540B947E-8B40-45BC-A8A2-6A0B894CBDA2}'; pid: ULong (12));
    DEVPKEY_Device_PostInstallInProgress : TDevPropKey = (fmtid: '{540B947E-8B40-45BC-A8A2-6A0B894CBDA2}'; pid: ULong (13));
    DEVPKEY_Device_Stack : TDevPropKey = (fmtid: '{540B947E-8B40-45BC-A8A2-6A0B894CBDA2}'; pid: ULong (14));
    DEVPKEY_Device_ExtendedConfigurationIds : TDevPropKey = (fmtid: '{540B947E-8B40-45BC-A8A2-6A0B894CBDA2}'; pid: ULong (15));
    DEVPKEY_Device_IsRebootRequired : TDevPropKey = (fmtid: '{540B947E-8B40-45BC-A8A2-6A0B894CBDA2}'; pid: ULong (16));
    DEVPKEY_Device_FirmwareDate : TDevPropKey = (fmtid: '{540B947E-8B40-45BC-A8A2-6A0B894CBDA2}'; pid: ULong (17));
    DEVPKEY_Device_FirmwareVersion : TDevPropKey = (fmtid: '{540B947E-8B40-45BC-A8A2-6A0B894CBDA2}'; pid: ULong (18));
    DEVPKEY_Device_FirmwareRevision : TDevPropKey = (fmtid: '{540B947E-8B40-45BC-A8A2-6A0B894CBDA2}'; pid: ULong (19));
    //
    // Device Session Id
    //
    DEVPKEY_Device_SessionId : TDevPropKey = (fmtid: '{83DA6326-97A6-4088-9453-A1923F573B29}'; pid: ULong (6));
    //
    // Device activity timestamp properties
    //
    DEVPKEY_Device_InstallDate : TDevPropKey = (fmtid: '{83DA6326-97A6-4088-9453-A1923F573B29}'; pid: ULong (100));
    DEVPKEY_Device_FirstInstallDate : TDevPropKey = (fmtid: '{83DA6326-97A6-4088-9453-A1923F573B29}'; pid: ULong (101));
    DEVPKEY_Device_LastArrivalDate : TDevPropKey = (fmtid: '{83DA6326-97A6-4088-9453-A1923F573B29}'; pid: ULong (102));
    DEVPKEY_Device_LastRemovalDate : TDevPropKey = (fmtid: '{83DA6326-97A6-4088-9453-A1923F573B29}'; pid: ULong (103));
    //
    // Device driver properties
    //
    DEVPKEY_Device_DriverDate : TDevPropKey = (fmtid: '{A8B865DD-2E3D-4094-AD97-E593A70C75D6}'; pid: ULong (2));
    DEVPKEY_Device_DriverVersion : TDevPropKey = (fmtid: '{A8B865DD-2E3D-4094-AD97-E593A70C75D6}'; pid: ULong (3));
    DEVPKEY_Device_DriverDesc : TDevPropKey = (fmtid: '{A8B865DD-2E3D-4094-AD97-E593A70C75D6}'; pid: ULong (4));
    DEVPKEY_Device_DriverInfPath : TDevPropKey = (fmtid: '{A8B865DD-2E3D-4094-AD97-E593A70C75D6}'; pid: ULong (5));
    DEVPKEY_Device_DriverInfSection : TDevPropKey = (fmtid: '{A8B865DD-2E3D-4094-AD97-E593A70C75D6}'; pid: ULong (6));
    DEVPKEY_Device_DriverInfSectionExt : TDevPropKey = (fmtid: '{A8B865DD-2E3D-4094-AD97-E593A70C75D6}'; pid: ULong (7));
    DEVPKEY_Device_MatchingDeviceId : TDevPropKey = (fmtid: '{A8B865DD-2E3D-4094-AD97-E593A70C75D6}'; pid: ULong (8));
    DEVPKEY_Device_DriverProvider : TDevPropKey = (fmtid: '{A8B865DD-2E3D-4094-AD97-E593A70C75D6}'; pid: ULong (9));
    DEVPKEY_Device_DriverPropPageProvider : TDevPropKey = (fmtid: '{A8B865DD-2E3D-4094-AD97-E593A70C75D6}'; pid: ULong (10));
    DEVPKEY_Device_DriverCoInstallers : TDevPropKey = (fmtid: '{A8B865DD-2E3D-4094-AD97-E593A70C75D6}'; pid: ULong (11));
    DEVPKEY_Device_ResourcePickerTags : TDevPropKey = (fmtid: '{A8B865DD-2E3D-4094-AD97-E593A70C75D6}'; pid: ULong (12));
    DEVPKEY_Device_ResourcePickerExceptions : TDevPropKey = (fmtid: '{A8B865DD-2E3D-4094-AD97-E593A70C75D6}'; pid: ULong (13));
    DEVPKEY_Device_DriverRank : TDevPropKey = (fmtid: '{A8B865DD-2E3D-4094-AD97-E593A70C75D6}'; pid: ULong (14));
    DEVPKEY_Device_DriverLogoLevel : TDevPropKey = (fmtid: '{A8B865DD-2E3D-4094-AD97-E593A70C75D6}'; pid: ULong (15));
    //
    // Device properties
    // These DEVPKEYs may be set by the driver package installed for a device.
    //
    DEVPKEY_Device_NoConnectSound : TDevPropKey = (fmtid: '{A8B865DD-2E3D-4094-AD97-E593A70C75D6}'; pid: ULong (17));
    DEVPKEY_Device_GenericDriverInstalled : TDevPropKey = (fmtid: '{A8B865DD-2E3D-4094-AD97-E593A70C75D6}'; pid: ULong (18));
    DEVPKEY_Device_AdditionalSoftwareRequested : TDevPropKey = (fmtid: '{A8B865DD-2E3D-4094-AD97-E593A70C75D6}'; pid: ULong (19));
    //
    // Device safe-removal properties
    //
    DEVPKEY_Device_SafeRemovalRequired : TDevPropKey = (fmtid: '{AFD97640-86A3-4210-B67C-289C41AABE55}'; pid: ULong (2));
    DEVPKEY_Device_SafeRemovalRequiredOverride : TDevPropKey = (fmtid: '{AFD97640-86A3-4210-B67C-289C41AABE55}'; pid: ULong (3));
    //
    // Device properties
    // These DEVPKEYs may be set by the driver package installed for a device.
    //
    DEVPKEY_DrvPkg_Model : TDevPropKey = (fmtid: '{CF73BB51-3ABF-44A2-85E0-9A3DC7A12132}'; pid: ULong (2));
    DEVPKEY_DrvPkg_VendorWebSite : TDevPropKey = (fmtid: '{CF73BB51-3ABF-44A2-85E0-9A3DC7A12132}'; pid: ULong (3));
    DEVPKEY_DrvPkg_DetailedDescription : TDevPropKey = (fmtid: '{CF73BB51-3ABF-44A2-85E0-9A3DC7A12132}'; pid: ULong (4));
    DEVPKEY_DrvPkg_DocumentationLink : TDevPropKey = (fmtid: '{CF73BB51-3ABF-44A2-85E0-9A3DC7A12132}'; pid: ULong (5));
    DEVPKEY_DrvPkg_Icon : TDevPropKey = (fmtid: '{CF73BB51-3ABF-44A2-85E0-9A3DC7A12132}'; pid: ULong (6));
    DEVPKEY_DrvPkg_BrandingIcon : TDevPropKey = (fmtid: '{CF73BB51-3ABF-44A2-85E0-9A3DC7A12132}'; pid: ULong (7));
    //
    // Device setup class properties
    // These DEVPKEYs correspond to the SetupAPI SPCRP_XXX setup class properties.
    //
    DEVPKEY_DeviceClass_UpperFilters : TDevPropKey = (fmtid: '{4321918B-F69E-470D-A5DE-4D88C75AD24B}'; pid: ULong (19));
    DEVPKEY_DeviceClass_LowerFilters : TDevPropKey = (fmtid: '{4321918B-F69E-470D-A5DE-4D88C75AD24B}'; pid: ULong (20));
    DEVPKEY_DeviceClass_Security : TDevPropKey = (fmtid: '{4321918B-F69E-470D-A5DE-4D88C75AD24B}'; pid: ULong (25));
    DEVPKEY_DeviceClass_SecuritySDS : TDevPropKey = (fmtid: '{4321918B-F69E-470D-A5DE-4D88C75AD24B}'; pid: ULong (26));
    DEVPKEY_DeviceClass_DevType : TDevPropKey = (fmtid: '{4321918B-F69E-470D-A5DE-4D88C75AD24B}'; pid: ULong (27));
    DEVPKEY_DeviceClass_Exclusive : TDevPropKey = (fmtid: '{4321918B-F69E-470D-A5DE-4D88C75AD24B}'; pid: ULong (28));
    DEVPKEY_DeviceClass_Characteristics : TDevPropKey = (fmtid: '{4321918B-F69E-470D-A5DE-4D88C75AD24B}'; pid: ULong (29));
    //
    // Device setup class properties
    //
    DEVPKEY_DeviceClass_Name : TDevPropKey = (fmtid: '{259ABFFC-50A7-47CE-AF08-68C9A7D73366}'; pid: ULong (2));
    DEVPKEY_DeviceClass_ClassName : TDevPropKey = (fmtid: '{259ABFFC-50A7-47CE-AF08-68C9A7D73366}'; pid: ULong (3));
    DEVPKEY_DeviceClass_Icon : TDevPropKey = (fmtid: '{259ABFFC-50A7-47CE-AF08-68C9A7D73366}'; pid: ULong (4));
    DEVPKEY_DeviceClass_ClassInstaller : TDevPropKey = (fmtid: '{259ABFFC-50A7-47CE-AF08-68C9A7D73366}'; pid: ULong (5));
    DEVPKEY_DeviceClass_PropPageProvider : TDevPropKey = (fmtid: '{259ABFFC-50A7-47CE-AF08-68C9A7D73366}'; pid: ULong (6));
    DEVPKEY_DeviceClass_NoInstallClass : TDevPropKey = (fmtid: '{259ABFFC-50A7-47CE-AF08-68C9A7D73366}'; pid: ULong (7));
    DEVPKEY_DeviceClass_NoDisplayClass : TDevPropKey = (fmtid: '{259ABFFC-50A7-47CE-AF08-68C9A7D73366}'; pid: ULong (8));
    DEVPKEY_DeviceClass_SilentInstall : TDevPropKey = (fmtid: '{259ABFFC-50A7-47CE-AF08-68C9A7D73366}'; pid: ULong (9));
    DEVPKEY_DeviceClass_NoUseClass : TDevPropKey = (fmtid: '{259ABFFC-50A7-47CE-AF08-68C9A7D73366}'; pid: ULong (10));
    DEVPKEY_DeviceClass_DefaultService : TDevPropKey = (fmtid: '{259ABFFC-50A7-47CE-AF08-68C9A7D73366}'; pid: ULong (11));
    DEVPKEY_DeviceClass_IconPath : TDevPropKey = (fmtid: '{259ABFFC-50A7-47CE-AF08-68C9A7D73366}'; pid: ULong (12));
    DEVPKEY_DeviceClass_DHPRebalanceOptOut : TDevPropKey = (fmtid: '{D14D3EF3-66CF-4BA2-9D38-0DDB37AB4701}'; pid: ULong (2));
    //
    // Other Device setup class properties
    //
    DEVPKEY_DeviceClass_ClassCoInstallers : TDevPropKey = (fmtid: '{713D1703-A2E2-49F5-9214-56472EF3DA5C}'; pid: ULong (2));
    //
    // Device interface properties
    //
    DEVPKEY_DeviceInterface_FriendlyName : TDevPropKey = (fmtid: '{026E516E-B814-414B-83CD-856D6FEF4822}'; pid: ULong (2));
    DEVPKEY_DeviceInterface_Enabled : TDevPropKey = (fmtid: '{026E516E-B814-414B-83CD-856D6FEF4822}'; pid: ULong (3));
    DEVPKEY_DeviceInterface_ClassGuid : TDevPropKey = (fmtid: '{026E516E-B814-414B-83CD-856D6FEF4822}'; pid: ULong (4));
    DEVPKEY_DeviceInterface_ReferenceString : TDevPropKey = (fmtid: '{026E516E-B814-414B-83CD-856D6FEF4822}'; pid: ULong (5));
    DEVPKEY_DeviceInterface_Restricted : TDevPropKey = (fmtid: '{026E516E-B814-414B-83CD-856D6FEF4822}'; pid: ULong (6));
    //
    // Device interface class properties
    //
    DEVPKEY_DeviceInterfaceClass_DefaultInterface : TDevPropKey = (fmtid: '{14C83A99-0B3F-44B7-BE4C-A178D3990564}'; pid: ULong (2));
    DEVPKEY_DeviceInterfaceClass_Name : TDevPropKey = (fmtid: '{14C83A99-0B3F-44B7-BE4C-A178D3990564}'; pid: ULong (3));
    //
    // Device Container Properties
    //
    DEVPKEY_DeviceContainer_Address : TDevPropKey = (fmtid: '{78C34FC8-104A-4ACA-9EA4-524D52996E57}'; pid: ULong (51));
    DEVPKEY_DeviceContainer_DiscoveryMethod : TDevPropKey = (fmtid: '{78C34FC8-104A-4ACA-9EA4-524D52996E57}'; pid: ULong (52));
    DEVPKEY_DeviceContainer_IsEncrypted : TDevPropKey = (fmtid: '{78C34FC8-104A-4ACA-9EA4-524D52996E57}'; pid: ULong (53));
    DEVPKEY_DeviceContainer_IsAuthenticated : TDevPropKey = (fmtid: '{78C34FC8-104A-4ACA-9EA4-524D52996E57}'; pid: ULong (54));
    DEVPKEY_DeviceContainer_IsConnected : TDevPropKey = (fmtid: '{78C34FC8-104A-4ACA-9EA4-524D52996E57}'; pid: ULong (55));
    DEVPKEY_DeviceContainer_IsPaired : TDevPropKey = (fmtid: '{78C34FC8-104A-4ACA-9EA4-524D52996E57}'; pid: ULong (56));
    DEVPKEY_DeviceContainer_Icon : TDevPropKey = (fmtid: '{78C34FC8-104A-4ACA-9EA4-524D52996E57}'; pid: ULong (57));
    DEVPKEY_DeviceContainer_Version : TDevPropKey = (fmtid: '{78C34FC8-104A-4ACA-9EA4-524D52996E57}'; pid: ULong (65));
    DEVPKEY_DeviceContainer_Last_Seen : TDevPropKey = (fmtid: '{78C34FC8-104A-4ACA-9EA4-524D52996E57}'; pid: ULong (66));
    DEVPKEY_DeviceContainer_Last_Connected : TDevPropKey = (fmtid: '{78C34FC8-104A-4ACA-9EA4-524D52996E57}'; pid: ULong (67));
    DEVPKEY_DeviceContainer_IsShowInDisconnectedState : TDevPropKey = (fmtid: '{78C34FC8-104A-4ACA-9EA4-524D52996E57}'; pid: ULong (68));
    DEVPKEY_DeviceContainer_IsLocalMachine : TDevPropKey = (fmtid: '{78C34FC8-104A-4ACA-9EA4-524D52996E57}'; pid: ULong (70));
    DEVPKEY_DeviceContainer_MetadataPath : TDevPropKey = (fmtid: '{78C34FC8-104A-4ACA-9EA4-524D52996E57}'; pid: ULong (71));
    DEVPKEY_DeviceContainer_IsMetadataSearchInProgress : TDevPropKey = (fmtid: '{78C34FC8-104A-4ACA-9EA4-524D52996E57}'; pid: ULong (72));
    DEVPKEY_DeviceContainer_MetadataChecksum : TDevPropKey = (fmtid: '{78C34FC8-104A-4ACA-9EA4-524D52996E57}'; pid: ULong (73));
    DEVPKEY_DeviceContainer_IsNotInterestingForDisplay : TDevPropKey = (fmtid: '{78C34FC8-104A-4ACA-9EA4-524D52996E57}'; pid: ULong (74));
    DEVPKEY_DeviceContainer_LaunchDeviceStageOnDeviceConnect : TDevPropKey = (fmtid: '{78C34FC8-104A-4ACA-9EA4-524D52996E57}'; pid: ULong (76));
    DEVPKEY_DeviceContainer_LaunchDeviceStageFromExplorer : TDevPropKey = (fmtid: '{78C34FC8-104A-4ACA-9EA4-524D52996E57}'; pid: ULong (77));
    DEVPKEY_DeviceContainer_BaselineExperienceId : TDevPropKey = (fmtid: '{78C34FC8-104A-4ACA-9EA4-524D52996E57}'; pid: ULong (78));
    DEVPKEY_DeviceContainer_IsDeviceUniquelyIdentifiable : TDevPropKey = (fmtid: '{78C34FC8-104A-4ACA-9EA4-524D52996E57}'; pid: ULong (79));
    DEVPKEY_DeviceContainer_AssociationArray : TDevPropKey = (fmtid: '{78C34FC8-104A-4ACA-9EA4-524D52996E57}'; pid: ULong (80));
    DEVPKEY_DeviceContainer_DeviceDescription1 : TDevPropKey = (fmtid: '{78C34FC8-104A-4ACA-9EA4-524D52996E57}'; pid: ULong (81));
    DEVPKEY_DeviceContainer_DeviceDescription2 : TDevPropKey = (fmtid: '{78C34FC8-104A-4ACA-9EA4-524D52996E57}'; pid: ULong (82));
    DEVPKEY_DeviceContainer_HasProblem : TDevPropKey = (fmtid: '{78C34FC8-104A-4ACA-9EA4-524D52996E57}'; pid: ULong (83));
    DEVPKEY_DeviceContainer_IsSharedDevice : TDevPropKey = (fmtid: '{78C34FC8-104A-4ACA-9EA4-524D52996E57}'; pid: ULong (84));
    DEVPKEY_DeviceContainer_IsNetworkDevice : TDevPropKey = (fmtid: '{78C34FC8-104A-4ACA-9EA4-524D52996E57}'; pid: ULong (85));
    DEVPKEY_DeviceContainer_IsDefaultDevice : TDevPropKey = (fmtid: '{78C34FC8-104A-4ACA-9EA4-524D52996E57}'; pid: ULong (86));
    DEVPKEY_DeviceContainer_MetadataCabinet : TDevPropKey = (fmtid: '{78C34FC8-104A-4ACA-9EA4-524D52996E57}'; pid: ULong (87));
    DEVPKEY_DeviceContainer_RequiresPairingElevation : TDevPropKey = (fmtid: '{78C34FC8-104A-4ACA-9EA4-524D52996E57}'; pid: ULong (88));
    DEVPKEY_DeviceContainer_ExperienceId : TDevPropKey = (fmtid: '{78C34FC8-104A-4ACA-9EA4-524D52996E57}'; pid: ULong (89));
    DEVPKEY_DeviceContainer_Category : TDevPropKey = (fmtid: '{78C34FC8-104A-4ACA-9EA4-524D52996E57}'; pid: ULong (90));
    DEVPKEY_DeviceContainer_Category_Desc_Singular : TDevPropKey = (fmtid: '{78C34FC8-104A-4ACA-9EA4-524D52996E57}'; pid: ULong (91));
    DEVPKEY_DeviceContainer_Category_Desc_Plural : TDevPropKey = (fmtid: '{78C34FC8-104A-4ACA-9EA4-524D52996E57}'; pid: ULong (92));
    DEVPKEY_DeviceContainer_Category_Icon : TDevPropKey = (fmtid: '{78C34FC8-104A-4ACA-9EA4-524D52996E57}'; pid: ULong (93));
    DEVPKEY_DeviceContainer_CategoryGroup_Desc : TDevPropKey = (fmtid: '{78C34FC8-104A-4ACA-9EA4-524D52996E57}'; pid: ULong (94));
    DEVPKEY_DeviceContainer_CategoryGroup_Icon : TDevPropKey = (fmtid: '{78C34FC8-104A-4ACA-9EA4-524D52996E57}'; pid: ULong (95));
    DEVPKEY_DeviceContainer_PrimaryCategory : TDevPropKey = (fmtid: '{78C34FC8-104A-4ACA-9EA4-524D52996E57}'; pid: ULong (97));
    DEVPKEY_DeviceContainer_UnpairUninstall : TDevPropKey = (fmtid: '{78C34FC8-104A-4ACA-9EA4-524D52996E57}'; pid: ULong (98));
    DEVPKEY_DeviceContainer_RequiresUninstallElevation : TDevPropKey = (fmtid: '{78C34FC8-104A-4ACA-9EA4-524D52996E57}'; pid: ULong (99));
    DEVPKEY_DeviceContainer_DeviceFunctionSubRank : TDevPropKey = (fmtid: '{78C34FC8-104A-4ACA-9EA4-524D52996E57}'; pid: ULong (100));
    DEVPKEY_DeviceContainer_AlwaysShowDeviceAsConnected : TDevPropKey = (fmtid: '{78C34FC8-104A-4ACA-9EA4-524D52996E57}'; pid: ULong (101));
    DEVPKEY_DeviceContainer_ConfigFlags : TDevPropKey = (fmtid: '{78C34FC8-104A-4ACA-9EA4-524D52996E57}'; pid: ULong (105));
    DEVPKEY_DeviceContainer_PrivilegedPackageFamilyNames : TDevPropKey = (fmtid: '{78C34FC8-104A-4ACA-9EA4-524D52996E57}'; pid: ULong (106));
    DEVPKEY_DeviceContainer_CustomPrivilegedPackageFamilyNames : TDevPropKey = (fmtid: '{78C34FC8-104A-4ACA-9EA4-524D52996E57}'; pid: ULong (107));
    DEVPKEY_DeviceContainer_IsRebootRequired : TDevPropKey = (fmtid: '{78C34FC8-104A-4ACA-9EA4-524D52996E57}'; pid: ULong (108));
    DEVPKEY_DeviceContainer_FriendlyName : TDevPropKey = (fmtid: '{656A3BB3-ECC0-43FD-8477-4AE0404A96CD}'; pid: ULong (12288));
    DEVPKEY_DeviceContainer_Manufacturer : TDevPropKey = (fmtid: '{656A3BB3-ECC0-43FD-8477-4AE0404A96CD}'; pid: ULong (8192));
    DEVPKEY_DeviceContainer_ModelName : TDevPropKey = (fmtid: '{656A3BB3-ECC0-43FD-8477-4AE0404A96CD}'; pid: ULong (8194));
    DEVPKEY_DeviceContainer_ModelNumber : TDevPropKey = (fmtid: '{656A3BB3-ECC0-43FD-8477-4AE0404A96CD}'; pid: ULong (8195));
    DEVPKEY_DeviceContainer_InstallInProgress : TDevPropKey = (fmtid: '{83DA6326-97A6-4088-9453-A1923F573B29}'; pid: ULong (9));

    // devpropdef.h
    //
    // Property type modifiers.  Used to modify base DEVPROP_TYPE_ values, as
    // appropriate.  Not valid as standalone DEVPROPTYPE values.
    //
    DEVPROP_TYPEMOD_ARRAY                   = $00001000;  // array of fixed-sized data elements
    DEVPROP_TYPEMOD_LIST                    = $00002000;  // list of variable-sized data elements

    //
    // Property data types.
    //
    DEVPROP_TYPE_EMPTY                      = $00000000;  // nothing, no property data
    DEVPROP_TYPE_NULL                       = $00000001;  // null property data
    DEVPROP_TYPE_SBYTE                      = $00000002;  // 8-bit signed int (SBYTE)
    DEVPROP_TYPE_BYTE                       = $00000003;  // 8-bit unsigned int (BYTE)
    DEVPROP_TYPE_INT16                      = $00000004;  // 16-bit signed int (SHORT)
    DEVPROP_TYPE_UINT16                     = $00000005;  // 16-bit unsigned int (USHORT)
    DEVPROP_TYPE_INT32                      = $00000006;  // 32-bit signed int (LONG)
    DEVPROP_TYPE_UINT32                     = $00000007;  // 32-bit unsigned int (ULONG)
    DEVPROP_TYPE_INT64                      = $00000008;  // 64-bit signed int (LONG64)
    DEVPROP_TYPE_UINT64                     = $00000009;  // 64-bit unsigned int (ULONG64)
    DEVPROP_TYPE_FLOAT                      = $0000000A;  // 32-bit floating-point (FLOAT)
    DEVPROP_TYPE_DOUBLE                     = $0000000B;  // 64-bit floating-point (DOUBLE)
    DEVPROP_TYPE_DECIMAL                    = $0000000C;  // 128-bit data (DECIMAL)
    DEVPROP_TYPE_GUID                       = $0000000D;  // 128-bit unique identifier (GUID)
    DEVPROP_TYPE_CURRENCY                   = $0000000E;  // 64 bit signed int currency value (CURRENCY)
    DEVPROP_TYPE_DATE                       = $0000000F;  // date (DATE)
    DEVPROP_TYPE_FILETIME                   = $00000010;  // file time (FILETIME)
    DEVPROP_TYPE_BOOLEAN                    = $00000011;  // 8-bit boolean (DEVPROP_BOOLEAN)
    DEVPROP_TYPE_STRING                     = $00000012;  // null-terminated string
    DEVPROP_TYPE_STRING_LIST = (DEVPROP_TYPE_STRING or DEVPROP_TYPEMOD_LIST);  // multi-sz string list
    DEVPROP_TYPE_SECURITY_DESCRIPTOR        = $00000013;  // self-relative binary SECURITY_DESCRIPTOR
    DEVPROP_TYPE_SECURITY_DESCRIPTOR_STRING = $00000014;  // security descriptor string (SDDL format)
    DEVPROP_TYPE_DEVPROPKEY                 = $00000015;  // device property key (DEVPROPKEY)
    DEVPROP_TYPE_DEVPROPTYPE                = $00000016;  // device property type (DEVPROPTYPE)
    DEVPROP_TYPE_BINARY      = (DEVPROP_TYPE_BYTE or DEVPROP_TYPEMOD_ARRAY);  // custom binary data
    DEVPROP_TYPE_ERROR                      = $00000017;  // 32-bit Win32 system error code
    DEVPROP_TYPE_NTSTATUS                   = $00000018;  // 32-bit NTSTATUS code
    DEVPROP_TYPE_STRING_INDIRECT            = $00000019;  // string resource (@[path\]<dllname>,-<strId>)

type
//
// Define type for reference to device information set
//
    PTDevPropType = ^TDevPropType;
    TDevPropType = ULong;

    TDEVINST = DWord;

const
// Values specifying the scope of a device property change
    DICS_FLAG_GLOBAL         = $00000001;  // make change in all hardware profiles
    DICS_FLAG_CONFIGSPECIFIC = $00000002;  // make change in specified profile only
    DICS_FLAG_CONFIGGENERAL  = $00000004;  // 1 or more hardware profile-specific

    DIREG_DEV  = $00000001; // Open/Create/Delete device key
    DIREG_DRV  = $00000002; // Open/Create/Delete driver key
    DIREG_BOTH = $00000004; // Delete both driver and Device key

//
// Device registry property codes
// (Codes marked as read-only (R) may only be used for
// SetupDiGetDeviceRegistryProperty)
//
// These values should cover the same set of registry properties
// as defined by the CM_DRP codes in cfgmgr32.h.
//
// Note that SPDRP codes are zero based while CM_DRP codes are one based!
//
    SPDRP_DEVICEDESC                  = $00000000; // DeviceDesc (R/W)
    SPDRP_HARDWAREID                  = $00000001; // HardwareID (R/W)
    SPDRP_COMPATIBLEIDS               = $00000002; // CompatibleIDs (R/W)
    SPDRP_UNUSED0                     = $00000003; // unused
    SPDRP_SERVICE                     = $00000004; // Service (R/W)
    SPDRP_UNUSED1                     = $00000005; // unused
    SPDRP_UNUSED2                     = $00000006; // unused
    SPDRP_CLASS                       = $00000007; // Class (R--tied to ClassGUID)
    SPDRP_CLASSGUID                   = $00000008; // ClassGUID (R/W)
    SPDRP_DRIVER                      = $00000009; // Driver (R/W)
    SPDRP_CONFIGFLAGS                 = $0000000A; // ConfigFlags (R/W)
    SPDRP_MFG                         = $0000000B; // Mfg (R/W)
    SPDRP_FRIENDLYNAME                = $0000000C; // FriendlyName (R/W)
    SPDRP_LOCATION_INFORMATION        = $0000000D; // LocationInformation (R/W)
    SPDRP_PHYSICAL_DEVICE_OBJECT_NAME = $0000000E; // PhysicalDeviceObjectName (R)
    SPDRP_CAPABILITIES                = $0000000F; // Capabilities (R)
    SPDRP_UI_NUMBER                   = $00000010; // UiNumber (R)
    SPDRP_UPPERFILTERS                = $00000011; // UpperFilters (R/W)
    SPDRP_LOWERFILTERS                = $00000012; // LowerFilters (R/W)
    SPDRP_BUSTYPEGUID                 = $00000013; // BusTypeGUID (R)
    SPDRP_LEGACYBUSTYPE               = $00000014; // LegacyBusType (R)
    SPDRP_BUSNUMBER                   = $00000015; // BusNumber (R)
    SPDRP_ENUMERATOR_NAME             = $00000016; // Enumerator Name (R)
    SPDRP_SECURITY                    = $00000017; // Security (R/W, binary form)
    SPDRP_SECURITY_SDS                = $00000018; // Security (W, SDS form)
    SPDRP_DEVTYPE                     = $00000019; // Device Type (R/W)
    SPDRP_EXCLUSIVE                   = $0000001A; // Device is exclusive-access (R/W)
    SPDRP_CHARACTERISTICS             = $0000001B; // Device Characteristics (R/W)
    SPDRP_ADDRESS                     = $0000001C; // Device Address (R)
{$IFDEF WINXP_UP}
    SPDRP_UI_NUMBER_DESC_FORMAT       = $0000001D; // UiNumberDescFormat (R/W)
    SPDRP_DEVICE_POWER_DATA           = $0000001E; // Device Power Data (R)
    SPDRP_REMOVAL_POLICY              = $0000001F; // Removal Policy (R)
    SPDRP_REMOVAL_POLICY_HW_DEFAULT   = $00000020; // Hardware Removal Policy (R)
    SPDRP_REMOVAL_POLICY_OVERRIDE     = $00000021; // Removal Policy Override (RW)
    SPDRP_INSTALL_STATE               = $00000022; // Device Install State (R)

    SPDRP_MAXIMUM_PROPERTY            = $00000023; // Upper bound on ordinals
{$ELSE}
    SPDRP_UI_NUMBER_DESC_FORMAT       = $0000001E; // UiNumberDescFormat (R/W)
    SPDRP_MAXIMUM_PROPERTY            = $0000001F; // Upper bound on ordinals
{$ENDIF WINXP_UP}

//
// Flags controlling what is included in the device information set built
// by SetupDiGetClassDevs
//
    DIGCF_DEFAULT         = $00000001; // only valid with DIGCF_DEVICEINTERFACE
    DIGCF_PRESENT         = $00000002;
    DIGCF_ALLCLASSES      = $00000004;
    DIGCF_PROFILE         = $00000008;
    DIGCF_DEVICEINTERFACE = $00000010;

//
// Flags for SetupDiGetClassPropertyKeys, SetupDiGetClassProperty, and
// SetupDiSetClassProperty.
//
    DICLASSPROP_INSTALLER = $00000001; // device setup class property
    DICLASSPROP_INTERFACE = $00000002; // device interface class property

type
{$IFNDEF FPC}
    {$IFNDEF DELPHI2006_UP}
	    ULONG_PTR = Cardinal;
    {$ENDIF}
{$ENDIF}

	// Define type for reference to device information set
	HDEVINFO = THandle;
    THDevInfo = HDEVINFO;

//
// Structure containing class image list information.
//
    PSPClassImageListData = ^TSPClassImageListData;
    SP_CLASSIMAGELIST_DATA = record
      cbSize: DWORD;
      ImageList: HIMAGELIST;
      Reserved: {$IFDEF CPUX64} DWORD_PTR {$ELSE} DWord {$ENDIF};
    end;
    TSPClassImageListData = SP_CLASSIMAGELIST_DATA;

	// Device information structure (references a device instance
	// that is a member of a device information set)
    PSPDevInfoData = ^SP_DEVINFO_DATA;
    SP_DEVINFO_DATA = record
        cbSize : DWORD;
        ClassGuid : TGUID;
        DevInst : DWord; // DEVINST handle
        Reserved : ULONG_PTR;
    end;
    TSPDevInfoData = SP_DEVINFO_DATA;
    PTSPDevInfoData = ^TSPDevInfoData;

var
    SetupDiDestroyDeviceInfoList : function (DeviceInfoSet: HDEVINFO) : BOOL;
    															  stdcall = NIL;

    SetupDiDestroyClassImageList : function (
    		 		  var ClassImageListData: TSPClassImageListData) : LongBool;
             													  stdcall = NIL;


	SetupDiEnumDeviceInfo : function (DeviceInfoSet: HDEVINFO;
    								  MemberIndex: DWORD;
                                     var DeviceInfoData: TSPDevInfoData) : BOOL;
                                       							  stdcall = NIL;

	SetupDiGetClassDescription : function (var ClassGuid: TGUID;
    									   ClassDescription: LPTSTR;
                                           ClassDescriptionSize: DWORD;
                                           var RequiredSize: DWORD) : BOOL;
                                            					  stdcall = NIL;

	SetupDiGetClassDevs : function (ClassGuid: PGUID; const Enumerator: LPTSTR;
  									hwndParent: HWND;
                                    Flags: DWORD) : HDEVINFO; stdcall = NIL;

	SetupDiGetClassDevsEx : function (ClassGuid: PGUID; const Enumerator: LPTSTR;
    								  hwndParent: HWND; Flags: DWORD;
                                      DeviceInfoSet: HDEVINFO;
                                      const MachineName: LPTSTR;
                                      Reserved: Pointer) : HDEVINFO;
                                      							  stdcall = NIL;


    SetupDiGetClassPropertyW : function (ClassGuid: PGUID;
                                         pPropertyKey: PTDevPropKey;
                                         pPropertyType: pTDevPropType;
                                         pPropertyBuffer: PBYTE;
                                         dwPropertyBufferSize: DWORD;
                                         pdwRequiredSize: PDWORD;
                                         dwFlags: DWORD) : BOOL; stdcall = NIL;

    SetupDiGetClassImageIndex : function (
    							     pClassImageListData: PSPClassImageListData;
                                     pClassGuid: PGUID;
                                     var ImageIndex: Integer) : LongBool;
                                  								  stdcall = NIL;

    SetupDiGetClassImageList : function (
    			          var ClassImageListData: TSPClassImageListData) : BOOL;
                  												  stdcall = NIL;


    SetupDiGetDevicePropertyW : function (hDeviceInfoSet: THDEVINFO;
                                          pDeviceInfoData: PTSPDevInfoData;
                                          pPropertyKey: PTDevPropKey;
                                          pPropertyType: pTDevPropType;
                                          pPropertyBuffer: PBYTE;
                                          dwPropertyBufferSize: DWORD;
                                          pdwRequiredSize: PDWORD;
                                          dwFlags: DWORD) : BOOL; stdcall = NIL;

    SetupDiGetDevicePropertyKeys : function (DeviceInfoSet: THDEVINFO;
                                             var DeviceInfoData: TSPDevInfoData;
                                             pPropertyKeyArray: PTaDevPropKey;
                                             PropertyKeyCount: DWord;
                                             pdwRequiredPropertyKeyCount: PDWord;
                                             dwFlags: DWord) : BOOL; stdcall = NIL;

    SetupDiGetDeviceRegistryProperty : function (DeviceInfoSet: HDEVINFO;
    									   const DeviceInfoData: TSPDevInfoData;
                                           Property_: DWORD;
                                           var PropertyRegDataType: DWORD;
                                           PropertyBuffer: PBYTE;
                                           PropertyBufferSize: DWORD;
                                           var RequiredSize: DWORD) : BOOL;
                                           						  stdcall = NIL;

    SetupDiGetDeviceRegistryPropertyW : function (DeviceInfoSet: HDEVINFO;
    									   const DeviceInfoData: TSPDevInfoData;
                                           Property_: DWORD;
                                           var PropertyRegDataType: DWORD;
                                           PropertyBuffer: PBYTE;
                                           PropertyBufferSize: DWORD;
                                           var RequiredSize: DWORD) : BOOL;
                                           						  stdcall = NIL;

    SetupDiOpenDevRegKey : function (DeviceInfoSet: HDEVINFO;
    								 var DeviceInfoData: TSPDevInfoData;
                                     Scope, HwProfile, KeyType: DWORD;
                                     samDesired: REGSAM) : HKEY; stdcall = NIL;

implementation

uses SysUtils,
	 VerifyU;

const
	cSetupApi = 'setupapi.dll';

{$IFDEF UNICODE}
    AWSuffix = 'W';
{$ELSE}
    AWSuffix = 'A';
{$ENDIF UNICODE}

var
    hSetupApi : THandle = 0;

(* ---- *)

{$I GetProcAddress.inc}

(* ---- *)

initialization
begin
    hSetupApi := LoadLibrary (cSetupApi);

    if (hSetupApi = 0) then
    	exit;

    SetupDiDestroyDeviceInfoList := GPA (hSetupApi,
    									 'SetupDiDestroyDeviceInfoList');
    SetupDiDestroyClassImageList := GPA (hSetupApi,
    									 'SetupDiDestroyClassImageList');
    SetupDiEnumDeviceInfo := GPA (hSetupApi, 'SetupDiEnumDeviceInfo');
    SetupDiGetClassDescription := GPA (hSetupApi,
    								   'SetupDiGetClassDescription' + AWSuffix);
    SetupDiGetClassDevs := GPA (hSetupApi, 'SetupDiGetClassDevs' + AWSuffix);
    SetupDiGetClassImageIndex := GPA (hSetupApi, 'SetupDiGetClassImageIndex');
    SetupDiGetClassImageList := GPA (hSetupApi, 'SetupDiGetClassImageList');
    SetupDiGetDeviceRegistryProperty := GPA (hSetupApi,
    										 'SetupDiGetDeviceRegistryProperty' +
                                             AWSuffix);
    SetupDiOpenDevRegKey := GPA (hSetupApi, 'SetupDiOpenDevRegKey');

    if (Win32Platform = VER_PLATFORM_WIN32_NT) and (Win32MajorVersion >= 5) then
    begin
		SetupDiGetClassDevsEx := GPA (hSetupApi,
        							  'SetupDiGetClassDevsEx' + AWSuffix);
        SetupDiGetDeviceRegistryPropertyW := GPA (hSetupApi,
    									   'SetupDiGetDeviceRegistryPropertyW');
    end; { if }

    if (Win32Platform = VER_PLATFORM_WIN32_NT) and (Win32MajorVersion >= 6) then
    begin
        SetupDiGetClassPropertyW := GPA (hSetupApi, 'SetupDiGetClassPropertyW');
		SetupDiGetDevicePropertyKeys := GPA (hSetupApi,
                                             'SetupDiGetDevicePropertyKeys');
        SetupDiGetDevicePropertyW := GPA (hSetupApi, 'SetupDiGetDevicePropertyW')
    end; { if }
end; { initialization }

(* ---- *)

finalization
begin
    if (hSetupApi <> 0) then
    	VerifyApi (FreeLibrary (hSetupApi));
end; { finalization }

(* ---- *)

end.

