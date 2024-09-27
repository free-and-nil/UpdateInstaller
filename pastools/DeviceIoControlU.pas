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
    Declarations copied from JwaWinIoctl.pas
    http://jedi-apilib.sourceforge.net
**)

unit DeviceIoControlU;

interface

uses Windows;

const
    // Max number of drives assuming primary/secondary, master/slave topology
	MAX_IDE_DRIVES = 16;

	IOCTL_STORAGE_QUERY_PROPERTY = $2D1400;

type
{$IFNDEF FPC}
  {$IFNDEF DELPHI_XE2_UP}
    {$IFDEF DELPHI2009_UP}
    DWord64 = UInt64;
    {$ELSE}
    DWord64 = Int64;
    {$ENDIF}  // DELPHI2009_UP
  {$ENDIF}  // DELPHI_XE2_UP
{$ENDIF}  // FPC

    STORAGE_PROPERTY_QUERY = packed record
      PropertyId: DWord;
      QueryType: DWord;
      AdditionalParameters: array[0..3] of Byte;
    end;

    STORAGE_DEVICE_DESCRIPTOR = packed record
      Version: ULONG;
      Size: ULONG;
      DeviceType: Byte;
      DeviceTypeModifier: Byte;
      RemovableMedia: Boolean;
      CommandQueueing: Boolean;
      VendorIdOffset: ULONG;
      ProductIdOffset: ULONG;
      ProductRevisionOffset: ULONG;
      SerialNumberOffset: ULONG;
      STORAGE_BUS_TYPE: DWord;
      RawPropertiesLength: ULONG;
      RawDeviceProperties: array[0..511] of Byte;
    end;

//
// Device interface class GUIDs.
//
// need these GUIDs outside conditional includes so that user can
//   #include <winioctl.h> in precompiled header
//   #include <initguid.h> in a single source file
//   #include <winioctl.h> in that source file a second time to instantiate the GUIDs
//

const
  PARTITION_BASIC_DATA_GUID: TGUID = (
    D1:$EBD0A0A2; D2:$B9E5; D3:$4433; D4:($87, $C0, $68, $B6, $B7, $26, $99, $C7));

  GUID_DEVINTERFACE_DISK: TGUID = (
    D1:$53f56307; D2:$b6bf; D3:$11d0; D4:($94, $f2, $00, $a0, $c9, $1e, $fb, $8b));
  GUID_DEVINTERFACE_CDROM: TGUID = (
    D1:$53f56308; D2:$b6bf; D3:$11d0; D4:($94, $f2, $00, $a0, $c9, $1e, $fb, $8b));
  GUID_DEVINTERFACE_PARTITION: TGUID = (
    D1:$53f5630a; D2:$b6bf; D3:$11d0; D4:($94, $f2, $00, $a0, $c9, $1e, $fb, $8b));
  GUID_DEVINTERFACE_TAPE: TGUID = (
    D1:$53f5630b; D2:$b6bf; D3:$11d0; D4:($94, $f2, $00, $a0, $c9, $1e, $fb, $8b));
  GUID_DEVINTERFACE_WRITEONCEDISK: TGUID = (
    D1:$53f5630c; D2:$b6bf; D3:$11d0; D4:($94, $f2, $00, $a0, $c9, $1e, $fb, $8b));
  GUID_DEVINTERFACE_VOLUME: TGUID = (
    D1:$53f5630d; D2:$b6bf; D3:$11d0; D4:($94, $f2, $00, $a0, $c9, $1e, $fb, $8b));
  GUID_DEVINTERFACE_MEDIUMCHANGER: TGUID = (
    D1:$53f56310; D2:$b6bf; D3:$11d0; D4:($94, $f2, $00, $a0, $c9, $1e, $fb, $8b));
  GUID_DEVINTERFACE_FLOPPY: TGUID = (
    D1:$53f56311; D2:$b6bf; D3:$11d0; D4:($94, $f2, $00, $a0, $c9, $1e, $fb, $8b));
  GUID_DEVINTERFACE_CDCHANGER: TGUID = (
    D1:$53f56312; D2:$b6bf; D3:$11d0; D4:($94, $f2, $00, $a0, $c9, $1e, $fb, $8b));
  GUID_DEVINTERFACE_STORAGEPORT: TGUID = (
    D1:$2accfe60; D2:$c130; D3:$11d2; D4:($b0, $82, $00, $a0, $c9, $1e, $fb, $8b));
  GUID_DEVINTERFACE_COMPORT: TGUID = (
    D1:$86e0d1e0; D2:$8089; D3:$11d0; D4:($9c, $e4, $08, $00, $3e, $30, $1f, $73));
  GUID_DEVINTERFACE_SERENUM_BUS_ENUMERATOR: TGUID = (
    D1:$4D36E978; D2:$E325; D3:$11CE; D4:($BF, $C1, $08, $00, $2B, $E1, $03, $18));

//
// Obsolete device interface class GUID names.
// (use of above GUID_DEVINTERFACE_* names is recommended).
//

  // MVB: Note that these "constants" are in reality aliases for the list above. Unfortunately you can't
  // define a GUID without using a type constant and you can't alias a type constant in Delphi...

  DiskClassGuid: TGUID = (
    D1:$53f56307; D2:$b6bf; D3:$11d0; D4:($94, $f2, $00, $a0, $c9, $1e, $fb, $8b));
  CdRomClassGuid: TGUID = (
    D1:$53f56308; D2:$b6bf; D3:$11d0; D4:($94, $f2, $00, $a0, $c9, $1e, $fb, $8b));
  PartitionClassGuid: TGUID = (
    D1:$53f5630a; D2:$b6bf; D3:$11d0; D4:($94, $f2, $00, $a0, $c9, $1e, $fb, $8b));
  TapeClassGuid: TGUID = (
    D1:$53f5630b; D2:$b6bf; D3:$11d0; D4:($94, $f2, $00, $a0, $c9, $1e, $fb, $8b));
  WriteOnceDiskClassGuid: TGUID = (
    D1:$53f5630c; D2:$b6bf; D3:$11d0; D4:($94, $f2, $00, $a0, $c9, $1e, $fb, $8b));
  VolumeClassGuid: TGUID = (
    D1:$53f5630d; D2:$b6bf; D3:$11d0; D4:($94, $f2, $00, $a0, $c9, $1e, $fb, $8b));
  MediumChangerClassGuid: TGUID = (
    D1:$53f56310; D2:$b6bf; D3:$11d0; D4:($94, $f2, $00, $a0, $c9, $1e, $fb, $8b));
  FloppyClassGuid: TGUID = (
    D1:$53f56311; D2:$b6bf; D3:$11d0; D4:($94, $f2, $00, $a0, $c9, $1e, $fb, $8b));
  CdChangerClassGuid: TGUID = (
    D1:$53f56312; D2:$b6bf; D3:$11d0; D4:($94, $f2, $00, $a0, $c9, $1e, $fb, $8b));
  StoragePortClassGuid: TGUID = (
    D1:$2accfe60; D2:$c130; D3:$11d2; D4:($b0, $82, $00, $a0, $c9, $1e, $fb, $8b));
  GUID_CLASS_COMPORT: TGUID = (
    D1:$86e0d1e0; D2:$8089; D3:$11d0; D4:($9c, $e4, $08, $00, $3e, $30, $1f, $73));
  GUID_SERENUM_BUS_ENUMERATOR: TGUID = (
    D1:$4D36E978; D2:$E325; D3:$11CE; D4:($BF, $C1, $08, $00, $2B, $E1, $03, $18));

//
// Define the various device type values.  Note that values used by Microsoft
// Corporation are in the range 0-32767, and 32768-65535 are reserved for use
// by customers.
//

type
  DEVICE_TYPE = DWord;

const
  FILE_DEVICE_BEEP                = $00000001;
  FILE_DEVICE_CD_ROM              = $00000002;
  FILE_DEVICE_CD_ROM_FILE_SYSTEM  = $00000003;
  FILE_DEVICE_CONTROLLER          = $00000004;
  FILE_DEVICE_DATALINK            = $00000005;
  FILE_DEVICE_DFS                 = $00000006;
  FILE_DEVICE_DISK                = $00000007;
  FILE_DEVICE_DISK_FILE_SYSTEM    = $00000008;
  FILE_DEVICE_FILE_SYSTEM         = $00000009;
  FILE_DEVICE_INPORT_PORT         = $0000000a;
  FILE_DEVICE_KEYBOARD            = $0000000b;
  FILE_DEVICE_MAILSLOT            = $0000000c;
  FILE_DEVICE_MIDI_IN             = $0000000d;
  FILE_DEVICE_MIDI_OUT            = $0000000e;
  FILE_DEVICE_MOUSE               = $0000000f;
  FILE_DEVICE_MULTI_UNC_PROVIDER  = $00000010;
  FILE_DEVICE_NAMED_PIPE          = $00000011;
  FILE_DEVICE_NETWORK             = $00000012;
  FILE_DEVICE_NETWORK_BROWSER     = $00000013;
  FILE_DEVICE_NETWORK_FILE_SYSTEM = $00000014;
  FILE_DEVICE_NULL                = $00000015;
  FILE_DEVICE_PARALLEL_PORT       = $00000016;
  FILE_DEVICE_PHYSICAL_NETCARD    = $00000017;
  FILE_DEVICE_PRINTER             = $00000018;
  FILE_DEVICE_SCANNER             = $00000019;
  FILE_DEVICE_SERIAL_MOUSE_PORT   = $0000001a;
  FILE_DEVICE_SERIAL_PORT         = $0000001b;
  FILE_DEVICE_SCREEN              = $0000001c;
  FILE_DEVICE_SOUND               = $0000001d;
  FILE_DEVICE_STREAMS             = $0000001e;
  FILE_DEVICE_TAPE                = $0000001f;
  FILE_DEVICE_TAPE_FILE_SYSTEM    = $00000020;
  FILE_DEVICE_TRANSPORT           = $00000021;
  FILE_DEVICE_UNKNOWN             = $00000022;
  FILE_DEVICE_VIDEO               = $00000023;
  FILE_DEVICE_VIRTUAL_DISK        = $00000024;
  FILE_DEVICE_WAVE_IN             = $00000025;
  FILE_DEVICE_WAVE_OUT            = $00000026;
  FILE_DEVICE_8042_PORT           = $00000027;
  FILE_DEVICE_NETWORK_REDIRECTOR  = $00000028;
  FILE_DEVICE_BATTERY             = $00000029;
  FILE_DEVICE_BUS_EXTENDER        = $0000002a;
  FILE_DEVICE_MODEM               = $0000002b;
  FILE_DEVICE_VDM                 = $0000002c;
  FILE_DEVICE_MASS_STORAGE        = $0000002d;
  FILE_DEVICE_SMB                 = $0000002e;
  FILE_DEVICE_KS                  = $0000002f;
  FILE_DEVICE_CHANGER             = $00000030;
  FILE_DEVICE_SMARTCARD           = $00000031;
  FILE_DEVICE_ACPI                = $00000032;
  FILE_DEVICE_DVD                 = $00000033;
  FILE_DEVICE_FULLSCREEN_VIDEO    = $00000034;
  FILE_DEVICE_DFS_FILE_SYSTEM     = $00000035;
  FILE_DEVICE_DFS_VOLUME          = $00000036;
  FILE_DEVICE_SERENUM             = $00000037;
  FILE_DEVICE_TERMSRV             = $00000038;
  FILE_DEVICE_KSEC                = $00000039;
  FILE_DEVICE_FIPS                = $0000003A;
  FILE_DEVICE_INFINIBAND          = $0000003B;

//
// Macro definition for defining IOCTL and FSCTL function control codes.  Note
// that function codes 0-2047 are reserved for Microsoft Corporation, and
// 2048-4095 are reserved for customers.
//

// function CTL_CODE(DeviceType, Func, Method, Access: WORD): DWord;

//
// Macro to extract device type out of the device io control code
//

// function DEVICE_TYPE_FROM_CTL_CODE(CtrlCode: DWord): WORD;

//
// Define the method codes for how buffers are passed for I/O and FS controls
//

const
  METHOD_BUFFERED   = 0;
  METHOD_IN_DIRECT  = 1;
  METHOD_OUT_DIRECT = 2;
  METHOD_NEITHER    = 3;

//
// Define some easier to comprehend aliases:
//   METHOD_DIRECT_TO_HARDWARE (writes, aka METHOD_IN_DIRECT)
//   METHOD_DIRECT_FROM_HARDWARE (reads, aka METHOD_OUT_DIRECT)
//

  METHOD_DIRECT_TO_HARDWARE     = METHOD_IN_DIRECT;
  METHOD_DIRECT_FROM_HARDWARE   = METHOD_OUT_DIRECT;

//
// Define the access check value for any access
//
//
// The FILE_READ_ACCESS and FILE_WRITE_ACCESS constants are also defined in
// ntioapi.h as FILE_READ_DATA and FILE_WRITE_DATA. The values for these
// constants *MUST* always be in sync.
//
//
// FILE_SPECIAL_ACCESS is checked by the NT I/O system the same as FILE_ANY_ACCESS.
// The file systems, however, may add additional access checks for I/O and FS controls
// that use this value.
//

const
  FILE_ANY_ACCESS     = 0;
  FILE_SPECIAL_ACCESS = FILE_ANY_ACCESS;
  FILE_READ_ACCESS    = $0001;           // file & pipe
  FILE_WRITE_ACCESS   = $0002;           // file & pipe

//
// IoControlCode values for storage devices
//

  IOCTL_STORAGE_BASE = FILE_DEVICE_MASS_STORAGE;

//
// The following device control codes are common for all class drivers.  They
// should be used in place of the older IOCTL_DISK, IOCTL_CDROM and IOCTL_TAPE
// common codes
//

const
  IOCTL_STORAGE_CHECK_VERIFY = (
    (IOCTL_STORAGE_BASE shl 16) or (FILE_READ_ACCESS shl 14) or
    ($0200 shl 2) or METHOD_BUFFERED);
  IOCTL_STORAGE_CHECK_VERIFY2 = (
    (IOCTL_STORAGE_BASE shl 16) or (FILE_ANY_ACCESS shl 14) or
    ($0200 shl 2) or METHOD_BUFFERED);
  IOCTL_STORAGE_MEDIA_REMOVAL = (
    (IOCTL_STORAGE_BASE shl 16) or (FILE_READ_ACCESS shl 14) or
    ($0201 shl 2) or METHOD_BUFFERED);
  IOCTL_STORAGE_EJECT_MEDIA = (
    (IOCTL_STORAGE_BASE shl 16) or (FILE_READ_ACCESS shl 14) or
    ($0202 shl 2) or METHOD_BUFFERED);
  IOCTL_STORAGE_LOAD_MEDIA = (
    (IOCTL_STORAGE_BASE shl 16) or (FILE_READ_ACCESS shl 14) or
    ($0203 shl 2) or METHOD_BUFFERED);
  IOCTL_STORAGE_LOAD_MEDIA2 = (
    (IOCTL_STORAGE_BASE shl 16) or (FILE_ANY_ACCESS shl 14) or
    ($0203 shl 2) or METHOD_BUFFERED);
  IOCTL_STORAGE_RESERVE = (
    (IOCTL_STORAGE_BASE shl 16) or (FILE_READ_ACCESS shl 14) or
    ($0204 shl 2) or METHOD_BUFFERED);
  IOCTL_STORAGE_RELEASE = (
    (IOCTL_STORAGE_BASE shl 16) or (FILE_READ_ACCESS shl 14) or
    ($0205 shl 2) or METHOD_BUFFERED);
  IOCTL_STORAGE_FIND_NEW_DEVICES = (
    (IOCTL_STORAGE_BASE shl 16) or (FILE_READ_ACCESS shl 14) or
    ($0206 shl 2) or METHOD_BUFFERED);

  IOCTL_STORAGE_EJECTION_CONTROL = (
    (IOCTL_STORAGE_BASE shl 16) or (FILE_ANY_ACCESS shl 14) or
    ($0250 shl 2) or METHOD_BUFFERED);
  IOCTL_STORAGE_MCN_CONTROL = (
    (IOCTL_STORAGE_BASE shl 16) or (FILE_ANY_ACCESS shl 14) or
    ($0251 shl 2) or METHOD_BUFFERED);

  IOCTL_STORAGE_GET_MEDIA_TYPES = (
    (IOCTL_STORAGE_BASE shl 16) or (FILE_ANY_ACCESS shl 14) or
    ($0300 shl 2) or METHOD_BUFFERED);
  IOCTL_STORAGE_GET_MEDIA_TYPES_EX = (
    (IOCTL_STORAGE_BASE shl 16) or (FILE_ANY_ACCESS shl 14) or
    ($0301 shl 2) or METHOD_BUFFERED);
  IOCTL_STORAGE_GET_MEDIA_SERIAL_NUMBER = (
    (IOCTL_STORAGE_BASE shl 16) or (FILE_ANY_ACCESS shl 14) or
    ($0304 shl 2) or METHOD_BUFFERED);
  IOCTL_STORAGE_GET_HOTPLUG_INFO = (
    (IOCTL_STORAGE_BASE shl 16) or (FILE_ANY_ACCESS shl 14) or
    ($0305 shl 2) or METHOD_BUFFERED);
  IOCTL_STORAGE_SET_HOTPLUG_INFO = (
    (IOCTL_STORAGE_BASE shl 16) or ((FILE_READ_ACCESS or FILE_WRITE_ACCESS) shl 14) or
    ($0306 shl 2) or METHOD_BUFFERED);

  IOCTL_STORAGE_RESET_BUS = (
    (IOCTL_STORAGE_BASE shl 16) or (FILE_READ_ACCESS shl 14) or
    ($0400 shl 2) or METHOD_BUFFERED);
  IOCTL_STORAGE_RESET_DEVICE = (
    (IOCTL_STORAGE_BASE shl 16) or (FILE_READ_ACCESS shl 14) or
    ($0401 shl 2) or METHOD_BUFFERED);
  IOCTL_STORAGE_BREAK_RESERVATION = (
    (IOCTL_STORAGE_BASE shl 16) or (FILE_READ_ACCESS shl 14) or
    ($0405 shl 2) or METHOD_BUFFERED);

  IOCTL_STORAGE_GET_DEVICE_NUMBER = (
    (IOCTL_STORAGE_BASE shl 16) or (FILE_ANY_ACCESS shl 14) or
    ($0420 shl 2) or METHOD_BUFFERED);

  IOCTL_STORAGE_PREDICT_FAILURE = (
    (IOCTL_STORAGE_BASE shl 16) or (FILE_ANY_ACCESS shl 14) or
    ($0440 shl 2) or METHOD_BUFFERED);

//
// These ioctl codes are obsolete.  They are defined here to avoid resuing them
// and to allow class drivers to respond to them more easily.
//

  OBSOLETE_IOCTL_STORAGE_RESET_BUS = (
    (IOCTL_STORAGE_BASE shl 16) or ((FILE_READ_ACCESS or FILE_WRITE_ACCESS) shl 14) or
    ($0400 shl 2) or METHOD_BUFFERED);

  OBSOLETE_IOCTL_STORAGE_RESET_DEVICE = (
    (IOCTL_STORAGE_BASE shl 16) or ((FILE_READ_ACCESS or FILE_WRITE_ACCESS) shl 14) or
    ($0401 shl 2) or METHOD_BUFFERED);

//
// IOCTL_STORAGE_GET_HOTPLUG_INFO
//

type
  PSTORAGE_HOTPLUG_INFO = ^STORAGE_HOTPLUG_INFO;
  _STORAGE_HOTPLUG_INFO = record
    Size: DWord; // version
    MediaRemovable: BOOLEAN; // ie. zip, jaz, cdrom, mo, etc. vs hdd
    MediaHotplug: BOOLEAN;   // ie. does the device succeed a lock even though its not lockable media?
    DeviceHotplug: BOOLEAN;  // ie. 1394, USB, etc.
    WriteCacheEnableOverride: BOOLEAN; // This field should not be relied upon because it is no longer used
  end;
  STORAGE_HOTPLUG_INFO = _STORAGE_HOTPLUG_INFO;
  TStorageHotplugInfo = STORAGE_HOTPLUG_INFO;
  PStorageHotplugInfo = PSTORAGE_HOTPLUG_INFO;

//
// IOCTL_STORAGE_GET_DEVICE_NUMBER
//
// input - none
//
// output - STORAGE_DEVICE_NUMBER structure
//          The values in the STORAGE_DEVICE_NUMBER structure are guaranteed
//          to remain unchanged until the system is rebooted.  They are not
//          guaranteed to be persistant across boots.
//

type
  PSTORAGE_DEVICE_NUMBER = ^STORAGE_DEVICE_NUMBER;
  _STORAGE_DEVICE_NUMBER = record
    //
    // The FILE_DEVICE_XXX type for this device.
    //
    DeviceType: DEVICE_TYPE;
    //
    // The number of this device
    //
    DeviceNumber: DWord;
    //
    // If the device is partitionable, the partition number of the device.
    // Otherwise -1
    //
    PartitionNumber: DWord;
  end;
  STORAGE_DEVICE_NUMBER = _STORAGE_DEVICE_NUMBER;
  TStorageDeviceNumber = STORAGE_DEVICE_NUMBER;
  PStorageDeviceNumber = PSTORAGE_DEVICE_NUMBER;

//
// Define the structures for scsi resets
//

  PSTORAGE_BUS_RESET_REQUEST = ^STORAGE_BUS_RESET_REQUEST;
  _STORAGE_BUS_RESET_REQUEST = record
    PathId: BYTE;
  end;
  STORAGE_BUS_RESET_REQUEST = _STORAGE_BUS_RESET_REQUEST;
  TStorageBusResetRequest = STORAGE_BUS_RESET_REQUEST;
  PStorageBusResetRequest = PSTORAGE_BUS_RESET_REQUEST;

//
// Break reservation is sent to the Adapter/FDO with the given lun information.
//

  STORAGE_BREAK_RESERVATION_REQUEST = record
    Length: DWord;
    _unused: Byte;
    PathId: Byte;
    TargetId: Byte;
    Lun: Byte;
  end;
  PSTORAGE_BREAK_RESERVATION_REQUEST = ^STORAGE_BREAK_RESERVATION_REQUEST;
  TStorageBreakReservationRequest = STORAGE_BREAK_RESERVATION_REQUEST;
  PStorageBreakReservationRequest = PSTORAGE_BREAK_RESERVATION_REQUEST;

//
// IOCTL_STORAGE_MEDIA_REMOVAL disables the mechanism
// on a storage device that ejects media. This function
// may or may not be supported on storage devices that
// support removable media.
//
// TRUE means prevent media from being removed.
// FALSE means allow media removal.
//

  PPREVENT_MEDIA_REMOVAL = ^PREVENT_MEDIA_REMOVAL;
  _PREVENT_MEDIA_REMOVAL = record
    PreventMediaRemoval: ByteBool;
  end;
  PREVENT_MEDIA_REMOVAL = _PREVENT_MEDIA_REMOVAL;
  TPreventMediaRemoval = PREVENT_MEDIA_REMOVAL;
  PPreventMediaRemoval = PPREVENT_MEDIA_REMOVAL;

//
//  This is the format of TARGET_DEVICE_CUSTOM_NOTIFICATION.CustomDataBuffer
//  passed to applications by the classpnp autorun code (via IoReportTargetDeviceChangeAsynchronous).
//

  _CLASS_MEDIA_CHANGE_CONTEXT = record
    MediaChangeCount: DWord;
    NewState: DWord;  // see MEDIA_CHANGE_DETECTION_STATE enum in classpnp.h in DDK
  end;
  CLASS_MEDIA_CHANGE_CONTEXT = _CLASS_MEDIA_CHANGE_CONTEXT;
  PCLASS_MEDIA_CHANGE_CONTEXT = ^CLASS_MEDIA_CHANGE_CONTEXT;
  TClassMediaChangeContext = CLASS_MEDIA_CHANGE_CONTEXT;
  PClassMediaChangeContext = PCLASS_MEDIA_CHANGE_CONTEXT;

  PTAPE_STATISTICS = ^TAPE_STATISTICS;
  _TAPE_STATISTICS = record
    Version: DWord;
    Flags: DWord;
    RecoveredWrites: LARGE_INTEGER;
    UnrecoveredWrites: LARGE_INTEGER;
    RecoveredReads: LARGE_INTEGER;
    UnrecoveredReads: LARGE_INTEGER;
    CompressionRatioReads: BYTE;
    CompressionRatioWrites: BYTE;
  end;
  TAPE_STATISTICS = _TAPE_STATISTICS;
  TTapeStatistics = TAPE_STATISTICS;
  PTapeStatistics = PTAPE_STATISTICS;

const
  RECOVERED_WRITES_VALID       = $00000001;
  UNRECOVERED_WRITES_VALID     = $00000002;
  RECOVERED_READS_VALID        = $00000004;
  UNRECOVERED_READS_VALID      = $00000008;
  WRITE_COMPRESSION_INFO_VALID = $00000010;
  READ_COMPRESSION_INFO_VALID  = $00000020;

type
  PTAPE_GET_STATISTICS = ^TAPE_GET_STATISTICS;
  _TAPE_GET_STATISTICS = record
    Operation: DWord;
  end;
  TAPE_GET_STATISTICS = _TAPE_GET_STATISTICS;
  TTapeGetStatistics = TAPE_GET_STATISTICS;
  PTapeGetStatistics = PTAPE_GET_STATISTICS;

const
  TAPE_RETURN_STATISTICS = 0;
  TAPE_RETURN_ENV_INFO   = 1;
  TAPE_RESET_STATISTICS  = 2;

//
// IOCTL_STORAGE_GET_MEDIA_TYPES_EX will return an array of DEVICE_MEDIA_INFO
// structures, one per supported type, embedded in the GET_MEDIA_TYPES struct.
//

const

  //
  // Following are defined in ntdddisk.h in the MEDIA_TYPE enum
  //
  // Unknown,                // Format is unknown
  // F5_1Pt2_512,            // 5.25", 1.2MB,  512 bytes/sector
  // F3_1Pt44_512,           // 3.5",  1.44MB, 512 bytes/sector
  // F3_2Pt88_512,           // 3.5",  2.88MB, 512 bytes/sector
  // F3_20Pt8_512,           // 3.5",  20.8MB, 512 bytes/sector
  // F3_720_512,             // 3.5",  720KB,  512 bytes/sector
  // F5_360_512,             // 5.25", 360KB,  512 bytes/sector
  // F5_320_512,             // 5.25", 320KB,  512 bytes/sector
  // F5_320_1024,            // 5.25", 320KB,  1024 bytes/sector
  // F5_180_512,             // 5.25", 180KB,  512 bytes/sector
  // F5_160_512,             // 5.25", 160KB,  512 bytes/sector
  // RemovableMedia,         // Removable media other than floppy
  // FixedMedia,             // Fixed hard disk media
  // F3_120M_512,            // 3.5", 120M Floppy
  // F3_640_512,             // 3.5" ,  640KB,  512 bytes/sector
  // F5_640_512,             // 5.25",  640KB,  512 bytes/sector
  // F5_720_512,             // 5.25",  720KB,  512 bytes/sector
  // F3_1Pt2_512,            // 3.5" ,  1.2Mb,  512 bytes/sector
  // F3_1Pt23_1024,          // 3.5" ,  1.23Mb, 1024 bytes/sector
  // F5_1Pt23_1024,          // 5.25",  1.23MB, 1024 bytes/sector
  // F3_128Mb_512,           // 3.5" MO 128Mb   512 bytes/sector
  // F3_230Mb_512,           // 3.5" MO 230Mb   512 bytes/sector
  // F8_256_128,             // 8",     256KB,  128 bytes/sector
  // F3_200Mb_512,           // 3.5",   200M Floppy (HiFD)
  //

  DDS_4mm            = $20;  // Tape - DAT DDS1,2,... (all vendors)
  MiniQic            = $21;  // Tape - miniQIC Tape
  Travan             = $22;  // Tape - Travan TR-1,2,3,...
  QIC                = $23;  // Tape - QIC
  MP_8mm             = $24;  // Tape - 8mm Exabyte Metal Particle
  AME_8mm            = $25;  // Tape - 8mm Exabyte Advanced Metal Evap
  AIT1_8mm           = $26;  // Tape - 8mm Sony AIT
  DLT                = $27;  // Tape - DLT Compact IIIxt, IV
  NCTP               = $28;  // Tape - Philips NCTP
  IBM_3480           = $29;  // Tape - IBM 3480
  IBM_3490E          = $2A;  // Tape - IBM 3490E
  IBM_Magstar_3590   = $2B;  // Tape - IBM Magstar 3590
  IBM_Magstar_MP     = $2C;  // Tape - IBM Magstar MP
  STK_DATA_D3        = $2D;  // Tape - STK Data D3
  SONY_DTF           = $2E;  // Tape - Sony DTF
  DV_6mm             = $2F;  // Tape - 6mm Digital Video
  DMI                = $30;  // Tape - Exabyte DMI and compatibles
  SONY_D2            = $31;  // Tape - Sony D2S and D2L
  CLEANER_CARTRIDGE  = $32;  // Cleaner - All Drive types that support Drive Cleaners
  CD_ROM             = $33;  // Opt_Disk - CD
  CD_R               = $34;  // Opt_Disk - CD-Recordable (Write Once)
  CD_RW              = $35;  // Opt_Disk - CD-Rewriteable
  DVD_ROM            = $36;  // Opt_Disk - DVD-ROM
  DVD_R              = $37;  // Opt_Disk - DVD-Recordable (Write Once)
  DVD_RW             = $38;  // Opt_Disk - DVD-Rewriteable
  MO_3_RW            = $39;  // Opt_Disk - 3.5" Rewriteable MO Disk
  MO_5_WO            = $3A;  // Opt_Disk - MO 5.25" Write Once
  MO_5_RW            = $3B;  // Opt_Disk - MO 5.25" Rewriteable (not LIMDOW)
  MO_5_LIMDOW        = $3C;  // Opt_Disk - MO 5.25" Rewriteable (LIMDOW)
  PC_5_WO            = $3D;  // Opt_Disk - Phase Change 5.25" Write Once Optical
  PC_5_RW            = $3E;  // Opt_Disk - Phase Change 5.25" Rewriteable
  PD_5_RW            = $3F;  // Opt_Disk - PhaseChange Dual Rewriteable
  ABL_5_WO           = $40;  // Opt_Disk - Ablative 5.25" Write Once Optical
  PINNACLE_APEX_5_RW = $41;  // Opt_Disk - Pinnacle Apex 4.6GB Rewriteable Optical
  SONY_12_WO         = $42;  // Opt_Disk - Sony 12" Write Once
  PHILIPS_12_WO      = $43;  // Opt_Disk - Philips/LMS 12" Write Once
  HITACHI_12_WO      = $44;  // Opt_Disk - Hitachi 12" Write Once
  CYGNET_12_WO       = $45;  // Opt_Disk - Cygnet/ATG 12" Write Once
  KODAK_14_WO        = $46;  // Opt_Disk - Kodak 14" Write Once
  MO_NFR_525         = $47;  // Opt_Disk - Near Field Recording (Terastor)
  NIKON_12_RW        = $48;  // Opt_Disk - Nikon 12" Rewriteable
  IOMEGA_ZIP         = $49;  // Mag_Disk - Iomega Zip
  IOMEGA_JAZ         = $4A;  // Mag_Disk - Iomega Jaz
  SYQUEST_EZ135      = $4B;  // Mag_Disk - Syquest EZ135
  SYQUEST_EZFLYER    = $4C;  // Mag_Disk - Syquest EzFlyer
  SYQUEST_SYJET      = $4D;  // Mag_Disk - Syquest SyJet
  AVATAR_F2          = $4E;  // Mag_Disk - 2.5" Floppy
  MP2_8mm            = $4F;  // Tape - 8mm Hitachi
  DST_S              = $50;  // Ampex DST Small Tapes
  DST_M              = $51;  // Ampex DST Medium Tapes
  DST_L              = $52;  // Ampex DST Large Tapes
  VXATape_1          = $53;  // Ecrix 8mm Tape
  VXATape_2          = $54;  // Ecrix 8mm Tape
  STK_9840           = $55;  // STK 9840
  LTO_Ultrium        = $56;  // IBM, HP, Seagate LTO Ultrium
  LTO_Accelis        = $57;  // IBM, HP, Seagate LTO Accelis
  DVD_RAM            = $58;  // Opt_Disk - DVD-RAM
  AIT_8mm            = $59;  // AIT2 or higher
  ADR_1              = $5A;  // OnStream ADR Mediatypes
  ADR_2              = $5B;
  STK_9940           = $5C;  // STK 9940

type
  STORAGE_MEDIA_TYPE = DWord;
  PSTORAGE_MEDIA_TYPE = ^STORAGE_MEDIA_TYPE;
  TStorageMediaType = STORAGE_MEDIA_TYPE;
  PStorageMediaType = ^TStorageMediaType;

const
  MEDIA_ERASEABLE  = $00000001;
  MEDIA_WRITE_ONCE = $00000002;
  MEDIA_READ_ONLY  = $00000004;
  MEDIA_READ_WRITE = $00000008;

  MEDIA_WRITE_PROTECTED   = $00000100;
  MEDIA_CURRENTLY_MOUNTED = DWord($80000000);

//
// Define the different storage bus types
// Bus types below 128 (0x80) are reserved for Microsoft use
//

const
  BusTypeUnknown     = 0;
  BusTypeScsi        = 1;
  BusTypeAtapi       = 2;
  BusTypeAta         = 3;
  BusType1394        = 4;
  BusTypeSsa         = 5;
  BusTypeFibre       = 6;
  BusTypeUsb         = 7;
  BusTypeRAID        = 8;
  BusTypeMaxReserved = $7F;

type
  STORAGE_BUS_TYPE = DWord;
  PSTORAGE_BUS_TYPE = ^STORAGE_BUS_TYPE;
  TStorageBusType = STORAGE_BUS_TYPE;
  PStorageBusType = PSTORAGE_BUS_TYPE;

  TDMIDiskInfo = record
    Cylinders: LARGE_INTEGER;
    MediaType: STORAGE_MEDIA_TYPE;
    TracksPerCylinder: DWord;
    SectorsPerTrack: DWord;
    BytesPerSector: DWord;
    NumberMediaSides: DWord;
    MediaCharacteristics: DWord; // Bitmask of MEDIA_XXX values.
  end;

  TDMIRemovableDiskInfo = record
    Cylinders: LARGE_INTEGER;
    MediaType: STORAGE_MEDIA_TYPE;
    TracksPerCylinder: DWord;
    SectorsPerTrack: DWord;
    BytesPerSector: DWord;
    NumberMediaSides: DWord;
    MediaCharacteristics: DWord; // Bitmask of MEDIA_XXX values.
  end;

  TDMITapeInfo = record
    MediaType: STORAGE_MEDIA_TYPE;
    MediaCharacteristics: DWord; // Bitmask of MEDIA_XXX values.
    CurrentBlockSize: DWord;
    BusType: STORAGE_BUS_TYPE;
    //
    // Bus specific information describing the medium supported.
    //
    case Integer of {BusSpecificData}
      0: ( {ScsiInformation}
        MediumType: BYTE;
        DensityCode: BYTE);
  end;

  PDEVICE_MEDIA_INFO = ^DEVICE_MEDIA_INFO;
  _DEVICE_MEDIA_INFO = record
    case Integer of
      0: (DiskInfo: TDMIDiskInfo);
      1: (RemovableDiskInfo: TDMIRemovableDiskInfo);
      2: (TapeInfo: TDMITapeInfo);
  end;
  DEVICE_MEDIA_INFO = _DEVICE_MEDIA_INFO;
  TDeviceMediaInfo = DEVICE_MEDIA_INFO;
  PDeviceMediaInfo = PDEVICE_MEDIA_INFO;

  PGET_MEDIA_TYPES = ^GET_MEDIA_TYPES;
  _GET_MEDIA_TYPES = record
    DeviceType: DWord; // FILE_DEVICE_XXX values
    MediaInfoCount: DWord;
    MediaInfo: array [0..0] of DEVICE_MEDIA_INFO;
  end;
  GET_MEDIA_TYPES = _GET_MEDIA_TYPES;
  TGetMediaTypes = GET_MEDIA_TYPES;
  PGetMediaTypes = PGET_MEDIA_TYPES;

//
// IOCTL_STORAGE_PREDICT_FAILURE
//
// input - none
//
// output - STORAGE_PREDICT_FAILURE structure
//          PredictFailure returns zero if no failure predicted and non zero
//                         if a failure is predicted.
//
//          VendorSpecific returns 512 bytes of vendor specific information
//                         if a failure is predicted
//

  PSTORAGE_PREDICT_FAILURE = ^STORAGE_PREDICT_FAILURE;
  _STORAGE_PREDICT_FAILURE = record
    PredictFailure: DWord;
    VendorSpecific: array [0..511] of BYTE;
  end;
  STORAGE_PREDICT_FAILURE = _STORAGE_PREDICT_FAILURE;
  TStoragePredictFailure = STORAGE_PREDICT_FAILURE;
  PStoragePredictFailure = PSTORAGE_PREDICT_FAILURE;

//
// IoControlCode values for disk devices.
//

const
  IOCTL_DISK_BASE = FILE_DEVICE_DISK;

  IOCTL_DISK_GET_DRIVE_GEOMETRY = (
    (IOCTL_DISK_BASE shl 16) or (FILE_ANY_ACCESS shl 14) or
    ($0000 shl 2) or METHOD_BUFFERED);

  IOCTL_DISK_GET_PARTITION_INFO = (
    (IOCTL_DISK_BASE shl 16) or (FILE_READ_ACCESS shl 14) or
    ($0001 shl 2) or METHOD_BUFFERED);

  IOCTL_DISK_SET_PARTITION_INFO = (
    (IOCTL_DISK_BASE shl 16) or ((FILE_READ_ACCESS or FILE_WRITE_ACCESS) shl 14) or
    ($0002 shl 2) or METHOD_BUFFERED);

  IOCTL_DISK_GET_DRIVE_LAYOUT = (
    (IOCTL_DISK_BASE shl 16) or (FILE_READ_ACCESS shl 14) or
    ($0003 shl 2) or METHOD_BUFFERED);

  IOCTL_DISK_SET_DRIVE_LAYOUT = (
    (IOCTL_DISK_BASE shl 16) or ((FILE_READ_ACCESS or FILE_WRITE_ACCESS) shl 14) or
    ($0004 shl 2) or METHOD_BUFFERED);

  IOCTL_DISK_VERIFY = (
    (IOCTL_DISK_BASE shl 16) or (FILE_ANY_ACCESS shl 14) or
    ($0005 shl 2) or METHOD_BUFFERED);

  IOCTL_DISK_FORMAT_TRACKS = (
    (IOCTL_DISK_BASE shl 16) or ((FILE_READ_ACCESS or FILE_WRITE_ACCESS) shl 14) or
    ($0006 shl 2) or METHOD_BUFFERED);

  IOCTL_DISK_REASSIGN_BLOCKS = (
    (IOCTL_DISK_BASE shl 16) or ((FILE_READ_ACCESS or FILE_WRITE_ACCESS) shl 14) or
    ($0007 shl 2) or METHOD_BUFFERED);

  IOCTL_DISK_PERFORMANCE = (
    (IOCTL_DISK_BASE shl 16) or (FILE_ANY_ACCESS shl 14) or
    ($0008 shl 2) or METHOD_BUFFERED);

  IOCTL_DISK_IS_WRITABLE = (
    (IOCTL_DISK_BASE shl 16) or (FILE_ANY_ACCESS shl 14) or
    ($0009 shl 2) or METHOD_BUFFERED);

  IOCTL_DISK_LOGGING = (
    (IOCTL_DISK_BASE shl 16) or (FILE_ANY_ACCESS shl 14) or
    ($000a shl 2) or METHOD_BUFFERED);

  IOCTL_DISK_FORMAT_TRACKS_EX = (
    (IOCTL_DISK_BASE shl 16) or ((FILE_READ_ACCESS or FILE_WRITE_ACCESS) shl 14) or
    ($000b shl 2) or METHOD_BUFFERED);

  IOCTL_DISK_HISTOGRAM_STRUCTURE = (
    (IOCTL_DISK_BASE shl 16) or (FILE_ANY_ACCESS shl 14) or
    ($000c shl 2) or METHOD_BUFFERED);

  IOCTL_DISK_HISTOGRAM_DATA = (
    (IOCTL_DISK_BASE shl 16) or (FILE_ANY_ACCESS shl 14) or
    ($000d shl 2) or METHOD_BUFFERED);

  IOCTL_DISK_HISTOGRAM_RESET = (
    (IOCTL_DISK_BASE shl 16) or (FILE_ANY_ACCESS shl 14) or
    ($000e shl 2) or METHOD_BUFFERED);

  IOCTL_DISK_REQUEST_STRUCTURE = (
    (IOCTL_DISK_BASE shl 16) or (FILE_ANY_ACCESS shl 14) or
    ($000f shl 2) or METHOD_BUFFERED);

  IOCTL_DISK_REQUEST_DATA = (
    (IOCTL_DISK_BASE shl 16) or (FILE_ANY_ACCESS shl 14) or
    ($0010 shl 2) or METHOD_BUFFERED);

  IOCTL_DISK_PERFORMANCE_OFF = (
    (IOCTL_DISK_BASE shl 16) or (FILE_ANY_ACCESS shl 14) or
    ($0018 shl 2) or METHOD_BUFFERED);

  IOCTL_DISK_CONTROLLER_NUMBER = (
    (IOCTL_DISK_BASE shl 16) or (FILE_ANY_ACCESS shl 14) or
    ($0011 shl 2) or METHOD_BUFFERED);

//
// IOCTL support for SMART drive fault prediction.
//

  SMART_GET_VERSION = (
    (IOCTL_DISK_BASE shl 16) or (FILE_READ_ACCESS shl 14) or
    ($0020 shl 2) or METHOD_BUFFERED);

  SMART_SEND_DRIVE_COMMAND = (
    (IOCTL_DISK_BASE shl 16) or ((FILE_READ_ACCESS or FILE_WRITE_ACCESS) shl 14) or
    ($0021 shl 2) or METHOD_BUFFERED);

  SMART_RCV_DRIVE_DATA = (
    (IOCTL_DISK_BASE shl 16) or ((FILE_READ_ACCESS or FILE_WRITE_ACCESS) shl 14) or
    ($0022 shl 2) or METHOD_BUFFERED);

//
// New IOCTLs for GUID Partition tabled disks.
//

// 23-11-2002: various bugs reported by Carsten Grafflage corrected

  IOCTL_DISK_GET_PARTITION_INFO_EX = (
    (IOCTL_DISK_BASE shl 16) or (FILE_ANY_ACCESS shl 14) or
    ($0012 shl 2) or METHOD_BUFFERED);

  IOCTL_DISK_SET_PARTITION_INFO_EX = (
    (IOCTL_DISK_BASE shl 16) or ((FILE_READ_ACCESS or FILE_WRITE_ACCESS) shl 14) or
    ($0013 shl 2) or METHOD_BUFFERED);

  IOCTL_DISK_GET_DRIVE_LAYOUT_EX = (
    (IOCTL_DISK_BASE shl 16) or (FILE_ANY_ACCESS shl 14) or
    ($0014 shl 2) or METHOD_BUFFERED);

  IOCTL_DISK_SET_DRIVE_LAYOUT_EX = (
    (IOCTL_DISK_BASE shl 16) or ((FILE_READ_ACCESS or FILE_WRITE_ACCESS) shl 14) or
    ($0015 shl 2) or METHOD_BUFFERED);

  IOCTL_DISK_CREATE_DISK = (
    (IOCTL_DISK_BASE shl 16) or ((FILE_READ_ACCESS or FILE_WRITE_ACCESS) shl 14) or
    ($0016 shl 2) or METHOD_BUFFERED);

  IOCTL_DISK_GET_LENGTH_INFO = (
    (IOCTL_DISK_BASE shl 16) or (FILE_READ_ACCESS shl 14) or
    ($0017 shl 2) or METHOD_BUFFERED);

  IOCTL_DISK_GET_DRIVE_GEOMETRY_EX = (
    (IOCTL_DISK_BASE shl 16) or (FILE_ANY_ACCESS shl 14) or
    ($0028 shl 2) or METHOD_BUFFERED);

  IOCTL_DISK_UPDATE_DRIVE_SIZE = (
    (IOCTL_DISK_BASE shl 16) or ((FILE_READ_ACCESS or FILE_WRITE_ACCESS) shl 14) or
    ($0032 shl 2) or METHOD_BUFFERED);

  IOCTL_DISK_GROW_PARTITION = (
    (IOCTL_DISK_BASE shl 16) or ((FILE_READ_ACCESS or FILE_WRITE_ACCESS) shl 14) or
    ($0034 shl 2) or METHOD_BUFFERED);

  IOCTL_DISK_GET_CACHE_INFORMATION = (
    (IOCTL_DISK_BASE shl 16) or (FILE_READ_ACCESS shl 14) or
    ($0035 shl 2) or METHOD_BUFFERED);

  IOCTL_DISK_SET_CACHE_INFORMATION = (
    (IOCTL_DISK_BASE shl 16) or ((FILE_READ_ACCESS or FILE_WRITE_ACCESS) shl 14) or
    ($0036 shl 2) or METHOD_BUFFERED);

  OBSOLETE_DISK_GET_WRITE_CACHE_STATE = (
    (IOCTL_DISK_BASE shl 16) or (FILE_READ_ACCESS shl 14) or
    ($0037 shl 2) or METHOD_BUFFERED);

  IOCTL_DISK_DELETE_DRIVE_LAYOUT = (
    (IOCTL_DISK_BASE shl 16) or ((FILE_READ_ACCESS or FILE_WRITE_ACCESS) shl 14) or
    ($0040 shl 2) or METHOD_BUFFERED);

//
// Called to flush cached information that the driver may have about this
// device's characteristics.  Not all drivers cache characteristics, and not
// cached properties can be flushed.  This simply serves as an update to the
// driver that it may want to do an expensive reexamination of the device's
// characteristics now (fixed media size, partition table, etc...)
//

  IOCTL_DISK_UPDATE_PROPERTIES = (
    (IOCTL_DISK_BASE shl 16) or (FILE_ANY_ACCESS shl 14) or
    ($0050 shl 2) or METHOD_BUFFERED);

//
//  Special IOCTLs needed to support PC-98 machines in Japan
//

  IOCTL_DISK_FORMAT_DRIVE = (
    (IOCTL_DISK_BASE shl 16) or ((FILE_READ_ACCESS or FILE_WRITE_ACCESS) shl 14) or
    ($00f3 shl 2) or METHOD_BUFFERED);

  IOCTL_DISK_SENSE_DEVICE = (
    (IOCTL_DISK_BASE shl 16) or (FILE_ANY_ACCESS shl 14) or
    ($00f8 shl 2) or METHOD_BUFFERED);

//
// The following device control codes are common for all class drivers.  The
// functions codes defined here must match all of the other class drivers.
//
// Warning: these codes will be replaced in the future by equivalent
// IOCTL_STORAGE codes
//

  IOCTL_DISK_CHECK_VERIFY = (
    (IOCTL_DISK_BASE shl 16) or (FILE_READ_ACCESS shl 14) or
    ($0200 shl 2) or METHOD_BUFFERED);

  IOCTL_DISK_MEDIA_REMOVAL = (
    (IOCTL_DISK_BASE shl 16) or (FILE_READ_ACCESS shl 14) or
    ($0201 shl 2) or METHOD_BUFFERED);

  IOCTL_DISK_EJECT_MEDIA = (
    (IOCTL_DISK_BASE shl 16) or (FILE_READ_ACCESS shl 14) or
    ($0202 shl 2) or METHOD_BUFFERED);

  IOCTL_DISK_LOAD_MEDIA = (
    (IOCTL_DISK_BASE shl 16) or (FILE_READ_ACCESS shl 14) or
    ($0203 shl 2) or METHOD_BUFFERED);

  IOCTL_DISK_RESERVE = (
    (IOCTL_DISK_BASE shl 16) or (FILE_READ_ACCESS shl 14) or
    ($0204 shl 2) or METHOD_BUFFERED);

  IOCTL_DISK_RELEASE = (
    (IOCTL_DISK_BASE shl 16) or (FILE_READ_ACCESS shl 14) or
    ($0205 shl 2) or METHOD_BUFFERED);

  IOCTL_DISK_FIND_NEW_DEVICES = (
    (IOCTL_DISK_BASE shl 16) or (FILE_READ_ACCESS shl 14) or
    ($0206 shl 2) or METHOD_BUFFERED);

  IOCTL_DISK_GET_MEDIA_TYPES = (
    (IOCTL_DISK_BASE shl 16) or (FILE_ANY_ACCESS shl 14) or
    ($0300 shl 2) or METHOD_BUFFERED);

//
// Define the partition types returnable by known disk drivers.
//

// List of partition identifiers for PCs
// http://www.win.tue.nl/~aeb/partitions/partition_types-1.html

const
  PARTITION_ENTRY_UNUSED    = $00; // Entry unused
  PARTITION_FAT_12          = $01; // 12-bit FAT entries
  PARTITION_XENIX_1         = $02; // Xenix
  PARTITION_XENIX_2         = $03; // Xenix
  PARTITION_FAT_16          = $04; // 16-bit FAT entries
  PARTITION_EXTENDED        = $05; // Extended partition entry
  PARTITION_HUGE            = $06; // Huge partition MS-DOS V4
  PARTITION_IFS             = $07; // IFS Partition
  PARTITION_OS2BOOTMGR      = $0A; // OS/2 Boot Manager/OPUS/Coherent swap
  PARTITION_FAT32           = $0B; // FAT32
  PARTITION_FAT32_XINT13    = $0C; // FAT32 using extended int13 services
  PARTITION_XINT13          = $0E; // Win95 partition using extended int13 services
  PARTITION_XINT13_EXTENDED = $0F; // Same as type 5 but uses extended int13 services
  PARTITION_Windows_RE	    = $27; // Windows RE hidden partition
  PARTITION_PREP            = $41; // PowerPC Reference Platform (PReP) Boot Partition
  PARTITION_LDM             = $42; // Logical Disk Manager partition
  PARTITION_UNIX            = $63; // Unix

  VALID_NTFT                = $C0; // NTFT uses high order bits

//
// The high bit of the partition type code indicates that a partition
// is part of an NTFT mirror or striped array.
//

  PARTITION_NTFT = $80; // NTFT partition

//
// The following macro is used to determine which partitions should be
// assigned drive letters.
//

//++
//
// BOOLEAN
// IsRecognizedPartition(
//     IN DWord PartitionType
//     )
//
// Routine Description:
//
//     This macro is used to determine to which partitions drive letters
//     should be assigned.
//
// Arguments:
//
//     PartitionType - Supplies the type of the partition being examined.
//
// Return Value:
//
//     The return value is TRUE if the partition type is recognized,
//     otherwise FALSE is returned.
//
//--

// function IsRecognizedPartition(PartitionType: DWord): Boolean;

//++
//
// BOOLEAN
// IsContainerPartition(
//     IN DWord PartitionType
//     )
//
// Routine Description:
//
//     This macro is used to determine to which partition types are actually
//     containers for other partitions (ie, extended partitions).
//
// Arguments:
//
//     PartitionType - Supplies the type of the partition being examined.
//
// Return Value:
//
//     The return value is TRUE if the partition type is a container,
//     otherwise FALSE is returned.
//
//--

// function IsContainerPartition(PartitionType: DWord): Boolean;

//++
//
// BOOLEAN
// IsFTPartition(
//     IN DWord PartitionType
//     )
//
// Routine Description:
//
//     This macro is used to determine if the given partition is an FT
//     partition.
//
// Arguments:
//
//     PartitionType - Supplies the type of the partition being examined.
//
// Return Value:
//
//     The return value is TRUE if the partition type is an FT partition,
//     otherwise FALSE is returned.
//
//--

// function IsFTPartition(PartitionType: DWord): Boolean;

//
// Define the media types supported by the driver.
//

type
  _MEDIA_TYPE = (
    Unknown,                // Format is unknown
    F5_1Pt2_512,            // 5.25", 1.2MB,  512 bytes/sector
    F3_1Pt44_512,           // 3.5",  1.44MB, 512 bytes/sector
    F3_2Pt88_512,           // 3.5",  2.88MB, 512 bytes/sector
    F3_20Pt8_512,           // 3.5",  20.8MB, 512 bytes/sector
    F3_720_512,             // 3.5",  720KB,  512 bytes/sector
    F5_360_512,             // 5.25", 360KB,  512 bytes/sector
    F5_320_512,             // 5.25", 320KB,  512 bytes/sector
    F5_320_1024,            // 5.25", 320KB,  1024 bytes/sector
    F5_180_512,             // 5.25", 180KB,  512 bytes/sector
    F5_160_512,             // 5.25", 160KB,  512 bytes/sector
    RemovableMedia,         // Removable media other than floppy
    FixedMedia,             // Fixed hard disk media
    F3_120M_512,            // 3.5", 120M Floppy
    F3_640_512,             // 3.5" ,  640KB,  512 bytes/sector
    F5_640_512,             // 5.25",  640KB,  512 bytes/sector
    F5_720_512,             // 5.25",  720KB,  512 bytes/sector
    F3_1Pt2_512,            // 3.5" ,  1.2Mb,  512 bytes/sector
    F3_1Pt23_1024,          // 3.5" ,  1.23Mb, 1024 bytes/sector
    F5_1Pt23_1024,          // 5.25",  1.23MB, 1024 bytes/sector
    F3_128Mb_512,           // 3.5" MO 128Mb   512 bytes/sector
    F3_230Mb_512,           // 3.5" MO 230Mb   512 bytes/sector
    F8_256_128,             // 8",     256KB,  128 bytes/sector
    F3_200Mb_512,           // 3.5",   200M Floppy (HiFD)
    F3_240M_512,            // 3.5",   240Mb Floppy (HiFD)
    F3_32M_512);            // 3.5",   32Mb Floppy
  MEDIA_TYPE = _MEDIA_TYPE;
  PMEDIA_TYPE = ^MEDIA_TYPE;
  TMediaType = MEDIA_TYPE;
  PMediaType = PMEDIA_TYPE;

//
// Define the input buffer structure for the driver, when
// it is called with IOCTL_DISK_FORMAT_TRACKS.
//

  PFORMAT_PARAMETERS = ^FORMAT_PARAMETERS;
  _FORMAT_PARAMETERS = record
    MediaType: MEDIA_TYPE;
    StartCylinderNumber: DWord;
    EndCylinderNumber: DWord;
    StartHeadNumber: DWord;
    EndHeadNumber: DWord;
  end;
  FORMAT_PARAMETERS = _FORMAT_PARAMETERS;
  TFormatParameters = FORMAT_PARAMETERS;
  PFormatParameters = PFORMAT_PARAMETERS;

//
// Define the BAD_TRACK_NUMBER type. An array of elements of this type is
// returned by the driver on IOCTL_DISK_FORMAT_TRACKS requests, to indicate
// what tracks were bad during formatting. The length of that array is
// reported in the `Information' field of the I/O Status Block.
//

  BAD_TRACK_NUMBER = WORD;
  PBAD_TRACK_NUMBER = ^WORD;

//
// Define the input buffer structure for the driver, when
// it is called with IOCTL_DISK_FORMAT_TRACKS_EX.
//

  PFORMAT_EX_PARAMETERS = ^FORMAT_EX_PARAMETERS;
  _FORMAT_EX_PARAMETERS = record
    MediaType: MEDIA_TYPE;
    StartCylinderNumber: DWord;
    EndCylinderNumber: DWord;
    StartHeadNumber: DWord;
    EndHeadNumber: DWord;
    FormatGapLength: WORD;
    SectorsPerTrack: WORD;
    SectorNumber: array [0..0] of WORD;
  end;
  FORMAT_EX_PARAMETERS = _FORMAT_EX_PARAMETERS;
  TFormatExParameters = FORMAT_EX_PARAMETERS;
  PFormatExParameters = PFORMAT_EX_PARAMETERS;

//
// The following structure is returned on an IOCTL_DISK_GET_DRIVE_GEOMETRY
// request and an array of them is returned on an IOCTL_DISK_GET_MEDIA_TYPES
// request.
//

  PDISK_GEOMETRY = ^DISK_GEOMETRY;
  _DISK_GEOMETRY = record
    Cylinders: LARGE_INTEGER;
    MediaType: MEDIA_TYPE;
    TracksPerCylinder: DWord;
    SectorsPerTrack: DWord;
    BytesPerSector: DWord;
  end;
  DISK_GEOMETRY = _DISK_GEOMETRY;
  TDiskGeometry = DISK_GEOMETRY;
  PDiskGeometry = PDISK_GEOMETRY;

//
// This wmi guid returns a DISK_GEOMETRY structure
//

const
  WMI_DISK_GEOMETRY_GUID: TGUID = (
    D1:$25007f51; D2:$57c2; D3:$11d1; D4:($a5, $28, $00, $a0, $c9, $06, $29, $10));

//
// The following structure is returned on an IOCTL_DISK_GET_PARTITION_INFO
// and an IOCTL_DISK_GET_DRIVE_LAYOUT request.  It is also used in a request
// to change the drive layout, IOCTL_DISK_SET_DRIVE_LAYOUT.
//

type
  PPARTITION_INFORMATION = ^PARTITION_INFORMATION;
  _PARTITION_INFORMATION = record
    StartingOffset: LARGE_INTEGER;
    PartitionLength: LARGE_INTEGER;
    HiddenSectors: DWord;
    PartitionNumber: DWord;
    PartitionType: BYTE;
    BootIndicator: ByteBool;
    RecognizedPartition: ByteBool;
    RewritePartition: ByteBool;
  end;
  PARTITION_INFORMATION = _PARTITION_INFORMATION;
  TPartitionInformation = PARTITION_INFORMATION;
  PPartitionInformation = PPARTITION_INFORMATION;

//
// The following structure is used to change the partition type of a
// specified disk partition using an IOCTL_DISK_SET_PARTITION_INFO
// request.
//

  PSET_PARTITION_INFORMATION = ^SET_PARTITION_INFORMATION;
  _SET_PARTITION_INFORMATION = record
    PartitionType: BYTE;
  end;
  SET_PARTITION_INFORMATION = _SET_PARTITION_INFORMATION;
  TSetPartitionInformation = _SET_PARTITION_INFORMATION;
  PSetPartitionInformation = PSET_PARTITION_INFORMATION;

//
// The following structures is returned on an IOCTL_DISK_GET_DRIVE_LAYOUT
// request and given as input to an IOCTL_DISK_SET_DRIVE_LAYOUT request.
//

  PDRIVE_LAYOUT_INFORMATION = ^DRIVE_LAYOUT_INFORMATION;
  _DRIVE_LAYOUT_INFORMATION = record
    PartitionCount: DWord;
    Signature: DWord;
    PartitionEntry: array [0..0] of PARTITION_INFORMATION;
  end;
  DRIVE_LAYOUT_INFORMATION = _DRIVE_LAYOUT_INFORMATION;
  TDriveLayoutInformation = DRIVE_LAYOUT_INFORMATION;
  PDriveLayoutInformation = PDRIVE_LAYOUT_INFORMATION;

//
// The following structure is passed in on an IOCTL_DISK_VERIFY request.
// The offset and length parameters are both given in bytes.
//

  PVERIFY_INFORMATION = ^VERIFY_INFORMATION;
  _VERIFY_INFORMATION = record
    StartingOffset: LARGE_INTEGER;
    Length: DWord;
  end;
  VERIFY_INFORMATION = _VERIFY_INFORMATION;
  TVerifyInformation = VERIFY_INFORMATION;
  PVerifyInformation = PVERIFY_INFORMATION;

//
// The following structure is passed in on an IOCTL_DISK_REASSIGN_BLOCKS
// request.
//

  PREASSIGN_BLOCKS = ^REASSIGN_BLOCKS;
  _REASSIGN_BLOCKS = record
    Reserved: WORD;
    Count: WORD;
    BlockNumber: array [0..0] of DWord;
  end;
  REASSIGN_BLOCKS = _REASSIGN_BLOCKS;
  TReassignBlocks = REASSIGN_BLOCKS;
  PReassignBlocks = PREASSIGN_BLOCKS;

//
// Support for GUID Partition Table (GPT) disks.
//

//
// There are currently two ways a disk can be partitioned. With a traditional
// AT-style master boot record (PARTITION_STYLE_MBR) and with a new, GPT
// partition table (PARTITION_STYLE_GPT). RAW is for an unrecognizable
// partition style. There are a very limited number of things you can
// do with a RAW partititon.
//

type
  _PARTITION_STYLE = (
    PARTITION_STYLE_MBR,
    PARTITION_STYLE_GPT,
    PARTITION_STYLE_RAW);
  PARTITION_STYLE = _PARTITION_STYLE;
  TPartitionStyle = PARTITION_STYLE;

//
// The following structure defines information in a GPT partition that is
// not common to both GPT and MBR partitions.
//

  PPARTITION_INFORMATION_GPT = ^PARTITION_INFORMATION_GPT;
  _PARTITION_INFORMATION_GPT = record
    PartitionType: TGuid; // Partition type. See table 16-3.
    PartitionId: TGuid; // Unique GUID for this partition.
    Attributes: DWord64; // See table 16-4.
    Name: array [0..35] of WCHAR; // Partition Name in Unicode.
  end;
  PARTITION_INFORMATION_GPT = _PARTITION_INFORMATION_GPT;
  TPartitionInformationGpt = PARTITION_INFORMATION_GPT;
  PPartitionInformationGpt = PPARTITION_INFORMATION_GPT;

//
//  The following are GPT partition attributes applicable for any
//  partition type. These attributes are not OS-specific
//

const
  GPT_ATTRIBUTE_PLATFORM_REQUIRED = $0000000000000001;

//
// The following are GPT partition attributes applicable when the
// PartitionType is PARTITION_BASIC_DATA_GUID.
//

  GPT_BASIC_DATA_ATTRIBUTE_NO_DRIVE_LETTER = DWord64($8000000000000000);
  GPT_BASIC_DATA_ATTRIBUTE_HIDDEN          = $4000000000000000;
  GPT_BASIC_DATA_ATTRIBUTE_READ_ONLY       = $1000000000000000;
  GPT_BASIC_DATA_ATTRIBUTE_SHADOW_COPY     = $2000000000000000;

//
// The following structure defines information in an MBR partition that is not
// common to both GPT and MBR partitions.
//

type
  PPARTITION_INFORMATION_MBR = ^PARTITION_INFORMATION_MBR;
  _PARTITION_INFORMATION_MBR = record
    PartitionType: BYTE;
    BootIndicator: BOOLEAN;
    RecognizedPartition: BOOLEAN;
    HiddenSectors: DWord;
  end;
  PARTITION_INFORMATION_MBR = _PARTITION_INFORMATION_MBR;
  TPartitionInformationMbr = PARTITION_INFORMATION_MBR;
  PPartitionInformationMbr = PPARTITION_INFORMATION_MBR;

//
// The structure SET_PARTITION_INFO_EX is used with the ioctl
// IOCTL_SET_PARTITION_INFO_EX to set information about a specific
// partition. Note that for MBR partitions, you can only set the partition
// signature, whereas GPT partitions allow setting of all fields that
// you can get.
//

  SET_PARTITION_INFORMATION_MBR = SET_PARTITION_INFORMATION;
  TSetPartitionInformationMbr = SET_PARTITION_INFORMATION_MBR;
  SET_PARTITION_INFORMATION_GPT = PARTITION_INFORMATION_GPT;
  TSetPartitionInformationGpt = SET_PARTITION_INFORMATION_GPT;

  PSET_PARTITION_INFORMATION_EX = ^SET_PARTITION_INFORMATION_EX;
  _SET_PARTITION_INFORMATION_EX = record
    PartitionStyle: PARTITION_STYLE;
    case Integer of
      0: (Mbr: SET_PARTITION_INFORMATION_MBR);
      1: (Gpt: SET_PARTITION_INFORMATION_GPT);
  end;
  SET_PARTITION_INFORMATION_EX = _SET_PARTITION_INFORMATION_EX;
  TSetPartitionInformationEx = SET_PARTITION_INFORMATION_EX;
  PSetPartitionInformationEx = PSET_PARTITION_INFORMATION_EX;

//
// The structure CREATE_DISK_GPT with the ioctl IOCTL_DISK_CREATE_DISK
// to initialize an virgin disk with an empty GPT partition table.
//

  PCREATE_DISK_GPT = ^CREATE_DISK_GPT;
  _CREATE_DISK_GPT = record
    DiskId: TGuid; // Unique disk id for the disk.
    MaxPartitionCount: DWord; // Maximim number of partitions allowable.
  end;
  CREATE_DISK_GPT = _CREATE_DISK_GPT;
  TCreateDiskGpt = CREATE_DISK_GPT;
  PCreateDiskGpt = PCREATE_DISK_GPT;

//
// The structure CREATE_DISK_MBR with the ioctl IOCTL_DISK_CREATE_DISK
// to initialize an virgin disk with an empty MBR partition table.
//

  PCREATE_DISK_MBR = ^CREATE_DISK_MBR;
  _CREATE_DISK_MBR = record
    Signature: DWord;
  end;
  CREATE_DISK_MBR = _CREATE_DISK_MBR;
  TCreateDiskMbr = CREATE_DISK_MBR;
  PCreateDiskMbr = PCREATE_DISK_MBR;

  PCREATE_DISK = ^CREATE_DISK;
  _CREATE_DISK = record
    PartitionStyle: PARTITION_STYLE;
    case Integer of
      0: (Mbr: CREATE_DISK_MBR);
      1: (Gpt: CREATE_DISK_GPT);
  end;
  CREATE_DISK = _CREATE_DISK;
  TCreateDisk = CREATE_DISK;
  PCreateDisk = PCREATE_DISK;

//
// The structure GET_LENGTH_INFORMATION is used with the ioctl
// IOCTL_DISK_GET_LENGTH_INFO to obtain the length, in bytes, of the
// disk, partition, or volume.
//

  PGET_LENGTH_INFORMATION = ^GET_LENGTH_INFORMATION;
  _GET_LENGTH_INFORMATION = record
    Length: LARGE_INTEGER;
  end;
  GET_LENGTH_INFORMATION = _GET_LENGTH_INFORMATION;
  TGetLengthInformation = GET_LENGTH_INFORMATION;
  PGetLengthInformation = PGET_LENGTH_INFORMATION;

//
// The PARTITION_INFORMATION_EX structure is used with the
// IOCTL_DISK_GET_DRIVE_LAYOUT_EX, IOCTL_DISK_SET_DRIVE_LAYOUT_EX,
// IOCTL_DISK_GET_PARTITION_INFO_EX and IOCTL_DISK_GET_PARTITION_INFO_EX calls.
//

  PPARTITION_INFORMATION_EX = ^PARTITION_INFORMATION_EX;
  _PARTITION_INFORMATION_EX = record
    PartitionStyle: PARTITION_STYLE;
    StartingOffset: LARGE_INTEGER;
    PartitionLength: LARGE_INTEGER;
    PartitionNumber: DWord;
    RewritePartition: BOOLEAN;
    case Integer of
      0: (Mbr: PARTITION_INFORMATION_MBR);
      1: (Gpt: PARTITION_INFORMATION_GPT);
  end;
  PARTITION_INFORMATION_EX = _PARTITION_INFORMATION_EX;
  TPartitionInformationEx = PARTITION_INFORMATION_EX;
  PPartitionInformationEx = PPARTITION_INFORMATION_EX;

//
// GPT specific drive layout information.
//

  PDRIVE_LAYOUT_INFORMATION_GPT = ^DRIVE_LAYOUT_INFORMATION_GPT;
  _DRIVE_LAYOUT_INFORMATION_GPT = record
    DiskId: TGuid;
    StartingUsableOffset: LARGE_INTEGER;
    UsableLength: LARGE_INTEGER;
    MaxPartitionCount: DWord;
  end;
  DRIVE_LAYOUT_INFORMATION_GPT = _DRIVE_LAYOUT_INFORMATION_GPT;
  TDriveLayoutInformationGpt = DRIVE_LAYOUT_INFORMATION_GPT;
  PDriveLayoutInformationGpt = PDRIVE_LAYOUT_INFORMATION_GPT;

//
// MBR specific drive layout information.
//

  PDRIVE_LAYOUT_INFORMATION_MBR = ^DRIVE_LAYOUT_INFORMATION_MBR;
  _DRIVE_LAYOUT_INFORMATION_MBR = record
    Signature: DWord;
  end;
  DRIVE_LAYOUT_INFORMATION_MBR = _DRIVE_LAYOUT_INFORMATION_MBR;
  TDriveLayoutInformationMbr = DRIVE_LAYOUT_INFORMATION_MBR;
  PDriveLayoutInformationMbr = PDRIVE_LAYOUT_INFORMATION_MBR;

//
// The structure DRIVE_LAYOUT_INFORMATION_EX is used with the
// IOCTL_SET_DRIVE_LAYOUT_EX and IOCTL_GET_DRIVE_LAYOUT_EX calls.
//

  PDRIVE_LAYOUT_INFORMATION_EX = ^DRIVE_LAYOUT_INFORMATION_EX;
  _DRIVE_LAYOUT_INFORMATION_EX = record
    PartitionStyle: DWord;
    PartitionCount: DWord;
    Union: record
      case Integer of
        0: (Mbr: DRIVE_LAYOUT_INFORMATION_MBR);
        1: (Gpt: DRIVE_LAYOUT_INFORMATION_GPT);
    end;
    PartitionEntry: array [0..0] of PARTITION_INFORMATION_EX;
  end;
  DRIVE_LAYOUT_INFORMATION_EX = _DRIVE_LAYOUT_INFORMATION_EX;
  TDriveLayoutInformationEx = DRIVE_LAYOUT_INFORMATION_EX;
  PDriveLayoutInformationEx = PDRIVE_LAYOUT_INFORMATION_EX;

//
// The DISK_GEOMETRY_EX structure is returned on issuing an
// IOCTL_DISK_GET_DRIVE_GEOMETRY_EX ioctl.
//

  _DETECTION_TYPE = (
    DetectNone,
    DetectInt13,
    DetectExInt13);
  DETECTION_TYPE = _DETECTION_TYPE;
  TDetectionType = DETECTION_TYPE;

  PDISK_INT13_INFO = ^DISK_INT13_INFO;
  _DISK_INT13_INFO = record
    DriveSelect: WORD;
    MaxCylinders: DWord;
    SectorsPerTrack: WORD;
    MaxHeads: WORD;
    NumberDrives: WORD;
  end;
  DISK_INT13_INFO = _DISK_INT13_INFO;
  TDiskInt13Info = DISK_INT13_INFO;
  PDiskInt13Info = PDISK_INT13_INFO;

  PDISK_EX_INT13_INFO = ^DISK_EX_INT13_INFO;
  _DISK_EX_INT13_INFO = record
    ExBufferSize: WORD;
    ExFlags: WORD;
    ExCylinders: DWord;
    ExHeads: DWord;
    ExSectorsPerTrack: DWord;
    ExSectorsPerDrive: DWord64;
    ExSectorSize: WORD;
    ExReserved: WORD;
  end;
  DISK_EX_INT13_INFO = _DISK_EX_INT13_INFO;
  TDiskExInt13Info = DISK_EX_INT13_INFO;
  PDiskExInt13Info = PDISK_EX_INT13_INFO;

  PDISK_DETECTION_INFO = ^DISK_DETECTION_INFO;
  _DISK_DETECTION_INFO = record
    SizeOfDetectInfo: DWord;
    DetectionType: DETECTION_TYPE;
    case Integer of
      0: (
        //
        // If DetectionType == DETECTION_INT13 then we have just the Int13
        // information.
        //
        Int13: DISK_INT13_INFO;
        //
        // If DetectionType == DETECTION_EX_INT13, then we have the
        // extended int 13 information.
        //
        ExInt13: DISK_EX_INT13_INFO); // If DetectionType == DetectExInt13
  end;
  DISK_DETECTION_INFO = _DISK_DETECTION_INFO;
  TDiskDetectionInfo = DISK_DETECTION_INFO;
  PDiskDetectionInfo = PDISK_DETECTION_INFO;

  PDISK_PARTITION_INFO = ^DISK_PARTITION_INFO;
  _DISK_PARTITION_INFO = record
    SizeOfPartitionInfo: DWord;
    PartitionStyle: PARTITION_STYLE; // PartitionStyle = RAW, GPT or MBR
    case Integer of
      0: (                           // If PartitionStyle == MBR
        Signature: DWord; // MBR Signature
        CheckSum: DWord); // MBR CheckSum
      1: (                           // If PartitionStyle == GPT
        DiskId: TGuid);
  end;
  DISK_PARTITION_INFO = _DISK_PARTITION_INFO;
  TDiskPartitionInfo = DISK_PARTITION_INFO;
  PDiskPartitionInfo = PDISK_PARTITION_INFO;

//
// The Geometry structure is a variable length structure composed of a
// DISK_GEOMETRY_EX structure followed by a DISK_PARTITION_INFO structure
// followed by a DISK_DETECTION_DATA structure.
//

type
  PDISK_GEOMETRY_EX = ^DISK_GEOMETRY_EX;
  _DISK_GEOMETRY_EX = record
    Geometry: DISK_GEOMETRY;    // Standard disk geometry: may be faked by driver.
    DiskSize: LARGE_INTEGER;    // Must always be correct
    Data: array [0..0] of BYTE; // Partition, Detect info
  end;
  DISK_GEOMETRY_EX = _DISK_GEOMETRY_EX;
  TDiskGeometryEx = DISK_GEOMETRY_EX;
  PDiskGeometryEx = PDISK_GEOMETRY_EX;

function IsContainerPartition(PartitionType: DWord): Boolean;
function IsFTPartition(PartitionType: DWord): Boolean;
function DiskGeometryGetPartition(Geometry: PDiskGeometryEx): PDiskPartitionInfo;
function DiskGeometryGetDetect(Geometry: PDiskGeometryEx): PDiskDetectionInfo;

implementation

function IsRecognizedPartition(PartitionType: DWord): Boolean;
begin
  Result :=
    (((PartitionType and PARTITION_NTFT) <> 0) and ((PartitionType and not $C0) = PARTITION_FAT_12)) or
    (((PartitionType and PARTITION_NTFT) <> 0) and ((PartitionType and not $C0) = PARTITION_IFS)) or
    (((PartitionType and PARTITION_NTFT) <> 0) and ((PartitionType and not $C0) = PARTITION_HUGE)) or
    (((PartitionType and PARTITION_NTFT) <> 0) and ((PartitionType and not $C0) = PARTITION_FAT32)) or
    (((PartitionType and PARTITION_NTFT) <> 0) and ((PartitionType and not $C0) = PARTITION_FAT32_XINT13)) or
    (((PartitionType and PARTITION_NTFT) <> 0) and ((PartitionType and not $C0) = PARTITION_XINT13)) or
    ((PartitionType) = PARTITION_FAT_12) or
    ((PartitionType) = PARTITION_FAT_16) or
    ((PartitionType) = PARTITION_IFS) or
    ((PartitionType) = PARTITION_HUGE) or
    ((PartitionType) = PARTITION_FAT32) or
    ((PartitionType) = PARTITION_FAT32_XINT13) or
    ((PartitionType) = PARTITION_XINT13);
end;

function IsContainerPartition(PartitionType: DWord): Boolean;
begin
  Result :=
    (PartitionType = PARTITION_EXTENDED) or
    (PartitionType = PARTITION_XINT13_EXTENDED);
end;

function IsFTPartition(PartitionType: DWord): Boolean;
begin
  Result := ((PartitionType and PARTITION_NTFT) <> 0) and IsRecognizedPartition(PartitionType);
end;

function DiskGeometryGetPartition(Geometry: PDiskGeometryEx): PDiskPartitionInfo;
begin
  Result := PDiskPartitionInfo (@Geometry^.Data[0]);
end;

function DiskGeometryGetDetect(Geometry: PDiskGeometryEx): PDiskDetectionInfo;
var
  Partition: PDiskPartitionInfo;
begin
  Partition := DiskGeometryGetPartition(Geometry);
  Result := PDiskDetectionInfo(PAnsiChar(Partition) + Partition^.SizeOfPartitionInfo*SizeOf(DWord));
end;

end.

