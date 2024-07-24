(**
	Declarations copied from the "Project JEDI API Header Library"
	http://www.delphi-jedi.org/apilibrary.html
**)

{$IFDEF FPC}
	{$MODE DELPHI}
{$ENDIF}

{$I .\..\switches.inc}

{$WARN SYMBOL_PLATFORM OFF}

unit ImageHlpU;

interface

uses Windows, SysUtils;

const
  IMAGE_FILE_LARGE_ADDRESS_AWARE = $0020;

type
    TImageSubSystem = (issUnknown, issNative, issWindowsGui, issWindowsCui,
                       issPadding1, issOS2Cui, issPadding2, issPoxixCui,
                       issNativeWindows, issWindowsCEGui, issEFI_Application,
                       issEFI_BootServiceDriver, issEFI_RuntimeDriver,
                       issEFI_ROM, issXbox, issPadding3,
                       issWindowsBootApplication, issXboxCodeCatalog);

  PIMAGE_OPTIONAL_HEADER32 = ^IMAGE_OPTIONAL_HEADER32;
  {$EXTERNALSYM PIMAGE_OPTIONAL_HEADER32}
  _IMAGE_OPTIONAL_HEADER = record
    //
    // Standard fields.
    //
    Magic: Word;
    MajorLinkerVersion: Byte;
    MinorLinkerVersion: Byte;
    SizeOfCode: DWord;
    SizeOfInitializedData: DWord;
    SizeOfUninitializedData: DWord;
    AddressOfEntryPoint: DWord;
    BaseOfCode: DWord;
    BaseOfData: DWord;
    //
    // NT additional fields.
    //
    ImageBase: DWord;
    SectionAlignment: DWord;
    FileAlignment: DWord;
    MajorOperatingSystemVersion: Word;
    MinorOperatingSystemVersion: Word;
    MajorImageVersion: Word;
    MinorImageVersion: Word;
    MajorSubsystemVersion: Word;
    MinorSubsystemVersion: Word;
    Win32VersionValue: DWord;
    SizeOfImage: DWord;
    SizeOfHeaders: DWord;
    CheckSum: DWord;
    Subsystem: Word;
    DllCharacteristics: Word;
    SizeOfStackReserve: DWord;
    SizeOfStackCommit: DWord;
    SizeOfHeapReserve: DWord;
    SizeOfHeapCommit: DWord;
    LoaderFlags: DWord;
    NumberOfRvaAndSizes: DWord;
    DataDirectory: array [0..IMAGE_NUMBEROF_DIRECTORY_ENTRIES - 1] of IMAGE_DATA_DIRECTORY;
  end;
  {$EXTERNALSYM _IMAGE_OPTIONAL_HEADER}
  IMAGE_OPTIONAL_HEADER32 = _IMAGE_OPTIONAL_HEADER;
  {$EXTERNALSYM IMAGE_OPTIONAL_HEADER32}
  TImageOptionalHeader32 = IMAGE_OPTIONAL_HEADER32;
  PImageOptionalHeader32 = PIMAGE_OPTIONAL_HEADER32;
  PTImageOptionalHeader32 = PIMAGE_OPTIONAL_HEADER32;

  PIMAGE_NT_HEADERS32 = ^IMAGE_NT_HEADERS32;
  {$EXTERNALSYM PIMAGE_NT_HEADERS32}
  _IMAGE_NT_HEADERS = record
    Signature: DWord;
    FileHeader: IMAGE_FILE_HEADER;
    OptionalHeader: IMAGE_OPTIONAL_HEADER32;
  end;
  {$EXTERNALSYM _IMAGE_NT_HEADERS}
  IMAGE_NT_HEADERS32 = _IMAGE_NT_HEADERS;
  {$EXTERNALSYM IMAGE_NT_HEADERS32}
  TImageNtHeaders32 = IMAGE_NT_HEADERS32;
  PImageNtHeaders32 = PIMAGE_NT_HEADERS32;
  PTImageNtHeaders32 = PIMAGE_NT_HEADERS32;

  TImgSecHdrMisc = record
    case Integer of
      0: (PhysicalAddress: DWord);
      1: (VirtualSize: DWord);
  end;

  PIMAGE_SECTION_HEADER = ^IMAGE_SECTION_HEADER;
  {$EXTERNALSYM PIMAGE_SECTION_HEADER}
  _IMAGE_SECTION_HEADER = record
    Name: array [0..IMAGE_SIZEOF_SHORT_NAME - 1] of BYTE;
    Misc: TImgSecHdrMisc;
    VirtualAddress: DWord;
    SizeOfRawData: DWord;
    PointerToRawData: DWord;
    PointerToRelocations: DWord;
    PointerToLinenumbers: DWord;
    NumberOfRelocations: Word;
    NumberOfLinenumbers: Word;
    Characteristics: DWord;
  end;
  {$EXTERNALSYM _IMAGE_SECTION_HEADER}
  IMAGE_SECTION_HEADER = _IMAGE_SECTION_HEADER;
  {$EXTERNALSYM IMAGE_SECTION_HEADER}
  TImageSectionHeader = IMAGE_SECTION_HEADER;
  PImageSectionHeader = PIMAGE_SECTION_HEADER;
  PTImageSectionHeader = PIMAGE_SECTION_HEADER;

  PLOADED_IMAGE = ^LOADED_IMAGE;
  {$EXTERNALSYM PLOADED_IMAGE}
  _LOADED_IMAGE = record
    ModuleName: LPSTR;
    hFile: THANDLE;
    MappedAddress: PUCHAR;
    FileHeader: PIMAGE_NT_HEADERS32;
    LastRvaSection: PIMAGE_SECTION_HEADER;
    NumberOfSections: ULONG;
    Sections: PIMAGE_SECTION_HEADER;
    Characteristics: ULONG;
    fSystemImage: ByteBool;
    fDOSImage: ByteBool;
    Links: LIST_ENTRY;
    SizeOfImage: ULONG;
  end;
  {$EXTERNALSYM _LOADED_IMAGE}
  LOADED_IMAGE = _LOADED_IMAGE;
  {$EXTERNALSYM LOADED_IMAGE}
  TLoadedImage = LOADED_IMAGE;
  PLoadedImage = PLOADED_IMAGE;
  PTLoadedImage = PLOADED_IMAGE;

function GetSubSystem (const sExeName: String) : TImageSubSystem;
function IsConsoleApp (const sExeName: String) : Boolean;
function IsLargeAddressAware (const sExeName: String) : Boolean;

var
	ImageLoad : function (DllName, DllPath: LPSTR) : PLOADED_IMAGE;
    	                                                          stdcall = NIL;
    ImageUnload : function (const LoadedImage: LOADED_IMAGE) : BOOL;
    	                                                          stdcall = NIL;

implementation

uses VerifyU;

ResourceString
	cLoadErr = 'Functions from "imagehlp.dll" not available';

(* ---- *)

function GetSubSystem (const sExeName: String) : TImageSubSystem;

var
	pImage : PLOADED_IMAGE;

begin
	if not (Assigned (ImageLoad)) then
    	raise EOSError.Create (cLoadErr);

    pImage := ImageLoad (PAnsiChar (AnsiString (ExtractFileName (sExeName))),
    					 PAnsiChar (AnsiString (ExtractFilePath (sExeName))));

    Win32Check (pImage <> NIL);

    Result := TImageSubSystem (pImage^.FileHeader^.OptionalHeader.Subsystem);

    VerifyApi (ImageUnload (pImage^));
end; { GetSubSystem }

(* ---- *)

function IsConsoleApp (const sExeName: String) : Boolean;
begin
    Result := GetSubSystem (sExeName) = issWindowsCui;
end; { IsConsoleApp }

(* ---- *)

function IsLargeAddressAware (const sExeName: String) : Boolean;

var
	pImage : PLOADED_IMAGE;

begin
	if not (Assigned (ImageLoad)) then
    	raise EOSError.Create (cLoadErr);

    pImage := ImageLoad (PAnsiChar (AnsiString (ExtractFileName (sExeName))),
    					 PAnsiChar (AnsiString (ExtractFilePath (sExeName))));

    Win32Check (pImage <> NIL);

    Result := (pImage^.Characteristics and IMAGE_FILE_LARGE_ADDRESS_AWARE) <> 0;

    VerifyApi (ImageUnload (pImage^));
end; { IsLargeAddressAware }

(* ---- *)

{$I GetProcAddress.inc}

(* ---- *)

const
	cImageHlp = 'imagehlp.dll';

var
	hImageHlp : THandle;

initialization
	hImageHlp := LoadLibrary (cImageHlp);

    if (hImageHlp <> 0) then
    begin
    	ImageLoad := GPA (hImageHlp, 'ImageLoad');
        ImageUnload := GPA (hImageHlp, 'ImageUnload');
    end; { if }

finalization
    if (hImageHlp <> 0) then
    	VerifyApi (FreeLibrary (hImageHlp));
end.

