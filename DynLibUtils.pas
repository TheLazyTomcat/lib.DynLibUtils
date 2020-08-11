unit DynLibUtils;

{$IF Defined(WINDOWS) or Defined(MSWINDOWS)}
  {$DEFINE Windows}
{$ELSEIF Defined(LINUX) and Defined(FPC)}
  {$DEFINE Linux}
{$ELSE}
  {$MESSAGE FATAL 'Unsupported operating system.'}
{$IFEND}

{$IFDEF FPC}
  {$MODE ObjFPC}
  {$MODESWITCH CLASSICPROCVARS+}
  {$DEFINE FPC_DisableWarns}
  {$MACRO ON}
{$ENDIF}
{$H+}

{$DEFINE DLU_SilenceCriticalErrors}

interface

uses
  {$IFDEF Windows}Windows,{$ENDIF} SysUtils, Classes;

type
  EDLUException = class(Exception);

  EDLUSymbolError      = class(EDLUException);
  EDLUInvalidParameter = class(EDLUException);
  EDLULibraryOpenError = class(EDLUException);

{$IFDEF Windows}      
{===============================================================================
    Process error mode management functions - declaration
===============================================================================}
{
  For details, see implementation.
}

var
  GetThreadErrorMode: Function: DWORD; stdcall;
  SetThreadErrorMode: Function(dwNewMode: DWORD; lpOldMode: LPDWORD): BOOL; stdcall;
  
{$ENDIF}

{===============================================================================
    Low-level functions - declaration
===============================================================================}
{
  Low-level functions are more-or-less just a wrappers for system calls.
  They are here to hide differences between operating systems.
}

type
  TDLULibraryHandle = {$IFDEF Windows}THandle{$ELSE}Pointer{$ENDIF};
  PDLULibraryHandle = ^TDLULibraryHandle;

//------------------------------------------------------------------------------

{
  CheckLibrary

  Returns true when passed parameter is initialized (is not null/nil), ie. it
  contains handle to a library, false otherwise.
}
Function CheckLibrary(LibHandle: TDLULibraryHandle): Boolean; overload;

{
  OpenLibrary

  Loads the requested library. It will return null/nil if it cannot be loaded.

  Windows OS - when the library cannot be loaded for whatever reason, the
               function will initiate critical system error, which will display
               an error dialog. If this behavior is undesirable, define symbol
               DLU_SilentCriticalErrors (either project-wide or in this unit,
               see higher).
}
Function OpenLibrary(const LibFileName: String): TDLULibraryHandle; overload;

{
  CloseLibrary

  Closes and potentially unloads the library (unloading is managed by OS).

  It checks the handle (function CheckLibrary) before processing, if it is not
  deem to be valid, it will exit without doing anything.

  Note that it vill invalide the library handle, irrespective of whether the OS
  unloads the library or not.
}
procedure CloseLibrary(var LibHandle: TDLULibraryHandle); overload;

{
  GetSymbolAddr

  Returns pointer to requested symbol. If it canot be resolved, it returns nil.

  If the library handle is not valid, it will raise an EDLUInvalidParameter
  exception.
}
Function GetSymbolAddr(LibHandle: TDLULibraryHandle; const SymbolName: String): Pointer; overload;

{
  GetSymbolAddr

  Stores address of requested symbol to Address output parameter.

  Returns true when the symbol was properly resolved, false otherwise (in which
  case the value of Address is undefined).

  If the library handle is not valid, it will raise an EDLUInvalidParameter
  exception.
}
Function GetSymbolAddr(LibHandle: TDLULibraryHandle; const SymbolName: String; out Address: Pointer): Boolean; overload;

{
  GetAndCheckSymbolAddr

  Tries to resolve requested symbol and return its address.

  If the requested symbol is not successfully resolved, then this function will
  raise an EDLUSymbolError exception. Otherwise it works the same as function
  GetSymbolAddr.

  If the library handle is not valid, it will raise an EDLUInvalidParameter
  exception.
}
Function GetAndCheckSymbolAddr(LibHandle: TDLULibraryHandle; const SymbolName: String): Pointer; overload;

{===============================================================================
    High-level functions - declaration
===============================================================================}
{
  Variables of type TDLULibraryContext must be explicitly initialized before
  first. Either zero the memory or assign the DefaultLibraryContext constant.
}
type
  TDLULibraryContext = record
    ReferenceCount: Integer;
    FileName:       String; // original file name (possibly with a path) passed to OpenLibrary
    LibraryHandle:  TDLULibraryHandle;
  end;
  PDLULibraryContext = ^TDLULibraryContext;

const
  DefaultLibraryContext: TDLULibraryContext = (
    ReferenceCount: 0;
    FileName:       '';
    LibraryHandle:  {$IFDEF Windows}0{$ELSE}nil{$ENDIF});

//------------------------------------------------------------------------------
{
  Following functions work mostly the same as their low-level counterparts,
  see their description for details.

  All High level functions are synchronized using one critical section, meaning
  only one of them can be executed at a time in the entire module.
  This is to ensure consistency of contexts, please be aware of that.  
}

Function CheckLibrary(Context: TDLULibraryContext): Boolean; overload;

{
  If OpenLibrary fails to load the requested library, it will raise an
  EDLULibraryOpenError exception.

  Returns reference count of the given context.
}
Function OpenLibrary(const LibFileName: String; var Context: TDLULibraryContext): Integer; overload;
{
  CloseLibrary returns true when the reference count reaches zero and the
  library is freen using system calls, false otherwise.
}
Function CloseLibrary(var Context: TDLULibraryContext): Boolean; overload;

Function GetSymbolAddr(Context: TDLULibraryContext; const SymbolName: String): Pointer; overload;
Function GetSymbolAddr(Context: TDLULibraryContext; const SymbolName: String; out Address: Pointer): Boolean; overload;
Function GetAndCheckSymbolAddr(Context: TDLULibraryContext; const SymbolName: String): Pointer; overload;

{
  ResolveSymbols

  Resolves given symbols and stores obtained pointers to variables pointed by
  the items in Addresses array. For each symbol name, the resolved address is
  stored at the same position in Addresses (eg. address for second name is
  stored at second position).

  If FailOnUnresolved is set to false, the function will try to resolve
  everything. That some names were not resoved is evidenced by return value
  being lower than length of Names array.

  WARNING - it is not possible to discern which symbols were not resolved, as
            any symbol can be correctly resolved to nil. You have to test each
            symbol separately for example in calls to overload of GetSymbolAddr
            that indicates failure/success.

  If FailOnUnresolved is set to true, the function will raise an EDLUSystemError
  exception on first unresolved symbol.

  Length of both Names and Addresses arrays must be the same, otherwise an
  EDLUInvalidParameter exception is raised.

  Returns number of succesfully resolved symbols.  
}
Function ResolveSymbols(Context: TDLULibraryContext; const Names: array of String; Addresses: array of PPointer; FailOnUnresolved: Boolean = False): Integer; overload;

{
  ResolveSymbols

  This function works the same as the first overload, but names of individual
  symbols is taken from passed string list and resulting addresses are stored
  at respective places in Objects property (typecasted to TObject).

  The paramter Symbols is not declared as TStrings because it does not properly
  implement Objects property.
}
Function ResolveSymbols(Context: TDLULibraryContext; Symbols: TStringList; FailOnUnresolved: Boolean = False): Integer; overload;

{
  OpenLibraryAndResolveSymbols

  Following functions are just an macro functions that calls OpenLibrary and
  then ResolveSymbols.

  For details on parameters, see description of ResolveSymbols functions.

  Return value is a return value of particular ResolveSymbols used in the
  implementation.
}
Function OpenLibraryAndResolveSymbols(const LibFileName: String; var Context: TDLULibraryContext;
  const Names: array of String; Addresses: array of PPointer; FailOnUnresolved: Boolean = False): Integer; overload;
Function OpenLibraryAndResolveSymbols(const LibFileName: String; var Context: TDLULibraryContext;
  Symbols: TStringList; FailOnUnresolved: Boolean = False): Integer; overload;

implementation

uses
  {$IFNDEF Windows}dl,{$ENDIF}
  StrRect;

{$IFDEF FPC_DisableWarns}
  {$DEFINE FPCDWM}
  {$DEFINE W4055:={$WARN 4055 OFF}} // Conversion between ordinals and pointers is not portable
{$ENDIF}

{$IFDEF Windows}
{===============================================================================
    Process error mode management functions - implementation
===============================================================================}
{-------------------------------------------------------------------------------
    Checking of Windows version
-------------------------------------------------------------------------------}

const
  VER_MINORVERSION     = $0000001;
  VER_MAJORVERSION     = $0000002;
  VER_SERVICEPACKMAJOR = $0000020;

  VER_GREATER_EQUAL = 3;

//------------------------------------------------------------------------------

type
  TOSVersionInfoExA = record
    dwOSVersionInfoSize:  DWORD;
    dwMajorVersion:       DWORD;
    dwMinorVersion:       DWORD;
    dwBuildNumber:        DWORD;
    dwPlatformId:         DWORD;
    szCSDVersion:         array[0..127] of AnsiChar;
    wServicePackMajor:    Word;
    wServicePackMinor:    Word;
    wSuiteMask:           Word;
    wProductType:         Byte;
    wReserved:            Byte;
  end;
  POSVersionInfoExA = ^TOSVersionInfoExA;

  TOSVersionInfoExW = record
    dwOSVersionInfoSize:  DWORD;
    dwMajorVersion:       DWORD;
    dwMinorVersion:       DWORD;
    dwBuildNumber:        DWORD;
    dwPlatformId:         DWORD;
    szCSDVersion:         array[0..127] of WideChar;
    wServicePackMajor:    Word;
    wServicePackMinor:    Word;
    wSuiteMask:           Word;
    wProductType:         Byte;
    wReserved:            Byte;
  end;
  POSVersionInfoExW = ^TOSVersionInfoExW;

{$IFDEF Unicode}
  TOSVersionInfoEx = TOSVersionInfoExW;
{$ELSE}
  TOSVersionInfoEx = TOSVersionInfoExA;
{$ENDIF}
  POSVersionInfoEx = ^TOSVersionInfoEx;

//------------------------------------------------------------------------------

// external functions
Function VerifyVersionInfoW(lpVersionInfo: POSVersionInfoExW; dwTypeMask: DWORD; dwlConditionMask: Int64): BOOL; stdcall; external kernel32;
Function VerifyVersionInfoA(lpVersionInfo: POSVersionInfoExA; dwTypeMask: DWORD; dwlConditionMask: Int64): BOOL; stdcall; external kernel32;
Function VerifyVersionInfo(lpVersionInfo: POSVersionInfoEx; dwTypeMask: DWORD; dwlConditionMask: Int64): BOOL; stdcall; external kernel32 name{$IFDEF Unicode}'VerifyVersionInfoW'{$ELSE}'VerifyVersionInfoA'{$ENDIF};

Function VerSetConditionMask(ConditionMask: Int64; TypeMask: DWORD; Condition: Byte): Int64; stdcall; external kernel32;

//------------------------------------------------------------------------------

Function IsWindowsVersionOrGreater(wMajorVersion,wMinorVersion,wServicePackMajor: Word): Boolean;
var
  OSVersion:      TOSVersionInfoEx;
  ConditionMask:  Int64;
begin
FillChar(Addr(OSVersion)^,SizeOf(TOSVersionInfoEx),0);
OSVersion.dwOSVersionInfoSize := SizeOf(TOSVersionInfoEx);
ConditionMask := VerSetConditionMask(
  VerSetConditionMask(
    VerSetConditionMask(0,VER_MAJORVERSION,VER_GREATER_EQUAL),
    VER_MINORVERSION,VER_GREATER_EQUAL),
  VER_SERVICEPACKMAJOR,VER_GREATER_EQUAL);
OSVersion.dwMajorVersion := wMajorVersion;
OSVersion.dwMinorVersion := wMinorVersion;
OSVersion.wServicePackMajor := wServicePackMajor;
Result := VerifyVersionInfo(@OSVersion,VER_MAJORVERSION or VER_MINORVERSION or VER_SERVICEPACKMAJOR,ConditionMask);
end;

//------------------------------------------------------------------------------

Function IsWindows7OrGreater: Boolean;
begin
Result := IsWindowsVersionOrGreater(6,1,0);
end;

{-------------------------------------------------------------------------------
    Error mode management
-------------------------------------------------------------------------------}
{
  "Solution" for thread safe error mode management (function GetThreadErrorMode
  and SetThreadErrorMode) in old systems (Vista and older).
  Note that this solution is NOT in itself completely thread safe (see further),
  it is here just to provide compatibility with older systems while allowing
  the use of newer system features.

  In Windows 7 and newer, the system functions GetThreadErrorMode and
  SetThreadErrorMode are used as usual.

  In older systems, wrappers with the same name as modern functions are used.
  These wrapers are calling SetErrorMode WinAPI function.
  Each of these wrappers is thread safe in the sense that only one of them can
  execute at a time. They are all protected by one critical section.

  Also, function OpenLibrary is protected by the same crit. section when symbol
  DLU_SilenceCriticalErrors is defined.

  Sadly, this protection mechanism will inevitably fail if SetErrorMode is
  called directly anywhere else. This can be circumvented by hooking the
  function, but lets not go down this rabbit hole.
}

// synchronization for threaded functions
var
  PEM_Synchronizer: TRTLCriticalSection;
  PEM_NeedSync:     Boolean = True;

//------------------------------------------------------------------------------

procedure PEM_Lock;
begin
If PEM_NeedSync then
  EnterCriticalSection(PEM_Synchronizer);
end;

//------------------------------------------------------------------------------

procedure PEM_Unlock;
begin
If PEM_NeedSync then
  LeaveCriticalSection(PEM_Synchronizer);
end;

//------------------------------------------------------------------------------

Function PEM_GetThreadErrorMode: DWORD; stdcall;
begin
PEM_Lock;
try
  // note that GetErrorMode is available only from Windows Vista
  Result := SetErrorMode(0);
  SetErrorMode(Result);
finally
  PEM_Unlock;
end;
end;

//------------------------------------------------------------------------------

Function PEM_SetThreadErrorMode(dwNewMode: DWORD; lpOldMode: LPDWORD): BOOL; stdcall;
begin
PEM_Lock;
try
  If Assigned(lpOldMode) then
    lpOldMode^ := SetErrorMode(dwNewMode)
  else
    SetErrorMode(dwNewMode);
  Result := True;
finally
  PEM_Unlock;
end;
end;

//------------------------------------------------------------------------------

procedure PEM_Initialize;
var
  Module: TDLULibraryHandle;
begin
// by default call wrappers
GetThreadErrorMode := PEM_GetThreadErrorMode;
SetThreadErrorMode := PEM_SetThreadErrorMode;
// for win7 and up, load "real" functions into procedural variables
If IsWindows7OrGreater then
  begin
    {
      kernel32.dll really should be loaded by this point, so there should be no
      need to call LoadLibrary (which might cause trouble because a need to call
      FreeLibrary and so on)
    }
    Module := GetModuleHandle('kernel32.dll');
    If Module <> 0 then
      begin
        @GetThreadErrorMode := GetProcAddress(Module,'GetThreadErrorMode');
        @SetThreadErrorMode := GetProcAddress(Module,'SetThreadErrorMode');
        PEM_NeedSync := False;
      end
    else raise EDLUException.Create('Kernel32.dll not loaded.');
  end;
If PEM_NeedSync then
  InitializeCriticalSection(PEM_Synchronizer);
end;

//------------------------------------------------------------------------------

procedure PEM_Finalize;
begin
If PEM_NeedSync then
  DeleteCriticalSection(PEM_Synchronizer);
end;

{$ENDIF}

{===============================================================================
    Low-level functions - implementation
===============================================================================}

Function CheckLibrary(LibHandle: TDLULibraryHandle): Boolean;
begin
{$IFDEF Windows}
Result := LibHandle <> 0;
{$ELSE}
Result := Assigned(LibHandle);
{$ENDIF}
end;

//------------------------------------------------------------------------------

Function OpenLibrary(const LibFileName: String): TDLULibraryHandle;
{$IF Defined(Windows) and Defined(DLU_SilenceCriticalErrors)}
var
  OldErrorMode: DWORD;
{$IFEND}
begin
{$IFDEF Windows}
{$IFDEF DLU_SilenceCriticalErrors}
PEM_Lock;
try
  OldErrorMode := GetThreadErrorMode;
  SetThreadErrorMode(OldErrorMode or SEM_FAILCRITICALERRORS,nil);
  try
{$ENDIF}
    Result := LoadLibraryEx(PSysChar(StrToSys(LibFileName)),0,0);
{$IFDEF DLU_SilenceCriticalErrors}
  finally
    SetThreadErrorMode(OldErrorMode,nil);
  end;
finally
  PEM_Unlock;
end;
{$ENDIF}
{$ELSE}
Result := dlopen(PSysChar(StrToSys(LibFileName)),RTLD_NOW);
{$ENDIF}
end;

//------------------------------------------------------------------------------

procedure CloseLibrary(var LibHandle: TDLULibraryHandle);
begin
If CheckLibrary(LibHandle) then
  begin
  {$IFDEF Windows}
    FreeLibrary(LibHandle);
    LibHandle := 0;
  {$ELSE}
    dlclose(LibHandle); // it can fail, but let's ignore it here
    LibHandle := nil;
  {$ENDIF}
  end;
end;

//------------------------------------------------------------------------------

Function GetSymbolAddr(LibHandle: TDLULibraryHandle; const SymbolName: String): Pointer;
begin
If CheckLibrary(LibHandle) then
{$IFDEF Windows}
  Result := GetProcAddress(LibHandle,PSysChar(StrToSys(SymbolName)))
{$ELSE}
  Result := dlsym(LibHandle,PSysChar(StrToSys(SymbolName)))
{$ENDIF}
else
{$IFDEF FPCDWM}{$PUSH}W4055{$ENDIF}
  raise EDLUInvalidParameter.CreateFmt('GetSymbolAddr: Invalid library handle 0x%p.',[Pointer(LibHandle)]);
{$IFDEF FPCDWM}{$POP}{$ENDIF}
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function GetSymbolAddr(LibHandle: TDLULibraryHandle; const SymbolName: String; out Address: Pointer): Boolean;
begin
If CheckLibrary(LibHandle) then
  begin
  {$IFDEF Windows}
    Address := GetProcAddress(LibHandle,PSysChar(StrToSys(SymbolName)));
    Result := Assigned(Address);
  {$ELSE}
  {
    dlsym can return a VALID nil value, to check for errors, we have to look
    into what dlerror function returns after a call to dlsym, if it does not
    return anything (null/nil), we can assume no error has occured
  }
    dlerror;  // clear last error
    Address := dlsym(LibHandle,PSysChar(StrToSys(SymbolName)));
    Result := not Assigned(dlerror);
  {$ENDIF}
  end
{$IFDEF FPCDWM}{$PUSH}W4055{$ENDIF}
else raise EDLUInvalidParameter.CreateFmt('GetSymbolAddr: Invalid library handle 0x%p.',[Pointer(LibHandle)]);
{$IFDEF FPCDWM}{$POP}{$ENDIF}
end;

//------------------------------------------------------------------------------

Function GetAndCheckSymbolAddr(LibHandle: TDLULibraryHandle; const SymbolName: String): Pointer;
{$IFNDEF Windows}
var
  ErrorMsg: PSysChar;
{$ENDIF}
begin
If CheckLibrary(LibHandle) then
  begin
  {$IFDEF Windows}
    Result := GetProcAddress(LibHandle,PSysChar(StrToSys(SymbolName)));
    If not Assigned(Result) then
      raise EDLUSymbolError.CreateFmt('GetAndCheckSymbolAddr: Unable to resolve symbol "%s" (0x%.8x).',[SymbolName,GetLastError]);
  {$ELSE}
    dlerror;  // clear last error
    Result := dlsym(LibHandle,PSysChar(StrToSys(SymbolName)));
    ErrorMsg := dlerror;
    If Assigned(ErrorMsg) then
      raise EDLUSymbolError.CreateFmt('GetAndCheckSymbolAddr: Unable to resolve symbol "%s" (%s).',[SymbolName,SysToStr(ErrorMsg)]);
  {$ENDIF}
  end
{$IFDEF FPCDWM}{$PUSH}W4055{$ENDIF}
else raise EDLUInvalidParameter.CreateFmt('GetAndCheckSymbolAddr: Invalid library handle 0x%p.',[Pointer(LibHandle)]);
{$IFDEF FPCDWM}{$POP}{$ENDIF}
end;

{===============================================================================
    High-level functions - implementation
===============================================================================}
{-------------------------------------------------------------------------------
    High-level functions - internals
-------------------------------------------------------------------------------}

var
  HLF_Synchronizer: TRTLCriticalSection;

//------------------------------------------------------------------------------

procedure HLF_Initialize;
begin
{$IF Defined(FPC) and not Defined(Windows)}
InitCriticalSection(HLF_Synchronizer);
{$ELSE}
InitializeCriticalSection(HLF_Synchronizer);
{$IFEND}
end;

//------------------------------------------------------------------------------

procedure HLF_Finalize;
begin
{$IF Defined(FPC) and not Defined(Windows)}
DoneCriticalSection(HLF_Synchronizer);
{$ELSE}
DeleteCriticalSection(HLF_Synchronizer);
{$IFEND}
end;

//------------------------------------------------------------------------------

procedure HLF_Lock;
begin
EnterCriticalSection(HLF_Synchronizer);
end;

//------------------------------------------------------------------------------

procedure HLF_Unlock;
begin
LeaveCriticalSection(HLF_Synchronizer);
end;

{-------------------------------------------------------------------------------
    High-level functions - public part
-------------------------------------------------------------------------------}

Function CheckLibrary(Context: TDLULibraryContext): Boolean;
begin
HLF_Lock;
try
  Result := (Context.ReferenceCount > 0) and CheckLibrary(Context.LibraryHandle);
finally
  HLF_Unlock;
end;
end;

//------------------------------------------------------------------------------

Function OpenLibrary(const LibFileName: String; var Context: TDLULibraryContext): Integer;
begin
Result := 0;
HLF_Lock;
try
  Inc(Context.ReferenceCount);
  If Context.ReferenceCount <= 1 then
    begin
      // first call on this context
      Context.ReferenceCount := 1;
      Context.FileName := LibFileName;
      Context.LibraryHandle := OpenLibrary(LibFileName);
      If not CheckLibrary(Context.LibraryHandle) then
        raise EDLULibraryOpenError.CreateFmt('OpenLibrary: Failed to open library "%s".',[LibFileName]);
    end;
  Result := Context.ReferenceCount;
finally
  HLF_Unlock;
end;
end;

//------------------------------------------------------------------------------

Function CloseLibrary(var Context: TDLULibraryContext): Boolean;
begin
HLF_Lock;
try
  Dec(Context.ReferenceCount);
  If Context.ReferenceCount <= 0 then
    begin
      Context.ReferenceCount := 0;
      Context.FileName := '';
      CloseLibrary(Context.LibraryHandle);
      Result := True;
    end
  else Result := False;
finally
  HLF_Unlock;
end;
end;

//------------------------------------------------------------------------------

Function GetSymbolAddr(Context: TDLULibraryContext; const SymbolName: String): Pointer;
begin
HLF_Lock;
try
  Result := GetSymbolAddr(Context.LibraryHandle,SymbolName);
finally
  HLF_Unlock;
end;
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function GetSymbolAddr(Context: TDLULibraryContext; const SymbolName: String; out Address: Pointer): Boolean;
begin
HLF_Lock;
try
  Result := GetSymbolAddr(Context.LibraryHandle,SymbolName,Address);
finally
  HLF_Unlock;
end;
end;

//------------------------------------------------------------------------------

Function GetAndCheckSymbolAddr(Context: TDLULibraryContext; const SymbolName: String): Pointer;
begin
HLF_Lock;
try
  Result := GetAndCheckSymbolAddr(Context.LibraryHandle,SymbolName);
finally
  HLF_Unlock;
end;
end;

//------------------------------------------------------------------------------

Function ResolveSymbols(Context: TDLULibraryContext; const Names: array of String; Addresses: array of PPointer; FailOnUnresolved: Boolean = False): Integer;
var
  i:  Integer;
begin
Result := 0;
HLF_Lock;
try
  If Length(Names) = Length(Addresses) then
    begin
      If FailOnUnresolved then
        begin
          For i := Low(Names) to High(Names) do
            Addresses[i]^ := GetAndCheckSymbolAddr(Context.LibraryHandle,Names[i]);
          Result := Length(Names);
        end
      else
        begin
          For i := Low(Names) to High(Names) do
            If GetSymbolAddr(Context.LibraryHandle,Names[i],Addresses[i]^) then
              Inc(Result);
        end;
    end
  else raise EDLUInvalidParameter.CreateFmt('ResolveSymbols: Length of arrays do not match (%d,%d).',[Length(Names),Length(Addresses)]);
finally
  HLF_Unlock;
end;
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function ResolveSymbols(Context: TDLULibraryContext; Symbols: TStringList; FailOnUnresolved: Boolean = False): Integer;
var
  i:        Integer;
  TempPtr:  Pointer;
begin
Result := 0;
HLF_Lock;
try
  If FailOnUnresolved then
    begin
      For i := 0 to Pred(Symbols.Count) do
        Symbols.Objects[i] := TObject(GetAndCheckSymbolAddr(Context.LibraryHandle,Symbols[i]));
      Result := Symbols.Count;
    end
  else
    begin
      For i := 0 to Pred(Symbols.Count) do
        If GetSymbolAddr(Context.LibraryHandle,Symbols[i],TempPtr) then
          begin
            Symbols.Objects[i] := TObject(TempPtr);
            Inc(Result);
          end
        else Symbols.Objects[i] := nil;
    end;
finally
  HLF_Unlock;
end;
end;

//------------------------------------------------------------------------------

Function OpenLibraryAndResolveSymbols(const LibFileName: String; var Context: TDLULibraryContext; const Names: array of String; Addresses: array of PPointer; FailOnUnresolved: Boolean = False): Integer;
begin
Result := 0;
HLF_Lock;
try
  OpenLibrary(LibFileName,Context);
  try
    Result := ResolveSymbols(Context,Names,Addresses,FailOnUnresolved);
  except
    CloseLibrary(Context);
  end;
finally
  HLF_Unlock;
end;
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function OpenLibraryAndResolveSymbols(const LibFileName: String; var Context: TDLULibraryContext; Symbols: TStringList; FailOnUnresolved: Boolean = False): Integer;
begin
Result := 0;
HLF_Lock;
try
  OpenLibrary(LibFileName,Context);
  try
    Result := ResolveSymbols(Context,Symbols,FailOnUnresolved);
  except
    CloseLibrary(Context);
  end;
finally
  HLF_Unlock;
end;
end;

{===============================================================================
    Unit initialization, finalization
===============================================================================}

{$IFDEF Windows}
initialization
  PEM_Initialize;
  HLF_Initialize

finalization
  HLF_Finalize;
  PEM_Finalize;
{$ENDIF}

end.
