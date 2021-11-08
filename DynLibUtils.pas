{-------------------------------------------------------------------------------

  This Source Code Form is subject to the terms of the Mozilla Public
  License, v. 2.0. If a copy of the MPL was not distributed with this
  file, You can obtain one at http://mozilla.org/MPL/2.0/.

-------------------------------------------------------------------------------}
{===============================================================================

  Dynamic Library Utilities

    Main aim of this small library is to encapsulate dynamic library loading
    and symbol resolving (ie. obtaining addresses of functions and variables)
    on different systems.
    Beyond that, only some simple macro functions are currently implemented.

  Version 1.1 (2021-11-08)

  Last change 2021-11-08

  ©2020-2021 František Milt

  Contacts:
    František Milt: frantisek.milt@gmail.com

  Support:
    If you find this code useful, please consider supporting its author(s) by
    making a small donation using the following link(s):

      https://www.paypal.me/FMilt

  Changelog:
    For detailed changelog and history please refer to this git repository:

      github.com/TheLazyTomcat/Lib.DynLibUtils

  Dependencies:
    StrRect        - github.com/TheLazyTomcat/Lib.StrRect
  * AuxTypes       - github.com/TheLazyTomcat/Lib.AuxTypes
  * WindowsVersion - github.com/TheLazyTomcat/Lib.WindowsVersion

  Libraries AuxTypes and WindowsVersion are only required when compiling for
  Windows OS.

===============================================================================}
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
  {$INLINE ON}
  {$DEFINE CanInline}
  {$DEFINE FPC_DisableWarns}
  {$MACRO ON}
{$ELSE}
  {$IF CompilerVersion >= 17 then}  // Delphi 2005+
    {$DEFINE CanInline}
  {$ELSE}
    {$UNDEF CanInline}
  {$IFEND}
{$ENDIF}
{$H+}

interface

uses
  {$IFDEF Windows}Windows,{$ENDIF} SysUtils, Classes;

{===============================================================================
    Library-specific exceptions
===============================================================================}
type
  EDLUException = class(Exception);

  EDLULibraryOpenError = class(EDLUException);  
  EDLUInvalidParameter = class(EDLUException);
  EDLUSymbolError      = class(EDLUException);

{$IFDEF Windows}
{===============================================================================
    Process error mode management - declaration
===============================================================================}

Function GetThreadErrorMode: DWORD;
Function SetThreadErrorMode(dwNewMode: DWORD; lpOldMode: LPDWORD): BOOL;

{$ENDIF}

{===============================================================================
    Common types, constants, variables
===============================================================================}
type
  TDLULibraryHandle = {$IFDEF Windows}THandle{$ELSE}Pointer{$ENDIF};
  PDLULibraryHandle = ^TDLULibraryHandle;

  // TDLUSymbol is used in macro functions for symbol resolving
  TDLUSymbol = record
    Name:       String;
    AddressVar: PPointer;
  end;
  PDLUSymbol = ^TDLUSymbol;

{===============================================================================
    Functions - declaration
===============================================================================}
{-------------------------------------------------------------------------------
    Functions - utility functions
-------------------------------------------------------------------------------}

// inline contructors for TDLUSymbol record
Function Symbol(const Name: String; AddressVar: PPointer): TDLUSymbol; overload; {$IFDEF CanInline}inline;{$ENDIF}
Function Symbol(AddressVar: PPointer; const Name: String): TDLUSymbol; overload; {$IFDEF CanInline}inline;{$ENDIF}

{-------------------------------------------------------------------------------
    Functions - core functions
-------------------------------------------------------------------------------}
{
  CheckLibrary

  Returns true when passed parameter is initialized (is not null/nil), ie. it
  contains handle to a library, false otherwise.
}
Function CheckLibrary(LibraryHandle: TDLULibraryHandle): Boolean;

{
  OpenLibrary

  Loads the requested library. It will return null/nil if it cannot be loaded.

  In Windows OS, when the library cannot be loaded for whatever reason, the
  called WinAPI function will initiate critical system error, which will
  display an error dialog (a window).
  This might be very obtrusive (eg. when probing DLL existence) - so if this
  behavior is undesirable, set SilentCriticalErrors parameter to true and this
  error dialog will be suppressed.

    NOTE - parameter SilentCriticalErrors has no effect in operating systems
           other than windows.

    WARNING - suppressing error dialog is inherently thread unsafe in systems
              older than Windows 7.
}
Function OpenLibrary(const LibFileName: String; SilentCriticalErrors: Boolean = False): TDLULibraryHandle; overload;

{
  OpenLibraryAndCheck

  Works the same as OpenLibrary (in fact it is calling it), but when the
  library cannot be loaded (null/nil handle returned) it will raise an
  EDLULibraryOpenError exception.
}
Function OpenAndCheckLibrary(const LibFileName: String; SilentCriticalErrors: Boolean = False): TDLULibraryHandle;

{
  CloseLibrary

  Closes and potentially unloads the library (unloading is managed by OS).

  It checks the handle (function CheckLibrary) before processing, if it is not
  deemed to be valid, it will exit without doing anything.

  Note that it will invalide the library handle, irrespective of whether the OS
  unloads the library or not.
}
procedure CloseLibrary(var LibraryHandle: TDLULibraryHandle); overload;

{
  GetSymbolAddr

  Returns pointer to requested symbol. If it canot be resolved, it returns nil.

  If the library handle is not valid, it will raise an EDLUInvalidParameter
  exception.
}
Function GetSymbolAddr(LibraryHandle: TDLULibraryHandle; const SymbolName: String): Pointer; overload;

{
  GetSymbolAddr

  Stores address of requested symbol to Address output parameter.

  Returns true when the symbol was properly resolved, false otherwise (in which
  case the value of Address is undefined).

  If the library handle is not valid, it will raise an EDLUInvalidParameter
  exception.
}
Function GetSymbolAddr(LibraryHandle: TDLULibraryHandle; const SymbolName: String; out Address: Pointer): Boolean; overload;

{
  GetAndCheckSymbolAddr

  Tries to resolve requested symbol and return its address.

  If the requested symbol is not successfully resolved, then this function will
  raise an EDLUSymbolError exception. Otherwise it works the same as function
  GetSymbolAddr.

  If the library handle is not valid, it will raise an EDLUInvalidParameter
  exception.
}
Function GetAndCheckSymbolAddr(LibraryHandle: TDLULibraryHandle; const SymbolName: String): Pointer;

{$message 'implement'}
//Function LibraryIsPresent(const LibFileName: String): Boolean;

//Function SymbolIsPresent(const LibFileName, SymbolName: String): Boolean;

{-------------------------------------------------------------------------------
    Functions - macro functions
-------------------------------------------------------------------------------}
{
  ResolveSymbolNames

  Resolves given symbols and stores obtained pointers to variables pointed by
  the items in Addresses array. For each symbol name, the resolved address is
  stored at the same position in Addresses (eg. address for second name is
  stored at second position).

  If FailOnUnresolved is set to false, the function will try to resolve
  everything. That some names were not resoved is evidenced by returned value
  being lower than length of Names array.

  WARNING - it is not possible to discern which symbols were not resolved, as
            any symbol can be CORRECTLY resolved to nil. You have to test each
            symbol separately for example in calls to overload of GetSymbolAddr
            that indicates failure/success.

  If FailOnUnresolved is set to true, the function will raise an EDLUSymbolError
  exception on first unresolved symbol.

  Length of both Names and Addresses arrays must be the same, otherwise an
  EDLUInvalidParameter exception is raised.

  Returns number of succesfully resolved symbols.  
}
Function ResolveSymbolNames(LibraryHandle: TDLULibraryHandle; const Names: array of String; Addresses: array of PPointer; FailOnUnresolved: Boolean = False): Integer;

{
  ResolveSymbol

  Resolves symbol whose name is given in Name field of parameter Symbol and
  stores the address to a variable pointed to by the field AddressVar in the
  same parameter.

  When FailOnUnresolved is se to true, then the function will raise an
  EDLUSymbolError exception when the symbol cannot be resolved, otherwise the
  failure (false) or success (true) is indicated in the result.
}
Function ResolveSymbol(LibraryHandle: TDLULibraryHandle; Symbol: TDLUSymbol; FailOnUnresolved: Boolean = False): Boolean;

{
  ResolveSymbols

  This function works the same as the ResolveSymbolNames, but names of
  individual symbols are taken from passed string list and resulting addresses
  are stored at respective places in Objects property (typecasted to TObject).

  The parameter Symbols is not declared as TStrings because that class does not
  fully implement Objects property.
}
Function ResolveSymbols(LibraryHandle: TDLULibraryHandle; Symbols: TStringList; FailOnUnresolved: Boolean = False): Integer; overload;

{
  ResolveSymbols

  This overload is only repeatedly calling function ResolveSymbol to resolve
  individual TDLUSymbol structures in the passed array, see description of
  ResolveSymbol for details on its behavior.

  Other parameters and result value behaves the same as in the first overload
  of ResolveSymbols.
}
Function ResolveSymbols(LibraryHandle: TDLULibraryHandle; Symbols: array of TDLUSymbol; FailOnUnresolved: Boolean = False): Integer; overload;

{
  OpenLibraryAndResolveSymbol(s/Names)

  Following functions are just an macro functions that are calling
  OpenAndCheckLibrary and then ResolveSymbol(s/Names). Refer to description of
  those functions for their behavior (raised exceptions, meaning of parameters,
  ...). 

  Returns a return value of particular ResolveSymbol(s/Names) used in the
  implementation of given function.

  If an exception is raised, then content of the output parameter LibraryHandle
  is undefined (when the exception is raised after the library was successfully
  opened, it gets closed)
}
Function OpenLibraryAndResolveSymbolNames(const LibFileName: String; out LibraryHandle: TDLULibraryHandle;
  const Names: array of String; Addresses: array of PPointer; FailOnUnresolved: Boolean = False; SilentCriticalErrors: Boolean = False): Integer;

Function OpenLibraryAndResolveSymbols(const LibFileName: String; out LibraryHandle: TDLULibraryHandle;
  Symbols: TStringList; FailOnUnresolved: Boolean = False; SilentCriticalErrors: Boolean = False): Integer; overload;

Function OpenLibraryAndResolveSymbols(const LibFileName: String; out LibraryHandle: TDLULibraryHandle;
  Symbols: array of TDLUSymbol; FailOnUnresolved: Boolean = False; SilentCriticalErrors: Boolean = False): Integer; overload;

{-------------------------------------------------------------------------------
    Functions - backward compatibility
-------------------------------------------------------------------------------}
{
  In version 1.1 of this library, the contexts were completely removed as they
  offered little to no benefit over simple handles.

  Following types, constants and functions are here to preserve at least some
  backward compatibility with existing code that was using contexts.
}
type
  TDLULibraryContext = TDLULibraryHandle;

const
  DefaultLibraryContext: TDLULibraryHandle = {$IFDEF Windows}0{$ELSE}nil{$ENDIF};

//------------------------------------------------------------------------------
{
  Only a simple wrapper for OpenAndCheckLibrary, refer to that function for
  full description.

  Always returns 1.
}
Function OpenLibrary(const LibFileName: String; out Context: TDLULibraryContext; SilentCriticalErrors: Boolean = False): Integer; overload;

implementation

uses
  {$IFNDEF Windows}dl,{$ENDIF}
  StrRect{$IFDEF Windows}, WindowsVersion{$ENDIF};

{$IFDEF FPC_DisableWarns}
  {$DEFINE FPCDWM}
  {$DEFINE W4055:={$WARN 4055 OFF}} // Conversion between ordinals and pointers is not portable
  {$DEFINE W5024:={$WARN 5024 OFF}} // Parameter "$1" not used
{$ENDIF}

{$IFDEF Windows}
{===============================================================================
    Process error mode management - implementation
===============================================================================}
{-------------------------------------------------------------------------------
    Process error mode management - internals
-------------------------------------------------------------------------------}
{
  "Solution" for thread safe error mode management (function GetThreadErrorMode
  and SetThreadErrorMode) in old systems (Windows Vista and older).
  Note that this solution is NOT in itself thread safe, it is here just to
  provide compatibility with older systems while allowing the use of newer
  system features.

  In Windows 7 and newer, the system functions GetThreadErrorMode and
  SetThreadErrorMode are used as usual.

  In older systems, functions that tries to emulate the functionality through
  use of SetErrorMode WinAPI function are called instead.
}

Function EMUL_GetThreadErrorMode: DWORD; stdcall;
begin
// note that GetErrorMode is available only from Windows Vista
Result := SetErrorMode(0);
SetErrorMode(Result);
end;

//------------------------------------------------------------------------------

Function EMUL_SetThreadErrorMode(dwNewMode: DWORD; lpOldMode: LPDWORD): BOOL; stdcall;
begin
If Assigned(lpOldMode) then
  lpOldMode^ := SetErrorMode(dwNewMode)
else
  SetErrorMode(dwNewMode);
Result := True;
end;

//------------------------------------------------------------------------------

var
  VAR_GetThreadErrorMode: Function: DWORD; stdcall = EMUL_GetThreadErrorMode;
  VAR_SetThreadErrorMode: Function(dwNewMode: DWORD; lpOldMode: LPDWORD): BOOL; stdcall = EMUL_SetThreadErrorMode;

//------------------------------------------------------------------------------

procedure PEM_Initialize;
var
  Module: TDLULibraryHandle;
begin
// for win7 and up, load "real" functions into procedural variables
If IsWindows7OrGreater then
  begin
    {
      kernel32.dll really should be loaded by this point, so there should be no
      need to call LoadLibrary (which might cause trouble because of a need to
      call FreeLibrary and so on)
    }
    Module := GetModuleHandle('kernel32.dll');
    If Module <> 0 then
      begin
        @VAR_GetThreadErrorMode := GetAndCheckSymbolAddr(Module,'GetThreadErrorMode');
        @VAR_SetThreadErrorMode := GetAndCheckSymbolAddr(Module,'SetThreadErrorMode');
      end
    else raise EDLULibraryOpenError.Create('Kernel32.dll not loaded.');
  end;
end;

{-------------------------------------------------------------------------------
    Process error mode management - public functions
-------------------------------------------------------------------------------}

Function GetThreadErrorMode: DWORD;
begin
Result := VAR_GetThreadErrorMode;
end;

//------------------------------------------------------------------------------

Function SetThreadErrorMode(dwNewMode: DWORD; lpOldMode: LPDWORD): BOOL;
begin
Result := VAR_SetThreadErrorMode(dwNewMode,lpOldMode);
end;

{$ENDIF}

{===============================================================================
    Functions - implementation
===============================================================================}
{-------------------------------------------------------------------------------
    Functions - utility functions
-------------------------------------------------------------------------------}

Function Symbol(const Name: String; AddressVar: PPointer): TDLUSymbol;
begin
Result.Name := Name;
Result.AddressVar := AddressVar;
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function Symbol(AddressVar: PPointer; const Name: String): TDLUSymbol;
begin
Result.Name := Name;
Result.AddressVar := AddressVar;
end;

{-------------------------------------------------------------------------------
    Functions - core functions
-------------------------------------------------------------------------------}

Function CheckLibrary(LibraryHandle: TDLULibraryHandle): Boolean;
begin
{$IFDEF Windows}
Result := LibraryHandle <> 0;
{$ELSE}
Result := Assigned(LibraryHandle);
{$ENDIF}
end;

//------------------------------------------------------------------------------

{$IFDEF FPCDWM}{$PUSH}W5024{$ENDIF}
Function OpenLibrary(const LibFileName: String; SilentCriticalErrors: Boolean = False): TDLULibraryHandle;
{$IFDEF Windows}
var
  OldErrorMode: DWORD;
begin
OldErrorMode := 0;
If SilentCriticalErrors then
  begin
    OldErrorMode := GetThreadErrorMode;
    SetThreadErrorMode(OldErrorMode or SEM_FAILCRITICALERRORS,nil);
  end;
try
  Result := LoadLibraryEx(PSysChar(StrToSys(LibFileName)),0,0);
finally
  If SilentCriticalErrors then
    SetThreadErrorMode(OldErrorMode,nil);
end;
end;
{$ELSE}
begin
Result := dlopen(PSysChar(StrToSys(LibFileName)),RTLD_NOW);
end;
{$ENDIF}
{$IFDEF FPCDWM}{$POP}{$ENDIF}

//------------------------------------------------------------------------------

Function OpenAndCheckLibrary(const LibFileName: String; SilentCriticalErrors: Boolean = False): TDLULibraryHandle;
begin
Result := OpenLibrary(LibFileName,SilentCriticalErrors);
If not CheckLibrary(Result) then
  raise EDLULibraryOpenError.CreateFmt('OpenLibrary: Failed to open library "%s".',[LibFileName]);
end;

//------------------------------------------------------------------------------

procedure CloseLibrary(var LibraryHandle: TDLULibraryHandle);
begin
If CheckLibrary(LibraryHandle) then
  begin
  {$IFDEF Windows}
    FreeLibrary(LibraryHandle);
    LibraryHandle := 0;
  {$ELSE}
    dlclose(LibraryHandle); // it can fail, but let's ignore it here
    LibraryHandle := nil;
  {$ENDIF}
  end;
end;

//------------------------------------------------------------------------------

Function GetSymbolAddr(LibraryHandle: TDLULibraryHandle; const SymbolName: String): Pointer;
begin
If CheckLibrary(LibraryHandle) then
{$IFDEF Windows}
  Result := GetProcAddress(LibraryHandle,PSysChar(StrToSys(SymbolName)))
{$ELSE}
  Result := dlsym(LibraryHandle,PSysChar(StrToSys(SymbolName)))
{$ENDIF}
else
{$IFDEF FPCDWM}{$PUSH}W4055{$ENDIF}
  raise EDLUInvalidParameter.CreateFmt('GetSymbolAddr: Invalid library handle (0x%p).',[Pointer(LibraryHandle)]);
{$IFDEF FPCDWM}{$POP}{$ENDIF}
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function GetSymbolAddr(LibraryHandle: TDLULibraryHandle; const SymbolName: String; out Address: Pointer): Boolean;
begin
If CheckLibrary(LibraryHandle) then
  begin
  {$IFDEF Windows}
    Address := GetProcAddress(LibraryHandle,PSysChar(StrToSys(SymbolName)));
    Result := Assigned(Address);
  {$ELSE}
  {
    dlsym can return a VALID nil value, to check for errors, we have to look
    into what dlerror function returns after a call to dlsym, if it does not
    return anything (null/nil), we can assume no error has occured
  }
    dlerror;  // clear last error
    Address := dlsym(LibraryHandle,PSysChar(StrToSys(SymbolName)));
    Result := not Assigned(dlerror);
  {$ENDIF}
  end
{$IFDEF FPCDWM}{$PUSH}W4055{$ENDIF}
else raise EDLUInvalidParameter.CreateFmt('GetSymbolAddr: Invalid library handle (0x%p).',[Pointer(LibraryHandle)]);
{$IFDEF FPCDWM}{$POP}{$ENDIF}
end;

//------------------------------------------------------------------------------

Function GetAndCheckSymbolAddr(LibraryHandle: TDLULibraryHandle; const SymbolName: String): Pointer;
{$IFNDEF Windows}
var
  ErrorMsg: PSysChar;
{$ENDIF}
begin
If CheckLibrary(LibraryHandle) then
  begin
  {$IFDEF Windows}
    Result := GetProcAddress(LibraryHandle,PSysChar(StrToSys(SymbolName)));
    If not Assigned(Result) then
      raise EDLUSymbolError.CreateFmt('GetAndCheckSymbolAddr: Unable to resolve symbol "%s" (0x%.8x).',[SymbolName,GetLastError]);
  {$ELSE}
    dlerror;  // clear last error
    Result := dlsym(LibraryHandle,PSysChar(StrToSys(SymbolName)));
    ErrorMsg := dlerror;
    If Assigned(ErrorMsg) then
      raise EDLUSymbolError.CreateFmt('GetAndCheckSymbolAddr: Unable to resolve symbol "%s" (%s).',[SymbolName,SysToStr(ErrorMsg)]);
  {$ENDIF}
  end
{$IFDEF FPCDWM}{$PUSH}W4055{$ENDIF}
else raise EDLUInvalidParameter.CreateFmt('GetAndCheckSymbolAddr: Invalid library handle (0x%p).',[Pointer(LibraryHandle)]);
{$IFDEF FPCDWM}{$POP}{$ENDIF}
end;

{-------------------------------------------------------------------------------
    Functions - macro functions
-------------------------------------------------------------------------------}

Function ResolveSymbolNames(LibraryHandle: TDLULibraryHandle; const Names: array of String; Addresses: array of PPointer; FailOnUnresolved: Boolean = False): Integer;
var
  i:  Integer;
begin
Result := 0;
If Length(Names) = Length(Addresses) then
  begin
    If FailOnUnresolved then
      begin
        For i := Low(Names) to High(Names) do
          Addresses[i]^ := GetAndCheckSymbolAddr(LibraryHandle,Names[i]);
        Result := Length(Names);
      end
    else
      begin
        For i := Low(Names) to High(Names) do
          If GetSymbolAddr(LibraryHandle,Names[i],Addresses[i]^) then
            Inc(Result);
      end;
  end
else raise EDLUInvalidParameter.CreateFmt('ResolveSymbolNames: Length of arrays do not match (%d,%d).',[Length(Names),Length(Addresses)]);
end;

//------------------------------------------------------------------------------

Function ResolveSymbol(LibraryHandle: TDLULibraryHandle; Symbol: TDLUSymbol; FailOnUnresolved: Boolean = False): Boolean;
begin
If FailOnUnresolved then
  begin
    Symbol.AddressVar^ := GetAndCheckSymbolAddr(LibraryHandle,Symbol.Name);
    Result := True; // if something would be wrong, the GetAndCheckSymbolAddr would raise an exception
  end
else Result := GetSymbolAddr(LibraryHandle,Symbol.Name,Symbol.AddressVar^);
end;

//------------------------------------------------------------------------------

Function ResolveSymbols(LibraryHandle: TDLULibraryHandle; Symbols: TStringList; FailOnUnresolved: Boolean = False): Integer;
var
  i:        Integer;
  TempPtr:  Pointer;
begin
Result := 0;
If FailOnUnresolved then
  begin
    For i := 0 to Pred(Symbols.Count) do
      Symbols.Objects[i] := TObject(GetAndCheckSymbolAddr(LibraryHandle,Symbols[i]));
    Result := Symbols.Count;
  end
else
  begin
    For i := 0 to Pred(Symbols.Count) do
      If GetSymbolAddr(LibraryHandle,Symbols[i],TempPtr) then
        begin
          Symbols.Objects[i] := TObject(TempPtr);
          Inc(Result);
        end
      else Symbols.Objects[i] := nil;
  end;
end;

//------------------------------------------------------------------------------

Function ResolveSymbols(LibraryHandle: TDLULibraryHandle; Symbols: array of TDLUSymbol; FailOnUnresolved: Boolean = False): Integer; overload;
var
  i:  Integer;
begin
Result := 0;
For i := Low(Symbols) to High(Symbols) do
  If ResolveSymbol(LibraryHandle,Symbols[i],FailOnUnresolved) then
    Inc(Result);
end;

//------------------------------------------------------------------------------

Function OpenLibraryAndResolveSymbolNames(const LibFileName: String; out LibraryHandle: TDLULibraryHandle;
  const Names: array of String; Addresses: array of PPointer; FailOnUnresolved: Boolean = False; SilentCriticalErrors: Boolean = False): Integer;
begin
LibraryHandle := OpenAndCheckLibrary(LibFileName,SilentCriticalErrors);
try
  Result := ResolveSymbolNames(LibraryHandle,Names,Addresses,FailOnUnresolved);
except
  CloseLibrary(LibraryHandle);
  raise;  // re-raise exception
end;
end;

//------------------------------------------------------------------------------

Function OpenLibraryAndResolveSymbols(const LibFileName: String; out LibraryHandle: TDLULibraryHandle;
  Symbols: TStringList; FailOnUnresolved: Boolean = False; SilentCriticalErrors: Boolean = False): Integer;
begin
LibraryHandle := OpenAndCheckLibrary(LibFileName,SilentCriticalErrors);
try
  Result := ResolveSymbols(LibraryHandle,Symbols,FailOnUnresolved);
except
  CloseLibrary(LibraryHandle);
  raise;
end;
end;

//------------------------------------------------------------------------------

Function OpenLibraryAndResolveSymbols(const LibFileName: String; out LibraryHandle: TDLULibraryHandle;
  Symbols: array of TDLUSymbol; FailOnUnresolved: Boolean = False; SilentCriticalErrors: Boolean = False): Integer;
begin
LibraryHandle := OpenAndCheckLibrary(LibFileName,SilentCriticalErrors);
try
  Result := ResolveSymbols(LibraryHandle,Symbols,FailOnUnresolved);
except
  CloseLibrary(LibraryHandle);
  raise;
end;
end;

{-------------------------------------------------------------------------------
    Functions - backward compatibility
-------------------------------------------------------------------------------}

Function OpenLibrary(const LibFileName: String; out Context: TDLULibraryContext; SilentCriticalErrors: Boolean = False): Integer;
begin
Context := TDLULibraryContext(OpenAndCheckLibrary(LibFileName,SilentCriticalErrors));
Result := 1;
end;

{===============================================================================
    Unit initialization, finalization
===============================================================================}

{$IFDEF Windows}
initialization
  PEM_Initialize;
{$ENDIF}

end.
