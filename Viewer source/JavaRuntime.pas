{
Copyright (c) 1998-2001 Jonathan Revusky
All rights reserved.

Redistribution and use in source and binary forms, with or without
modification, are permitted provided that the following conditions
are met:

1. Redistributions of source code must retain the above copyright
notice, this list of conditions and the following disclaimer.
2. Redistributions in binary form must reproduce the above copyright
notice, this list of conditions and the following disclaimer in the
documentation and/or other materials provided with the distribution.
3. All advertising materials mentioning features or use of this software
must display the following acknowledgement:
This product includes software developed by Jonathan Revusky
4. The name of the author may not be used to endorse or promote products
derived from this software without specific prior written permission.

THIS SOFTWARE IS PROVIDED BY THE AUTHOR ``AS IS'' AND ANY EXPRESS OR
IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES
OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED.
IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR ANY DIRECT, INDIRECT,
INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT
NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
(INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF
THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
}

unit JavaRuntime;

// This unit is devoted to locating the JVM install directory
// and using the invocation API to create a JVM.
// All of the code here is Win32-specific heuristics.

interface

uses Classes, Windows, Registry, SysUtils, JNI, JUtils, JNIWrapper;

type

  JvmType = (SunJava1, SunJava2, MSJava);
  RuntimeOptions = set of JvmType;
  PPJavaVM = ^PJavaVM;

  TGetDefaultArgs = function (args : Pointer) : jint; stdcall;
  TCreateVM = function (vm : PPJavaVM ; penv : PPJNIEnv ; p : Pointer) : jint; stdcall;
  TGetCreatedVMs = function (vmBuf : PPJavaVM; buflen : Integer; nVMs : PJInt) : jint; stdcall;
  TExitProc = procedure (exitCode : jint) ; stdcall;
  TAbortProc = procedure ; stdcall;
  TPrintf = function (filepointer : Pointer ; const format : pchar ; args : va_list) : jint ; stdcall;

  EJvmException = class(Exception);
  EJavaRuntimeNotFound = class(Exception);
  EJavaRuntimeCreation = class(Exception);
  EClasspathException = class(Exception);

  TClassPath = class(TStringList)
  private
    // creates an instance based on a string.
    constructor Create;
  
    // Given a name of a class and its filename, perform a sanity check
    // to see if the fully qualified classname is consistent with this
    // filename classpath-wise.
    function sanityCheck(classname, filename : String) : String;
    // Performs similar sanity check on a .java file.
    function SanityCheckSource(filename : String) : String;
    procedure addDir(dir : String);
    procedure addPath(path : String);
  public
    function FullPath : String;
    class function getDefault : TClasspath;
    class function getBootPath : TClasspath;
  end;

    // class to encapsulate the location of the java runtime
    // and the use of the JNI invocation API.
  TJavaRuntime = class
  private
    FJava11 : Boolean;
    FHotspot : Boolean;
    FMS : Boolean;
    FJavaHome : String;
    FRuntimeLib : String;
    FJavaVM :  TJavaVM;
    DLLHandle : THandle;
    vmargs : JDK1_1InitArgs;
    vmargs2 : JavaVMInitArgs;
    FClasspath : TClasspath;
    FProperties : TStrings;
    FExitProc : TExitProc;
    FAbortProc : TAbortProc;
    FPrintf : TPrintf;
    FDebugPort, FVerbose, FDisableAsyncGC, FVerboseGC, FEnableClassGC,
    FVerifyMode, FCheckSource, FMinHeapSize, FMaxHeapSize, FJavaStackSize,
    FNativeStackSize : Integer;
    function FindJava11 : Boolean;
    function FindJava12 : Boolean;
    function FindMSJava : Boolean;
    function CheckJavaRegistryKey(key : String) : boolean;
    function GetClasspath : String;
    procedure setClasspath(S: String);
    procedure SetNativeStackSize(Size : Integer);
    procedure SetJavaStackSize(Size : Integer);
    procedure setMinHeapSize(Size : Integer);
    procedure setMaxHeapSize(Size : Integer);
    procedure setVerifyMode(Arg : Integer);
    procedure SetCheckSource(arg : Integer);
    procedure SetEnableClassGC(B : Boolean);
    procedure setVerboseGC(B:Boolean);
    procedure SetDisableAsyncGC(B: Boolean);
    procedure setVerbose(B : Boolean);
    procedure setDebugPort(Port : Integer);
    procedure setDebugging(Arg : Integer);
    procedure setAbortProc(proc : TAbortProc);
    procedure setExitProc(proc : TExitProc);
    procedure setPrintf(printproc : TPrintf);
    procedure Initialize; // Loads the DLL.
    procedure InitJava11;
    procedure InitJava2;
  public
    // processes a command-line option
    procedure processCommandLineOption(S : String);
    // processes a bunch of command line options passed in a container.
    procedure processCommandLine(Options : TStrings);
    procedure addProperty(S: String);
    function sanityCheck(classname, filename : String) : String;
    function sanityCheckSource(filename : String) : String;
    procedure addToClasspath(filename : String);
    function GetVM : TJavaVM; //Instantiates the JVM
    procedure CallMain(const ClassName : String ; args : TStrings);
    procedure CallExit(val : Integer);
    procedure Wait;
    property RuntimeLib : String read FRuntimeLib;
    property JavaHome : String read FJavaHome;
    property Classpath : String read getClasspath write setClasspath;
    property IsJava11 : Boolean read FJava11;
    property IsMS : Boolean read FMS;
    property Hotspot : Boolean read FHotspot write FHotspot;
    
    // write-only properties that only work before instantiating VM.
    property NativeStackSize : Integer write SetNativeStackSize;
    property JavaStackSize : Integer write SetJavaStackSize;
    property CheckSource : Integer write setCheckSource;
    property MinHeapSize : Integer write setMinHeapSize;
    property MaxHeapSize : Integer write setMaxHeapSize;
    property VerifyMode : Integer write setVerifyMode;
    property EnableClassGC : Boolean write setEnableClassGC;
    property VerboseGC : Boolean write setVerboseGC;
    property DisableAsyncGC : Boolean write setDisableAsyncGC;
    property Verbose : Boolean write setVerbose;
    property DebugPort : Integer write setDebugPort;
    property Debugging : Integer write setDebugging;
    property AbortProc : TAbortProc write setAbortProc;
    property ExitProc : TexitProc write setExitProc;
    property Printf : TPrintf write setPrintf;
    
    constructor Create(option : JvmType); overload ;
    constructor Create(jHome,RunTime : string; Is11 : boolean); overload ;

    destructor Destroy; override;
    class function GetDefault : TJavaRuntime;
    class procedure SetJava11(Java11 : Boolean);
    class procedure SetMSJava(MSJava : Boolean);
    class procedure setAppClassPath(path : String);
    class procedure setBasePath(path : String);
    class procedure setNeedTools(B : Boolean); // a bit of a hack for use by SmartJC.
    class procedure SetClassicVM(B : Boolean);
  end;
  
  function getPackageName(filename : String) : String;

implementation

var
  SystemDirBuf : Array[0..MAX_PATH] of WideChar;
  NeedsJDK : Boolean; // // True, if we need the sun.* classes for compilation, etc.
  Prefers11 : Boolean; // Do we look for java 1.1 first?
  PrefersMS : Boolean; // Do we look for MS JVM first?
  UseClassicVM : Boolean; // Do we use the classic VM?
  GetDefaultArgs : TGetDefaultArgs;
  CreateVM : TCreateVM;
  GetCreatedVMs : TGetCreatedVMs;
  instanceCount : Integer;
  searchrec : TSearchRec;
  AppClassPath : String; // classpath specified
  BasePath : String; // The class considered to be the base path, found from snooping in classfile.
  cpath : TClasspath; // the singleton TClasspath instance.
  bootpath : TClasspath; // the TClasspath that represents the boot path
  DefaultRuntime : TJavaRuntime; // singleton JavaRuntime instance.

const
  IBM_JRE_11_KEY  = '\SOFTWARE\IBM\IBM WIN32 Runtime Environment, Java(TM) Edition';
  IBM_JDK_117_KEY = '\SOFTWARE\IBM\IBM WIN32 Developer Kit, Java(TM) Edition\1.1.7';
  IBM_JDK_118_KEY = '\SOFTWARE\IBM\IBM WIN32 Developer Kit, Java(TM) Tech. Edition\1.1.8';
  PLUGIN_11_KEY   = '\SOFTWARE\JavaSoft\Java Plug-in\1.1';              // Home
  JRE_11_KEY      = '\SOFTWARE\JavaSoft\Java Runtime Environment\1.1';  // JavaHome, Microversion
  JB_KEY          = '\SOFTWARE\JavaSoft\Java Runtime\1.1.6';            // JavaHome
  JDK_11_KEY      = '\SOFTWARE\JavaSoft\Java Development Kit\1.1';      // JavaHome, Microversion
  JRE_12_KEY      = '\SOFTWARE\JavaSoft\Java Runtime Environment\1.2';  // JavaHome, RuntimeLib
  JRE_13_KEY      = '\SOFTWARE\JavaSoft\Java Runtime Environment\1.3';
  PLUGIN_12_KEY   = '\SOFTWARE\JavaSoft\Java Plug-in\1.2';              // JavaHome, RuntimeLib
  PLUGIN_13_KEY   = '\SOFTWARE\JavaSoft\Java Plug-in\1.3';              // JavaHome, RuntimeLib
  JDK_12_KEY      = '\SOFTWARE\JavaSoft\Java Development Kit\1.2';      // JavaHome, Microversion
  JDK_13_KEY      = '\SOFTWARE\JavaSoft\Java Development Kit\1.3';

  JRE11Keys : array[1..3] of String = (PLUGIN_11_KEY, IBM_JRE_11_KEY, JRE_11_KEY);
  JDK11Keys : array[1..4] of String = (IBM_JDK_118_KEY, IBM_JDK_117_KEY, JDK_11_KEY, JB_KEY);
  JRE12Keys : array[1..4] of String = (JRE_13_KEY, PLUGIN_13_KEY, JRE_12_KEY, PLUGIN_12_KEY);
  BootClasspath : String = '';

  procedure StripComments(var Line : String; var InComment : Boolean); forward;
  
  procedure TJavaRuntime.Initialize;
  begin
    if DLLHandle <> 0 then
      exit; // already initialized.
    DLLHandle := LoadLibrary(PChar(FRuntimeLib));
    if DLLHandle = 0 then 
      raise EJavaRuntimeCreation.Create('Could not load DLL ' + FRuntimeLib);
    @CreateVM := getProcAddress(DLLHandle, 'JNI_CreateJavaVM');
    @GetDefaultArgs := getProcAddress(DLLHandle, 'JNI_GetDefaultJavaVMInitArgs');
    @GetCreatedVMs := getProcAddress(DLLHandle, 'JNI_GetCreatedJavaVMs');
    if (@CreateVM = Nil) or (@GetDefaultArgs = Nil) or (@GetCreatedVMs = Nil) then
      raise EJavaRuntimeCreation.Create('Dynamic Link Library ' + FRuntimeLib + ' is not valid.');
    vmargs.version := $00010001;
    vmargs2.version := $00010002;
    GetDefaultArgs(@vmargs);
  end;
  
  function TJavaRuntime.GetVM : TJavaVM;
  var
    PVM : PJavaVM;
    penv : PJNIEnv;
    args : Pointer;
  begin
    if FJavaVM <> Nil then
    begin
      result := FJavaVM;
      Exit;
    end;
    if @CreateVM = Nil then 
      Initialize;
    if IsJava11 then 
    begin
      InitJava11;
      args := @vmargs;
    end
    else
    begin
      InitJava2;
      args := @vmargs2;
    end;
    if CreateVM(@pvm, @penv, args) <>0 then
      raise EJavaRuntimeCreation.Create('Could not create JVM');
    TJavaVM.setThreadPenv(penv);
    FJavaVM := TJavaVM.Create(PVM);
    result := FJavaVM;
  end;

  procedure TJavaRuntime.InitJava11;
  begin
    vmargs.properties := convertStrings(FProperties);
    vmargs.classpath := PChar(Classpath);
    vmargs.Verbose := FVerbose;
    vmargs.DisableAsyncGC := FDisableAsyncGC;
    vmargs.EnableVerboseGC := FVerboseGC;
    vmargs.EnableClassGC := FEnableClassGC;
    vmargs.CheckSource := FCheckSource;
    vmargs.VerifyMode := FVerifyMode;
    if Assigned(FExitProc) then
      vmargs.Exit := FExitProc;
    if Assigned(FAbortProc)  then 
      vmargs.abort := FAbortProc;
    if Assigned(FPrintf) then 
      vmargs.vfprintf := FPrintf;
    if FDebugPort <> 0 then 
      vmargs.DebugPort := FDebugPort;
    if FMinHeapSize >0 then 
      vmargs.MinHeapSize := FMinHeapSize;
    if FMaxHeapSize >0 then 
      vmargs.MaxHeapSize := FMaxHeapSize;
    if FJavaStackSize >0 then 
      vmargs.JavaStackSize := FJavaStackSize;
    if FNativeStackSize >0 then 
      vmargs.NativeStackSize := FNativeStackSize;
  end;

  procedure TJavaRuntime.InitJava2;
  var
    I : Integer;
    S : String;
    PVMOption, PVO : PJavaVMOption;
  begin
      // Just handle classpath and properties for now.
      
    VMArgs2.Noptions := 1+ FProperties.Count;
    if (FVerbose <>0) or (FVerboseGC <>0) then inc(VMArgs2.Noptions);
    if FVerboseGC <>0 then inc(VMArgs2.Noptions);
    if FMinHeapSize >0 then inc(VMArgs2.Noptions);
    if FMaxHeapSize >0 then inc(VMArgs2.Noptions);
    if BootClasspath <> '' then inc(VMArgs2.Noptions);
    if FEnableClassGC<>0 then inc(VMArgs2.NOptions);
    if Assigned(FExitProc) then inc(VMargs2.NOptions);
    if Assigned(FAbortProc)  then inc(VMArgs2.NOptions);
    if Assigned(FPrintf) then inc(VMArgs2.NOptions);
      
    vmargs2.ignoreUnrecognized := True;
    PVMOption := AllocMem(sizeof(JavaVMOPtion) * VMargs2.NOptions);
    PVO := PVMOption;
    S := '-Djava.class.path=' + Classpath;
    PVMOption^.optionString := StrNew(PChar(S));
    PVMOption^.extraInfo := Nil;
    inc(PVO);
      
    for I:= 0 to  FProperties.Count -1 do
    begin
      S := '-D' + FProperties[I];
      PVO^.optionString := StrNew(PChar(S));
      inc(PVO);
    end;
      
    if (FVerbose<>0) or (FVerboseGC <>0) then 
    begin
      S := '-verbose:';
      if FVerbose<>0
        then S := S + 'class';
        if FVerboseGC<>0
          then S := S + ',';
      if FVerboseGC<>0
        then S := S + 'gc';
      PVO^.optionString := StrNew(Pchar(S));
      inc(PVO);
    end;

    if FMinHeapSize>0 then
    begin
      PVO^.optionString := StrNew(PChar('-Xms' + IntToStr(FMinHeapSize)));
      inc(PVO);
    end;
      
    if FMaxHeapSize>0 then
    begin
      PVO^.optionString := StrNew(PChar('-Xmx' + IntToStr(FMaxHeapSize)));
      inc(PVO);
    end;
      
    if FEnableClassGC <>0 then
    begin
      PVO^.optionString := StrNew('-Xnoclassgc');
      inc(PVO);
    end;
      
    if BootClasspath <> '' then
    begin
      PVO^.optionString := StrNew(PChar('-Xbootclasspath/p:' + BootClasspath));
      inc(PVO);
    end;

    if Assigned(FPrintf) then 
    begin
      PVO^.optionString := StrNew(PChar('exit'));
      PVO^.ExtraInfo := @FPrintf;
      inc(PVO);
    end;

    if Assigned(FExitProc) then
    begin
      PVO^.optionString := StrNew(PChar('exit'));
      PVO^.ExtraInfo := @FExitProc;
      inc(PVO);
    end;

    if Assigned(FAbortProc) then
    begin
      PVO^.optionString := StrNew(PChar('abort'));
      PVO^.ExtraInfo := @FAbortProc;
    end;
      
    vmargs2.options := PVMOption;
    vmargs2.version := $00010002;
  end;

  
  //convenience wrappers.
  
  procedure TJavaRuntime.CallMain(const ClassName : String ; args : TStrings);
  begin
    TJavaVM.CallMain(className, args);
  end;
  
  procedure TJavaRuntime.Wait;
  begin
    if FJavaVM <> Nil then 
    begin
      if isMS then 
        Sleep(INFINITE)
      else 
        FJavaVM.Wait;
    end;
  end;
  
  procedure TJavaRuntime.CallExit(val : Integer);
  begin
    TJavaVm.CallExit(val);
  end;

  procedure TJavaRuntime.processCommandLineOption(S : String);
  var
    L  : String;
    function extractSize(S : String) : Integer;
    begin
      if S[length(S)] = 'k' 
        then Result := $400
      else 
        if S[length(S)] = 'm' 
          then Result := $100000
        else Result  := 1;
      if Result<>1 
        then S:= Copy(S, 1, length(S)-1);
      Result := Result * StrToIntDef(S, 0);
    end;
  begin
    L  := LowerCase(S);
    if (L = '-v') or (L = 'verbose') 
      then Verbose := true
    else if (L = '-verbosegc') 
      then VerboseGC := true
    else if (L = '-noasync') 
      then DisableAsyncGC := true
    else if (L = '-noclassgc') 
      then EnableClassGC := false
    else if (L = '-verify') 
      then VerifyMode := 2
    else if (L = '-noverify') 
      then VerifyMode := 0
    else if (L = '-verifyremote') 
      then VerifyMode :=1
    else if (L = '-nojit') 
      then addProperty('java.compiler=')
    else if Copy(L, 1, 3) = '-cp' 
      then FClasspath.addPath(Copy(S, 5, length(S)))
    else if Copy(L, 1, 10) = '-classpath' 
      then FClasspath.addPath(Copy(S, 12, length(S)))
    else if Copy(L, 1, 2) = '-d' 
      then addProperty(Copy(S, 3, length(S)))
    else if Copy(L, 1, 3) = '-ms' 
      then MinHeapSize := ExtractSize(Copy(L, 4, length(L)))
    else if Copy(L, 1, 3) = '-mx' 
      then MaxHeapSize := ExtractSize(Copy(L, 4, length(L)))
    else if Copy(L, 1, 3) = '-ss' 
      then NativeStackSize := ExtractSize(Copy(L, 4, length(L)))
    else if Copy(L, 1, 3) = '-oss' 
      then NativeStackSize := ExtractSize(Copy(L, 5, length(L)));
  end;
  
  procedure TJavaRuntime.processCommandLine(Options : TStrings);
  var
    I: Integer;
  begin
    for I:= 0 to Options.Count-1 do 
      processCommandLineOption(Options[I]);
  end;
  
  class function TJavaRuntime.GetDefault : TJavaRuntime;
  var
    FirstChoice, SecondChoice, ThirdChoice, temp : JvmType;
  begin
    if DefaultRuntime = Nil then
    begin
      FirstChoice := SunJava2;
      SecondChoice := SunJava1;
      ThirdChoice := MSJava;
      if PrefersMS then
      begin
        FirstChoice := MSJava;
        SecondChoice := SunJava2;
        ThirdChoice := SunJava1;
      end;
      if Prefers11 then
      begin
        temp := FirstChoice;
        FirstChoice := SunJava1;
        SecondChoice := Temp;
      end;
      try
        DefaultRuntime := TJavaRuntime.Create(FirstChoice);
      except on EJavaRuntimeNotFound do
        try
          DefaultRuntime := TJavaRuntime.Create(SecondChoice);
        except on EJavaRuntimeNotFound do
          DefaultRuntime :=TJavaRuntime.Create(ThirdChoice);
        end;
      end;
    end;
    result := DefaultRuntime;
  end;

  class procedure TJavaRuntime.SetJava11(Java11 : Boolean);
  begin
    Prefers11 := Java11;
  end;

  class procedure TJavaRuntime.SetClassicVM(B : Boolean);
  begin
    UseClassicVM := B;
  end;
  
  class procedure TJavaRuntime.SetMSJava(MSJava : Boolean);
  begin
    PrefersMS := MSJava;
  end;
  
  class procedure TJavaRuntime.setNeedTools(B : Boolean);
  begin
    NeedsJDK := True;
  end;

  procedure TJavaRuntime.addToClasspath(filename : String);
  begin
    FClasspath.addDir(filename);
  end;
  
  function TJavaRuntime.getClasspath : String;
  var
    CPath : TClasspath;
    Reg : TRegistry;
    GotKey : Boolean;
  begin
    CPath := TClasspath.getDefault;
    if ((not FJava11) and NeedsJDK) then 
    begin
      reg := TRegistry.Create;
      reg.RootKey := HKEY_LOCAL_MACHINE;
      GotKey := Reg.OpenKey(JDK_13_KEY, false);
      if not GotKey then
        GotKey := Reg.OpenKey(JDK_12_KEY, false);
      if GotKey then
      begin
        if reg.ValueExists('JavaHome') then 
          CPath.addDir(reg.ReadString('JavaHome') + '\lib\tools.jar');
      end;
      reg.Free;
    end;
    result := CPath.Fullpath;
  end;
  
  procedure TJavaRuntime.setClasspath(S : String);
  begin
    FClasspath := TClasspath.getDefault;
    FClasspath.addPath(S);
  end;

  constructor TJavaRuntime.Create(jHome,RunTime : string; Is11 : boolean);
  begin
     if DefaultRuntime <> Nil then
        raise EJavaRuntimeCreation.Create('Can only instantiate one Java runtime per process');
     FJava11 := is11 ;
     FJavaHome := jHome ;
     FRuntimeLib := RunTime ;

     DefaultRuntime := Self; // set the singleton
     FClasspath := TClasspath.getDefault;
     FProperties := TStringList.Create;
     FVerifyMode := 1;
  end ;

  constructor TJavaRuntime.Create(option : JvmType);
  begin
    if DefaultRuntime <> Nil then
      raise EJavaRuntimeCreation.Create('Can only instantiate one Java runtime per process');
    case option of
      SunJava1 :
        if not FindJava11 then
          raise EJavaRuntimeNotFound.Create('Java 1.1 runtime not found');
      SunJava2 :
        if not FindJava12 then
          raise EJavaRuntimeNotFOund.Create('Java 2 runtime not found');
      MSJava :
        if not FindMSJava then
          raise EJavaRuntimeNotFound.Create('MS Java runtime not found');
    end;
    DefaultRuntime := Self; // set the singleton
    FClasspath := TClasspath.getDefault;
    FProperties := TStringList.Create;
    FVerifyMode := 1;
  end;

  destructor TJavaRuntime.Destroy;
  begin
    DefaultRuntime := Nil;
      if (dllHandle <>0) and (instanceCount = 0) then
        if FreeLibrary(dllHandle) then 
          dllHandle := 0;
    FProperties.free ;
    inherited Destroy;
  end;
  
  function TJavaRuntime.FindMSJava : Boolean;
  var
    DLLPath : String;
  begin
    result := False;
    GetSystemDirectory(SystemDirBuf, MAX_PATH);
    DLLPath := String(SystemDirBuf) + '\msjava.dll';
    if FileExists(DLLPath) then 
    begin
      FJava11 := true;
      FRuntimeLib := DLLPath;
      FJavaHome := String(SystemDirBuf);
      FMS := true;
      result := true;
    end;
  end;
  
  function TJavaRuntime.FindJava12 : Boolean;
  var
    I : Integer;
  begin
    result := false;
    for I:=Low(JRE12Keys) to High(JRE12Keys) do 
      if (CheckJavaRegistryKey(JRE12Keys[I])) then
      begin
        FJava11 := false; //This is a 1.2 VM.
        result := true; // success!
        Exit;
      end;
  end;

{heuristics to find a java 1.1 runtime.}

  function TJavaRuntime.FindJava11 : Boolean;
  var
    I: Integer;
  begin
    // First look on the system path.
    FRuntimeLib := FindOnSystemPath('javai.dll');
    if FRuntimeLib <> '' then
    begin
      FJavaHome := ExtractFileDir(ExtractFileDir(FRuntimeLib));
      result := true;
      FJava11 := true;
      exit; // success!
    end;
  
  // Failing that, search the Windows registry for location.
  
    if not needsJDK then
    begin
      for I:=Low(JRE11Keys) to High(JRE11Keys) do
      begin
        if (CheckJavaRegistryKey(JRE11Keys[I])) then
        begin
          result := true; // success!
          FJava11 := true;
          Exit;
        end;
      end;
    end;
    for I:=Low(JDK11Keys) to High(JDK11Keys) do
    begin
      if (CheckJavaRegistryKey(JDK11Keys[I])) then
      begin
        result := true; // success!
        FJava11 := true;
        Exit;
      end;
    end;
    result := false; // failure.
  end;

  {Checks the Java registry key given as an argument.
  Returns true on success and sets the FJavaLib and FJavaHome
  fields}

  function TJavaRuntime.CheckJavaRegistryKey(key : String) : boolean;
  var
    reg : TRegistry;
    S, HotspotLib : String;
  begin
    result := false;
    reg := TRegistry.Create;
    try
      reg.RootKey := HKEY_LOCAL_MACHINE;
      if reg.OpenKey(key, false) then
      begin
        if reg.ValueExists('RuntimeLib') then
          begin
            S:= reg.ReadString('RuntimeLib');
            if S = '' then S := reg.ReadString('JavaHome') + '\bin\classic\jvm.dll';
            if FileExists(S) then
              begin
                result := true;
                if not UseClassicVM then
                  begin
                    HotspotLib := ExtractFileDir(ExtractFileDir(S)) + '\hotspot\jvm.dll';
                    if FileExists(HotspotLib) then begin
                      S := HotspotLib;
                      FHotspot := True;
                    end;
                  end;
                FRuntimeLib := S;
                if reg.ValueExists('JavaHome') then
                  FJavaHome := reg.ReadString('JavaHome')
                else
                  FJavaHome := ExtractFileDir(ExtractFileDir(ExtractFileDir(FRuntimeLib)));
              end;
            Exit;
          end
        else begin
          if reg.ValueExists('JavaHome') then
            S := reg.ReadString('JavaHome')
          else if reg.valueExists('Home') then
              S := reg.ReadString('Home')
          else if reg.valueExists('java_home') then
              S := reg.ReadString('java_home')
          else
            Exit; // failure!
        end;
      end
      else
        Exit;

     // Now check that it's really there.
      if S[length(S)] = Chr(92) then
        S := Copy(S, 1, length(S)-1);
      if FileExists(S + '\bin\javai.dll') then
      begin
        FRuntimeLib := S + '\bin\javai.dll'; // Success!
        FJavaHome := S;
        result := true;
      end;
    finally
      Reg.Free;
    end;
  end;

  procedure TJavaRuntime.SetNativeStackSize(Size : Integer);
  begin
    if Size > 0 then
       FNativeStackSize := Size;
  end;


  procedure TJavaRuntime.SetJavaStackSize(Size : Integer);
  begin
    if Size > 0 then 
       FJavaStackSize := Size;
  end;
  
  procedure TJavaRuntime.setMinHeapSize(Size : Integer);
  begin
    if Size  > 0 then 
       FMinHeapSize := Size;
  end;
  
  procedure TJavaRuntime.setMaxHeapSize(Size : Integer);
  begin
    if Size  > 0 then 
       FMaxHeapSize := Size;
  end;
  
  procedure TJavaRuntime.setVerifyMode(Arg : Integer);
  begin
    FVerifyMode := Arg;
  end;
  
  procedure TJavaRuntime.SetCheckSource(arg : Integer);
  begin
    FCheckSource := arg;
  end;
  
  procedure TJavaRuntime.SetEnableClassGC(B : Boolean);
  begin
    FEnableClassGC := Integer(B); 
  end;
  
  procedure TJavaRuntime.setVerboseGC(B:Boolean);
  begin
    FVerboseGC := Integer(B);
  end;
  
  procedure TJavaRuntime.SetDisableAsyncGC(B: Boolean);
  begin
    FDisableAsyncGC := Integer(B);
  end;
  
  procedure TJavaRuntime.setVerbose(B : Boolean);
  begin
    FVerbose := Integer(B);
  end;
  
  procedure TJavaRuntime.setDebugPort(Port : Integer);
  begin
    FDebugPort := Port;
  end;
  
  procedure TJavaRuntime.setDebugging(Arg : Integer);
  begin
  end;
  
  procedure TJavaRuntime.setAbortProc(proc : TAbortProc);
  begin
    FAbortproc := proc;
  end;
  
  procedure TJavaRuntime.setExitProc(proc : TExitProc);
  begin
    FExitProc := Proc;
  end;
  
  procedure TJavaRuntime.setPrintf(printproc : TPrintf);
  begin
    fprintf := printproc;
  end;
  
  function TJavaRuntime.sanityCheck(classname, filename : String) : String;
  begin
    result := FClasspath.sanityCheck(classname, filename);
  end;
  
  function TJavaRuntime.sanityCheckSource(filename : String) : String;
  begin
    result := FClasspath.sanityCheckSource(filename);
  end;
  
  procedure TJavaRuntime.addProperty(S: String);
  begin
    FProperties.add(S);
  end;

  procedure addAllArchives(C : TClasspath; directory, pattern : String);
  begin
    if FindFirst(Directory + pattern, faAnyFile, searchrec) = 0 then
    begin
      cpath.addDir(Directory + searchrec.Name);
      while FindNext(searchrec) = 0 do 
        cpath.addDir(Directory + searchrec.Name);
    end;
    FindClose(searchrec);
  end;

  
  class function TClasspath.getDefault : TClassPath;
  var
    Home, libjars, ThirdPartyDir : String;
    Runtime : TJavaRuntime;
    SearchRec : TSearchRec;
  begin
    if cpath = Nil then
    begin
      cpath := TClasspath.Create;
      Runtime := TJavaRuntime.GetDefault;
      Home := Runtime.JavaHome;


      if FileExists(Home + '\classes') then cpath.addDir(Home + '\classes');

      // Now see if there are any other jars or zips in there and add them.

      Libjars := Home + '\lib\*.jar';
      addAllArchives(cpath, Home + '\lib\ext\', '*.jar');
      addAllArchives(cpath, Home + '\lib\ext\', '*.zip');
      addAllArchives(cpath, Home + '\lib\', '*.jar');
      addAllArchives(cpath, Home + '\lib\', '*.zip');
//          if BaseClassPath = '' then
//             BaseClassPath := GetEnvironmentString('CLASSPATH');

      ThirdPartyDir := GetEnvironmentString('JARS_DIR');
      if ThirdPartyDir = '' then ThirdPartyDir := '.';
      if ThirdPartyDir[Length(ThirdPartyDir)] <> '\' then
        ThirdPartyDir := ThirdPartyDir + '\';

      if FindFirst(ThirdPartyDir + '*.jar', 0, searchRec) = 0 then
      repeat
        cpath.AddDir(ThirdPartyDir + searchRec.name);
      until FindNext(SearchRec) <> 0;

      cpath.addPath(GetEnvironmentString('CLASSPATH'));
      cpath.addPath(AppClassPath);
      cpath.addPath(BasePath);
      cpath.addDir(getCurrentDir); // Maybe better off without this.
    end;
    result := cpath;
  end;


  class function TClasspath.getBootPath : TClassPath;
  var
    Home, ThirdPartyDir : String;
    SearchRec : TSearchRec;
  begin
    if bootpath = Nil then
    begin
      bootpath := TClasspath.Create;
      Home := TJavaRuntime.GetDefault.JavaHome;
      if FileExists(Home + '\classes') then 
        cpath.addDir(Home + '\classes');

      // Now see if there are any other jars or zips in there and add them.

      addAllArchives(cpath, Home + '\lib\ext\', '*.jar');
      addAllArchives(cpath, Home + '\lib\ext\', '*.zip');
      addAllArchives(cpath, Home + '\lib\', '*.jar');
      addAllArchives(cpath, Home + '\lib\', '*.zip');
//          if BaseClassPath = '' then
//             BaseClassPath := GetEnvironmentString('CLASSPATH');

      ThirdPartyDir := GetEnvironmentString('JARS_DIR');
      if ThirdPartyDir = '' then ThirdPartyDir := '.';
      if ThirdPartyDir[Length(ThirdPartyDir)] <> '\' then
        ThirdPartyDir := ThirdPartyDir + '\';

      if FindFirst(ThirdPartyDir + '*.jar', 0, searchRec) = 0 then
      repeat
        cpath.AddDir(ThirdPartyDir + searchRec.name);
      until FindNext(SearchRec) <> 0;

      cpath.addPath(GetEnvironmentString('CLASSPATH'));
      cpath.addPath(AppClassPath);
      cpath.addPath(BasePath);
      cpath.addDir(getCurrentDir); // Maybe better off without this.
    end;
    result := cpath;
  end;

  constructor TClasspath.Create;
  begin
  end;


  procedure TClasspath.addPath(Path : String);
  var
    Len: Integer;
    Dirs  : TStringList;
    I : Integer;
  begin
    Dirs  := TStringList.Create;
    repeat
      Len := Pos(';', Path);
      if Len > 1 then 
        Dirs.add(Copy(Path, 1, Len-1));
      Path := Copy(Path, Len +1, Length(Path));
    until Len=0;
    if length(Path)>0 then 
      Dirs.add(Path);
    for I := Dirs.Count downto 1 do
      addDir(Dirs[I-1]);
    Dirs.Free;
  end;
  
  procedure TClasspath.addDir(dir : String);
  var
    S: String;
    I : Integer;
  begin
    S := ExpandFileName(dir);
    if (S[length(S)] ='\') and (S[length(S)-1] <> ':') then 
      S := Copy(S, 1, length(S)-1);
    I := IndexOf(S);
    if I>=0 then
      Delete(I);
    add(S);
  end;
  
  
  function TClasspath.FullPath : String;
  var
    I: Integer;
  begin
    result := '';
    for I:=Count downto 1 do
    begin
      if I < Count then 
        result := result + ';';
      result := result + Strings[I-1];
    end;
  end;


// Sets the part of the classpath that is specific to the app.
  
  class procedure TJavaRuntime.setAppClassPath(path : String);
  begin
    AppClassPath := path;
  end;

  procedure addAllFilesToPath(Directory, Pattern : String; var Path : String);
  begin
    if FindFirst(Directory + pattern, faAnyFile, searchrec) = 0 then
    begin
      Path := Path + ';' + Directory + searchrec.Name;
      while FindNext(searchrec) = 0 do 
        Path := Path + ';' + Directory + searchrec.Name;
    end;
      FindClose(searchrec);
  end;

  class procedure TJavaRuntime.setBasePath(Path : String);
  var
    Dir : String;
  begin
    BasePath := ExpandFileName(Path);
    Dir := ExtractFilePath(ExpandFileName(BasePath));
    addAllFilesToPath(Dir, '*.zip', BasePath);
    AddAllFilesToPath(Dir, '*.jar', BasePath);
    addAllFilesToPath(Dir + 'lib\', '*.zip', BasePath);
    AddAllFilesToPath(Dir + 'lib\', '*.jar', BasePath);
    addAllFilesToPath(Dir + 'libs\', '*.zip', BasePath);
    AddAllFilesToPath(Dir + 'libs\', '*.jar', BasePath);
{
    addAllFilesToPath(Dir + '..\lib\', '*.zip', BasePath);
    AddAllFilesToPath(Dir + '..\lib\', '*.jar', BasePath);
    addAllFilesToPath(Dir + '..\libs\', '*.zip', BasePath);
    AddAllFilesToPath(Dir + '..\libs\', '*.jar', BasePath);
}      
    if CPath <> Nil then
      CPath.AddPath(BasePath);
  end;

  function TClassPath.sanityCheck(classname, filename : String) : String;
  var
    fullFile, pathName, package, basePath, temp : String;
    I : Integer;
    Oops : Boolean;
  begin
    fullFile := ExpandFileName(filename);
    pathName := ExtractFileDir(fullfile);
    temp := toBackSlash(classname); // temp is string where the / is now \.
    for I := length(temp) downto 1 do
    begin
      if temp[I] = '\' then break;
    end;
    if I =0 then // no slashes, anonymous package
    begin
      addDir(pathName); // put the filename's path on the classpath
      setCurrentDirectory(PChar(pathName));
      Exit;
    end;
    package := Copy(temp, 1, I-1);
    Oops := Length(Package) > Length(PathName) - 3;
    if not Oops then
    begin
      Temp := Copy(PathName, 1+Length(PathName) - Length(Package), Length(Package));
      Oops := (LowerCase(Temp) <> LowerCase(Package));
    end;
    if Oops then // There is a problem.
      raise EClasspathException.Create('File ' + fullFile + ' should be on relative path ' +package);
    basePath := Copy(pathName, 1, length(PathName) - Length(Temp));
    addDir(basePath);
    result := BasePath;
  end;
  
  function TClasspath.SanityCheckSource(filename : String) : String;
  var
    Package, Classname : String;
  begin
    Package := getPackageName(Filename);
    Classname := Package + ExtractFileName(Filename);
    ChopExtension(Classname);
    result := SanityCheck(Classname, Filename);
  end;
  
  // Get the package name inside a source file.
  // This code is a bit messy. Maybe I'll clean it up later.
  
  function getPackageName(filename : String) : String;
  var
    T : TextFile;
    inComment : Boolean;
    Line : String;
    I : Integer;
  begin
   AssignFile(T, filename);
   Reset(T);
   inComment := false;
   while not Eof(T) do 
   begin
     ReadLn(T, Line);
     StripComments(Line, InComment);
     I := Pos('package', Line);
     if I>0 then 
     begin
       Result := Copy(Line, I+8, length(Line));
       I := Pos(';', Result);
       if I>0 then 
         begin
           Result := Trim(Copy(Result, 1, I-1));
           break;
         end;
       if Pos('{', Line)>0 then 
         break;
     end;
   end;
   CloseFile(T);
   if length(Result) > 0 then Result := Result + '.';
  end;
  
  procedure StripComments(var Line : String; var InComment : Boolean);
  var
    S : String;
    I : Integer;
  begin
    S := '';
    if InComment then 
    begin
      I := Pos('*/', Line);
      if I>0 then 
        begin
          Line := Copy(Line, 2+I, length(Line));
          InComment := False;
          StripComments(Line, InComment);
        end
      else 
        Line := '';
    end
    else begin
      I := Pos('/*', Line);
      if I>0 then 
        begin
          InComment := True;
          S := Copy(Line, 1, I-1);
          Line := Copy(Line, I+2, Length(Line));
          StripComments(Line, InComment);
        end;
      Line := S + Line;
    end;
    I := Pos('//', Line);
    if I>0 then 
      Line := Copy(Line, 1, I-1);
  end;
end.
