{
  Include that unit in your applicatin just after the TraceTool unit to enable Stack tracing.

  You MUST have JEDI Code Library (JCL) installed to use that unit.
  See http://www.delphi-jedi.org then go to "JCL Code Library" section
  Or go directly to http://sourceforge.net/projects/jcl for download
  The actual code is tested with the JVCL 1.9 beta version.
  The JVCL 1.22 and the 1.9 are quite similar (just one flag is added)

  Stack tracing is not include by default in tracetool for some reasons :
  - Not everyone want to install JEDI.
  - Target size.
  - stack information is not always usefull.
  - Some other solutions exist for stack tracing.
    This unit is just a wrapper for the JCL way.
    You can write another wrapper (see the initialization section in this file)

  Important note : To display correctly the stack (and related), don't forget to set
                   'Detailed' Map in the Linker project options.

  Author : Thierry Parent
}

unit StackTrace;

interface

// file not found 'JclDebug' -> install JEDI Code Library and add to delphi library path
uses Classes , windows , SysUtils, TraceTool , JclDebug  ;

   procedure JCLDebug_SendStack  (NodeEx : ITraceNodeEx ; Level: Integer = 0) ;
   procedure JCLDebug_SendCaller (NodeEx : ITraceNodeEx ; Level: Integer = 0) ;
   function  JCLDebug_SendMethod (NodeEx : ITraceNodeEx ; const Addr: Pointer) : string ;
   function  IsMapFileDetailled  (const Addr: Pointer): boolean ;

var
   DisplayAllStack : boolean = false;

implementation

//------------------------------------------------------------------------------

function GetModulePath(const Module: HMODULE): string;
var
  L: Integer;
begin
  L := MAX_PATH + 1;
  SetLength(Result, L);
  L := Windows.GetModuleFileName(Module, Pointer(Result), L);
  SetLength(Result, L);
end;

//------------------------------------------------------------------------------

function ModuleFromAddr(const Addr: Pointer): HMODULE;
var
  MI: TMemoryBasicInformation;
begin
  VirtualQuery(Addr, MI, SizeOf(MI));
  if MI.State <> MEM_COMMIT then
    Result := 0
  else
    Result := HMODULE(MI.AllocationBase);
end;


//------------------------------------------------------------------------------

procedure JCLDebug_SendStack (NodeEx : ITraceNodeEx; Level: integer = 0) ;
var
   i : integer ;
   group : TMemberNode ;
   isOneDebug : boolean ;
   startlevel : integer ;

   function GetLocationInfoStr(const Addr: Pointer): string;
   var
     Info: TJclLocationInfo;
     Module : HMODULE;
   begin
     Module := ModuleFromAddr(Addr);

     Result := Format('[%p]{%-12s} ', [Addr,ExtractFileName(GetModulePath(Module))]);

     if GetLocationInfo(Addr, Info) then
     begin
       if Info.LineNumber > 0 then
         Result := result + Format('%s.%s (Line %u)', [Info.UnitName, Info.ProcedureName, Info.LineNumber])
       else
         if Info.UnitName <> '' then
           Result := result + Format('%s.%s', [Info.UnitName, Info.ProcedureName])
         else
           Result := result + Format('%s', [Info.ProcedureName]);
     end
   end;
begin

   isOneDebug := false ;
   group := TMemberNode.create ('Stack information') ;
   group.ViewerKind := CST_VIEWER_STACK ;
   NodeEx.Members.Add (group) ;

   with TJclStackInfoList.Create(DisplayAllStack, 0, nil) do
   try
     if DisplayAllStack then
        startlevel := 3+Level
     else
        startlevel := 1+Level ;

     for I := startlevel to Count - 1 do begin

        // Error here ? replace .CallerAdr by .CallerAddr 
        // Depend of the Jedi version you use.

        if IsMapFileDetailled (Items[I].CallerAdr) then   // .CallerAddr 
           isOneDebug := true ;
        group.Add (GetLocationInfoStr(Items[I].CallerAdr)) ;
     end ;
        if isOneDebug = false then begin
           group.Add ('->No detailed map file') ;
           group.Add ('->See Project/Options/Linker') ;
        end ;

   finally
      Free;
   end;
end ;

//------------------------------------------------------------------------------

procedure JCLDebug_SendCaller (NodeEx : ITraceNodeEx ; Level: Integer = 0) ;
var
   group : TMemberNode ;
   ptr : Pointer ;
begin
   group := TMemberNode.create ('Caller information') ;
   group.ViewerKind := CST_VIEWER_STACK ;
   NodeEx.Members.Add (group) ;
   ptr := Caller(Level+1) ;
   group.Add (GetLocationInfoStr (ptr)); // all other options have default value (false)
   if IsMapFileDetailled (ptr) = false then begin
      group.Add ('->No detailed map file') ;
      group.Add ('->See Project/Options/Linker') ;
   end ;
end ;

//------------------------------------------------------------------------------

function JCLDebug_SendMethod (NodeEx : ITraceNodeEx ; const Addr: Pointer) : string;
var
  group : TMemberNode ;
begin
   result := GetLocationInfoStr (Addr) ;
   if NodeEx = nil then
      exit ;
   group := TMemberNode.create ('Method information') ;
   group.ViewerKind := CST_VIEWER_STACK ;
   NodeEx.Members.Add (group) ;
   group.Add (result) ;  // all other options have default value (false)
   if IsMapFileDetailled (Addr) = false then begin
      group.Add ('->No detailed map file') ;
      group.Add ('->See Project/Options/Linker') ;
   end ;
end ;

//------------------------------------------------------------------------------

function IsMapFileDetailled  (const Addr: Pointer): boolean ;
var
  Info : TJclLocationInfo;
begin
   result := false ;
   if GetLocationInfo(Addr, Info) = false then
      exit ;
   if Info.LineNumber <= 0 then
      exit  ;

   result := true ;
end ;

//------------------------------------------------------------------------------
//------------------------------------------------------------------------------
//------------------------------------------------------------------------------

initialization
   SendStackProc  := JCLDebug_SendStack  ;
   SendCallerProc := JCLDebug_SendCaller ;
   SendMethodProc := JCLDebug_SendMethod ;

end.
