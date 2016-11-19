{*****************************************************************************
  Name           : PSCXP
  Author         : POL OFFICE COMMON
  Description    : Windows XP support
  History        :

  Date         By                   Description
  ----         --                   -----------
  27-10-2005      POL OFFICE COMMON     Initial creation of the Unit.
 *****************************************************************************}
{$I PSCControls.inc}

unit PSCXP;

interface
uses
  Windows,
  Messages,
  SysUtils,
  {$IFDEF VERSION7}
  UxTheme,
  {$ELSE}
  PSCTheme,
  {$ENDIF}
  Controls,
  ComCtrls;

function IsWindowsXP : boolean;
function CheckComCtl : boolean;

implementation

function CheckComCtl : Boolean;
begin
  Result := (HiWord(GetComCtlVersion ) >= 6);
end;

function IsWindowsXP : boolean;
var
 osvi : OSVERSIONINFO;
begin
  ZeroMemory(@osvi, sizeof(OSVERSIONINFO));
  osvi.dwOSVersionInfoSize := sizeof(OSVERSIONINFO);
  GetVersionEx(osvi);
  Result := (osvi.dwMajorVersion > 5) or ((osvi.dwMajorVersion = 5) and (osvi.dwMinorVersion >= 1));
end;

procedure InitXPLook;
begin
  if (IsWindowsXP and CheckComCtl) then
   begin
      InitThemeLibrary;
   end;
end;

initialization
 InitXPLook;

finalization
  FreeThemeLibrary;

end.

