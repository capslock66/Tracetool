{
  ABOUT Box

  Author : Thierry Parent

  HomePage :  http://www.codeproject.com/csharp/TraceTool.asp
  Download :  http://sourceforge.net/projects/tracetool/
  See License.txt for license information   

}

unit unt_about;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ShellAPI, ExtCtrls;

type
  TFrmAbout = class(TForm)
    Button1: TButton;
    Label5: TLabel;
    LabelAuthor: TLabel;
    Label7: TLabel;
    Label8: TLabel;
    Label12: TLabel;
    Panel3: TPanel;
    Label14: TLabel;
    Label16: TLabel;
    Label17: TLabel;
    Label18: TLabel;
    Label6: TLabel;
    LabelVersion: TLabel;
    Image1: TImage;
    Label2: TLabel;
    procedure lblWebPageClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure LabelAuthorDblClick(Sender: TObject);
    procedure Image1DblClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
    nbDblClick : integer ;
  end;

var
  FrmAbout: TFrmAbout;

implementation

{$R *.dfm}

uses unt_tool,unt_TraceWin ;


//------------------------------------------------------------------------------

function GetFileVersion(const Filename: string): string;
var
  VerInfSize, Sz: Cardinal;
  VerInfo: Pointer;
  FxFileInfo: PVSFixedFileInfo;

  function MSLSToString(MS, LS: DWORD): string;
  begin
    { Modify this text formatting by needs }
    Result := Format('%d.%d.%d Build %d',
      [MS SHR 16, MS AND $FFFF, LS SHR 16, LS AND $FFFF]);
  end;

begin
  Result := '';
  if FileExists(Filename) then
  begin
    VerInfSize := GetFileVersionInfoSize(PCHAR(Filename), Sz);
    if VerInfSize > 0 then
    begin
      VerInfo := AllocMem(VerInfSize);
      try
        GetFileVersionInfo(PCHAR(Filename), 0, VerInfSize, VerInfo);
        VerQueryValue(VerInfo, '\\', POINTER(FxFileInfo), Sz);
        if Sz > 0 then
          with FxFileInfo^ do
            Result := MSLSToString(dwFileVersionMS, dwFileVersionLS);
      finally
        FreeMem(VerInfo);
      end;
    end;
  end;
end;

//------------------------------------------------------------------------------

procedure TFrmAbout.FormCreate(Sender: TObject);
begin
   nbDblClick := 0 ;
   labelversion.caption := 'TraceTool ' + GetFileVersion(ParamStr(0));
end;

//------------------------------------------------------------------------------

procedure TFrmAbout.lblWebPageClick(Sender: TObject);
var
  FileName : string ;
  Parameters : string ;
begin
  FileName := (Sender as TLabel).Caption ;
  Parameters := '' ;
  ShellExecute(Application.Handle, nil, PChar(FileName), PChar(Parameters),
                            PChar(ExtractFilePath(FileName)), SW_SHOWNORMAL);

end;

//------------------------------------------------------------------------------

procedure TFrmAbout.LabelAuthorDblClick(Sender: TObject);
begin
   inc(nbDblClick) ;
   if nbDblClick < 3 then
      exit ;

   if FrmInternalTraces.Parent = nil then
      FrmInternalTraces.DockToMainPanel() ;
   FrmInternalTraces.Visible := true ;
   FrmInternalTraces.SetActivePage() ; // PageControlChange() ;
end;

//------------------------------------------------------------------------------
procedure TFrmAbout.Image1DblClick(Sender: TObject);
var
   x : integer ;
begin
   inc(nbDblClick) ;
   if nbDblClick < 3 then
      exit ;

   x := 0 ;
   x := 10 div x ; // exception
   if x = 0 then ;
end;

end.
