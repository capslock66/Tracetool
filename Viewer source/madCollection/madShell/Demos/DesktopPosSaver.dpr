program DesktopPosSaver;

uses
  Forms,
  DesktopPos in 'DesktopPos.pas' {FDesktopSaver};

{$R mad.res}

begin
  Application.Initialize;
  Application.CreateForm(TFDesktopSaver, FDesktopSaver);
  Application.Run;
end.
