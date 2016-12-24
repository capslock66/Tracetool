{**************************************************************************************************}
{                                                                                                  }
{ Last modified: $Date:: 2007-09-17 23:41:02 +0200 (lun. 17 sept. 2007)                          $ }
{ Revision:      $Rev:: 2175                                                                     $ }
{ Author:        $Author:: outchy                                                                $ }
{                                                                                                  }
{**************************************************************************************************}

program QJediInstaller;

uses
  QForms,
  JediInstall in 'JediInstall.pas',
  JclInstall in 'JclInstall.pas',
  JediInstallConfigIni in 'JediInstallConfigIni.pas',
  JclResources in '../source/common/JclResources.pas',
  JclBorlandTools in '../source/common/JclBorlandTools.pas',
  QJediGUIReadme in 'ClxGui/QJediGUIReadme.pas' {ReadmeFrame: TFrame},
  QJediGUIInstall in 'ClxGui/QJediGUIInstall.pas' {InstallFrame: TFrame},
  QJediGUIMain in 'ClxGui/QJediGUIMain.pas' {MainForm};

{$R *.res}

begin
  Application.Initialize;
  Application.Title := 'JEDI Installer';
  InstallCore.Execute;
end.
