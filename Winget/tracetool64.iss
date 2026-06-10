; Script Inno Setup to install Tracetool

[Setup]
AppName=Tracetool
AppVersion=15.0
DefaultDirName={commonpf}\Tracetool
DefaultGroupName=Tracetool
OutputBaseFilename=Setup64
Compression=lzma
SolidCompression=yes
OutputDir="..\Dist"

[Files]
Source: "..\Viewer\clientaccesspolicy.xml";     DestDir: "{app}";
Source: "..\Viewer\crossdomain.xml";            DestDir: "{app}";
Source: "..\Viewer\debug.bat";                  DestDir: "{app}";
Source: "..\Viewer\DotNetWrapper64.dll";        DestDir: "{app}";
Source: "..\Viewer\FastMM_FullDebugMode64.dll"; DestDir: "{app}";
Source: "..\Viewer\TraceTool_Icon.png";         DestDir: "{app}";
Source: "..\Viewer\TraceTool.exe";              DestDir: "{app}";
Source: "..\Viewer\TracetoolConfig.xml";        DestDir: "{app}"; Flags: onlyifdoesntexist
Source: "..\Viewer\WebSock\*";                  DestDir: "{app}\WebSock"; 
Source: "..\Javascript\tracetool.js";           DestDir: "{app}";
Source: "..\Javascript\tracetool.jmin.js";      DestDir: "{app}";
Source: "..\Cpp\Source\tracetool.cpp";          DestDir: "{app}\Cpp";
Source: "..\Cpp\Source\tracetool.h";            DestDir: "{app}\Cpp";
Source: "..\Delphi\Delphi Library\*.pas";       DestDir: "{app}\Delphi";
Source: "..\Delphi\Delphi Library\*.inc";       DestDir: "{app}\Delphi";
Source: "..\Delphi\Delphi Library\*.txt";       DestDir: "{app}\Delphi";
Source: "..\Java\Src\tracetool\*.java";         DestDir: "{app}\Java";
Source: "..\Python\Src\tracetool\*.py";         DestDir: "{app}\Python";

[Icons]
Name: "{group}\Tracetool"; Filename: "{app}\Tracetool.exe"




