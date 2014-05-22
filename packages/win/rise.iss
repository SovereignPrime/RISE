; Script generated by the Inno Setup Script Wizard.
; SEE THE DOCUMENTATION FOR DETAILS ON CREATING INNO SETUP SCRIPT FILES!

#define MyAppName "RISE"
#define MyAppVersion "0.0.30"
#define MyAppPublisher "Sovereign Prime"
#define MyAppURL "http://souvereignprime.com/"
#define MyAppExeName "rise.exe"

[Setup]
; NOTE: The value of AppId uniquely identifies this application.
; Do not use the same AppId value in installers for other applications.
; (To generate a new GUID, click Tools | Generate GUID inside the IDE.)
AppId={{25DE33E6-557A-4275-94A8-6FD06F51A804}
AppName={#MyAppName}
AppVersion={#MyAppVersion}
;AppVerName={#MyAppName} {#MyAppVersion}
AppPublisher={#MyAppPublisher}
AppPublisherURL={#MyAppURL}
AppSupportURL={#MyAppURL}
AppUpdatesURL={#MyAppURL}
DefaultDirName={pf}\{#MyAppName}
DefaultGroupName={#MyAppName}
LicenseFile=C:\RISE\LICENSE
OutputDir=C:\RISE\rel\Release
OutputBaseFilename=setup
Compression=lzma
SolidCompression=yes

[Languages]
Name: "english"; MessagesFile: "compiler:Default.isl"

[Tasks]
Name: "desktopicon"; Description: "{cm:CreateDesktopIcon}"; GroupDescription: "{cm:AdditionalIcons}"; Flags: unchecked
Name: "quicklaunchicon"; Description: "{cm:CreateQuickLaunchIcon}"; GroupDescription: "{cm:AdditionalIcons}"; Flags: unchecked; OnlyBelowVersion: 0,6.1

[Files]
; Source: "C:\RISE\rel\rise\bin\rise.exe"; DestDir: "{app}"; Flags: ignoreversion
Source: "C:\RISE\rel\rise\*"; DestDir: "{app}"; Flags: ignoreversion recursesubdirs createallsubdirs
Source: "C:\Qt\5.3\mingw482_32\plugins\platforms\*"; DestDir: "{app}\bin\platforms"; Flags: recursesubdirs
Source: "C:\Qt\5.3\mingw482_32\bin\*.dll"; DestDir: "{app}\bin"
; NOTE: Don't use "Flags: ignoreversion" on any shared system files

[Dirs]
Name: "{app}/spool"; Flags: uninsneveruninstall
Name: "{app}/scratch"; Flags: uninsneveruninstall

[Icons]
Name: "{group}\{#MyAppName}"; Filename: "{app}\{#MyAppExeName}"; WorkingDir: "{app}\bin"
Name: "{commondesktop}\{#MyAppName}"; Filename: "{app}\{#MyAppExeName}"; Tasks: desktopicon; WorkingDir: "{app}\bin"
Name: "{userappdata}\Microsoft\Internet Explorer\Quick Launch\{#MyAppName}"; Filename: "{app}\{#MyAppExeName}"; WorkingDir: "{app}\bin"; Tasks: quicklaunchicon

[Run]
Filename: "{app}\build-ini.bat"; StatusMsg: "Configuring..."; Flags: nowait
Filename: "{app}\{#MyAppExeName}"; Description: "{cm:LaunchProgram,{#StringChange(MyAppName, '&', '&&')}}"; Flags: nowait postinstall skipifsilent

