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
OutputBaseFilename={#MyAppName}_{#MyAppVersion}
Compression=lzma
SolidCompression=yes

[Languages]
Name: "english"; MessagesFile: "compiler:Default.isl"

[Tasks]
Name: "desktopicon"; Description: "{cm:CreateDesktopIcon}"; GroupDescription: "{cm:AdditionalIcons}"; Flags: unchecked
Name: "quicklaunchicon"; Description: "{cm:CreateQuickLaunchIcon}"; GroupDescription: "{cm:AdditionalIcons}"; Flags: unchecked; OnlyBelowVersion: 0,6.1

[Files]
Source: "C:\RISE\rel\rise\*"; DestDir: "{app}"; Flags: ignoreversion recursesubdirs createallsubdirs
Source: "C:\Qt\5.3\mingw482_32\plugins\platforms\*"; DestDir: "{app}\bin\platforms"; Flags: recursesubdirs
Source: "C:\Qt\5.3\mingw482_32\bin\*.dll"; DestDir: "{app}\bin"
; NOTE: Don't use "Flags: ignoreversion" on any shared system files

[Dirs]
Name: "{app}/spool"; Flags: uninsneveruninstall
Name: "{app}/scratch"; Flags: uninsneveruninstall

[Code]
function InitializeSetup(): Boolean;
var
  ErrorCode: Integer;
  RedistInstalled : Boolean;
  Result1 : Boolean;
begin
  RedistInstalled := RegKeyExists(HKLM,'SOFTWARE\Microsoft\Windows\CurrentVersion\Uninstall\{196BB40D-1578-3D01-B289-BEFC77A11A1E}');
  if RedistInstalled then
  begin
    Result := true;
  end else
  begin
    RedistInstalled := RegKeyExists(HKLM,'SOFTWARE\Microsoft\Windows\CurrentVersion\Uninstall\{196BB40D-1578-3D01-B289-BEFC77A11A1E}');
    if RedistInstalled then
    begin
      Result := true;
    end else
    begin
      Result1 := MsgBox('This setup requires Microsoft Visual C++ 2010 Redistributable Package (x86). Please install Visual C++ 2010 Redistributable Package (x86) and run this setup again.  '#13#10' '#13#10'Do you want to download Microsoft Visual C++ 2010 Redistributable Package (x86) now?',
        mbConfirmation, MB_YESNO) = idYes;
      if Result1 =false then
      begin
        Result:=false;
      end else
      begin
        Result:=false;
        ShellExec('open',
          'http://download.microsoft.com/download/5/B/C/5BC5DBB3-652D-4DCE-B14A-475AB85EEF6E/vcredist_x86.exe',
          '','',SW_SHOWNORMAL,ewNoWait,ErrorCode);
      end;
    end;
  end;
end;

[Icons]
Name: "{group}\{#MyAppName}"; Filename: "{app}\bin\{#MyAppExeName}"; WorkingDir: "{app}\bin"
Name: "{commondesktop}\{#MyAppName}"; Filename: "{app}\bin\{#MyAppExeName}"; Tasks: desktopicon; WorkingDir: "{app}\bin"
Name: "{userappdata}\Microsoft\Internet Explorer\Quick Launch\{#MyAppName}"; Filename: "{app}\bin\{#MyAppExeName}"; WorkingDir: "{app}\bin"; Tasks: quicklaunchicon

[Run]
Filename: {app}\bin\vcredist_x86.exe; Parameters: "/q:a /c:""VCREDI~3.EXE /q:a /c:""""msiexec /i vcredist.msi /qn"""" """; WorkingDir: {app}\bin; StatusMsg: Installing Visual Studio 2010 C++ CRT Libraries...
Filename: "{app}\build-ini.bat"; StatusMsg: "Configuring..."; Flags: nowait
Filename: "{app}\bin\{#MyAppExeName}"; WorkingDir: "{app}\bin"; Description: "{cm:LaunchProgram,{#StringChange(MyAppName, '&', '&&')}}"; Flags: nowait postinstall skipifsilent

