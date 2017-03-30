unit mSettings;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, IniFiles, Graphics, fgl;

type

  { TBoardSettings }

  TBoardSettings = class
  private
    FBackgroundColor: TColor;
    FBlackSquareColor: TColor;
    FShowBorderBottom: boolean;
    FShowBorderLeft: boolean;
    FShowBorderRight: boolean;
    FShowBorderTop: boolean;
    FWhiteSquareColor: TColor;
  public
    property BackgroundColor: TColor read FBackgroundColor write FBackgroundColor;
    property BlackSquareColor: TColor read FBlackSquareColor write FBlackSquareColor;
    property ShowBorderBottom: boolean read FShowBorderBottom write FShowBorderBottom;
    property ShowBorderLeft: boolean read FShowBorderLeft write FShowBorderLeft;
    property ShowBorderRight: boolean read FShowBorderRight write FShowBorderRight;
    property ShowBorderTop: boolean read FShowBorderTop write FShowBorderTop;
    property WhiteSquareColor: TColor read FWhiteSquareColor write FWhiteSquareColor;
  end;

  { TUCIEngineSettings }

  TUCIEngineSettings = class
  private
    FAuthors: string;
    FCreationDate: TDateTime;
    FExecuteFile: string;
    FName: string;
    FStandard: boolean;
  public
    property Authors: string read FAuthors write FAuthors;
    property CreationDate: TDateTime read FCreationDate write FCreationDate;
    property ExecuteFile: string read FExecuteFile write FExecuteFile;
    property Name: string read FName write FName;
    property Standard: boolean read FStandard write FStandard;
  end;

  TUCIEngineSettingsList = specialize TFPGObjectList<TUCIEngineSettings>;

  { TSettings }

  TSettings = class
  private
    FBoardSettings: TBoardSettings;
    FEngineConfigFile: string;
    FUCIEngineSettingsList: TUCIEngineSettingsList;
  public
    constructor Create(const AFileName: string);
    destructor Destroy; override;
    procedure SaveToFile(const AFileName: string);
  public
    property BoardSettings: TBoardSettings read FBoardSettings;
    property EngineConfigFile: string read FEngineConfigFile write FEngineConfigFile;
    property UCIEngineSettingsList: TUCIEngineSettingsList read FUCIEngineSettingsList write FUCIEngineSettingsList;
  end;

implementation

{ TSettings }

constructor TSettings.Create(const AFileName: string);
var
  IniFile: TIniFile;
  EngineNames: TStringList;
  UCIEngineSettings: TUCIEngineSettings;
  s: string;
begin
  IniFile := TIniFile.Create(AFileName);
  FBoardSettings := TBoardSettings.Create;
  try
    // Board settings
    FBoardSettings.BackgroundColor :=
      StringToColor(IniFile.ReadString('Board', 'BackgroundColor', '$4BC0EA'));
    FBoardSettings.BlackSquareColor :=
      StringToColor(IniFile.ReadString('Board', 'BlackSquareColor', '$2666B2'));
    FBoardSettings.ShowBorderBottom := IniFile.ReadBool('Board', 'ShowBottom', True);
    FBoardSettings.ShowBorderLeft := IniFile.ReadBool('Board', 'ShowLeft', True);
    FBoardSettings.ShowBorderRight := IniFile.ReadBool('Board', 'ShowRight', True);
    FBoardSettings.ShowBorderTop := IniFile.ReadBool('Board', 'ShowTop', True);
    FBoardSettings.WhiteSquareColor :=
      StringToColor(IniFile.ReadString('Board', 'WhiteSquareColor', '$DAEBF3'));
    // Engine settings
    FEngineConfigFile := IniFile.ReadString('Engines', 'ConfigFile', 'engines.ini');

  finally
    IniFile.Free;
  end;
  // Load Engine data
  IniFile := TIniFile.Create(EngineConfigFile);
  FUCIEngineSettingsList := TUCIEngineSettingsList.Create;
  try
    EngineNames := TStringList.Create;
    IniFile.ReadSections(EngineNames);
    for s in EngineNames do
    begin
      UCIEngineSettings := TUCIEngineSettings.Create;
      UCIEngineSettings.Name := s;
      UCIEngineSettings.Authors := IniFile.ReadString(s, 'Authors', '');
      UCIEngineSettings.CreationDate := IniFile.ReadFloat(s, 'Date', 0);
      UCIEngineSettings.ExecuteFile := IniFile.ReadString(s, 'Executable', '');
      UCIEngineSettings.Standard := IniFile.ReadBool(s, 'Standard', False);
      FUCIEngineSettingsList.Add(UCIEngineSettings);
    end;
  finally
    EngineNames.Free;
    IniFile.Free;
  end;
end;

destructor TSettings.Destroy;
begin
  FBoardSettings.Free;
  FUCIEngineSettingsList.Free;
end;

procedure TSettings.SaveToFile(const AFileName: string);
var
  IniFile: TIniFile;
  UCIEngineSettings: TUCIEngineSettings;
begin
  IniFile := TIniFile.Create(AFileName);
  try
    // Board settings
    IniFile.WriteString('Board', 'BackgroundColor',
      ColorToString(FBoardSettings.BackgroundColor));
    IniFile.WriteString('Board', 'BlackSquareColor',
      ColorToString(FBoardSettings.BlackSquareColor));
    IniFile.WriteBool('Board', 'ShowBottom', FBoardSettings.ShowBorderBottom);
    IniFile.WriteBool('Board', 'ShowLeft', FBoardSettings.ShowBorderLeft);
    IniFile.WriteBool('Board', 'ShowRight', FBoardSettings.ShowBorderRight);
    IniFile.WriteBool('Board', 'ShowTop', FBoardSettings.ShowBorderTop);
    IniFile.WriteString('Board', 'WhiteSquareColor',
      ColorToString(FBoardSettings.WhiteSquareColor));
    // Engine settings
    IniFile.WriteString('Engines', 'ConfigFile', FEngineConfigFile);
  finally
    IniFile.Free;
  end;
  // Save engine data
  IniFile := TIniFile.Create(FEngineConfigFile);
  try
    for UCIEngineSettings in FUCIEngineSettingsList do
    begin
      IniFile.WriteString(UCIEngineSettings.Name, 'Authors', UCIEngineSettings.Authors);
      IniFile.WriteFloat(UCIEngineSettings.Name, 'Date', UCIEngineSettings.CreationDate);
      IniFile.WriteString(UCIEngineSettings.Name, 'Executable', UCIEngineSettings.ExecuteFile);
      IniFile.WriteBool(UCIEngineSettings.Name, 'Standard', UCIEngineSettings.Standard);
    end;
  finally
    IniFile.Free;
  end;
end;

end.
