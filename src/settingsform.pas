{ PGNViewer - This file contains the settings window
  Copyright (C) 2017  Jan Dette

  This program is free software: you can redistribute it and/or modify
  it under the terms of the GNU General Public License as published by
  the Free Software Foundation, either version 3 of the License, or
  (at your option) any later version.

  This program is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
  GNU General Public License for more details.

  You should have received a copy of the GNU General Public License
  along with this program.  If not, see <http://www.gnu.org/licenses/>.
}
unit SettingsForm;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ComCtrls,
  ExtCtrls, StdCtrls, Board, previewposition, UCI, UCIOptions, mSettings;

type

  { TSettingsForm }

  TSettingsForm = class(TForm)
    Board1: TBoard;
    Board2: TBoard;
    btNewEngine: TButton;
    btEditEngine: TButton;
    btDeleteEngine: TButton;
    bStandard: TButton;
    bApply: TButton;
    cbBlack: TColorButton;
    cbWhite: TColorButton;
    CheckGroup1: TCheckGroup;
    cbBackground: TColorButton;
    ColorDialog1: TColorDialog;
    GroupBox1: TGroupBox;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    lEngines: TLabel;
    lvEngines: TListView;
    OpenDialog1: TOpenDialog;
    PageControl1: TPageControl;
    Panel1: TPanel;
    Panel2: TPanel;
    tsEngines: TTabSheet;
    tsBoard: TTabSheet;
    TreeView1: TTreeView;
    procedure btNewEngineClick(Sender: TObject);
    procedure bApplyClick(Sender: TObject);
    procedure cbBackgroundColorChanged(Sender: TObject);
    procedure cbBlackColorChanged(Sender: TObject);
    procedure cbWhiteColorChanged(Sender: TObject);
    procedure CheckGroup1ItemClick(Sender: TObject; Index: integer);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure TreeView1SelectionChanged(Sender: TObject);
  private
    EngineList: TUCIEngineSettingsList;
    FSaveFile: string;
    FSettings: TSettings;
    procedure UpdateEngineListView;
  public
    property Settings: TSettings read FSettings write FSettings;
    property SaveFile: string read FSaveFile write FSaveFile;
  end;

var
  SettingsForm1: TSettingsForm;

implementation

{$R *.lfm}

{ TSettingsForm }

procedure TSettingsForm.TreeView1SelectionChanged(Sender: TObject);
begin
  PageControl1.TabIndex := TreeView1.Selected.Index;
end;

procedure TSettingsForm.UpdateEngineListView;
var
  UCIEngineSettings: TUCIEngineSettings;
  Listitem: TListItem;
begin
  lvEngines.Clear;
  for UCIEngineSettings in EngineList do
  begin
    ListItem := lvEngines.Items.Add;
    Listitem.Caption := UCIEngineSettings.Name;
    if UCIEngineSettings.Standard then
      Listitem.SubItems.Add('Ja')
    else
      Listitem.SubItems.Add('Nein');
    Listitem.SubItems.Add(DateTimeToStr(UCIEngineSettings.CreationDate));
    Listitem.SubItems.Add(UCIEngineSettings.Authors);
  end;
end;

procedure TSettingsForm.cbBlackColorChanged(Sender: TObject);
begin
  Board1.BlackSquareColor := cbBlack.ButtonColor;
  Board2.BlackSquareColor := cbBlack.ButtonColor;
end;

procedure TSettingsForm.cbBackgroundColorChanged(Sender: TObject);
begin
  Board1.Border.Background := cbBackground.ButtonColor;
end;

procedure TSettingsForm.btNewEngineClick(Sender: TObject);
var
  NewEngine: TUCIEngine;
  UCIEngineSettings: TUCIEngineSettings;
begin
  if OpenDialog1.Execute then
  begin
    NewEngine := TUCIEngine.Create(nil);
    NewEngine.ProcessName := OpenDialog1.FileName;
    NewEngine.Init;
    NewEngine.Quit;
    UCIEngineSettings := TUCIEngineSettings.Create;
    UCIEngineSettings.Authors := NewEngine.Author;
    UCIEngineSettings.CreationDate := Now;
    UCIEngineSettings.ExecuteFile := OpenDialog1.FileName;
    UCIEngineSettings.Name := NewEngine.EngineName;
    UCIEngineSettings.Standard := False;
    EngineList.Add(UCIEngineSettings);
    UpdateEngineListView;
    NewEngine.Free;
  end;
end;

procedure TSettingsForm.bApplyClick(Sender: TObject);
begin
  // Board Colors
  Settings.BoardSettings.BackgroundColor := cbBackground.ButtonColor;
  Settings.BoardSettings.BlackSquareColor := cbBlack.ButtonColor;
  Settings.BoardSettings.WhiteSquareColor := cbWhite.ButtonColor;
  // Border sides
  Settings.BoardSettings.ShowBorderLeft := CheckGroup1.Checked[0];
  Settings.BoardSettings.ShowBorderRight := CheckGroup1.Checked[1];
  Settings.BoardSettings.ShowBorderTop := CheckGroup1.Checked[2];
  Settings.BoardSettings.ShowBorderBottom := CheckGroup1.Checked[3];
  // Engines
  Settings.UCIEngineSettingsList.Free;
  Settings.UCIEngineSettingsList := EngineList;

  Settings.SaveToFile(SaveFile);
end;

procedure TSettingsForm.cbWhiteColorChanged(Sender: TObject);
begin
  Board1.WhiteSquareColor := cbWhite.ButtonColor;
  Board2.WhiteSquareColor := cbWhite.ButtonColor;
end;

procedure TSettingsForm.CheckGroup1ItemClick(Sender: TObject; Index: integer);
begin
  if CheckGroup1.Checked[Index] then
    Board1.Border.Style := Board1.Border.Style + [TBorderStyle((Index + 1) mod 4)]
  else
    Board1.Border.Style := Board1.Border.Style - [TBorderStyle((Index + 1) mod 4)];
end;

procedure TSettingsForm.FormCreate(Sender: TObject);
var
  i: integer;
begin
  Board1.PieceDirectory := '../Pieces';
  Board1.CurrentPosition := TPreviewPosition2x2.Create;
  Board2.PieceDirectory := '../Pieces';
  Board2.CurrentPosition := TPreviewPosition2x6.Create;
  TreeView1.Selected := TreeView1.Items[0];
  for i := 0 to 3 do
    CheckGroup1.Checked[i] := True;
  EngineList := TUCIEngineSettingsList.Create;
end;

procedure TSettingsForm.FormDestroy(Sender: TObject);
begin
  if Self.ModalResult <> mrOk then
    EngineList.Free;
end;

procedure TSettingsForm.FormShow(Sender: TObject);
var
  i: integer;
  UCIEngineSettings, Temp: TUCIEngineSettings;
begin
  // Color buttons
  cbBackground.ButtonColor := Settings.BoardSettings.BackgroundColor;
  cbBlack.ButtonColor := Settings.BoardSettings.BlackSquareColor;
  cbWhite.ButtonColor := Settings.BoardSettings.WhiteSquareColor;
  // CheckGroup1
  CheckGroup1.Checked[0] := Settings.BoardSettings.ShowBorderLeft;
  CheckGroup1.Checked[1] := Settings.BoardSettings.ShowBorderRight;
  CheckGroup1.Checked[2] := Settings.BoardSettings.ShowBorderTop;
  CheckGroup1.Checked[3] := Settings.BoardSettings.ShowBorderBottom;
  for i := 0 to 3 do
    CheckGroup1ItemClick(Self, i);
  // Engines
  for UCIEngineSettings in Settings.UCIEngineSettingsList do
  begin
    Temp := TUCIEngineSettings.Create;
    Temp.Authors := UCIEngineSettings.Authors;
    Temp.CreationDate := UCIEngineSettings.CreationDate;
    Temp.ExecuteFile := UCIEngineSettings.ExecuteFile;
    Temp.Name := UCIEngineSettings.Name;
    Temp.Standard := UCIEngineSettings.Standard;
    EngineList.Add(Temp);
  end;
  UpdateEngineListView;
end;

end.

