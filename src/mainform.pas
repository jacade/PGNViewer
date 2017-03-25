{ PGNViewer - This file contains the main window
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
unit mainform;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ExtCtrls,
  ComCtrls, Menus, ActnList, StdActns, Board, NotationMemo, EngineView, PGNGame,
  Position, Database, PGNdbase, MoveList, AboutForm, SettingsForm;

type

  { TForm1 }

  TForm1 = class(TForm)
    ActionList1: TActionList;
    Board1: TBoard;
    EngineView1: TEngineView;
    FileExit1: TFileExit;
    FileOpen1: TFileOpen;
    MainMenu1: TMainMenu;
    miOptions: TMenuItem;
    miSettings: TMenuItem;
    miAbout: TMenuItem;
    miHelp: TMenuItem;
    miFile: TMenuItem;
    miFileNew: TMenuItem;
    miFileOpen: TMenuItem;
    miFileExit: TMenuItem;
    NotationMemo1: TNotationMemo;
    PageControl1: TPageControl;
    Panel1: TPanel;
    Panel2: TPanel;
    procedure Board1MovePlayed(AMove: TMove);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure miAboutClick(Sender: TObject);
    procedure miSettingsClick(Sender: TObject);
  private
    Databases: TDatabaseList;
    BaseIndex, GameIndex: integer;
  public
    { public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.lfm}

{ TForm1 }

procedure TForm1.FormCreate(Sender: TObject);
begin
  BaseIndex := 0;
  GameIndex := 0;
  Databases := TDatabaseList.Create;
  Databases.Add(TPGNDatabase.Create);
  Databases.Items[0].Add(TPGNGame.Create);
  with Databases.Items[0].Items[0] do
  begin
    InitialPosition.SetupInitialPosition;
    CurrentPosition.SetupInitialPosition;
  end;
  Board1.PieceDirectory := './Pieces';
  Board1.CurrentPosition := TStandardPosition.Create;
  Board1.CurrentPosition.Copy(Databases.Items[0].Items[0].CurrentPosition);
end;

procedure TForm1.Board1MovePlayed(AMove: TMove);
begin
  if Databases.Items[BaseIndex].Items[GameIndex].CurrentPosition.ValidateMove(AMove) then
  begin

  end;
end;

procedure TForm1.FormDestroy(Sender: TObject);
begin
  Databases.Free;
end;

procedure TForm1.miAboutClick(Sender: TObject);
begin
  AboutForm1.ShowModal;
end;

procedure TForm1.miSettingsClick(Sender: TObject);
begin
  SettingsForm1.ShowModal;
end;

end.
