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
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs,
  ExtCtrls, ComCtrls, Menus, ActnList, StdActns, StdCtrls, Board, NotationMemo,
  VisualUCIEngine, PGNGame, Position, Database, PGNdbase, MoveList, AboutForm,
  SettingsForm, Game, UCI;

type

  { TForm1 }

  TForm1 = class(TForm)
    ActionList1: TActionList;
    Board1: TBoard;
    Button1: TButton;
    FileExit1: TFileExit;
    FileOpen1: TFileOpen;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    Label6: TLabel;
    MainMenu1: TMainMenu;
    Memo1: TMemo;
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
    Panel3: TPanel;
    Panel4: TPanel;
    Panel5: TPanel;
    VisualUCIEngine1: TVisualUCIEngine;
    procedure Board1MovePlayed(AMove: TMove);
    procedure Button1Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure miAboutClick(Sender: TObject);
    procedure miSettingsClick(Sender: TObject);
    procedure NotationMemo1ClickMove(Sender: TObject; AMove: TMove);
    procedure NotationMemo1Enter(Sender: TObject);
    procedure NotationMemo1SelectionChange(Sender: TObject);
    procedure VisualUCIEngine1BestMove(Sender: TObject; BestMove, Ponder: TMove
      );
    procedure VisualUCIEngine1Info(Sender: TObject; Info: TInfo; InfoMask: TInfoMask);
  private
    Databases: TDatabaseList;
    BaseIndex, GameIndex: integer;
    CurrentGame: TGame;
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
  CurrentGame := Databases.Items[0].Items[0];
  CurrentGame.InitialPosition.SetupInitialPosition;
  CurrentGame.CurrentPosition.SetupInitialPosition;
  Board1.PieceDirectory := '../Pieces';
  Board1.CurrentPosition := TStandardPosition.Create;
  Board1.CurrentPosition.Copy(Databases.Items[0].Items[0].CurrentPosition);
  with NotationMemo1.AddLineStyle^ do
  begin
    CommentaryStyle.Color := clGreen;
    CommentaryStyle.Style := [];
    MoveStyle.Color := clBlack;
    MoveStyle.Style := [fsBold];
    NAGStyle.Color := clRed;
    NAGStyle.Style := [];
    NumberStyle.Color := clBlack;
    NumberStyle.Style := [fsBold];
    CommentaryNewLine := True;
    CommentaryIndent := 25;
    NeedsNewLine := False;
    LineIndent := 0;
  end;
  with NotationMemo1.AddLineStyle^ do
  begin
    CommentaryStyle.Color := clGreen;
    CommentaryStyle.Style := [];
    MoveStyle.Color := clBlue;
    MoveStyle.Style := [];
    NAGStyle.Color := clBlue;
    NAGStyle.Style := [];
    NumberStyle.Color := clBlue;
    NumberStyle.Style := [];
    CommentaryNewLine := False;
    NeedsNewLine := True;
    LineIndent := 25;
  end;
  with NotationMemo1.MoveToStrOptions do
  begin
    CaptureSymbol := csx;
    PieceLetters := PieceLetters_DE;
    PromotionSymbol := psNone;
    ShowEnPassantSuffix := False;
    ShowPawnLetter := False;
  end;
  VisualUCIEngine1.ProcessName := '/usr/local/bin/stockfish';
  VisualUCIEngine1.Init;
  VisualUCIEngine1.NewGame;
end;

procedure TForm1.Board1MovePlayed(AMove: TMove);
var
  i: integer;
begin
  if Databases.Items[BaseIndex].Items[GameIndex].CurrentPosition.ValidateMove(AMove) then
  begin
    if CurrentGame.CurrentPlyNode.Children.Size = 0 then
    begin
      // New move at the end entered, play it
      CurrentGame.AddMove(AMove);
    end
    else
    begin
      for i := 0 to CurrentGame.CurrentPlyNode.Children.Size - 1 do
      begin
        // the new move is the same as one old one, so we can play it
        if (AMove = CurrentGame.CurrentPlyNode.Children[i].Data.Move) then
        begin
          CurrentGame.GoOneMoveForward(i);
          Break;
        end;
      end;
      case QuestionDlg('Hier existiert schon ein Zug',
          'An dieser Stelle existiert bereits ein Zug. Was soll getan werden?',
          mtInformation, [30, 'Zug ersetzen', 31, 'Neue Variante',
          32, 'Neue Hauptvariante', 33, 'Abbrechen'], '') of
        30: CurrentGame.ReplaceMainLine(AMove);
        31: CurrentGame.AddMoveAsSideLine(AMove);
        32: CurrentGame.AddMoveAsNewMainLine(AMove);
        33:
        begin
          AMove.Free;
          Exit;
        end;
      end;
    end;
  end
  else
    AMove.Free;
  Board1.CurrentPosition.Copy(CurrentGame.CurrentPosition);
  Board1.Invalidate;
  NotationMemo1.SetTextFromGame(CurrentGame);
  NotationMemo1.HighlightMove(CurrentGame.CurrentPlyNode.Data.Move);
  // Let the Engine play
  if not CurrentGame.CurrentPosition.WhitesTurn then
  begin
    VisualUCIEngine1.SetUpPosition(TStandardPosition(Board1.CurrentPosition).ToFEN);
    VisualUCIEngine1.Go(nil, False, 0, 0, 0, 0, -1, 20, 0, 0, 0, False);
  end;
end;

procedure TForm1.Button1Click(Sender: TObject);
begin
  if button1.Tag = 0 then
  begin
    VisualUCIEngine1.SetUpPosition(TStandardPosition(Board1.CurrentPosition).ToFEN);
    VisualUCIEngine1.Go(nil, False, 0, 0, 0, 0, -1, 0, 0, 0, 0, True);
    Button1.Caption := 'Stop';
    Button1.Tag := 1;
  end
  else
  begin
    VisualUCIEngine1.Stop;
    Button1.Caption := 'Start';
    Button1.Tag := 0;
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

procedure TForm1.NotationMemo1ClickMove(Sender: TObject; AMove: TMove);
begin
  CurrentGame.GoToPositionAfterMove(AMove);
  NotationMemo1.HighlightMove(AMove);
  Board1.CurrentPosition.Copy(CurrentGame.CurrentPosition);
  Board1.Invalidate;
end;

procedure TForm1.NotationMemo1Enter(Sender: TObject);
begin
  // never allow focus on NotationMemo1
  Board1.SetFocus;
end;

procedure TForm1.NotationMemo1SelectionChange(Sender: TObject);
begin
  // Don't allow any selection
  if NotationMemo1.SelLength > 0 then
    NotationMemo1.SelLength := 0;
end;

procedure TForm1.VisualUCIEngine1BestMove(Sender: TObject; BestMove,
  Ponder: TMove);
begin
  Board1MovePlayed(BestMove);
  if Assigned(Ponder) then
    Ponder.Free;
end;

procedure TForm1.VisualUCIEngine1Info(Sender: TObject; Info: TInfo; InfoMask: TInfoMask);
begin
  if imCP in InfoMask then
    Label1.Caption := FloatToStr(Info.Score.CP / 100) + ' Tiefe:';
  Label2.Left := Label1.Left + label1.Width + 10;
  if imDepth in InfoMask then
    Label2.Caption := IntToStr(Info.Depth);
  Label3.Left := Label2.Left + Label2.Width + 10;
  Label4.Left := label3.Left + Label3.Width + 10;
  if imNodes in InfoMask then
    Label4.Caption := IntToStr(Info.Nodes div 1000) + 'k (' +
      IntToStr(Info.NPS div 1000) + 'kn/s)';
  Label5.Left := Label4.Left + Label4.Width + 10;
  Label6.Left := Label5.Left + Label5.Width + 10;
  if imTime in InfoMask then
    Label6.Caption := FloatToStr(Info.Time / 1000) + 's';
  if imCurrLine in InfoMask then
  begin
    Info.CurrLine.Free;
  end;
  if imPV in InfoMask then
  begin
    Memo1.Lines.Clear;
    Memo1.Lines.AddStrings(Info.PV);
    Info.PV.Free;
  end;
end;

end.
