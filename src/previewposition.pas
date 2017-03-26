{ PGNViewer - This file contains a position to display when setting board colors
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
unit previewposition;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Position, Pieces, MoveList;

type

  { TPreviewPosition }

  TPreviewPosition = class(TPosition)
  public
    function GetAllLegalMoves: TMoveList; override;
    function MoveToStr(AMove: TMove; MoveToStrOptions: TMoveToStrOptions): string;
      override;
    procedure PlayMove(AMove: TMove); override;
    procedure SetupInitialPosition; override;
    function ValidateMove(AMove: TMove): boolean; override;
  end;

  { TPreviewPosition2x2 }

  TPreviewPosition2x2 = class(TPreviewPosition)
  protected
    function GetCountOfFiles: byte; override;
    function GetCountOfRanks: byte; override;
    function GetSquares(Index: integer): TPieceType; override;
  end;

  { TPreviewPosition2x6 }

  TPreviewPosition2x6 = class(TPreviewPosition)
  protected
    function GetCountOfFiles: byte; override;
    function GetCountOfRanks: byte; override;
    function GetSquares(Index: integer): TPieceType; override;
  end;

implementation

{ TPreviewPosition }

function TPreviewPosition.GetAllLegalMoves: TMoveList;
begin
  Result := nil;
end;

function TPreviewPosition.MoveToStr(AMove: TMove;
  MoveToStrOptions: TMoveToStrOptions): string;
begin
  Result := '';
end;

procedure TPreviewPosition.PlayMove(AMove: TMove);
begin

end;

procedure TPreviewPosition.SetupInitialPosition;
begin

end;

function TPreviewPosition.ValidateMove(AMove: TMove): boolean;
begin
  Result := False;
end;

{ TPreviewPosition2x2 }

function TPreviewPosition2x2.GetCountOfFiles: byte;
begin
  Result := 2;
end;

function TPreviewPosition2x2.GetCountOfRanks: byte;
begin
  Result := 2;
end;

function TPreviewPosition2x2.GetSquares(Index: integer): TPieceType;
begin
  case Index of
    0: Result := ptBQueen;
    1: Result := ptBKing;
    2: Result := ptWQueen;
    3: Result := ptWKing;
  end;
end;

{ TPreviewPosition2x6 }

function TPreviewPosition2x6.GetCountOfFiles: byte;
begin
  Result := 6;
end;

function TPreviewPosition2x6.GetCountOfRanks: byte;
begin
  Result := 2;
end;

function TPreviewPosition2x6.GetSquares(Index: integer): TPieceType;
begin
  case Index of
    0: Result := ptBRook;
    1: Result := ptBKnight;
    2: Result := ptBBishop;
    3: Result := ptBQueen;
    4: Result := ptBKing;
    5: Result := ptBPawn;
    6: Result := ptWRook;
    7: Result := ptWKnight;
    8: Result := ptWBishop;
    9: Result := ptWQueen;
    10: Result := ptWKing;
    11: Result := ptWPawn;
  end;
end;

end.

