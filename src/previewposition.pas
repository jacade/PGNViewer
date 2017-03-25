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
  Classes, SysUtils, Position, Pieces;

type

  { TPreviewPosition }

  TPreviewPosition = class(TPosition)
  protected
    function GetCountOfFiles: byte; override;
    function GetCountOfRanks: byte; override;
    function GetSquares(Index: integer): TPieceType; override;
  public

  end;

implementation

{ TPreviewPosition }

function TPreviewPosition.GetCountOfFiles: byte;
begin
  Result := 2;
end;

function TPreviewPosition.GetCountOfRanks: byte;
begin
  Result := 2;
end;

function TPreviewPosition.GetSquares(Index: integer): TPieceType;
begin
  case Index of
    0: Result := ptBQueen;
    1: Result := ptBKing;
    2: Result := ptWQueen;
    3: Result := ptWKing;
  end;
end;

end.

