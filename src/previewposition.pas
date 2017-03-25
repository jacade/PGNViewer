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

