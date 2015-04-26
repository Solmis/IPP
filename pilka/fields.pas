unit fields;

{$mode objfpc}{$H+}

interface

  uses
    Classes, SysUtils, Math;

  //Ustawia wymiary boiska i wylicza wszystkie zmienne potrzebne do jego rysowania
  procedure initFields(const bw, bh: Integer);

  //Ustawia typ gry (0 - gracz vs gracz, 1 - gracz vs komputer, 2 - komputer vs komputer)
  procedure initGameType(const gt: Integer);

  //Wylicza i zwraca x na planszy dla pozycji l
  function getX(const l: Integer): Integer;

  //Wylicza i zwraca y na planszy dla pozycji t
  function getY(const t: Integer): Integer;

  //Wylicza i zwraca pozycję l dla podanego x na planszy
  function toFieldX(const x: Integer): Integer;

  //Wylicza i zwraca pozycję t dla podanego y na planszy
  function toFieldY(const y: Integer): Integer;

  //Wylicza i zwraca indeks pola o zadanych pozycjach l i t
  function getNum(const l, t: Integer): Integer;

  type
    LineList = ^mylnlist;
      mylnlist = record
        p1, p2: TPoint;
        player: Integer;
        prev: LineList;
        next: LineList;
      end;

    SmallLineList = ^mysmlnlist;
      mysmlnlist = record
        p1, p2: TPoint;
        next: SmallLineList;
      end;

  const

    IMG_W = 580;
    IMG_H = 660;
    IMG_DIFF = IMG_H - IMG_W;
    ERR_MARGIN = 15;
    BOARD_MARGIN = 2;
    STD_BOARD_WIDTH = 8;
    STD_BOARD_HEIGHT = 10;
    MAX_BOARD_SIZE = 16;
    MIN_BOARD_SIZE = 4;

  var
    fieldSize, boardWidth, boardHeight, startX, startY, goalLeft, goalRight, gameType: Integer;

implementation

  procedure initGameType(const gt: Integer);
  begin
    gameType := gt;
  end;

  procedure initFields(const bw, bh: Integer);
  var
    field_width, field_height: Integer;
  begin
    boardWidth := bw;
    boardHeight := bh;
    startX := boardWidth div 2;
    startY := boardHeight div 2;
    goalLeft := startX - 1;
    goalRight := startX + 1;
    field_width := floor(IMG_W div (bw + (BOARD_MARGIN * 2)));
    field_height := floor(IMG_H div (bh + (BOARD_MARGIN * 2)));
    fieldSize := min(field_width, field_height);
  end;

  function getX(const l: Integer): Integer;
  begin
    getX := (l + BOARD_MARGIN) * fieldSize;
  end;

  function getY(const t: Integer): Integer;
  begin
    getY := (t + BOARD_MARGIN) * fieldSize;
  end;

  function toFieldX(const x: Integer): Integer;
  var
    diff: Integer;
  begin
    diff := x mod fieldSize;
    if (diff > ERR_MARGIN) and (diff - fieldSize < -ERR_MARGIN) then
      toFieldX := -1
    else
      toFieldX := ((x + (fieldSize div 2)) div fieldSize) - BOARD_MARGIN;
  end;

  function toFieldY(const y: Integer): Integer;
  begin
    toFieldY := toFieldX(y);
  end;

  function getNum(const l, t: Integer): Integer;
  begin
    getNum := t * (boardWidth + 1) + l;
  end;

end.

