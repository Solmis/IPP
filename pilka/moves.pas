unit moves;

{$mode objfpc}{$H+}

interface

  uses
    Classes, SysUtils, fields, Dialogs;

  //Tworzy objMoves
  procedure makeMoves;

  type
    TMoves = class
    private
      { private declarations }

      //Maksymalny rozmiar planszy (16 na 16 pól)
      const
        MAX_WIDTH = 17;
        MAX_HEIGHT = 17;

      //dotX, dotY - aktualna pozycja piłki
      //connectTable - zawiera informacje o połączeniach pomiędzy polami
      //winner - imię zwycięzcy
      var
        dotX, dotY: Integer;
        connectTable: array[0..MAX_WIDTH * MAX_HEIGHT, 0..MAX_WIDTH * MAX_HEIGHT] of Boolean;
        winner: String;

      public
        { public declarations }

        procedure init;
        function getDotX: Integer;
        function getDotY: Integer;

        //Inicjalizuje nową tablicę connectTable
        procedure clearConnections;

        //Ustala, kto jest komputerem
        procedure computers;

        //Zmienia pozycję piłki na l, t
        procedure moveTo(const l, t: Integer);

        //Sprawdza, czy punkty (x1, y1) oraz (x2, y2) są połączone, x i y w jednostkach pól [l, t]
        function pointsConnected(const x1, y1, x2, y2: Integer): Boolean;

        //Łączy punkty (x1, y1) oraz (x2, y2), jednostka: jednostka pól [l, t]
        procedure connectPoints(const x1, y1, x2, y2: Integer);

        //Rozłącza punkty (x1, y1) oraz (x2, y2), jednostka: jednostka pól [l, t]
        procedure disconnectPoints(const x1, y1, x2, y2: Integer);

        //Zwraca informację, czy można wykonać ruch z aktualnej pozycji na pozycję dX, dY,
        //wyrażoną w jednostkach pól [l, t]
        function moveIsPossible(const dX, dY: Integer): Boolean;

        //Zwraca true, gdy punkt o podanym indeksie nie został jeszcze odwiedzony
        function pointAlone(const num: Integer): Boolean;

        //Zwraca informację, czy obecny stan gry jest końcowy (ktoś wygrał)
        function gameEnd: Boolean;

        //Zwraca imię gracza, który zwyciężył
        function getWinner: String;

        //Zwraca proponowany jednosegmentowy ruch dla gracza lub komputera
        function getHint: SmallLineList;

        //Zwraca proponowany jednosegmentowy ruch dla drugiego komputera
        //(inny sposób wyznaczania ruchu, by nudno nie było)
        function getCompMove: SmallLineList;

        //Czyści listę jednokierunkową typu SmallLineList
        procedure clearHint(var hint: SmallLineList);

      //turn - przechowuje nr id gracza, który powinien teraz się ruszyć
      //player1, player2 - imiona graczy
      //isComputer[i] - przechowuje informacje, czy gracz o indeksie i jest komputerowy
      //firstMove - zmienna ustawiana na true po zakończeniu ruchu i ustawiana na false
      //po wybraniu pierwszego segmentu ruchu
      var
        turn: Integer;
        player1, player2: String;
        isComputer: array[0..1] of Boolean;
        firstMove: Boolean;
    end;

  var
    objMoves: TMoves;

implementation

  procedure makeMoves;
  begin
    objMoves := TMoves.Create;
  end;

  procedure TMoves.init;
  begin
    clearConnections;
    dotX := startX;
    dotY := startY;
    winner := '';
    computers;
    Randomize;

    firstMove := true;
  end;

  procedure TMoves.computers;
  begin
    case gameType of
      0: begin
           isComputer[0] := false;
           isComputer[1] := false;
         end;
      1: begin
           isComputer[0] := false;
           isComputer[1] := true;
         end;
      2: begin
           isComputer[0] := true;
           isComputer[1] := true;
         end;
    end;
  end;

  procedure TMoves.clearConnections;
  var
    i, j: Integer;
  begin
    for i := 0 to (boardWidth + 1) * (boardHeight + 1) do
    for j := 0 to (boardWidth + 1) * (boardHeight + 1) do
    begin
      if i = j then
        connectTable[i, j] := true
      else
        connectTable[i, j] := false;
    end;
  end;

  function TMoves.getDotX: Integer;
  begin
    getDotX := dotX;
  end;

  function TMoves.getDotY: Integer;
  begin
    getDotY := dotY;
  end;

  procedure TMoves.moveTo(const l, t: Integer);
  begin
    dotX := l;
    dotY := t;
  end;

  //checks if points are connected, if they're out of pitch, function returns true
  function TMoves.pointsConnected(const x1, y1, x2, y2: Integer): Boolean;
  var
    i, j, max: Integer;
  begin
    i := getNum(x1, y1);
    j := getNum(x2, y2);
    max := (boardWidth + 1) * (boardHeight + 1);

    if (i < 0) or (j < 0) or (i > max) or (j > max) then
      pointsConnected := true
    else
    begin
      if connectTable[i, j] = true then
        pointsConnected := true
      else
        pointsConnected := false;
    end;
  end;

  procedure TMoves.connectPoints(const x1, y1, x2, y2: Integer);
  var
    i, j, max: Integer;
  begin
    i := getNum(x1, y1);
    j := getNum(x2, y2);
    max := (boardWidth + 1) * (boardHeight + 1);
    if (i >= 0) and (j >= 0) and (i <= max) and (j <= max) then
    begin
      connectTable[i, j] := true;
      connectTable[j, i] := true;
    end;
  end;

  procedure TMoves.disconnectPoints(const x1, y1, x2, y2: Integer);
  var
    i, j, max: Integer;
  begin
    i := getNum(x1, y1);
    j := getNum(x2, y2);
    max := (boardWidth + 1) * (boardHeight + 1);
    if (i >= 0) and (j >= 0) and (i <= max) and (j <= max) then
    begin
      connectTable[i, j] := false;
      connectTable[j, i] := false;
    end;
  end;

  function TMoves.moveIsPossible(const dX, dY: Integer): Boolean;
  begin
    moveIsPossible := false;

    if (abs(dX - dotX) <= 1) and (abs(dY - dotY) <= 1) then
    begin
      if (dX > 0) and (dX < boardWidth) and (dY > 0) and (dY < boardHeight) then
      begin
        if not pointsConnected(dX, dY, dotX, dotY) then
          moveIsPossible := true;
      end
      else if (dotX = startX) and ((dotY = 0) or (dotY = boardHeight)) then
      begin
        moveIsPossible := true;
      end
      else if (dotY = 0) or (dotY = boardHeight) then
      begin
        if (dotX = goalLeft) then
        begin
          if (dX = dotX + 1) then
          begin
            if ((dY = dotY - 1) and (dotY = 0)) or ((dY = dotY + 1) and (dotY = boardHeight)) then
              moveIsPossible := true
            else if dY = dotY then
              moveIsPossible := true
            else if not pointsConnected(dX, dY, dotX, dotY) then
              moveIsPossible := true;
          end
        end
        else if (dotX = goalRight) then
        begin
          if (dX = dotX - 1) then
          begin
            if ((dY = dotY - 1) and (dotY = 0)) or ((dY = dotY + 1) and (dotY = boardHeight)) then
              moveIsPossible := true
            else if dY = dotY then
              moveIsPossible := true
            else if not pointsConnected(dX, dY, dotX, dotY) then
              moveIsPossible := true;
          end
        end
        else
        begin
          if (dotX > 0) and (dotX < boardWidth) and (dY <> dotY) and (dY >= 0) and (dY <= boardHeight) then
          begin
            if not pointsConnected(dX, dY, dotX, dotY) then
              moveIsPossible := true;
          end;
        end;
      end
      else if (dX = 0) or (dX = boardWidth) then
      begin
        if (dotX <> dX) and (dotY >= 0) and (dY <= boardHeight) then
        begin
          if not pointsConnected(dX, dY, dotX, dotY) then
            moveIsPossible := true;
        end;
      end
      else if (dY = 0) or (dY = boardHeight) then
      begin
        if (dotY <> dY) and (dX >= 0) and (dX <= boardWidth) then
        begin
          if not pointsConnected(dX, dY, dotX, dotY) then
            moveIsPossible := true;
        end;
      end;
    end;
  end;

  function TMoves.pointAlone(const num: Integer): Boolean;

    function isOnEdge(const n: Integer): Boolean;
    var
      prev: Integer;
    begin
      prev := (boardWidth + 1) * boardHeight;
      isOnEdge := false;
      //?
      if (n < 0) then
        isOnEdge := true;
      if ((n mod (boardWidth + 1)) = 0) or ((n mod (boardWidth + 1)) = boardWidth) then
        isOnEdge := true;
      if ((n >= 0) and (n <= goalLeft)) or ((n >= goalRight) and (n <= boardWidth + 1)) then
        isOnEdge := true;
      if ((n >= prev) and (n <= prev + goalLeft)) or ((n >= prev + goalRight) and (n <= prev + boardWidth + 1)) then
        isOnEdge := true;
    end;

  var
    i: Integer;
  begin
    pointAlone := true;
    if (isOnEdge(num)) then
      pointAlone := false
    else
    begin
      for i := 0 to (boardWidth + 1) * (boardHeight + 1) do
      begin
        if (num <> i) and (connectTable[num, i]) then
        begin
          pointAlone := false;
          break;
        end;
      end;
    end;
  end;

  function TMoves.gameEnd: Boolean;
  var
    resu: Boolean;
  begin
    if dotY < 0 then
    begin
      resu := true;
      winner := player2;
    end
    else if dotY > boardHeight then
    begin
      resu := true;
      winner := player1;
    end
    else
    begin
      resu := true;
      if moveIsPossible(dotX-1, dotY-1) then
        resu := false;
      if moveIsPossible(dotX-1, dotY) then
        resu := false;
      if moveIsPossible(dotX, dotY-1) then
        resu := false;
      if moveIsPossible(dotX-1, dotY+1) then
        resu := false;
      if moveIsPossible(dotX+1, dotY-1) then
        resu := false;
      if moveIsPossible(dotX+1, dotY+1) then
        resu := false;
      if moveIsPossible(dotX+1, dotY) then
        resu := false;
      if moveIsPossible(dotX, dotY+1) then
        resu := false;

      if resu then
      begin
        if turn = 0 then
          winner := player2
        else
          winner := player1;
      end;
    end;

    gameEnd := resu;
  end;

  function TMoves.getWinner: String;
  begin
    getWinner := winner;
  end;

  function TMoves.getHint: SmallLineList;
  var
    hint: SmallLineList;
    direction, i, j: Integer;
    moveThere: array[-1..1] of Boolean;
    movePrior: array[1..3] of Integer;
    chosen: Boolean;
    loser: TPoint;
  begin
    if turn = 0 then
      direction := 1
    else
      direction := -1;

    New(hint);
    hint^.next := nil;
    hint^.p1 := Point(getX(dotX), getY(dotY));

    if dotX < startX then
    begin
      movePrior[1] := 1;
      movePrior[2] := 0;
      movePrior[3] := -1;
    end
    else if dotX = startX then
    begin
      movePrior[1] := 0;
      i := Random(2);
      if i = 0 then
        movePrior[2] := 1
      else
        movePrior[2] := -1;
      movePrior[3] := - movePrior[2];
    end
    else
    begin
      movePrior[1] := -1;
      movePrior[2] := 0;
      movePrior[3] := 1;
    end;

    chosen := false;

    for i := 1 downto -1 do
    begin
      if not chosen then
      begin
        for j := -1 to 1 do
        begin
          moveThere[j] := false;
          if moveIsPossible(dotX + j, dotY + (i * direction)) then
          begin
            connectPoints(dotX, dotY, dotX + j, dotY + (i * direction));
            moveTo(dotX + j, dotY + (i * direction));
            if not gameEnd then
              moveThere[j] := true
            else
            begin
              if (winner = player1) and (turn = 0) then
                moveThere[j] := true
              else if (winner = player2) and (turn = 1) then
                moveThere[j] := true
              else
              begin
                //after moveTo
                loser := Point(getX(dotX), getY(dotY));
              end;
            end;
            moveTo(dotX - j, dotY - (i * direction));
            disconnectPoints(dotX, dotY, dotX + j, dotY + (i * direction));
          end;
        end;

        for j := 1 to 3 do
        begin
          if moveThere[movePrior[j]] then
          begin
            hint^.p2 := Point(getX(dotX + movePrior[j]), getY(dotY + (i * direction)));
            chosen := true;
            break;
          end;
        end;
      end;
    end;

    if chosen then
      getHint := hint
    else
    begin
      hint^.p2 := loser;
      getHint := hint;
    end;
  end;

  function TMoves.getCompMove: SmallLineList;
  var
    hint, resu: SmallLineList;
    direction, i, j: Integer;
    moveThere: array[-1..1] of Boolean;
    movePrior: array[1..3] of Integer;
    chosen: Boolean;
    loser: TPoint;
  begin
    //dummy
    New(resu);
    resu^.p1 := Point(0, 0);
    resu^.p2 := Point(0, 0);
    resu^.next := nil;
    hint := resu;

    if turn = 0 then
      direction := 1
    else
      direction := -1;

    New(resu^.next);
    resu := resu^.next;
    resu^.next := nil;
    resu^.p1 := Point(getX(dotX), getY(dotY));

    movePrior[1] := Random(3) - 1;

    if movePrior[1] = -1 then
    begin
      movePrior[2] := Random(2);
      movePrior[3] := (movePrior[2] + 1) mod 2;
    end
    else if movePrior[1] = 0 then
    begin
      movePrior[2] := Random(2);
      if movePrior[2] = 0 then
      begin
        movePrior[2] := -1;
        movePrior[3] := 1;
      end
      else
        movePrior[3] := -1;
    end
    else
    begin
      movePrior[2] := Random(2) - 1;
      if movePrior[2] = 0 then
        movePrior[3] := -1
      else
        movePrior[3] := 0;
    end;

    chosen := false;

    for i := 1 downto -1 do
    begin
      if not chosen then
      begin
        for j := -1 to 1 do
        begin
          moveThere[j] := false;
          if moveIsPossible(dotX + j, dotY + (i * direction)) then
          begin
            connectPoints(dotX, dotY, dotX + j, dotY + (i * direction));
            moveTo(dotX + j, dotY + (i * direction));
            if not gameEnd then
              moveThere[j] := true
            else
            begin
              if (winner = player1) and (turn = 0) then
                moveThere[j] := true
              else if (winner = player2) and (turn = 1) then
                moveThere[j] := true
              else
              begin
                //After moveTo
                loser := Point(getX(dotX), getY(dotY));
              end;
            end;
            moveTo(dotX - j, dotY - (i * direction));
            disconnectPoints(dotX, dotY, dotX + j, dotY + (i * direction));
          end;
        end;

        for j := 1 to 3 do
        begin
          if moveThere[movePrior[j]] then
          begin
            resu^.p2 := Point(getX(dotX + movePrior[j]), getY(dotY + (i * direction)));
            chosen := true;
            break;
          end;
        end;
      end;
    end;

    if chosen then
      getCompMove := hint^.next
    else
    begin
      resu^.p2 := loser;
      getCompMove := hint^.next;
    end;
  end;

  procedure TMoves.clearHint(var hint: SmallLineList);
  var
    tmp: SmallLineList;
  begin
    while hint <> nil do
    begin
      tmp := hint;
      hint := hint^.next;
      Dispose(tmp);
    end;
  end;

end.

