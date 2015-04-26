unit history;

{$mode objfpc}{$H+}

interface

  uses
    Classes, SysUtils, Dialogs, Controls, moves, fields;

  //Tworzy obiekt historii
  procedure makeHistory;

  type
    THistory = class
    private
      { private declarations }

      //Kopiuje linie z listy z historią ruchów do listy bez historii (z moveHistory do withoutHist)
      procedure noMoreHistory;

      //Dodaje ruch do historii
      procedure addMove(const p1, p2: TPoint; const player: Integer);

      //Zwraca true, gdy podana linia znajduje się już w liście withoutHist
      function lineInList(const p1, p2: TPoint): Boolean;

      procedure clearList(var l: LineList);

      //moveHistoryFront -> wskaźnik do początku historii (atrapy)
      //moveHistoryBack -> wskaźnik do momentu w historii, który jest wyświetlany na planszy
      //withoutHist -> wskaźnik do początku listy zawierającej linie bez historii (po edycji)
      //withoutHistBack -> wskaźnik do ost. el. listy withoutHist
      //iter, smIter -> iteratory po listach
      var
        moveHistoryFront, moveHistoryBack, iter: LineList;
        withoutHist, withoutHistBack, smIter: SmallLineList;

      public
        { public declarations }

        procedure init;

        //Próbuje dodać ruch do historii, jeśli gracz odmówi porzucenia historii, to zwraca false.
        function addLastMove(const p1, p2: TPoint; const player: Integer): Boolean;

        //Dodaje linię bez historii do listy withoutHist
        procedure addLine(const p1, p2: TPoint);

        //Usuwa linię z listy bez historii withoutHist
        procedure removeLine(const p1, p2: TPoint);

        //Uaktualnia dane o połączeniach między punktami w objMoves zgodnie z withoutHist
        procedure updateConnections;

        //Ustawia iterator dla ostatniego ruchu w historii
        function lastMoveIterator: Boolean;

        //Ustawia iterator na początku listy (moveHistoryFront)
        procedure resetIterator;

        //Ustawia smIter na początku listy (withoutHist)
        procedure resetSmIterator;

        //Zwraca true, gdy w liście moveHistoryFront znajdują się elementy za iteratorem
        function hasNext: Boolean;

        //Analogicznie, to co powyżej dla listy withoutHist
        function smHasNext: Boolean;

        //Zwraca kolejną wartość z listy moveHistoryFront
        function nextValue: LineList;

        //Zwraca kolejną wartość z listy wihoutHist
        function smNextValue: SmallLineList;


        function canUndo: Boolean;
        function canRedo: Boolean;
        procedure undoMove;
        procedure redoMove;

        //Cofa cały ruch
        procedure undoEntireMove;

        //Przywraca cały ruch
        procedure redoEntireMove;

        //Ustawia historię w odpowiednim momencie (np. po wczytaniu gry z pliku)
        procedure setIndex(const ind: Integer);

        //Pobiera indeks w historii np. na potrzeby zapisania stanu gry do pliku
        function getIndex: Integer;

        //Gdy historia jest pusta, ustala czyj jest ruch [edytor]
        procedure setTurn(const t: Integer);

        //Sprawdza, czy historia jest pusta. Jeśli tak - zwraca true. Jeśli nie - pyta
        //użytkownika, czy chce porzucić historię. Jeśli chce - historia zostaje
        //usunięta i funkcja zwraca true, a jeśli nie chce - funkcja zwraca false.
        function historyLost: Boolean;

        procedure clearSmList(var sl: SmallLineList);
        procedure cleanUp;

        //Trzyma nr id gracza, który ma się ruszyć po zakończeniu edycji
        var
          editedTurn: Integer;
    end;

  var
    objHistory: THistory;

implementation

  procedure makeHistory;
  begin
    objHistory := THistory.Create;
  end;

  procedure THistory.init;
  begin
    //Atrapa
    New(moveHistoryFront);
    moveHistoryFront^.p1 := Point(0, 0);
    moveHistoryFront^.p2 := Point(0, 0);
    moveHistoryFront^.player := -1;
    moveHistoryFront^.next := nil;
    moveHistoryFront^.prev := nil;
    //Atrapa
    New(withoutHist);
    withoutHist^.p1 := Point(0, 0);
    withoutHist^.p2 := Point(0, 0);
    withoutHist^.next := nil;
    withoutHistBack := withoutHist;
    smIter := withoutHist;

    moveHistoryBack := moveHistoryFront;
    iter := moveHistoryFront;
  end;

  procedure THistory.addMove(const p1, p2: TPoint; const player: Integer);
  begin
    New(moveHistoryBack^.next);
    moveHistoryBack^.next^.prev := moveHistoryBack;
    moveHistoryBack := moveHistoryBack^.next;
    moveHistoryBack^.p1 := p1;
    moveHistoryBack^.p2 := p2;
    moveHistoryBack^.player := player;
    moveHistoryBack^.next := nil;
  end;

  function THistory.lineInList(const p1, p2: TPoint): Boolean;
  var
    tmp: SmallLineList;
  begin
    tmp := withoutHist^.next;
    lineInList := false;
    while tmp <> nil do
    begin
      if (PointsEqual(tmp^.p1, p1) and PointsEqual(tmp^.p2, p2)) or
         (PointsEqual(tmp^.p1, p2) and PointsEqual(tmp^.p2, p1)) then
      begin
        lineInList := true;
        tmp := nil;
      end
      else
        tmp := tmp^.next;
    end;
  end;

  procedure THistory.clearList(var l: LineList);
  var
    tmp: LineList;
  begin
    while l <> nil do
    begin
      tmp := l;
      l := l^.next;
      Dispose(tmp);
    end;
  end;

  procedure THistory.clearSmList(var sl: SmallLineList);
  var
    tmp: SmallLineList;
  begin
    while sl <> nil do
    begin
      tmp := sl;
      sl := sl^.next;
      Dispose(tmp);
    end;
  end;

  function THistory.addLastMove(const p1, p2: TPoint; const player: Integer): Boolean;
  begin
    if moveHistoryBack^.next <> nil then
    begin
      if MessageDlg('Ta operacja wymaga usunięcia historii ruchów od tego miejsca w przyszłość. Usunąć?',
                    mtConfirmation, [mbYes, mbNo], 0) = mrYes then
      begin
        clearList(moveHistoryBack^.next);
        addMove(p1, p2, player);
        addLastMove := true;
      end
      else
        addLastMove := false;
    end;
    addMove(p1, p2, player);
    addLastMove := true;
  end;

  procedure THistory.addLine(const p1, p2: TPoint);
  begin
    if not lineInList(p1, p2) then
    begin
      New(withoutHistBack^.next);
      withoutHistBack := withoutHistBack^.next;
      withoutHistBack^.p1 := p1;
      withoutHistBack^.p2 := p2;
      withoutHistBack^.next := nil;
    end;
  end;

  procedure THistory.removeLine(const p1, p2: TPoint);
  var
    tmp, prev: SmallLineList;
  begin
    prev := withoutHist;
    tmp := withoutHist^.next;
    while tmp <> nil do
    begin
      if (PointsEqual(tmp^.p1, p1) and PointsEqual(tmp^.p2, p2)) or
         (PointsEqual(tmp^.p1, p2) and PointsEqual(tmp^.p2, p1)) then
      begin
        prev^.next := tmp^.next;
        if prev^.next = nil then
          withoutHistBack := prev;
        Dispose(tmp);
        tmp := nil;
      end
      else
      begin
        prev := tmp;
        tmp := tmp^.next;
      end;
    end;
  end;

  procedure THistory.updateConnections;
  var
    tmp: LineList;
    stmp: SmallLineList;
  begin
    stmp := withoutHist^.next;
    tmp := moveHistoryFront^.next;
    with objMoves do
    begin
      clearConnections;
      while stmp <> nil do
      begin
        connectPoints(toFieldX(stmp^.p1.x), toFieldY(stmp^.p1.y), toFieldX(stmp^.p2.x), toFieldY(stmp^.p2.y));
        stmp := stmp^.next;
      end;
      while tmp <> nil do
      begin
        connectPoints(toFieldX(tmp^.p1.x), toFieldY(tmp^.p1.y), toFieldX(tmp^.p2.x), toFieldY(tmp^.p2.y));
        tmp := tmp^.next;
      end;
    end;
  end;

  procedure THistory.resetIterator;
  begin
    iter := moveHistoryFront;
  end;

  procedure THistory.resetSmIterator;
  begin
    smIter := withoutHist;
  end;

  function THistory.smHasNext: Boolean;
  begin
    smHasNext := (smIter^.next <> nil);
  end;

  function THistory.hasNext: Boolean;
  begin
    hasNext := ((moveHistoryFront <> moveHistoryBack) and (iter <> moveHistoryBack));
  end;

  function THistory.nextValue: LineList;
  begin
    iter := iter^.next;
    nextValue := iter;
  end;

  function THistory.smNextValue: SmallLineList;
  begin
    smIter := smIter^.next;
    smNextValue := smIter;
  end;

  function THistory.lastMoveIterator: Boolean;
  var
    player: Integer;
    tmp: LineList;
  begin
    player := moveHistoryBack^.player;
    tmp := moveHistoryBack;
    if tmp^.prev = nil then
      lastMoveIterator := false
    else
    begin
      while (tmp^.prev <> nil) and (tmp^.player = player) do
        tmp := tmp^.prev;
      iter := tmp;
      lastMoveIterator := true;
    end;
  end;

  function THistory.canUndo: Boolean;
  begin
    if moveHistoryBack^.prev <> nil then
      canUndo := true
    else
      canUndo := false;
  end;

  function THistory.canRedo: Boolean;
  begin
    if moveHistoryBack^.next <> nil then
      canRedo := true
    else
      canRedo := false;
  end;

  procedure THistory.undoMove;
  begin
    if canUndo then
    begin
      with moveHistoryBack^ do
        objMoves.disconnectPoints(toFieldX(p1.x), toFieldY(p1.y), toFieldX(p2.x), toFieldY(p2.y));
      moveHistoryBack := moveHistoryBack^.prev;
      objMoves.turn := moveHistoryBack^.next^.player;
    end;
  end;

  procedure THistory.undoEntireMove;
  var
    player: Integer;
  begin
    player := moveHistoryBack^.player;
    while canUndo and (moveHistoryBack^.player = player) do
    begin
      with moveHistoryBack^ do
        objMoves.disconnectPoints(toFieldX(p1.x), toFieldY(p1.y), toFieldX(p2.x), toFieldY(p2.y));
      moveHistoryBack := moveHistoryBack^.prev;
    end;
    objMoves.turn := moveHistoryBack^.next^.player;
  end;

  procedure THistory.redoMove;
  begin
    if canRedo then
    begin
      moveHistoryBack := moveHistoryBack^.next;
      with moveHistoryBack^ do
        objMoves.connectPoints(toFieldX(p1.x), toFieldY(p1.y), toFieldX(p2.x), toFieldY(p2.y));
      if moveHistoryBack^.next = nil then //!
        objMoves.turn := (moveHistoryBack^.player + 1) mod 2
      else
        objMoves.turn := moveHistoryBack^.next^.player;
    end;
  end;

  procedure THistory.redoEntireMove;
  var
    player: Integer;
  begin
    if canRedo then
    begin
      moveHistoryBack := moveHistoryBack^.next;
      player := moveHistoryBack^.player;
      if canRedo and (moveHistoryBack^.next^.player = player) then
      begin
        while canRedo and (moveHistoryBack^.player = player) do
        begin
          moveHistoryBack := moveHistoryBack^.next;
          with moveHistoryBack^ do
            objMoves.connectPoints(toFieldX(p1.x), toFieldY(p1.y), toFieldX(p2.x), toFieldY(p2.y));
        end;
      end;

      if moveHistoryBack^.next = nil then //!
        objMoves.turn := (moveHistoryBack^.player + 1) mod 2
      else
        objMoves.turn := moveHistoryBack^.next^.player;
    end;
  end;

  procedure THistory.setIndex(const ind: Integer);
  var
    i: Integer;
    tmp: LineList;
  begin
    i := 0;
    tmp := moveHistoryFront;
    while i < ind do
    begin
      if tmp = nil then
        ShowMessage('Błąd: Błędny indeks momentu w historii!')
      else
        tmp := tmp^.next;
      Inc(i);
    end;
    if tmp = nil then ShowMessage('Błąd: Błędny indeks momentu w historii!');
    moveHistoryBack := tmp;
  end;

  function THistory.getIndex: Integer;
  var
    i: Integer;
    tmp: LineList;
  begin
    i := 0;
    tmp := moveHistoryFront;
    while tmp <> moveHistoryBack do
    begin
      tmp := tmp^.next;
      Inc(i);
    end;
    getIndex := i;
  end;

  procedure THistory.setTurn(const t: Integer);
  begin
    if historyLost then
      editedTurn := t;
  end;

  function THistory.historyLost: Boolean;
  begin
    if moveHistoryFront^.next = nil then
      historyLost := true
    else
    begin
      if MessageDlg('Ta operacja wymaga usunięcia historii ruchów. Usunąć?',
                    mtConfirmation, [mbYes, mbNo], 0) = mrYes then
      begin
        noMoreHistory;
        historyLost := true;
      end
      else
        historyLost := false;
    end;
  end;

  procedure THistory.noMoreHistory;
  var
    tmp: LineList;
  begin
    tmp := moveHistoryFront^.next;
    while tmp <> nil do
    begin
      New(withoutHistBack^.next);
      withoutHistBack := withoutHistBack^.next;
      with withoutHistBack^ do
      begin
        next := nil;
        p1 := tmp^.p1;
        p2 := tmp^.p2;
      end;
      tmp := tmp^.next;
    end;
    clearList(moveHistoryFront^.next);
    moveHistoryBack := moveHistoryFront;
  end;

  procedure THistory.cleanUp;
  begin
    clearList(moveHistoryFront);
    clearSmList(withoutHist);
    moveHistoryBack := nil;
  end;

end.

