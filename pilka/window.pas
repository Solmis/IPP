unit window;

{$mode objfpc}{$H+}

interface

  uses
    Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ExtCtrls,
    StdCtrls, Math, moves, history, fields, saveLoad, instrform, newgameform;

  type

    { TeltWindow }

    TeltWindow = class(TForm)
      eltBallPos: TButton;
      eltBtTurn: TButton;
      eltBtStart: TButton;
      eltBtNewGame: TButton;
      eltBtEdit: TButton;
      eltHint: TButton;
      eltInstruct: TButton;
      eltBtBigUndo: TButton;
      eltBtBigRedo: TButton;
      eltBtRedo: TButton;
      eltChange: TButton;
      eltBtUndo: TButton;
      eltBackground: TImage;
      eltBall: TImage;
      eltBtSave: TButton;
      eltBtLoad: TButton;
      eltPlayer1: TLabel;
      eltPlayer2: TLabel;
      eltWinInfo: TLabel;
      eltTimer: TTimer;
      procedure eltBackgroundMouseDown(Sender: TObject; Button: TMouseButton;
        Shift: TShiftState; X, Y: Integer);
      procedure eltBackgroundMouseUp(Sender: TObject; Button: TMouseButton;
        Shift: TShiftState; X, Y: Integer);
      procedure eltBallPosClick(Sender: TObject);
      procedure eltBtEditClick(Sender: TObject);
      procedure eltBtNewGameClick(Sender: TObject);
      procedure eltBackgroundClick(Sender: TObject);
      procedure eltBackgroundMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
      procedure eltBtBigRedoClick(Sender: TObject);
      procedure eltBtBigUndoClick(Sender: TObject);
      procedure eltBtLoadClick(Sender: TObject);
      procedure eltBtRedoClick(Sender: TObject);
      procedure eltBtSaveClick(Sender: TObject);
      procedure eltBtStartClick(Sender: TObject);
      procedure eltBtTurnClick(Sender: TObject);
      procedure eltBtUndoClick(Sender: TObject);
      procedure eltChangeClick(Sender: TObject);
      procedure eltHintClick(Sender: TObject);
      procedure eltInstructClick(Sender: TObject);
      procedure eltTimerTimer(Sender: TObject);
      procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
      procedure FormCreate(Sender: TObject);
    private
      { private declarations }

      //Zmienia pozycję piłki (grafiki) na planszy na podane x, y
      procedure moveBall(const x, y: Integer);

      //Przekazuje ruch następnemu graczowi
      procedure changePlayer;

      //Zmienia kolor ostatniego ruchu na szary, gdy rozpoczął się następny
      procedure newMove;

      //Rysuje puste boisko
      procedure drawPitch;

      //Rysuje boisko wraz z wykonanymi ruchami
      procedure redrawPitch;

      //Sprawdza czyja jest kolej (dopisuje 'rusza się', sprawdza, czy ktoś wygrał,
      //                           zleca wyk. ruchu gracza komp., gdy jego kolej)
      procedure updateTurn;

      //Wykonuje ruch gracza komputerowego
      procedure makeComputerMove;

      //Rysuje na planszy łamaną będąca proponowanym ruchem
      procedure showHint;

      //Wykonuje ruch na pole (dX, dY), jeśli jest to możliwe
      procedure tryMove(const dX, dY: Integer);

      //Przystosowuje okienko do edycji stanu gry
      procedure startEdit;

      //Kończy etap edycji i powraca do gry
      procedure finishEdit;


      //DOT_SIZE - rozmiar piłki
      //FONT_H - wysokość czcionki, którą napisane są imiona graczy
      //PITCH_LN_W - grubość linii granicznej boiska
      //MOVE_LN_W - grubość linii, jaką rysowane są ruchy
      //GRID_LN_W - grubość linii, które tworzą siatkę planszy
      //MAX_NAME_LEN - maksymalna ilość znaków w imieniu gracza
      const
        DOT_SIZE = 10;
        DOT_HALF = DOT_SIZE div 2;
        FONT_H = 30;
        MY_GREEN = 1726998;
        PITCH_LN_W = 4;
        MOVE_LN_W = 2;
        GRID_LN_W = 2;
        MAX_NAME_LEN = 10;

    public
      { public declarations }

      //Tworzy nową grę
      procedure newGame;

      //Ładuje grę z pliku
      procedure loadGame;

      //hintTaken - określa, czy gracz korzysta obecnie z podpowiedzi
      //hintTurn - nr porządkowy gracza, który korzysta z podpowiedzi
      //editing - określa, czy użytkownik jest w trakcie edycji stanu gry
      //firstPoint - pierwszy punkt wybierany do narysowania linii
      //editChosenField - określa, czy po naciśnięciu LPM lub PPM został wybrany punkt [edytor]
      var
        mouseX, mouseY, hintTurn: Integer;
        hintTaken, editing, chooseBallPos, editChosenField: Boolean;
        firstPoint: TPoint;
    end;

  var
    eltWindow: TeltWindow;

implementation

{$R *.lfm}

  { TeltWindow }

  procedure TeltWindow.moveBall(const x, y: Integer);
  begin
    with eltBall do
    begin
      Left := getX(x) - DOT_HALF;
      Top := getY(y) - DOT_HALF;
    end;
  end;

  procedure TeltWindow.updateTurn;
  begin
    with objMoves do
    begin
      if turn = 0 then
      begin
        eltPlayer1.Caption := player1 + ' (rusza się)';
        eltPlayer1.Font.Color := clYellow;
        eltPlayer2.Caption := player2;
        eltPlayer2.Font.Color := clWhite;
      end
      else
      begin
        eltPlayer1.Caption := player1;
        eltPlayer1.Font.Color := clWhite;
        eltPlayer2.Caption := player2 + ' (rusza się)';
        eltPlayer2.Font.Color := clYellow;
      end;
      eltPlayer1.Left := getX(goalLeft);
      eltPlayer1.Top := getY(-1) - FONT_H;
      eltPlayer2.Left := getX(goalLeft);
      eltPlayer2.Top := getY(boardHeight + 1);

      if gameEnd then
        eltWinInfo.Caption := 'Wygrał gracz'#10' ' + getWinner
      else
        eltWinInfo.Caption := '';

      if isComputer[turn] and not gameEnd and (gameType = 1) then
      begin
        if not eltTimer.Enabled then
          eltTimer.Enabled := true;
      end;
    end;
  end;

  procedure TeltWindow.changePlayer;
  begin
    with objMoves do
    begin
      Inc(turn);
      turn := (turn mod 2);
      firstMove := true;
      updateTurn;
    end;
  end;

  procedure TeltWindow.newMove;
  var
    tmp: LineList;
  begin
    with eltBackground.Canvas do
    begin
      Pen.Color := clGray;
      Pen.Width := MOVE_LN_W;
      if objHistory.lastMoveIterator then
      begin
        while objHistory.hasNext do
        begin
          tmp := objHistory.nextValue;
          Line(tmp^.p1, tmp^.p2);
        end;
      end;
    end;
  end;

  procedure TeltWindow.newGame;
  begin
    hintTaken := false;
    editing := false;
    objHistory.init;
    with objMoves do
    begin
      init;
      drawPitch;
      eltBtUndo.Enabled := false;
      eltBtRedo.Enabled := false;
      eltBtBigRedo.Enabled := false;
      eltBtBigUndo.Enabled := false;
      //Start position
      moveBall(getDotX, getDotY);
      case gameType of
        0: begin
             repeat
               player1 := InputBox('Podaj imię pierwszego gracza', 'Maks. 10 liter', 'Gracz1');
             until Length(player1) <= MAX_NAME_LEN;
             player2 := InputBox('Podaj imię drugiego gracza', 'Maks. 10 liter', 'Gracz2');
             while (player1 = player2) or (Length(player2) > 10) do
             begin
               if player1 = player2 then
                 player2 := InputBox('Podaj imię drugiego gracza', 'Imiona muszą być różne!', 'Gracz2')
               else
                 player2 := InputBox('Podaj imię drugiego gracza', 'Maks. 10 liter!', 'Gracz2');
             end;
             eltBtStart.Visible := false;
           end;
        1: begin
             repeat
               player1 := InputBox('Podaj imię pierwszego gracza', 'Maks. 10 liter', 'Gracz');
             until Length(player1) <= MAX_NAME_LEN;
             if player1 <> '[Komputer]' then
               player2 := '[Komputer]'
             else
               player2 := '[GraczKomp]';
             eltBtStart.Visible := false;
           end;
        2: begin
             player1 := '[Komp_1]';
             player2 := '[Komp_2]';
             eltBtStart.Visible := true;
           end;
      end;
      turn := 0;

      if gameType <> 2 then
        updateTurn;
    end;
  end;

  procedure TeltWindow.loadGame;
  begin
    updateTurn;
    redrawPitch;
    updateTurn;
    eltBtRedo.Enabled := objHistory.canRedo;
    eltBtBigRedo.Enabled := objHistory.canRedo;
    eltBtUndo.Enabled := objHistory.canUndo;
    eltBtBigUndo.Enabled := objHistory.canUndo;
    editing := false;
  end;

  procedure TeltWindow.drawPitch;
  var
    i, w, h: Integer;
  begin
    with eltBackground.Canvas do
    begin
      //Green background
      Brush.Color := MY_GREEN;
      Pen.Color := MY_GREEN;
      Rectangle(0, 0, eltBackground.Width, eltBackground.Height);
      //Grid
      Pen.Color := clGreen;
      Pen.Width := GRID_LN_W;
      w := max(boardWidth + 1, boardHeight + 2);
      h := w + (IMG_DIFF div fieldSize) + 1;
      for i := -1 to w do
        Line(getX(i), 0, getX(i), eltBackground.Height);
      for i := -1 to h do
        Line(0, getY(i), eltBackground.Width, getY(i));
      //White border
      Pen.Color := clWhite;
      Pen.Width := PITCH_LN_W;

      Line(getX(0), getY(0), getX(0), getY(boardHeight));
      Line(getX(boardWidth), getY(0), getX(boardWidth), getY(boardHeight));

      Line(getX(0), getY(0), getX(goalLeft), getY(0));
      Line(getX(goalRight), getY(0), getX(boardWidth), getY(0));
      Line(getX(0), getY(boardHeight), getX(goalLeft), getY(boardHeight));
      Line(getX(goalRight), getY(boardHeight), getX(boardWidth), getY(boardHeight));

      Line(getX(goalLeft), getY(-1), getX(goalRight), getY(-1));
      Line(getX(goalLeft), getY(boardHeight + 1), getX(goalRight), getY(boardHeight + 1));

      Line(getX(goalLeft), getY(-1), getX(goalLeft), getY(0));
      Line(getX(goalRight), getY(-1), getX(goalRight), getY(0));
      Line(getX(goalLeft), getY(boardHeight), getX(goalLeft), getY(boardHeight + 1));
      Line(getX(goalRight), getY(boardHeight), getX(goalRight), getY(boardHeight + 1));
    end;
  end;

  procedure TeltWindow.redrawPitch;
  var
    tmp: LineList;
    stmp: SmallLineList;
  begin
    drawPitch;

    with eltBackground.Canvas do
    begin
      Pen.Color := clGray;
      Pen.Width := MOVE_LN_W;
    end;

    with objHistory do
    begin
      resetSmIterator;
      while smHasNext do
      begin
        stmp := smNextValue;
        eltBackground.Canvas.Line(stmp^.p1, stmp^.p2);
      end;
      resetIterator;
      tmp := nil;
      while hasNext do
      begin
        tmp := nextValue;
        eltBackground.Canvas.Line(tmp^.p1, tmp^.p2);
      end;
      if tmp = nil then
        objMoves.moveTo(startX, startY)
      else
        objMoves.moveTo(toFieldX(tmp^.p2.x), toFieldY(tmp^.p2.y));
      moveBall(objMoves.getDotX, objMoves.getDotY);
    end;

    with eltBackground.Canvas do
    begin
      Pen.Color := clWhite;
      if objHistory.lastMoveIterator then
      begin
        while objHistory.hasNext do
        begin
          tmp := objHistory.nextValue;
          Line(tmp^.p1, tmp^.p2);
        end;
      end;
    end;
  end;

  procedure TeltWindow.FormCreate(Sender: TObject);
  begin
    makeMoves;
    makeHistory;
    objHistory.init;
    objMoves.init;

    //Drawing ball
    with eltBall.Canvas do
    begin
      Brush.Color := MY_GREEN;
      Pen.Color := MY_GREEN;
      Rectangle(0, 0, DOT_SIZE, DOT_SIZE);
      Pen.Width := DOT_SIZE;
      Pen.Color := clWhite;
      Line(DOT_HALF, DOT_HALF, DOT_HALF, DOT_HALF);
    end;
  end;

  procedure TeltWindow.eltBackgroundMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
  begin
    mouseX := x;
    mouseY := y;
  end;

  procedure TeltWindow.eltBtBigRedoClick(Sender: TObject);
  begin
    objHistory.redoEntireMove;
    redrawPitch;
    updateTurn;
    eltBtUndo.Enabled := objHistory.canUndo;
    eltBtBigUndo.Enabled := objHistory.canUndo;
    eltBtRedo.Enabled := objHistory.canRedo;
    eltBtBigRedo.Enabled := objHistory.canRedo;
  end;

  procedure TeltWindow.eltBtBigUndoClick(Sender: TObject);
  begin
    objHistory.undoEntireMove;
    redrawPitch;
    updateTurn;
    eltBtUndo.Enabled := objHistory.canUndo;
    eltBtBigUndo.Enabled := objHistory.canUndo;
    eltBtRedo.Enabled := objHistory.canRedo;
    eltBtBigRedo.Enabled := objHistory.canRedo;
  end;

  procedure TeltWindow.eltBtLoadClick(Sender: TObject);
  begin
    if loadQuery then
    begin
      updateTurn;
      redrawPitch;
      updateTurn;
      eltBtRedo.Enabled := objHistory.canRedo;
      eltBtBigRedo.Enabled := objHistory.canRedo;
      eltBtUndo.Enabled := objHistory.canUndo;
      eltBtBigUndo.Enabled := objHistory.canUndo;
    end;
  end;

  procedure TeltWindow.eltBtSaveClick(Sender: TObject);
  begin
    saveQuery;
  end;

procedure TeltWindow.eltBtStartClick(Sender: TObject);
begin
  with eltBtStart do
  begin
    if Caption = 'Start' then
    begin
      updateTurn;
      eltTimer.Enabled := true;
      Caption := 'Stop';
    end
    else
    begin
      Caption := 'Start';
      eltTimer.Enabled := false;
    end;
  end;
end;

procedure TeltWindow.eltBtTurnClick(Sender: TObject);
begin
  if MessageDlg('Czy rozpoczynać ma gracz ' + objMoves.player1 + '?',
                mtConfirmation, [mbYes, mbNo], 0) = mrYes then
  begin
      if objHistory.historyLost then
        objHistory.setTurn(0);
  end
  else
  begin
    if objHistory.historyLost then
      objHistory.setTurn(1);
  end;
  updateTurn;
  eltTimer.Enabled := false; //?
end;

  procedure TeltWindow.eltBtRedoClick(Sender: TObject);
  begin
    objHistory.redoMove;
    redrawPitch;
    updateTurn;
    eltBtRedo.Enabled := objHistory.canRedo;
    eltBtBigRedo.Enabled := objHistory.canRedo;
    eltBtUndo.Enabled := objHistory.canUndo;
    eltBtBigUndo.Enabled := objHistory.canUndo;
  end;

  procedure TeltWindow.eltBtUndoClick(Sender: TObject);
  begin
    objHistory.undoMove;
    redrawPitch;
    updateTurn;
    eltBtUndo.Enabled := objHistory.canUndo;
    eltBtBigUndo.Enabled := objHistory.canUndo;
    eltBtRedo.Enabled := objHistory.canRedo;
    eltBtBigRedo.Enabled := objHistory.canRedo;
  end;

  procedure TeltWindow.eltChangeClick(Sender: TObject);
  var
    new_width, new_height: Integer;
    s: String;
  begin
    repeat
      s := InputBox('Podaj szerokość planszy',
                    'Szerokość musi być liczbą parzystą z przedziału ' + IntToStr(MIN_BOARD_SIZE)
                    + ' - ' + IntToStr(MAX_BOARD_SIZE), IntToStr(STD_BOARD_WIDTH));
      new_width := StrToInt(s);
    until (new_width mod 2 = 0) and (new_width >= MIN_BOARD_SIZE) and (new_width <= MAX_BOARD_SIZE);

    repeat
      s := InputBox('Podaj wysokość planszy',
                    'Wysokość musi być liczbą parzystą z przedziału ' + IntToStr(MIN_BOARD_SIZE)
                    + ' - ' + IntToStr(MAX_BOARD_SIZE), IntToStr(STD_BOARD_HEIGHT));
      new_height := StrToInt(s);
    until (new_height mod 2 = 0) and (new_height >= MIN_BOARD_SIZE) and (new_height <= MAX_BOARD_SIZE);

    if (new_width <> boardWidth) or (new_height <> boardHeight) then
    begin
      if MessageDlg('Czy chcesz rozpocząć grę od nowa?', mtConfirmation, [mbYes, mbNo], 0) = mrYes then
      begin
        if eltNewGameForm.Execute then
        begin
          initFields(new_width, new_height);
          objHistory.cleanUp;
          eltWindow.newGame;
        end;
      end;
    end;
  end;

  procedure TeltWindow.eltHintClick(Sender: TObject);
  begin
    with objMoves do
    begin
      if not gameEnd and not isComputer[turn] then
      begin
        redrawPitch;
        showHint;
      end;
    end;
  end;

  procedure TeltWindow.showHint;
  var
    resu, iter: SmallLineList;
    x, y, dX, dY, prevX, prevY: Integer;
  begin
    resu := objMoves.getHint;
    hintTaken := true;
    hintTurn := objMoves.turn;
    iter := resu;

    x := objMoves.getDotX;
    y := objMoves.getDotY;

    with eltBackground.Canvas.Pen do
    begin
      Color := clYellow;
      Width := MOVE_LN_W;
    end;

    with objMoves do
    begin
      dX := toFieldX(resu^.p2.x);
      dY := toFieldY(resu^.p2.y);
      if pointAlone(getNum(dX, dY)) then
        turn := (turn + 1) mod 2;
      connectPoints(x, y, dX, dY);
      moveTo(dX, dY);

      while objMoves.turn = hintTurn do
      begin
        iter^.next := objMoves.getHint;
        iter := iter^.next;

        dX := toFieldX(iter^.p2.x);
        dY := toFieldY(iter^.p2.y);
        prevX := toFieldX(iter^.p1.x);
        prevY := toFieldY(iter^.p1.y);

        if pointAlone(getNum(dX, dY)) then
          turn := (turn + 1) mod 2;
        connectPoints(prevX, prevY, dX, dY);
        moveTo(dX, dY);
      end;
    end;

    iter := resu;
    while iter <> nil do
    begin
      with eltBackground.Canvas do
        Line(iter^.p1, iter^.p2);
      dX := toFieldX(iter^.p2.x);
      dY := toFieldY(iter^.p2.y);
      prevX := toFieldX(iter^.p1.x);
      prevY := toFieldY(iter^.p1.y);
      objMoves.disconnectPoints(prevX, prevY, dX, dY);
      iter := iter^.next;
    end;

    objMoves.moveTo(x, y);
    objMoves.turn := hintTurn;
    objMoves.clearHint(resu);
  end;

  procedure TeltWindow.eltInstructClick(Sender: TObject);
  begin
    eltInstrForm.Show;
  end;

procedure TeltWindow.eltTimerTimer(Sender: TObject);
begin
  if objMoves.isComputer[objMoves.turn] and not objMoves.gameEnd and not editing then
    makeComputerMove
  else
    eltTimer.Enabled := false;
end;

  procedure TeltWindow.FormClose(Sender: TObject; var CloseAction: TCloseAction);
  begin
    objHistory.cleanUp;
    objHistory.Free;
  end;

  procedure TeltWindow.eltBackgroundClick(Sender: TObject);
  var
    dX, dY: Integer;
  begin
    if not editing then
    begin
      if not objMoves.isComputer[objMoves.turn] then
      begin
        if objMoves.moveIsPossible(toFieldX(mouseX), toFieldY(mouseY)) then
        begin
          tryMove(toFieldX(mouseX), toFieldY(mouseY));

          if objMoves.gameEnd then
            hintTaken := false;

          if hintTaken then
          begin
            redrawPitch;
            if objMoves.turn = hintTurn then
              showHint
            else
              hintTaken := false;
          end;
        end;
      end;
    end
    else
    begin
      dX := toFieldX(mouseX);
      dY := toFieldY(mouseY);

      if (dX >= 0) and (dX <= boardWidth) and (dY >= 0) and (dY <= boardHeight) then
      begin
        if chooseBallPos then
        begin
          if (dX <> objMoves.getDotX) or (dY <> objMoves.getDotY) then
          begin
            if objHistory.historyLost then
            begin
              objMoves.moveTo(dX, dY);
              moveBall(dX, dY);
              startX := dX;
              startY := dY;
            end;
          end;
          chooseBallPos := false;
          eltBallPos.Caption := 'Pozycja piłki';
        end;
      end;
    end;
  end;

  procedure TeltWindow.eltBtNewGameClick(Sender: TObject);
  begin
    if eltNewGameForm.Execute then
    begin
      objHistory.cleanUp;
      eltWindow.newGame;
    end;
  end;

  procedure TeltWindow.eltBtEditClick(Sender: TObject);
  begin
    if editing then
    begin
      finishEdit;
      eltBtEdit.Caption := 'Edytuj stan gry';
    end
    else
    begin
      startEdit;
      eltBtEdit.Caption := 'Powróć do gry';
    end;
  end;

procedure TeltWindow.eltBallPosClick(Sender: TObject);
begin
  if chooseBallPos then
  begin
    chooseBallPos := false;
    eltBallPos.Caption := 'Pozycja piłki';
  end
  else
  begin
    eltBallPos.Caption := 'Anuluj';
    chooseBallPos := true;
    ShowMessage('Kliknij LPM na planszę, by wybrać pozycję piłki');
  end;
end;

procedure TeltWindow.eltBackgroundMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
  dX, dY: Integer;
begin
  editChosenField := false;

  if editing and not chooseBallPos then
  begin
    dX := toFieldX(x);
    dY := toFieldY(y);
    if (dX >= 0) and (dX <= boardWidth) and (dY >= 0) and (dY <= boardHeight) then
    begin
      firstPoint := Point(getX(dX), getY(dY));
      editChosenField := true;
    end;
  end;
end;

procedure TeltWindow.eltBackgroundMouseUp(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
  dX, dY, diffX, diffY: Integer;
begin
  if editing and not chooseBallPos and editChosenField then
  begin
    dX := toFieldX(x);
    dY := toFieldY(y);
    diffX := abs(dX - toFieldX(firstPoint.x));
    diffY := abs(dY - toFieldY(firstPoint.y));
    if (dX >= 0) and (dX <= boardWidth) and (dY >= 0) and (dY <= boardHeight) then
    begin
      if (diffX <= 1) and (diffY <= 1) and (diffX + diffY > 0) then
      begin
        if objHistory.historyLost then
        begin
          if Button = mbLeft then
          begin
            objHistory.addLine(firstPoint, Point(getX(dX), getY(dY)))
          end
          else if Button = mbRight then
            objHistory.removeLine(firstPoint, Point(getX(dX), getY(dY)));
          redrawPitch;
        end;
      end;
    end;
    editChosenField := false;
  end;
end;

  procedure TeltWindow.makeComputerMove;
  var
    sl: SmallLineList;
  begin
    if objMoves.turn = 0 then
      sl := objMoves.getHint
    else
      sl := objMoves.getCompMove;

    if sl = nil then
      ShowMessage('Komputer nie wie co zrobić.')
    else
    begin
      if hintTaken then
        hintTaken := false;
      tryMove(toFieldX(sl^.p2.x), toFieldY(sl^.p2.y));
    end;
  end;

  procedure TeltWindow.tryMove(const dX, dY: Integer);
  var
    oldX, oldY: Integer;
    player_change: Boolean;
  begin
    with objMoves do
    begin
      player_change := false;

      if firstMove then
      begin
        newMove;
        firstMove := false;
      end;

      if (objHistory.addLastMove(Point(getX(getDotX), getY(getDotY)),
                                 Point(getX(dX), getY(dY)), turn)) = true then
      begin
        eltBtUndo.Enabled := true;
        eltBtBigUndo.Enabled := true;
        eltBtRedo.Enabled := false;
        eltBtBigRedo.Enabled := false;

        connectPoints(getDotX, getDotY, dX, dY);
        oldX := getDotX;
        oldY := getDotY;
        moveTo(dX, dY);

        if not gameEnd then
        begin
          disconnectPoints(oldX, oldY, dX, dY);

          if pointAlone(getNum(dX, dY)) then
            player_change := true;

          connectPoints(oldX, oldY, dX, dY);
        end;

        moveTo(oldX, oldY);

        with eltBackground.Canvas do
        begin
          Pen.Width := MOVE_LN_W;
          Pen.Color := clWhite;
          Line(getX(getDotX), getY(getDotY), getX(dX), getY(dY));
        end;

        moveBall(dX, dY);
        moveTo(dX, dY);

        if player_change then
          changePlayer;
      end;
    end;
    updateTurn;
  end;

  procedure TeltWindow.startEdit;
  begin
    eltWinInfo.Visible := false;
    eltBtNewGame.Visible := false;
    eltHint.Visible := false;
    eltBtLoad.Visible := false;
    eltBtSave.Visible := false;
    eltBtRedo.Visible := false;
    eltBtBigRedo.Visible := false;
    eltBtUndo.Visible := false;
    eltBtBigUndo.Visible := false;
    eltChange.Visible := false;

    eltBallPos.Visible := true;
    eltBallPos.Caption := 'Pozycja piłki';
    chooseBallPos := false;
    eltBtTurn.Visible := true;

    objHistory.editedTurn := objMoves.turn;
    editing := true;
    firstPoint := Point(-1, -1);
  end;

  procedure TeltWindow.finishEdit;
  begin
    eltWinInfo.Visible := true;
    eltBtNewGame.Visible := true;
    eltHint.Visible := true;
    eltBtLoad.Visible := true;
    eltBtSave.Visible := true;
    eltBtRedo.Visible := true;
    eltBtBigRedo.Visible := true;
    eltBtUndo.Visible := true;
    eltBtBigUndo.Visible := true;
    eltChange.Visible := true;

    eltBallPos.Visible := false;
    eltBtTurn.Visible := false;

    editing := false;
    objMoves.turn := objHistory.editedTurn;
    with objMoves do
      objHistory.addLastMove(Point(getX(getDotX), getY(getDotY)), Point(getX(getDotX), getY(getDotY)), turn);
    //Aktualizacja startX i startY
    initFields(boardWidth, boardHeight);
    objHistory.updateConnections;
    updateTurn; //?
  end;

end.

