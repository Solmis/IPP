unit saveLoad;

{$mode objfpc}{$H+}

interface

  uses
    Classes, SysUtils, history, moves, fields, Dialogs, Controls;

  //Otwiera okno dialogowe wczytywania stanu gry z pliku, zwraca true, gdy gracz
  //wybierze plik, a false, gdy zrezygnuje z wczytywania
  function loadQuery: Boolean;

  //Otwiera okno dialogowe zapisu stanu gry do pliku
  procedure saveQuery;

  //Wczytuje stan gry z pliku o podanej ścieżce dostępu
  procedure loadSavedGame(const path: String);

  //Zapisuje stan gry do pliku o podanej ścieżce dostępu
  procedure saveGame(const path: String);

implementation

  function loadQuery: Boolean;
  var
    loadDlg: TOpenDialog;
    path: String;
  begin
    path := '';
    loadDlg := TOpenDialog.Create(nil);
    loadDlg.InitialDir := GetCurrentDir;
    loadDlg.Options := [ofFileMustExist];
    loadDlg.Filter := 'Plik stanu gry (*.sav)|*.sav';

    if loadDlg.Execute then
      path := loadDlg.FileName;

    loadDlg.Free;

    if path <> '' then
    begin
      loadSavedGame(path);
      loadQuery := true;
    end
    else
      loadQuery := false;
  end;

  procedure saveQuery;
  var
    saveDlg: TSaveDialog;
    path: String;
  begin
    path := '';
    saveDlg := TSaveDialog.Create(nil);
    saveDlg.Title := 'Wybierz ścieżkę zapisu stanu gry';
    saveDlg.InitialDir := GetCurrentDir;
    saveDlg.Filter := 'Plik stanu gry (*.sav)|*.sav';

    if saveDlg.Execute then
      path := saveDlg.FileName;

    saveDlg.Free;

    if path <> '' then
    begin
      if (Length(path) <= 4) then
        path := path + '.sav'
      else
      begin
        if Copy(path, Length(path)-3, Length(path)) <> '.sav' then
          path := path + '.sav';
      end;
      saveGame(path);
      ShowMessage('Pomyślnie zapisano stan gry');
    end
  end;

  procedure loadSavedGame(const path: String);
  var
    f: TextFile;
    //three: "---"
    three, s: String;
    ind, i: Integer;
    data: array[1..5] of Integer;
  begin
    objHistory.cleanUp;
    objHistory.init;

    if FileExists(path) then
    begin
      AssignFile(f, path);
      Reset(f);
      ReadLn(f, objMoves.player1);
      ReadLn(f, objMoves.player2);
      ReadLn(f, s);
      gameType := StrToInt(s);
      ReadLn(f, s);
      boardWidth := StrToInt(s);
      ReadLn(f, s);
      boardHeight := StrToInt(s);
      initFields(boardWidth, boardHeight);
      ReadLn(f, three);
      data[1] := 0;
      while data[1] <> -1 do
      begin
        for i := 1 to 4 do
          Read(f, data[i]);
        if data[1] <> -1 then
          objHistory.addLine(Point(data[1], data[2]), Point(data[3], data[4]));
        ReadLn(f);
      end;
      ReadLn(f, three);
      //Reading index
      ReadLn(f, s);
      ind := StrToInt(s);
      ReadLn(f, three);
      while not Eof(f) do
      begin
        for i := 1 to 5 do
          Read(f, data[i]);
        objHistory.addLastMove(Point(data[1], data[2]), Point(data[3], data[4]), data[5]);
        ReadLn(f);
      end;
      objMoves.turn := data[5]; //?
      CloseFile(f);
      objHistory.setIndex(ind);
      objHistory.updateConnections;
      objMoves.computers;
    end;
  end;

  procedure saveGame(const path: String);
  var
    f: TextFile;
    tmp: LineList;
    stmp: SmallLineList;
  begin
    AssignFile(f, path);
    Rewrite(f);
    WriteLn(f, objMoves.player1);
    WriteLn(f, objMoves.player2);
    WriteLn(f, gameType);
    WriteLn(f, boardWidth);
    WriteLn(f, boardHeight);
    WriteLn(f, '---');
    objHistory.resetSmIterator;
    while objHistory.smHasNext do
    begin
      stmp := objHistory.smNextValue;
      WriteLn(f, stmp^.p1.x, ' ', stmp^.p1.y, ' ', stmp^.p2.x, ' ', stmp^.p2.y);
    end;
    WriteLn(f, '-1 -1 -1 -1');
    WriteLn(f, '---');
    WriteLn(f, objHistory.getIndex);
    WriteLn(f, '---');
    objHistory.resetIterator;
    while objHistory.hasNext do
    begin
      tmp := objHistory.nextValue;
      WriteLn(f, tmp^.p1.x, ' ', tmp^.p1.y, ' ', tmp^.p2.x, ' ', tmp^.p2.y, ' ', tmp^.player);
    end;
    CloseFile(f);
  end;

end.

