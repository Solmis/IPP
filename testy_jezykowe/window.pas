unit window;

{$mode objfpc}{$H+}

interface

  uses
    Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
    ExtCtrls, Menus, TestSetUnit, GapUnit;

  type

    { TeltWindow }

    TeltWindow = class(TForm)
      eltScore: TButton;
      eltCheck: TButton;
      eltBtLoad: TButton;
      eltCorrect: TLabel;
      eltIncorrect: TLabel;
      eltNext: TButton;
      eltStoreroom: TPanel;
      eltControl: TPanel;
      eltProg: TLabel;
      eltProgNum: TLabel;
      eltCorrNum: TLabel;
      eltIncorrNum: TLabel;
      eltStart: TLabel;
      eltMainBox: TPanel;
      eltEnd: TLabel;
      procedure eltBtLoadClick(Sender: TObject);
      procedure eltCheckClick(Sender: TObject);
      procedure eltNextClick(Sender: TObject);
      procedure eltScoreClick(Sender: TObject);
      procedure FormCreate(Sender: TObject);
      procedure eltStartClick(Sender: TObject);
      procedure FormDestroy(Sender: TObject);
      procedure labelOnClick(Sender: TObject);
      procedure gapOnClick(Sender: TObject);
      procedure createGaps(wl: WordList);
      procedure createLabels(wl: WordList);
    private
      { private declarations }
    public
      { public declarations }
      chosenElt: TLabel;
      tests: TestSet;
      gaps, words, stats: WordList;
      correct, incorrect, all, ind, filled: LongInt;
      path: String;
      testIsFinal: Boolean;
    end;

  var
    eltWindow: TeltWindow;

implementation

  {$R *.lfm}

  { TeltWindow }

  //Odświeża labele w menu po prawej stronie
  procedure refreshVals();
  var
    s: String;
  begin
    with eltWindow do
    begin
      eltCorrNum.Caption := IntToStr(correct);
      eltIncorrNum.Caption := IntToStr(incorrect);
      s := IntToStr(ind);
      Insert('/', s, Length(s)+1);
      Insert(IntToStr(all), s, Length(s)+1);
      eltProgNum.Caption := s;
    end;
  end;

  //Usuwa dynamicznie zaalokowane kontrolki
  procedure cleanUp();
  begin
    with eltWindow do
    begin
      freeWordList(gaps);
      freeWordList(words);
      freeWordList(stats);
    end;
  end;

  //Resetuje wszystkie wartości po załadowaniu nowego testu
  procedure resetVals(ts: TestSet);
  begin
    with eltWindow do
    begin
      correct := 0;
      incorrect := 0;
      filled := 0;
      chosenElt := nil;
      testIsFinal := false;
      if ts = nil then
      begin
        ind := 0;
        all := 0;
        eltCheck.Visible := false;
        eltStart.Visible := false;
      end
      else
      begin
        ind := 1;
        all := ts^.counter;
        eltCheck.Visible := true;
        eltStart.Visible := true;
      end;
      eltScore.Enabled := true;
      eltScore.Visible := false;
      eltEnd.Visible := false;
      eltNext.Visible := false;
    end;
    refreshVals();
  end;

  //Inicjalizacja programu
  procedure TeltWindow.FormCreate(Sender: TObject);
  begin
    tests := nil;
    path := '';
    resetVals(nil);
  end;

  //Zdarzenie onClick dla luki w zdaniu
  procedure TeltWindow.labelOnClick(Sender: TObject);
  begin
    if not testIsFinal then
    begin
      if chosenElt <> nil then
      begin
        TLabel(Sender).Caption := chosenElt.Caption;
        if chosenElt.Font.Color = clTeal then
          chosenElt.Visible := false
        else if chosenElt.Font.Color = clMaroon then
        begin
          chosenElt.Caption := StringOfChar('.', DOTS_NUM);
          chosenElt.Font.Color := clDkGray;
        end;
        chosenElt := nil;
        words := alignWords(words);
      end
      else
      begin
        if TLabel(Sender).Caption <> StringOfChar('.', DOTS_NUM) then
        begin
          //Przenoszenie między lukami
          chosenElt := TLabel(Sender);
          chosenElt.Font.Color := clMaroon;
        end;
      end;
    end;
  end;

  //Zdarzenie onClick dla słowa w magazynie
  procedure TeltWindow.gapOnClick(Sender: TObject);
  begin
    if not testIsFinal then
    begin
      if chosenElt <> nil then chosenElt.Font.Color := clBlack;
      chosenElt := TLabel(Sender);
      chosenElt.Font.Color := clTeal;
    end;
  end;

  //Dodaje zdarzenie onClick dla słów w magazynie
  procedure TeltWindow.createGaps(wl: WordList);
  begin
    while wl <> nil do
    begin
      wl^.val.OnClick := @gapOnClick;
      wl := wl^.next;
    end;
  end;

  //Dodaje zdarzenie onClick dla luk w zdaniu
  procedure TeltWindow.createLabels(wl: WordList);
  begin
    while wl <> nil do
    begin
      if (wl^.val.Caption = StringOfChar('.', DOTS_NUM)) then
        wl^.val.OnClick := @labelOnClick;
      wl := wl^.next;
    end;
  end;

  //Rozpoczyna zestaw testów
  procedure TeltWindow.eltStartClick(Sender: TObject);
  begin
    tests := loadTest(path);
    gaps := loadGaps(tests^.iter^.correctAns^.next, eltStoreroom);
    words := loadWords(tests^.iter^.words^.next, eltMainBox);
    createLabels(words);
    createGaps(gaps);
    resetVals(tests);
    eltStart.Visible := false;
  end;

  //Zwalnia pamięć przed zakończeniem pracy programu
  procedure TeltWindow.FormDestroy(Sender: TObject);
  begin
    cleanUp();
    freeTest(tests);
  end;

  //Wybieranie pliku z zestawem testów
  procedure TeltWindow.eltBtLoadClick(Sender: TObject);
  var
    loadDlg: TOpenDialog;
  begin
    loadDlg := TOpenDialog.Create(self);
    loadDlg.InitialDir := GetCurrentDir;
    loadDlg.Options := [ofFileMustExist];
    loadDlg.Filter := 'Plik testu (*.tsf)|*.tsf';

    if loadDlg.Execute then
    begin
      path := loadDlg.FileName;
      freeTest(tests);
      cleanUp();
      eltStart.Visible := true;
      eltEnd.Visible := false;
    end;

    loadDlg.Free;
  end;

  //Sprawdza, czy luki zostały poprawnie uzupełnione i wypisuje wynik
  procedure TeltWindow.eltCheckClick(Sender: TObject);
  var
    wl, gl, tmp, prev: WordList;
  begin
    testIsFinal := true;
    wl := words;
    gl := gaps;
    while wl <> nil do
    begin
      if wl^.val.Font.Color = clDkGray then
      begin
        if wl^.val.Caption <> StringOfChar('.', DOTS_NUM) then
          Inc(filled);

        if wl^.val.Caption = gl^.val.Caption then
        begin
          Inc(correct);
          wl^.val.Font.Color := clLime;
        end
        else
        begin
          Inc(incorrect);
          wl^.val.Font.Color := clRed;
          wl^.val.Font.Style := [fsStrikeOut];
          tmp := wl^.next;
          New(wl^.next);
          prev := wl;
          wl := wl^.next;
          wl^.next := tmp;
          wl^.val := TLabel.Create(prev^.val.Parent);
          with wl^.val do
          begin
            Parent := prev^.val.Parent;
            Caption := gl^.val.Caption;
            Font.Color := clLime;
            Font.Size := 20;
          end;
          words := alignWords(words);
        end;
        gl := gl^.next;
      end;
      wl := wl^.next;
    end;

    if ind = all then eltScore.Visible := true
    else eltNext.Visible := true;
    eltCheck.Visible := false;
    refreshVals();
  end;

  //Przechodzi do następnego testu
  procedure TeltWindow.eltNextClick(Sender: TObject);
  begin
    if ind < all then
    begin
      cleanUp();
      tests^.iter := tests^.iter^.next;
      gaps := loadGaps(tests^.iter^.correctAns^.next, eltStoreroom);
      words := loadWords(tests^.iter^.words^.next, eltMainBox);
      createLabels(words);
      createGaps(gaps);
      eltNext.Visible := false;
      eltCheck.Visible := true;
      testIsFinal := false;
      Inc(ind);
      refreshVals();
    end;
  end;

  //Wyświetla wynik
  procedure TeltWindow.eltScoreClick(Sender: TObject);

    procedure addWord(wl: WordList; s: String; n: Integer);
    begin
      while wl <> nil do
      begin
        if wl^.next = nil then
        begin
          New(wl^.next);
          wl := wl^.next;
          wl^.next := nil;
          wl^.val := TLabel.Create(eltMainBox);
          with wl^.val do
          begin
            Parent := eltMainBox;
            Caption := s;
            Top := eltEnd.Top + eltEnd.Height + 50*n;
            Left := eltEnd.Left - 200;
            Font.Color := clBlack;
            Font.Size := 20;
          end;
        end;
        wl := wl^.next;
      end;
    end;

  var
    s: String;
    i: Integer;
    tmp: WordList;
  begin
    cleanUp();
    eltEnd.Visible := true;
    //Atrapa
    New(stats);
    stats^.next := nil;
    stats^.val := nil;
    i := correct*100 div (correct + incorrect);
    s := 'Poprawne odpowiedzi: ' + IntToStr(correct) + ' (' +
         IntToStr(i) + '%)';
    addWord(stats, s,  1);
    s := 'Niepoprawne odpowiedzi: ' + IntToStr(incorrect) + ' (' +
         IntToStr(100-i) + '%)';
    addWord(stats, s, 2);
    s := 'Uzupełnione luki: ' + IntToStr(filled) + ' (' +
         IntToStr(filled*100 div (correct + incorrect)) + '%)';
    addWord(stats, s, 3);
    if filled > 0 then i := correct*100 div filled
    else i := 0;
    s := 'Popr. uzupełnione: ' + IntToStr(i) +
         '% z uzupełnionych';
    addWord(stats, s, 4);
    eltScore.Enabled := false;
    tmp := stats;
    stats := stats^.next;
    Dispose(tmp);
  end;

end.
