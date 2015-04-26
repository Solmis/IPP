unit GapUnit;

{$mode objfpc}{$H+}

interface

  uses
    Classes, SysUtils, Controls, StdCtrls, ExtCtrls, Graphics, Dialogs, TestSetUnit;

  type
    WordList = ^myword;
    myword = record
      val: TLabel;
      next: WordList;
    end;

  const
    DOTS_NUM = 10;
    PUNCT_SET = [',', '.', ':', ';', '?', '!'];

  //Tworzy kontrolki do pojedynczego testu (słowa w magazynie)
  function loadGaps(sl: StringList; const labParent: TPanel): WordList;

  //Tworzy kontrolki do pojedynczego testu (słowa w zdaniu)
  function loadWords(sl: StringList; const labParent: TPanel): WordList;

  //Rozmieszcza kontrolki w równych odstępach od siebie
  function alignWords(wl: WordList): WordList;

  //Zwalnia pamięć zajmowaną przez WordList, wraz z kontrolkami
  procedure freeWordList(var wl: WordList);

implementation

  //Wypełnia atrybuty labela podanego jej jako argument
  procedure fillLabel(lab: TLabel; word: String);
  begin
    with lab do
    begin
      Font.size := 20;
      if word = '....' then
      begin
        Caption := StringOfChar('.', DOTS_NUM);
        Font.Color := clDkGray;
      end
      else
      begin
        Caption := word;
        Font.Color := clBlack;
      end;
      Visible := true;
      AutoSize := true;
    end;
  end;

  //Tworzy listę labeli przy pomocy listy stringów
  function loadLabels(sl: StringList; const labParent: TPanel): WordList;
  var
    resu, tmp: WordList;
  begin
    New(resu);
    resu^.next := nil;
    resu^.val := nil;
    tmp := resu;

    while sl <> nil do
    begin
      New(tmp^.next);
      tmp := tmp^.next;
      tmp^.val := TLabel.Create(labParent);
      tmp^.val.Parent := labParent;
      fillLabel(tmp^.val, sl^.val);
      sl := sl^.next;
    end;

    loadLabels := resu^.next;
    Dispose(resu);
  end;

  //Losowo rozmieszcza kontrolki z listy (wywoływane dla magazynu słów)
  procedure randomPos(wl: WordList);
  var
    rand, tmp, l: WordList;
    r: Integer;
  begin
    Randomize;
    New(rand);
    rand^.val := nil;
    rand^.next := nil;
    while wl <> nil do
    begin
      l := rand;
      while l <> nil do
      begin
        r := Random(3);
        if (r = 0) or (l^.next = nil) then
        begin
          tmp := l^.next;
          New(l^.next);
          l := l^.next;
          l^.val := wl^.val;
          l^.next := tmp;
          break;
        end;
        l := l^.next;
      end;
      wl := wl^.next;
    end;

    //Pomijamy atrapę
    rand^.next := alignWords(rand^.next);

    while rand <> nil do
    begin
      l := rand;
      rand := rand^.next;
      Dispose(l);
    end;
  end;

  function loadGaps(sl: StringList; const labParent: TPanel): WordList;
  var
    resu: WordList;
  begin
    resu := loadLabels(sl, labParent);
    randomPos(resu);
    loadGaps := resu;
  end;

  function alignWords(wl: WordList): WordList;
  const
    MARGIN = 10;
    MARGIN_TOP = 30;
    MAX_W = 570;
  var
    t, l: Integer;
    resu: WordList;
    s: String;
  begin
    resu := wl;
    t := 0;
    l := 0;

    while wl <> nil do
    begin
      s := wl^.val.Caption;

      if (s[1] in PUNCT_SET) and (Length(s) <= 3) then
      begin
        wl^.val.SetBounds(l, t, wl^.val.Width, wl^.val.Height);
      end
      else
      begin
        if wl^.val.Width + l + MARGIN > MAX_W then
        begin
          t := t + MARGIN_TOP;
          l := 0;
        end;

        l := l + MARGIN;
        wl^.val.SetBounds(l, t, wl^.val.Width, wl^.val.Height);
        l := l + wl^.val.Width;
      end;
      wl := wl^.next;
    end;

    alignWords := resu;
  end;

  function loadWords(sl: StringList; const labParent: TPanel): WordList;
  var
    resu: WordList;
  begin
    resu := loadLabels(sl, labParent);
    loadWords := alignWords(resu);
  end;

  procedure freeWordList(var wl: WordList);
  var
    tmp: WordList;
  begin
    while wl <> nil do
    begin
      tmp := wl;
      wl := wl^.next;
      tmp^.val.Free;
      Dispose(tmp);
    end;
  end;

end.

