unit TestSetUnit;

{$mode objfpc}{$H+}

interface

  uses Classes, SysUtils, Dialogs;

  type
       StringList = ^listStr;
       listStr = record
         val: String;
         next: StringList;
       end;

       TestList = ^listEl;
       listEl = record
         correctAns: StringList;
         words: StringList;
         next: TestList;
       end;

       TestSet = ^myset;
       myset = record
         counter: LongInt;
         list: TestList;
         iter: TestList;
       end;

  //Ładuje test z pliku o scieżce s
  function loadTest(s: String): TestSet;

  //Zwalnia pamięć zajmowaną przez strukturę TestSet i ustawia wskaźnik na nil
  procedure freeTest(var ts: TestSet);

implementation

  //Czyta słowo z pliku (do pierwszego wystąpienia " ")
  function readWord(var f: TextFile): String;
  var
      resu: String;
      c: Char;
  begin
    resu := '';
    c := 'a';
    while c <> ' ' do
    begin
      Read(f, c);
      if c <> ' ' then
        Insert(c, resu, Length(resu)+1);
    end;
    readWord := resu;
  end;

  //Czyta pojedynczy test z pliku (zdanie do uzupełnienia)
  procedure readTest(var f: TextFile; tl: TestList);
  var
      s: String;
      cAns, wrd: StringList;
      ans: Boolean;
  begin
    cAns := tl^.correctAns;
    wrd := tl^.words;
    //Czytam "{"
    readWord(f);

    s := readWord(f);
    while s <> '<' do
    begin
      ans := false;
      if s[1] = '[' then
      begin
        ans := true;
        s := Copy(s, 2, Length(s)-2);
        New(cAns^.next);
        cAns := cAns^.next;
        cAns^.val := s;
        cAns^.next := nil;
      end;

      New(wrd^.next);
      wrd := wrd^.next;
      wrd^.next := nil;

      if ans then
        wrd^.val := '....'
      else
        wrd^.val := s;
      s := readWord(f);
    end;

    s := readWord(f);
    while s <> '>' do
    begin
      New(cAns^.next);
      cAns := cAns^.next;
      cAns^.val := s;
      cAns^.next := nil;
      s := readWord(f);
    end;
    //Czytanie "}"
    Read(f);
  end;

  //Wczytuje wszystkie testy z pliku do struktury TestSet
  procedure parseAndLoad(var f: TextFile; tests: TestSet);
  var
      tl: TestList;
  begin
    tl := tests^.list;

    while not Eof(f) do
    begin
      New(tl^.next);
      tl := tl^.next;

      //Atrapy
      New(tl^.correctAns);
      tl^.correctAns^.val := 'dummy';
      tl^.correctAns^.next := nil;
      New(tl^.words);
      tl^.words^.val := 'dummy';
      tl^.words^.next := nil;

      tl^.next := nil;
      readTest(f, tl);
      Inc(tests^.counter);
      Readln(f);
    end;
  end;

  function loadTest(s: String): TestSet;
  var
      f: TextFile;
      tests: TestSet;
  begin
    tests := nil;

    if FileExists(s) = true then
    begin
      AssignFile(f, s);
      Reset(f);
      New(tests);
      tests^.counter := 0;
      //Atrapa
      New(tests^.list);
      with tests^.list^ do
      begin
        next := nil;
        words := nil;
        correctAns := nil;
      end;
      parseAndLoad(f, tests);
      tests^.iter := tests^.list^.next;
      CloseFile(f);
    end;

    loadTest := tests;
  end;

  procedure freeStringList(sl: StringList);
  var
      l: StringList;
  begin
    while sl <> nil do
    begin
      l := sl;
      sl := sl^.next;
      Dispose(l);
    end;
  end;

  procedure freeTest(var ts: TestSet);
  var
      tl, tmp: TestList;
  begin
    if ts <> nil then
    begin
      tl := ts^.list;
      Dispose(ts);
      ts := nil;
      while tl <> nil do
      begin
        freeStringList(tl^.correctAns);
        freeStringList(tl^.words);
        tmp := tl;
        tl := tl^.next;
        Dispose(tmp);
      end;
    end;
  end;

end.

