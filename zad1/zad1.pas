/////////////////////////////////////
//Autor:           Michał Sołtysiak
//Indeks:          347246
//Data ukończenia: 24 Mar 2014
/////////////////////////////////////

program Zadanie1;

uses drzewa;

//MAX_POPR - ograniczenie na poprawne polecenie
const MAX_POPR = 40;

type char_tab = array[1..MAX_POPR] of Char;

var polecenie: String;

//Konwersja string do LongInt
function stringToInt(const s: string):LongInt;
var mnoznik, i, liczba: LongInt;
begin
    mnoznik := 1;
    liczba := 0;

    for i := Length(s) downto 1 do begin
        liczba := liczba + mnoznik*(Ord(s[i])-Ord('0'));
        mnoznik := mnoznik*10;
    end;

    stringToInt := liczba;
end;

//Konwersja char do string
function charToString(const c: char_tab; len: Integer):String;
var s: String;
    i: Integer;
begin
    SetLength(s, len);
    for i := 1 to len do s[i] := c[i];
    charToString := s;
end;

//zwraca true, gdy string zaczyna się dwoma zerami
function czyZeraWiodace(const s: String):Boolean;
begin
    if (Length(s) >= 2) and (s[1] = '0') and (s[2] in ['0'..'9']) then czyZeraWiodace := true
    else czyZeraWiodace := false;
end;

//Funkcja czytająca polecenie
function Czytaj():String;
var i: LongInt;
    c: char_tab;
    wynik: String;
begin
    i := 1;
    while not eoln do begin

        if i <= MAX_POPR then read(c[i])
        else read(c[1]); //to, co zostanie wczytane, będzie zignorowane
        inc(i);

    end;
    dec(i);

    if i <= MAX_POPR then wynik := charToString(c, i)
    else wynik := 'niepoprawne';

    readln; //czytamy EOLN

    if Length(wynik) = 0 then begin
        if not eof then wynik := 'niepoprawne'
        else czysc(-1); //czyścimy wszystkie funkcje (zwalniamy pamięć)
    end;
    
    Czytaj := wynik;
end;

//Obsługa polecenia czysc
procedure wyczysc(s: String);
var i: Integer;
    nawias, blad: Boolean;
    wynik: LongInt;
begin
    nawias := false;
    blad := false;
    i := 1;

    //usuwamy 'czysc(' (prefiks polecenia)
    s := Copy(s, 7, Length(s)-7+1);

    //minimalna długość poprawnego polecenia
    if Length(s) < 2 then blad := true;

    while i <= Length(s) do begin

        if s[i] = ')' then begin
            if not nawias then nawias := true
            else blad := true;
        end
        else if not (s[i] in ['0'..'9']) then blad := true;

        if blad then i := Length(s)+1; //wyjście z pętli
        inc(i);
    end;

    if not nawias then blad := true;
    if czyZeraWiodace(s) then blad := true;

    if blad then writeln('zignorowano')
    else begin
        wynik := czysc(stringToInt(Copy(s, 1, Length(s)-1)));

        if wynik = -1 then writeln('zignorowano')
        else writeln('wezlow: ', wynik);
    end;
end;

//Obsługa polecenia suma
procedure sumuj(const s: String);

var s_ile: array[1..3] of Integer;

    //zwraca informację o poprawności (true, jeśli niepoprawne)
    //ustawia liczniki nadfunkcji (s_ile)
    function parsujSume(const str: String):Boolean;
    var blad: Boolean;
        i, znaki: Integer;
    begin
        blad := false;
        znaki := 0;
        for i := 1 to 3 do s_ile[i] := 0;
        i := 1;

        while i <= Length(str) do begin

            if str[i] in ['0'..'9'] then begin
                if znaki = 0 then inc(s_ile[1])
                else if znaki = 1 then inc(s_ile[2])
                else if znaki = 3 then inc(s_ile[3])
                else blad := true;
            end
            else if str[i] = ',' then begin
                if (znaki = 0) and (s_ile[1] > 0) then inc(znaki)
                else blad := true;
            end
            else if str[i] = '.' then begin
                if (znaki in [1..2]) and (s_ile[2] > 0) then inc(znaki)
                else blad := true;
            end
            else if str[i] = ')' then begin
                if (znaki = 3) and (s_ile[3] > 0) then inc(znaki)
                else blad := true;
            end
            else blad := true;

            if blad then i := Length(str)+1; //wyjście z pętli
            inc(i);

        end;

        if znaki < 4 then blad := true;
        parsujSume := blad;
    end;


var blad: Boolean;
    wynik: LongInt;
    s_tmp, s1, s2, s3: String;

begin
    s_tmp := Copy(s, 6, Length(s)-6+1);

    if Length(s_tmp) = 0 then blad := true
    else blad := parsujSume(s_tmp);

    if blad then writeln('zignorowano')
    else begin
        s1 := Copy(s_tmp, 1, s_ile[1]);
        s2 := Copy(s_tmp, s_ile[1]+1+1, s_ile[2]);
        s3 := Copy(s_tmp, s_ile[1]+1+s_ile[2]+2+1, s_ile[3]);

	if czyZeraWiodace(s1) or czyZeraWiodace(s2) or czyZeraWiodace(s3) then blad := true;

	if blad then writeln('zignorowano')
	else begin

            if stringToInt(s2) <= stringToInt(s3) then begin
                wynik := suma(stringToInt(s1), stringToInt(s2), stringToInt(s3));

                if wynik = -1 then writeln('zignorowano')
                else writeln(s, '=', wynik);
	        end
            else writeln('zignorowano');
	end;
    end;
end;

//Obsługa operacji przypisania
procedure przypisz(const s: String);

var arg, wart: Integer;

    //zwraca informację o poprawności (true, jeśli niepoprawne)
    //ustawia arg i wart - liczniki z nadfunkcji
    function parsujPrzypisanie(const s: String):Boolean;
    var i: Integer;
        blad, trzyznaki: Boolean;
    begin
        blad := false;
        trzyznaki := false;
        arg := 0;
        wart := 0;
        i := 1;

        while i <= Length(s) do begin
        
            if s[i] in ['0'..'9'] then begin
                if not trzyznaki then inc(arg)
                else inc(wart);
            end
            else if s[i] = ')' then begin

                if trzyznaki then blad := true
                else begin

                    if (arg > 0) and (Length(s) > i+2) then begin
                        if (s[i+1] = ':') and (s[i+2] = '=') then trzyznaki := true
                        else blad := true;
                        i := i+2;
                    end
                    else blad := true;
                end;

            end
            else blad := true;

            if blad then i := Length(s)+1; //wyjście z pętli
            inc(i);
        end;

        if wart <= 0 then blad := true;
        parsujPrzypisanie := blad;
    end;


var blad: Boolean;
    s_arg, s_wart: String;
    wynik: LongInt;

begin
    //na wejściu mamy 'f(' plus możliwe coś więcej
    if Length(s) = 2 then blad := true
    else blad := parsujPrzypisanie(Copy(s, 3, Length(s)-3+1));

    if blad then writeln('zignorowano')
    else begin
        s_arg := Copy(s, 2+1, arg);
        s_wart := Copy(s, 2+arg+3+1, wart);

        if czyZeraWiodace(s_arg) or czyZeraWiodace(s_wart) then wynik := -1
        else wynik := przypisanie(stringToInt(s_arg), stringToInt(s_wart));

        if wynik = -1 then writeln('zignorowano')
        else writeln('wezlow: ', wynik);
    end;
end;

begin
    inicjuj;

    //Wczytujemy polecenie bez użycia readln
    polecenie := Czytaj();

    //Obsługa poleceń
    while Length(polecenie) <> 0 do begin

        if Copy(polecenie, 1, 2) = 'f(' then przypisz(polecenie)
        else if Copy(polecenie, 1, 5) = 'suma(' then sumuj(polecenie)
        else if Copy(polecenie, 1, 6) = 'czysc(' then wyczysc(polecenie)
        else writeln('zignorowano');

        polecenie := Czytaj();

    end;
end.
