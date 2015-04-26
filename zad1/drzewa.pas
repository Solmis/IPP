/////////////////////////////////////
//Autor:           Michał Sołtysiak
//Indeks:          347246
//Data ukończenia: 24 Mar 2014
/////////////////////////////////////

unit drzewa;

interface

    procedure inicjuj;
    function przypisanie(argument, wartosc: LongInt):LongInt;
    function suma(nr_funkcji, lewy_argument, prawy_argument: LongInt):LongInt;
    function czysc(nr_funkcji: LongInt):LongInt;

implementation

    //MAX_W - maksymalna wartość ze zbioru wartości f
    //MAX_POL - maksymalna liczba poleceń, które należy obsłużyć
    //MAX_ARG - maksymalna wartość z dziedziny f
    const MAX_W = 1000;
          MAX_POL = 1000000;
          MAX_ARG = 1000000000;

    //type drzewo:
    //suma - suma lewego poddrzewa wraz z wartością węzła w którym jesteśmy
    //p_sum - suma prawego poddrzewa bez wartości węzła w którym jesteśmy
    //refer - licznik referencji, nowy korzeń ma 0 referencji, a inne węzły 1
    //x - argument, y - wartość funkcji
    type
        drzewo = ^wezel;
        wezel = record
            suma, p_sum, x, y, refer: LongInt;
            lsyn, psyn: drzewo;
        end;

        dwaDrzewa = record
            min, ojciec_min: drzewo;
            lsyn: Boolean;
        end;

    //korzen  - tablica zawierająca korzenie drzew bst reprezentujących funkcję o zadanym indeksie
    //wezly   - licznik węzłów
    //ost_funkcja - pierwszy indeks tablicy korzen, ktory nie zostal zajety przez żadną funkcję
    //l_poleceń   - liczba poprawnych poleceń, które zostały wykonane
    var korzen: array[1..MAX_POL] of drzewo;
        wezly: LongInt;
        ost_funkcja: LongInt;
        l_polecen: LongInt;

    //procedura inicjująca moduł
    procedure inicjuj;
    var i: LongInt;
    begin
        for i := 1 to MAX_POL do korzen[i] := nil;
        wezly := 0;
        ost_funkcja := 1;
        l_polecen := 0;
    end;

    //dla niepoprawnych danych zwraca -1
    //w przeciwnym przypadku zwraca liczbę wszystkich węzłów po przypisaniu
    function przypisanie(argument, wartosc: LongInt):LongInt;

        //zwraca true, gdy nalezy dodac nowa funkcje (argumentu nie ma w drzewie lub inne f(arg))
        function zmiana(d: drzewo):Boolean;
        begin
            if d = nil then zmiana := true
            else begin
                if d^.x = argument then begin
                    if d^.y = wartosc then zmiana := false
                    else zmiana := true;
                end
                else if d^.x < argument then zmiana := zmiana(d^.psyn)
                else zmiana := zmiana(d^.lsyn);
            end;
        end;

        //zwraca true, gdy wezel o argumencie x istnieje w drzewie d
        function czyIstnieje(d: drzewo; const x: LongInt):Boolean;
        begin
            if d = nil then czyIstnieje := false
            else begin
                if d^.x = x then czyIstnieje := true
                else if d^.x < x then czyIstnieje := czyIstnieje(d^.psyn, x)
                else czyIstnieje := czyIstnieje(d^.lsyn, x);
            end;
        end;

        //tworzy nowy wezel z podaną wartością i argumentem
        procedure stworzWezel(d: drzewo; const x, y, ref: LongInt);
        begin
            d^.x := x;
            d^.y := y;
            d^.refer := ref;
            d^.suma := -1;
            d^.p_sum := -1;
            d^.lsyn := nil;
            d^.psyn := nil;
        end;

        //szuka w drzewie węzła o najmniejszym argumencie
        //zwraca rekord: wskaznik do tego wezla i jego ojca oraz informacje, czy nasz wezel jest lsynem
        function znajdzMin(d, d_ojciec: drzewo; const l_syn: Boolean):dwaDrzewa;
        var wynik: dwaDrzewa;
        begin

            //lsyn = nil, czyli niżej nie będzie nic mniejszego
            if d^.lsyn = nil then begin
                wynik.min := d;
                wynik.ojciec_min := d_ojciec;
                wynik.lsyn := l_syn;
                znajdzMin := wynik;
            end
            else znajdzMin := znajdzMin(d^.lsyn, d, true);

        end;

        //zwraca wskaznik do nowo utworzonego wezla o argumencie x
        //zakladamy, ze wezel o argumencie x istnieje
        function dotworz(d_poprz, d: drzewo; const x: LongInt):drzewo;
        var wynik: drzewo;
        begin
            if x > d_poprz^.x then begin
                new(d^.psyn);
                inc(wezly);
                d^.lsyn := d_poprz^.lsyn;
                if d^.lsyn <> nil then inc(d^.lsyn^.refer);
                stworzWezel(d^.psyn, d_poprz^.psyn^.x, d_poprz^.psyn^.y, 1);
                wynik := dotworz(d_poprz^.psyn, d^.psyn, x);
                dotworz := wynik;
            end
            else if x < d_poprz^.x then begin
                new(d^.lsyn);
                inc(wezly);
                d^.psyn := d_poprz^.psyn;
                if d^.psyn <> nil then inc(d^.psyn^.refer);
                stworzWezel(d^.lsyn, d_poprz^.lsyn^.x, d_poprz^.lsyn^.y, 1);
                wynik := dotworz(d_poprz^.lsyn, d^.lsyn, x);
                dotworz := wynik;
            end
            else begin
                d^.psyn := d_poprz^.psyn;
                if d^.psyn <> nil then inc(d^.psyn^.refer);
                dotworz := d;
            end;
        end;

        //usuwa węzeł d z drzewa według zadanego schematu
        procedure usunWezel(d, d_poprz, d_ojciec: drzewo; const b_lsyn: Boolean);
        var dwa: dwaDrzewa;
            tmp: drzewo;
        begin

            if d_poprz^.psyn = nil then begin
                if d_poprz^.lsyn = nil then begin

                    if d_ojciec = nil then korzen[ost_funkcja] := nil
                    else begin
                        if b_lsyn then d_ojciec^.lsyn := nil
                        else d_ojciec^.psyn := nil;
                    end;

                    dispose(d);
                    dec(wezly);
                end
                else begin

                    if b_lsyn then begin
                        if d_ojciec <> nil then begin
                            d_ojciec^.lsyn := d_poprz^.lsyn;
                            dispose(d);
                            dec(wezly);
                            if d_poprz^.lsyn <> nil then inc(d_poprz^.lsyn^.refer);
                        end;
                    end
                    else begin
                        if d_ojciec <> nil then begin
                            d_ojciec^.psyn := d_poprz^.lsyn;
                            if d_poprz^.lsyn <> nil then inc(d_poprz^.lsyn^.refer);
                            dispose(d);
                            dec(wezly);
                        end
                        else begin
                            //korzen zastepujemy jego lsynem
                            korzen[ost_funkcja] := d_poprz^.lsyn;
                            inc(d_poprz^.lsyn^.refer);
                            {tmp := korzen[ost_funkcja];
                            stworzWezel(tmp, d_poprz^.lsyn^.x, d_poprz^.lsyn^.y, 1);
                            tmp^.lsyn := d_poprz^.lsyn^.lsyn;
                            tmp^.psyn := d_poprz^.lsyn^.psyn;
                            if d_poprz^.lsyn^.lsyn <> nil then inc(d_poprz^.lsyn^.lsyn^.refer);
                            if d_poprz^.lsyn^.psyn <> nil then inc(d_poprz^.lsyn^.psyn^.refer);}
                            dispose(d);
                            dec(wezly);
                        end;
                    end;
                end;
            end
            else begin
                dwa := znajdzMin(d_poprz^.psyn, d, false);

                if dwa.ojciec_min^.x = d^.x then begin
                    stworzWezel(d, dwa.min^.x, dwa.min^.y, 1);
                    d^.lsyn := d_poprz^.lsyn;
                    if d^.lsyn <> nil then inc(d^.lsyn^.refer);
                    d^.psyn := dwa.min^.psyn;
                    if d^.psyn <> nil then inc(d^.psyn^.refer);
                end
                else begin
                    stworzWezel(d, dwa.min^.x, dwa.min^.y, 1);

                    //chcemy dotworzyc sciezke do ojca minimum
                    d := dotworz(d_poprz, d, dwa.ojciec_min^.x);

                    d^.lsyn := dwa.min^.psyn;
                    if d^.lsyn <> nil then inc(d^.lsyn^.refer); //!
                end;
            end;
        end;

        //tworzy drzewo
        procedure stworzDrzewo(d, d_poprz, d_ojciec: drzewo; const b_lsyn: Boolean; const x, y: LongInt);
        begin
            if x > d^.x then begin
                d^.lsyn := d_poprz^.lsyn;
                if d^.lsyn <> nil then inc(d^.lsyn^.refer);
                new(d^.psyn);
                inc(wezly);

                //jeśli psyn poprzedniego drzewa nie istnieje, to możemy wstawić nasz węzeł,
                //w przeciwnym wypadku tworzymy węzeł identyczny z tym z d_poprz
                //i wywołujemy się rekurencyjnie dla kolejnej pary "równoległych" węzłów
                if d_poprz^.psyn = nil then stworzWezel(d^.psyn, x, y, 1)
                else begin
                    stworzWezel(d^.psyn, d_poprz^.psyn^.x, d_poprz^.psyn^.y, 1);
                    stworzDrzewo(d^.psyn, d_poprz^.psyn, d, false, x, y);
                end;
            end
            else if x < d^.x then begin
                d^.psyn := d_poprz^.psyn;
                if d^.psyn <> nil then inc(d^.psyn^.refer);
                new(d^.lsyn);
                inc(wezly);

                // [to samo co wyżej, tylko że dla lewego syna]
                if d_poprz^.lsyn = nil then stworzWezel(d^.lsyn, x, y, 1)
                else begin
                    stworzWezel(d^.lsyn, d_poprz^.lsyn^.x, d_poprz^.lsyn^.y, 1);
                    stworzDrzewo(d^.lsyn, d_poprz^.lsyn, d, true, x, y);
                end;
            end
            else {x = d^.x} begin
                if y <> 0 then begin
                    d^.y := y;
                    d^.lsyn := d_poprz^.lsyn;
		            d^.psyn := d_poprz^.psyn;
		            if d^.lsyn <> nil then inc(d^.lsyn^.refer);
		            if d^.psyn <> nil then inc(d^.psyn^.refer);
                end
                else usunWezel(d, d_poprz, d_ojciec, b_lsyn);
            end;
        end;

        //liczy sumy dla węzłów w drzewie d, gdy natrafi na wezel poprzedniego drzewa,
        //to przepisuje sume, w przeciwnym wypadku liczy dalej
        function policzSumy(d: drzewo):LongInt;
        var l_syn, p_syn: LongInt;
        begin
            if d = nil then policzSumy := 0
            else begin
                if d^.refer > 1 then policzSumy := d^.suma + d^.p_sum
                else begin
                    l_syn := policzSumy(d^.lsyn);
                    p_syn := policzSumy(d^.psyn);

                    d^.suma := d^.y + l_syn;
                    d^.p_sum := p_syn;
                    policzSumy := d^.suma + p_syn;
                end;
            end;
        end;


    var dodaj: Boolean;
        korz, k_poprz: drzewo;

    begin
        if l_polecen >= MAX_POL then przypisanie := -1
        else begin

            if (argument <= MAX_ARG) and (wartosc <= MAX_W) then begin

                if ost_funkcja > 1 then begin

                    //k_poprz - korzeń poprzedniej funkcji
                    k_poprz := korzen[ost_funkcja-1];

                    if wartosc = 0 then dodaj := czyIstnieje(k_poprz, argument)
                    else dodaj := zmiana(k_poprz);

                    if dodaj then begin
                        new(korzen[ost_funkcja]);
                        korz := korzen[ost_funkcja];

                        if k_poprz = nil then begin
                            stworzWezel(korz, argument, wartosc, 1);
                            korz^.suma := wartosc;
                            korz^.p_sum := 0;
                        end
                        else begin
                            stworzWezel(korz, k_poprz^.x, k_poprz^.y, 1);

                            //tworzenie nowego drzewa
                            stworzDrzewo(korz, k_poprz, nil, false, argument, wartosc);
                            korz := korzen[ost_funkcja]; //aktualizacja
                            policzSumy(korz);
                        end;
                        if korzen[ost_funkcja] <> nil then inc(wezly); //korzen
                    end
                    else begin
                        korzen[ost_funkcja] := k_poprz;
                        if k_poprz <> nil then inc(k_poprz^.refer);
                    end;

                end
                else begin //tworzymy pierwsza funkcje

                    if wartosc <> 0 then begin
                        new(korzen[ost_funkcja]);
                        stworzWezel(korzen[ost_funkcja], argument, wartosc, 1);
                        korzen[ost_funkcja]^.suma := wartosc;
                        korzen[ost_funkcja]^.p_sum := 0;
                        inc(wezly);
                    end;

                end;

                inc(ost_funkcja);
                przypisanie := wezly;
                inc(l_polecen);
            end
            else przypisanie := -1;
        end;
    end;

    //dla niepoprawnych danych zwraca -1, wpp zwraca sumę
    function suma(nr_funkcji, lewy_argument, prawy_argument: LongInt):LongInt;
    var d: drzewo;
        s1, s2: LongInt;

        //zwraca sumę od 0 do a w zadanym drzewie
        function sumaOdZera(d: drzewo; const a: LongInt):LongInt;
        var wynik: LongInt;
        begin
            if d = nil then sumaOdZera := 0
            else begin

                if d^.x = a then sumaOdZera := d^.suma
                else if d^.x > a then begin
                    wynik := sumaOdZera(d^.lsyn, a);
                    sumaOdZera := wynik;
                end
                else begin
                    wynik := d^.suma + sumaOdZera(d^.psyn, a);
                    sumaOdZera := wynik;
                end;

            end;
        end;

    begin
        if l_polecen >= MAX_POL then suma := -1
        else begin

            if (lewy_argument <= MAX_ARG) and (prawy_argument <= MAX_ARG) then begin

                if nr_funkcji = 0 then begin
                    suma := 0;
                    inc(l_polecen);
                end
                else if nr_funkcji >= ost_funkcja then suma := -1
                else begin
                    d := korzen[nr_funkcji];
                    s1 := sumaOdZera(d, lewy_argument-1);
                    s2 := sumaOdZera(d, prawy_argument);
                    suma := s2-s1;
                    inc(l_polecen);
                end;
            end
            else suma := -1;
        end;
    end;

    //dla argumentu -1 zwalnia całą zaalokowaną pamięć
    //dla niepoprawnych danych zwraca -1, wpp zwraca liczbę węzłów
    function czysc(nr_funkcji: LongInt):LongInt;

        //zwraca ile wezlow usunieto
        function rek(d: drzewo):LongInt;
        var l, p: LongInt;
        begin
            if d = nil then rek := 0
            else begin
                if d^.refer = 1 then begin
                    l := rek(d^.lsyn);
                    p := rek(d^.psyn);
                    dispose(d);
                    rek := l+p+1;
                end
                else begin
                    dec(d^.refer);
                    rek := 0;
                end;
            end;
        end;

        //usuwa funkcje i zwraca liczbę węzłów, które pozostały (nadal zaalokowane)
        function usunFunkcje(const nr: LongInt):LongInt;
        var korz: drzewo;
            l, p: LongInt;
        begin
            korz := korzen[nr];

            if korz <> nil then begin

                //jeśli nie ma identycznej funkcji, to możemy zacząć ją usuwać
                if korz^.refer = 1 then begin
                    l := rek(korz^.lsyn);
                    p := rek(korz^.psyn);
                    dispose(korz);
                    wezly := wezly-(l+p+1);
                end
                else dec(korz^.refer);

                korzen[nr] := nil;
            end;

            usunFunkcje := wezly;
        end;

        //zwalnia całą zaalokowaną pamięć
        procedure zwolnijCalaPamiec;
        var i, w: LongInt;
        begin
            i := 1;
            w := wezly;

            while w <> 0 do begin
                w := usunFunkcje(i);
                inc(i);
            end;
        end;

    begin
        if nr_funkcji = -1 then begin
            zwolnijCalaPamiec;
            czysc := 0;
        end
        else begin
            if l_polecen >= MAX_POL then czysc := -1
            else begin

                if nr_funkcji = 0 then begin
                    czysc := wezly;
                    inc(l_polecen);
                end
                else if nr_funkcji >= ost_funkcja then
                    czysc := -1
                else begin
                    inc(l_polecen);
                    czysc := usunFunkcje(nr_funkcji);
                end;

            end;
        end;
    end;

end.
