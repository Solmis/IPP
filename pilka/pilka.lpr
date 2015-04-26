program pilka;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Interfaces, // this includes the LCL widgetset
  Forms, window, menu, moves, history, fields, ai, saveLoad, instrform,
  newgameform
  { you can add units after this };

{$R *.res}

begin
  RequireDerivedFormResource := True;
  Application.Initialize;
  Application.CreateForm(TeltWindow, eltWindow);
  Application.ShowMainForm := false;
  Application.CreateForm(TeltMenu, eltMenu);
  eltMenu.Show;
  Application.CreateForm(TeltInstrForm, eltInstrForm);
  Application.CreateForm(TeltNewGameForm, eltNewGameForm);
  Application.Run;
end.

