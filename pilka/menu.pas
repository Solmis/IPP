unit menu;

{$mode objfpc}{$H+}

interface

  uses
    Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ExtCtrls,
    StdCtrls, types, window, saveLoad, fields, instrform, newgameform, history,
    moves;

  type

    { TeltMenu }

    TeltMenu = class(TForm)
      eltMenuBgr: TImage;
      eltNewGame: TLabel;
      eltLoadGame: TLabel;
      eltQuit: TLabel;
      eltInstructions: TLabel;
      procedure onMouseEnter(Sender: TObject);
      procedure onMouseLeave(Sender: TObject);
      procedure eltLoadGameClick(Sender: TObject);
      procedure eltInstructionsClick(Sender: TObject);
      procedure eltQuitClick(Sender: TObject);
      procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
      procedure FormCreate(Sender: TObject);
      procedure eltNewGameClick(Sender: TObject);
    private
      { private declarations }
    public
      { public declarations }
    end;

  var
    eltMenu: TeltMenu;

implementation

{$R *.lfm}

  { TeltMenu }

  procedure TeltMenu.FormCreate(Sender: TObject);
  begin
    if FileExists('ball.png') then
      eltMenuBgr.Picture.LoadFromFile('ball.png')
    else
      ShowMessage('Błąd: brak pliku ball.png');
  end;

  procedure TeltMenu.eltQuitClick(Sender: TObject);
  begin
    eltMenu.Close;
  end;

  procedure TeltMenu.FormClose(Sender: TObject; var CloseAction: TCloseAction);
  begin
    eltWindow.Close;
  end;

  procedure TeltMenu.eltLoadGameClick(Sender: TObject);
  begin
    if loadQuery then
    begin
      eltWindow.loadGame;
      eltMenu.Hide;
      eltWindow.Show;
    end;
  end;

  procedure TeltMenu.onMouseEnter(Sender: TObject);
  begin
    TLabel(Sender).Font.Color := clLime;
    TLabel(Sender).Font.Style := [fsBold];
  end;

  procedure TeltMenu.onMouseLeave(Sender: TObject);
  begin
    TLabel(Sender).Font.Color := clWhite;
    TLabel(Sender).Font.Style := [];
  end;

  procedure TeltMenu.eltInstructionsClick(Sender: TObject);
  begin
    eltInstrForm.Show;
  end;

  procedure TeltMenu.eltNewGameClick(Sender: TObject);
  begin
    eltMenu.Hide;
    initGameType(-1);
    if eltNewGameForm.Execute then
    begin
      eltWindow.Show;
      initFields(STD_BOARD_WIDTH, STD_BOARD_HEIGHT);
      eltWindow.newGame;
    end
    else
      eltMenu.Show;
  end;

end.

