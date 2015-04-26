unit newgameform;

{$mode objfpc}{$H+}

interface

  uses
    Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls, fields;

  type

    { TeltNewGameForm }

    TeltNewGameForm = class(TForm)
      eltHumans: TButton;
      eltHumanComp: TButton;
      eltComputers: TButton;
      procedure eltComputersClick(Sender: TObject);
      procedure eltHumanCompClick(Sender: TObject);
      procedure eltHumansClick(Sender: TObject);
    private
      { private declarations }
    public
      { public declarations }
      function Execute: Boolean;
    end;

  var
    eltNewGameForm: TeltNewGameForm;

implementation

{$R *.lfm}

  { TeltNewGameForm }

  procedure TeltNewGameForm.eltHumansClick(Sender: TObject);
  begin
    initGameType(0);
    eltNewGameForm.Hide;
  end;

  procedure TeltNewGameForm.eltHumanCompClick(Sender: TObject);
  begin
    initGameType(1);
    eltNewGameForm.Hide;
  end;

  procedure TeltNewGameForm.eltComputersClick(Sender: TObject);
  begin
    initGameType(2);
    eltNewGameForm.Hide;
  end;

  function TeltNewGameForm.Execute: Boolean;
  begin
    Result := (ShowModal = mrOK);
  end;

end.

