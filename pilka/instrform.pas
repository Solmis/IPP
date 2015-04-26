unit instrform;

{$mode objfpc}{$H+}

interface

  uses
    Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls;

  type

    { TeltInstrForm }

    TeltInstrForm = class(TForm)
      eltControls: TLabel;
      eltEditorDesc: TLabel;
      eltEditor: TLabel;
      eltCtrlDesc: TLabel;
    private
      { private declarations }
    public
      { public declarations }
    end;

  var
    eltInstrForm: TeltInstrForm;

implementation

{$R *.lfm}

end.

