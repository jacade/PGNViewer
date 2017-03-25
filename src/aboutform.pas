unit AboutForm;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ExtCtrls,
  StdCtrls;

type

  { TAboutForm1 }

  TAboutForm1 = class(TForm)
    Button1: TButton;
    Image1: TImage;
    Memo1: TMemo;
    Panel1: TPanel;
    procedure Button1Click(Sender: TObject);
  private
    { private declarations }
  public
    { public declarations }
  end;

var
  AboutForm1: TAboutForm1;

implementation

{$R *.lfm}

{ TAboutForm1 }

procedure TAboutForm1.Button1Click(Sender: TObject);
begin
  Self.Close;
end;

end.

