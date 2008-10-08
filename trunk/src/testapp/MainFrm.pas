unit MainFrm;

interface

uses
  Classes,
  Controls,
  Dialogs,
  ExtCtrls,
  Forms,
  Graphics,
  Messages,
  StdCtrls,
  SysUtils,
  Variants,
  Windows;

type
  TForm1 = class(TForm)
    Memo1: TMemo;
    Panel1: TPanel;
    btnGenerate: TButton;
    procedure btnGenerateClick(Sender: TObject);
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

uses
  IntfParser,
  MockGenerator;

procedure TForm1.btnGenerateClick(Sender: TObject);
var
  parser: TIntfParser;
  MockGen: TMockGenerator;
begin
  parser := TIntfParser.Create;
  try
    parser.LoadAndRun('TestInterfaces.pas');
    MockGen := TMockGenerator.Create('MyUnit', 'TestInterfaces', parser.AUnit);
    try
      MockGen.Output(Memo1.Lines);
    finally
      FreeAndNil(MockGen);
    end;
  finally
    FreeAndNil(parser);
  end;
end;

end.
