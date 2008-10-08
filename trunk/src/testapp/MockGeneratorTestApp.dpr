program MockGeneratorTestApp;

uses
  Forms,
  MainFrm in 'MainFrm.pas' {Form1},
  TestInterfaces in 'TestInterfaces.pas',
  MockGenerator in '..\generator\MockGenerator.pas',
  IntfParser in '..\parser\IntfParser.pas',
  TokenClasses in '..\parser\TokenClasses.pas',
  TokenInterfaces in '..\parser\TokenInterfaces.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
