unit IntfParser;

interface

uses
  CastaliaSimplePasPar,
  TokenClasses,
  TokenInterfaces;

type
  TIntfParser = class(TmwSimplePasPar)
  private
    FUnit: IUnit;
    FCurrentInterface: IInterface;
    FCurrentFunction: IFunction;
    FCurrentMethod: IMethod;
    FCurrentParameter: IParameter;
    FCurrentProperty: IProperty;
    FLastTypeName: string;
  protected
    procedure UnitName; override;
    procedure InterfaceType; override;
    procedure TypeName; override;
    procedure ClassFunctionHeading; override;
    procedure FunctionMethodName; override;
    procedure ReturnType; override;
    procedure ClassProcedureHeading; override;
    procedure ProcedureMethodName; override;
    procedure ParameterFormal; override;
    procedure ParameterName; override;
    procedure NewFormalParameterType; override;
  public
    constructor Create;
    destructor Destroy; override;
    property AUnit: IUnit read FUnit;
  end;

implementation

uses
  SysUtils;

{ TIntfParser }

constructor TIntfParser.Create;
begin
  inherited;
  FUnit := CreateUnit;
  FCurrentInterface := nil;
  FCurrentFunction := nil;
  FCurrentMethod := nil;
  FCurrentParameter := nil;
  FCurrentProperty := nil;
  FLastTypeName := '';
end;

destructor TIntfParser.Destroy;
begin
  FUnit := nil;
  inherited;
end;
//==============================================================================
// Unit level stuff
//==============================================================================
procedure TIntfParser.UnitName;
begin
  FUnit.Name := Lexer.Token;
  inherited;
end;

//==============================================================================
// Interface level stuff.
//==============================================================================
procedure TIntfParser.TypeName;
begin
  FLastTypeName := Lexer.Token;
  inherited;
end;

procedure TIntfParser.InterfaceType;
begin
  FCurrentInterface := FUnit.Interfaces.Add;
  FCurrentInterface.Name := FLastTypeName;
  inherited;
  FCurrentInterface := nil;
end;

//==============================================================================
// Function stuff
//==============================================================================
procedure TIntfParser.ClassFunctionHeading;
begin
  if Assigned(FCurrentInterface) then
    FCurrentFunction := FCurrentInterface.Functions.Add;
  inherited;
  FCurrentFunction := nil;
end;

procedure TIntfParser.FunctionMethodName;
begin
  if Assigned(FCurrentFunction) then
    FCurrentFunction.Name := Lexer.Token;
  inherited;
end;

procedure TIntfParser.ReturnType;
begin
  if Assigned(FCurrentFunction) then
    FCurrentFunction.ReturnType := Lexer.Token;
  inherited;
end;

//==============================================================================
// Procedure stuff.
//==============================================================================
procedure TIntfParser.ClassProcedureHeading;
begin
  if Assigned(FCurrentInterface) then
    FCurrentMethod := FCurrentInterface.Methods.Add;
  inherited;
  FCurrentMethod := nil;
end;

procedure TIntfParser.ProcedureMethodName;
begin
  if Assigned(FCurrentMethod) then
    FCurrentMethod.Name := Lexer.Token;
  inherited;
end;
//==============================================================================

procedure TIntfParser.NewFormalParameterType;
begin
  if Assigned(FCurrentParameter) then
    FCurrentParameter.DataType := Lexer.Token;
  inherited;
end;

procedure TIntfParser.ParameterFormal;
begin         
  if Assigned(FCurrentMethod) then
    FCurrentParameter := FCurrentMethod.Params.Add
  else if Assigned(FCurrentFunction) then
    FCurrentParameter := FCurrentFunction.Params.Add;
  inherited;
  FCurrentParameter := nil;
end;

procedure TIntfParser.ParameterName;
begin 
  if Assigned(FCurrentParameter) then
    FCurrentParameter.Name := Lexer.Token;
  inherited;
end;

end.
