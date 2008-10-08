unit MockGenerator;

interface

uses
  Classes,
  TokenInterfaces;

type
  TMockGenerator = class
  private
    FLines: TStrings;
    FUnit: IUnit;
    FUnitName: string;
    FSourceUnitName: string;
  protected
    {procedure AddImplementationFunction(IntfName: string; Method: IFunction);
    procedure AddImplementationMethod(IntfName: string; Method: IMethod);
    procedure AddImplementationProcedure(IntfName: string; Method: IMethod);
    procedure AddImplementationType(Intf: IInterface);
    procedure AddInterfaceMethod(Method: IMethod);
    procedure AddInterfaceType(Intf: IInterface);
    procedure CreateMembers;
    function GetArgList(Method: IMethod): string;
    function GetUntypedArgList(Method: IMethod): string;
    procedure ParseStrings(UnitName: string; intfParser: TIntfParser);}
    procedure Add; overload;
    procedure Add(const Value: string); overload;
    procedure Add(const Value: string; const Params: array of const); overload;
    procedure OutputHeader;
    procedure OutputInterface;
    procedure OutputImplementation;
  public
    constructor Create(UnitName, SourceUnitName: string; AUnit: IUnit);
    procedure Output(Lines: TStrings);
  end;

implementation

uses
  SysUtils;

{ TMockGenerator }

constructor TMockGenerator.Create(UnitName, SourceUnitName: string; AUnit: IUnit);
begin
  inherited Create;
  FUnitName := UnitName;
  FSourceUnitName := SourceUnitName;
  FUnit := AUnit;
end;         

procedure TMockGenerator.Add;
begin
  FLines.Add('');
end;

procedure TMockGenerator.Add(const Value: string);
begin
  FLines.Add(Value);
end;

procedure TMockGenerator.Add(const Value: string; const Params: array of const);
begin
  FLines.Add(Format(Value, Params));
end;

procedure TMockGenerator.Output(Lines: TStrings);
begin
  FLines := Lines;
  FLines.Clear;
  OutputHeader;
  OutputInterface;
  OutputImplementation;
  Add('end.');
end;

procedure TMockGenerator.OutputHeader;
begin
  Add('unit %s;', [FUnitName]);
  Add;
  Add('(**');
  Add('  WARNING - AUTO-GENERATED MOCK!');
  Add('  Change this unit if you want to, but be aware that any changes you make will');
  Add('  be lost if you regenerate the mock object (for instance, if the interface');
  Add('  changes).');
  Add;
  Add('  My advice is to create a descendent class of your auto-generated mock - in a');
  Add('  different unit - and override things there.  That way you get to keep them.');
  Add;
  Add('  Also, the auto-generate code is not yet smart enough to generate stubs for');
  Add('  inherited interfaces.  In that case, change your mock declaration to inherit');
  Add('  from a mock implementation that implements the missing interface.  This,');
  Add('  unfortunately, is a violation of the directive above.  I''m working on it.');
  Add;
  Add('  You may also need to manually change the unit name, above.  Another thing');
  Add('  I am working on.');
  Add('**)');
  Add;
end;   

procedure TMockGenerator.OutputImplementation;
begin
  Add('implementation');
  Add;
end;
                                         
procedure TMockGenerator.OutputInterface;
var
  sUses: string;
begin
  Add('interface');
  Add;            
  Add('uses');
  for sUses in FUnit.UsesUnits do
    Add('  %s,', [sUses]);
  Add('  PascalMock,');
  Add('  %s;', [FSourceUnitName]);
  Add;
  Add('type');
  Add('  IThingy = interface;');
  Add;
end;

{procedure TMockGenerator.AddImplementationFunction(IntfName: string; Method: IMethod);
var
  Args: string;
begin
  //TODO: We need to be smarter here about returning pointers, interfaces, etc.
  Args := GetArgList(Method);
  FLines.Add(Format('function TMock%s.%s%s:%s;', [IntfName, Method.Name, Args, Method.ReturnType]));
  FLines.Add('begin');
  Args := GetUntypedArgList(Method);
  if (Args <> '') then
    FLines.Add(Format('  Result := AddCall(''%s'').WithParams([%s]).ReturnValue;', [Method.Name, Args]))
  else
    FLines.Add(Format('  Result := AddCall(''%s'').ReturnValue;', [Method.Name]));
  FLines.Add('end;');
end;

procedure TMockGenerator.AddImplementationMethod(IntfName: string; Method: IMethod);
begin
  if (Method.ReturnType = '') then
    AddImplementationProcedure(IntfName, Method)
  else
    AddImplementationFunction(IntfName, Method);
  FLines.Add('');
end;

procedure TMockGenerator.AddImplementationProcedure(IntfName: string; Method: TMethodInfo);
var
  Args: string;
begin
  Args := GetArgList(Method);
  FLines.Add(Format('procedure TMock%s.%s%s;', [IntfName, Method.Name, Args]));
  FLines.Add('begin');
  Args := GetUntypedArgList(Method);
  if (Args <> '') then
    FLines.Add(Format('  AddCall(''%s'').WithParams([%s]);', [Method.Name, Args]))
  else
    FLines.Add(Format('  AddCall(''%s'');', [Method.Name]));
  FLines.Add('end;');
end;

procedure TMockGenerator.AddImplementationType(Intf: TInterfaceInfo);
var
  method: TMethodInfo;
begin
  for method in Intf.Methods do
    AddImplementationMethod(Intf.Name, method);
end;

procedure TMockGenerator.AddInterfaceMethod(Method: TMethodInfo);
var
  Args: string;
begin
  Args := GetArgList(Method);
  if (Method.ReturnType = '') then
    FLines.Add(Format('    procedure %s%s;', [Method.Name, Args]))
  else
    FLines.Add(Format('    function %s%s:%s;', [Method.Name, Args, Method.ReturnType]))
end;

procedure TMockGenerator.AddInterfaceType(Intf: TInterfaceInfo);
var
  method: TMethodInfo;
begin
  FLines.Add(Format('  TMock%0:s = class(TMock, %0:s)', [Intf.Name]));
  FLines.Add('  public');
  for method in Intf.Methods do
    AddInterfaceMethod(method);
  FLines.Add('  end;');
  FLines.Add('');
end;

procedure TMockGenerator.CreateMembers;
begin
  FLines := TStringList.Create;
end;

function TMockGenerator.GetArgList(Method: TMethodInfo): string;
var
  arg: TArgumentInfo;
begin
  Result := '';
  for arg in Method.Arguments do
    Result := Result + Format(' %s: %s;', [arg.Name, arg.DataType]);
  if (Result <> '') then
    Result := '(' + Copy(Result, 2, Length(Result) - 2) + ')';
end;

function TMockGenerator.GetUntypedArgList(Method: TMethodInfo): string;
var
  arg: TArgumentInfo;
begin
  Result := '';
  for arg in Method.Arguments do
    Result := Result + ' ' + arg.Name + ',';
  if (Result <> '') then
    Result := Copy(Result, 2, Length(Result) - 2);
end;

procedure TMockGenerator.ParseStrings(UnitName: string; intfParser: TInterfaceParser);
var
  intf: TInterfaceInfo;
begin
  FLines.Add('unit Unit1;');
  FLines.Add('');
  FLines.Add('(**');
  FLines.Add('  WARNING - AUTO-GENERATED MOCK!');
  FLines.Add('  Change this unit if you want to, but be aware that any changes you make will');
  FLines.Add('  be lost if you regenerate the mock object (for instance, if the interface');
  FLines.Add('  changes).');
  FLines.Add('');
  FLines.Add('  My advice is to create a descendent class of your auto-generated mock - in a');
  FLines.Add('  different unit - and override things there.  That way you get to keep them.');
  FLines.Add('');
  FLines.Add('  Also, the auto-generate code is not yet smart enough to generate stubs for');
  FLines.Add('  inherited interfaces.  In that case, change your mock declaration to inherit');
  FLines.Add('  from a mock implementation that implements the missing interface.  This,');
  FLines.Add('  unfortunately, is a violation of the directive above.  I''m working on it.');
  FLines.Add('');
  FLines.Add('  You may also need to manually change the unit name, above.  Another thing');
  FLines.Add('  I am working on.');
  FLines.Add('**)');
  FLines.Add('');
  FLines.Add('interface');
  FLines.Add('');
  FLines.Add('uses');
  if (intfParser.UsesClause <> '') then
    FLines.Add('  ' + intfParser.UsesClause);
  FLines.Add('  PascalMock,');
  FLines.Add(Format('  %s;', [UnitName]));
  FLines.Add('');
  FLines.Add('type');
  for intf in intfParser.Interfaces do
    AddInterfaceType(intf);
  FLines.Add('implementation');
  FLines.Add('');
  for intf in intfParser.Interfaces do
    AddImplementationType(intf);
  FLines.Add('end.');
end; }

end.
