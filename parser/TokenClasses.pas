unit TokenClasses;

interface   

uses
  TokenInterfaces;

function CreateUnit: IUnit;

implementation

uses
  Classes,
  SysUtils;

type
  TNamedItem = class(TInterfacedObject, INamedItem)
  private 
    FName: string;
    function GetName: string;
    procedure SetName(const Value: string);
  public
    property Name: string read GetName write SetName;
  end;

  TGenericList = class(TInterfacedObject, IGenericList)
  private
    function GetCount: Integer;
  protected
    FList: IInterfaceList;
  public
    constructor Create;
    property Count: Integer read GetCount;
  end;

  TParameter = class(TNamedItem, IParameter)
  private
    FIndexName: string;
    FIndexType: string;
    FModifier: string;
    FType: string;
    function GetType: string;
    procedure SetType(const Value: string);
    function GetModifier: string;
    procedure SetModifier(const Value: string);
    function GetIndexName: string;
    function GetIndexType: string;
    procedure SetIndexName(const Value: string);
    procedure SetIndexType(const Value: string);
  public
    property DataType: string read GetType write SetType;   
    property Modifier: string read GetModifier write SetModifier;  
    property IndexName: string read GetIndexName write SetIndexName;
    property IndexType: string read GetIndexType write SetIndexType;
  end;

  TParameterList = class(TGenericList, IParameterList)
  private
    function Add: IParameter;
    function GetItems(Index: Integer): IParameter;
  public
    property Items[Index: Integer]: IParameter read GetItems; default;
  end;

  TMethod = class(TParameter, IMethod)
  private
    FParams: IParameterList;
    function GetParams: IParameterList;
  public
    constructor Create;
    destructor Destroy; override;
    property Params: IParameterList read GetParams;
  end;

  TMethodList = class(TGenericList, IMethodList)
  private
    function Add: IMethod;
    function GetItems(Index: Integer): IMethod;
  public
    property Items[Index: Integer]: IMethod read GetItems; default;
  end; 

  TFunction = class(TMethod, IFunction)
  private
    FReturnType: string;
    function GetReturnType: string;
    procedure SetReturnType(const Value: string);
  public
    property ReturnType: string read GetReturnType write SetReturnType;
  end;    

  TFunctionList = class(TGenericList, IFunctionList)
  private
    function Add: IFunction;
    function GetItems(Index: Integer): IFunction;
  public
    property Items[Index: Integer]: IFunction read GetItems; default;
  end;

  TProperty = class(TParameter, IProperty)
  private
    FReader: string;
    FWriter: string;
    function GetReader: string;
    function GetWriter: string;
    procedure SetReader(const Value: string);
    procedure SetWriter(const Value: string);
  public
    property Reader: string read GetReader write SetReader;
    property Writer: string read GetWriter write SetWriter;
  end;

  TPropertyList = class(TGenericList, IPropertyList)
  private
    function Add: IProperty;
    function GetItems(Index: Integer): IProperty;
  public
    property Items[Index: Integer]: IProperty read GetItems; default;
  end; 

  TAncestor = class(TInterfacedObject, IAncestor)
  private
    FName: string;
    function GetName: string;
    procedure SetName(const Value: string);
  public      
    property Name: string read GetName write SetName;
  end;

  TAncestorList = class(TGenericList, IAncestorList)
  private
    function Add: IAncestor;
    function GetItems(Index: Integer): IAncestor;
  public
    property Items[Index: Integer]: IAncestor read GetItems; default;
  end;

  TInterface = class(TNamedItem, IInterface)
  private
    FAncestors: IAncestorList;
    FFunctions: IFunctionList;
    FMethods: IMethodList;
    FProperties: IPropertyList;
    function GetAncestors: IAncestorList;
    function GetFunctions: IFunctionList;
    function GetMethods: IMethodList;
    function GetProperties: IPropertyList;
  public
    constructor Create;
    destructor Destroy; override;
    property Ancestors: IAncestorList read GetAncestors;
    property Functions: IFunctionList read GetFunctions;
    property Methods: IMethodList read GetMethods;
    property Properties: IPropertyList read GetProperties;
  end;

  TInterfacesList = class(TGenericList, IInterfacesList)
  private
    function Add: IInterface;
    function GetItems(Index: Integer): IInterface;
  public
    property Items[Index: Integer]: IInterface read GetItems; default;
  end;  

  TUnit = class(TNamedItem, IUnit)
  private
    FInterfaces: IInterfacesList;
    function GetInterfaces: IInterfacesList;
  public
    constructor Create;
    destructor Destroy; override;
    property Interfaces: IInterfacesList read GetInterfaces;
  end;

function CreateUnit: IUnit;
begin
  Result := TUnit.Create;
end;

{ TParameter }  

function TParameter.GetIndexName: string;
begin
  Result := FIndexName;
end;

function TParameter.GetIndexType: string;
begin
  Result := FIndexType;
end;

function TParameter.GetModifier: string;
begin
  Result := FModifier;
end;

function TParameter.GetType: string;
begin
  Result := FType;
end;

procedure TParameter.SetIndexName(const Value: string);
begin
  FIndexName := Value;
end;

procedure TParameter.SetIndexType(const Value: string);
begin
  FIndexType := Value;
end;

procedure TParameter.SetModifier(const Value: string);
begin
  FModifier := Value;
end;

procedure TParameter.SetType(const Value: string);
begin
  FType := Value;
end;

{ TParameterList }

function TParameterList.Add: IParameter;
begin
  Result := TParameter.Create;
  FList.Add(Result);
end;

function TParameterList.GetItems(Index: Integer): IParameter;
begin
  Result := IParameter(FList.Items[Index]);
end;

{ TMethod }

constructor TMethod.Create;
begin
  inherited;
  FParams := TParameterList.Create;
end;

destructor TMethod.Destroy;
begin
  FParams := nil;
  inherited;
end;

function TMethod.GetParams: IParameterList;
begin
  Result := FParams;
end;

{ TNamedItem }

function TNamedItem.GetName: string;
begin
  Result := FName;
end;

procedure TNamedItem.SetName(const Value: string);
begin
  FName := Value;
end;

{ TGenericList }

constructor TGenericList.Create;
begin
  inherited;
  FList := TInterfaceList.Create;
end;

function TGenericList.GetCount: Integer;
begin
  Result := FList.Count;
end;

{ TMethodList }

function TMethodList.Add: IMethod;
begin
  Result := TMethod.Create;
  FList.Add(Result);
end;

function TMethodList.GetItems(Index: Integer): IMethod;
begin
  Result := IMethod(FList.Items[Index]);
end;

{ TFunction }

function TFunction.GetReturnType: string;
begin
  Result := FReturnType;
end;

procedure TFunction.SetReturnType(const Value: string);
begin
  FReturnType := Value;
end;

{ TFunctionList }

function TFunctionList.Add: IFunction;
begin
  Result := TFunction.Create;
  FList.Add(Result);
end;

function TFunctionList.GetItems(Index: Integer): IFunction;
begin
  Result := IFunction(FList.Items[Index]);
end;

{ TProperty }

function TProperty.GetReader: string;
begin
  Result := FReader;
end;

function TProperty.GetWriter: string;
begin
  Result := FWriter;
end;

procedure TProperty.SetReader(const Value: string);
begin
  FReader := Value;
end;

procedure TProperty.SetWriter(const Value: string);
begin
  FWriter := Value;
end;

{ TPropertyList }

function TPropertyList.Add: IProperty;
begin
  Result := TProperty.Create;
  FList.Add(Result);
end;

function TPropertyList.GetItems(Index: Integer): IProperty;
begin
  Result := IProperty(FList.Items[Index]);
end;

{ TInterface }

constructor TInterface.Create;
begin
  inherited;
  FAncestors := TAncestorList.Create;
  FFunctions := TFunctionList.Create;
  FMethods := TMethodList.Create;
  FProperties := TPropertyList.Create;
end;

destructor TInterface.Destroy;
begin
  FAncestors := nil;
  FFunctions := nil;
  FMethods := nil;
  FProperties := nil;
  inherited;
end;

function TInterface.GetAncestors: IAncestorList;
begin
  Result := FAncestors;
end;

function TInterface.GetFunctions: IFunctionList;
begin
  Result := FFunctions;
end;

function TInterface.GetMethods: IMethodList;
begin
  Result:= FMethods;
end;

function TInterface.GetProperties: IPropertyList;
begin
  Result := FProperties;
end;

{ TInterfacesList }

function TInterfacesList.Add: IInterface;
begin
  Result := TInterface.Create;
  FList.Add(Result);
end;

function TInterfacesList.GetItems(Index: Integer): IInterface;
begin
  Result := IInterface(FList.Items[Index]);
end;

{ TUnit }

constructor TUnit.Create;
begin
  inherited;
  FInterfaces := TInterfacesList.Create;
end;

destructor TUnit.Destroy;
begin
  FInterfaces := nil;
  inherited;
end;

function TUnit.GetInterfaces: IInterfacesList;
begin
  Result := FInterfaces;
end;

{ TAncestorList }

function TAncestorList.Add: IAncestor;
begin
  Result := TAncestor.Create;
  FList.Add(Result);
end;

function TAncestorList.GetItems(Index: Integer): IAncestor;
begin
  Result := IAncestor(FList.Items[Index]);
end;

{ TAncestor }

function TAncestor.GetName: string;
begin
  Result := FName;
end;

procedure TAncestor.SetName(const Value: string);
begin
  FName := Value;
end;

end.
 