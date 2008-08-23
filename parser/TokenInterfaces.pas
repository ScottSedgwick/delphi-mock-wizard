unit TokenInterfaces;

interface

type
  INamedItem = interface
    function GetName: string;
    procedure SetName(const Value: string);
    property Name: string read GetName write SetName;
  end;

  IParameter = interface(INamedItem)
    function GetType: string;
    procedure SetType(const Value: string);
    property DataType: string read GetType write SetType;
  end;

  IGenericList = interface
    function GetCount: Integer;
    property Count: Integer read GetCount;
  end;

  IParameterList = interface(IGenericList)
    function Add: IParameter;
    function GetItems(Index: Integer): IParameter;
    property Items[Index: Integer]: IParameter read GetItems; default;
  end;

  IMethod = interface(INamedItem)
    function GetParams: IParameterList;
    property Params: IParameterList read GetParams;
  end;

  IMethodList = interface(IGenericList)
    function Add: IMethod;
    function GetItems(Index: Integer): IMethod;
    property Items[Index: Integer]: IMethod read GetItems; default;
  end;

  IFunction = interface(IMethod)
    function GetReturnType: string;
    procedure SetReturnType(const Value: string);
    property ReturnType: string read GetReturnType write SetReturnType;
  end;

  IFunctionList = interface(IGenericList)
    function Add: IFunction;
    function GetItems(Index: Integer): IFunction;
    property Items[Index: Integer]: IFunction read GetItems; default;
  end;

  IProperty = interface(IParameter)
    function GetReader: string;
    procedure SetReader(const Value: string);
    function GetWriter: string;
    procedure SetWriter(const Value: string);
    property Reader: string read GetReader write SetReader;
    property Writer: string read GetWriter write SetWriter;
  end;

  IPropertyList = interface(IGenericList)
    function Add: IProperty;
    function GetItems(Index: Integer): IProperty;
    property Items[Index: Integer]: IProperty read GetItems; default;
  end;

  IInterface = interface(INamedItem)
    function GetFunctions: IFunctionList;
    function GetMethods: IMethodList;
    function GetProperties: IPropertyList;
    property Functions: IFunctionList read GetFunctions;
    property Methods: IMethodList read GetMethods;
    property Properties: IPropertyList read GetProperties;
  end;

  IInterfacesList = interface(IGenericList)
    function Add: IInterface;
    function GetItems(Index: Integer): IInterface;
    property Items[Index: Integer]: IInterface read GetItems; default;
  end;

  IUnit = interface(INamedItem)
    function GetInterfaces: IInterfacesList;
    property Interfaces: IInterfacesList read GetInterfaces;
  end;

implementation

end.
 