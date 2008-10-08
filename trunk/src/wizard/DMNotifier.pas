unit DMNotifier;

interface

uses
  Classes,
  Controls,
  Dialogs,
  Forms,
  Graphics,
  Messages,
  SysUtils,
  ToolsAPI,
  Windows;

type
  TNotifierModule = class(TDataModule, IUnknown, IOTANotifier)
  private
    FRefCount: Integer;
    FUseRefCount: Boolean;

    FOnAfterSave: TNotifyEvent;
    FOnBeforeSave: TNotifyEvent;
    FOnDestroyed: TNotifyEvent;
    FOnModified: TNotifyEvent;

    function IUnknown.QueryInterface = QueryInterface;
    function IUnknown._AddRef = _AddRef;
    function IUnknown._Release = _Release;
  protected
    { IUnknown }
    function QueryInterface(const IID: TGUID; out Obj): HResult; reintroduce; stdcall;
    function _AddRef: Integer; stdcall;
    function _Release: Integer; stdcall;
    { IOTANotifier }
    procedure AfterSave;
    procedure BeforeSave;
    procedure Destroyed;
    procedure Modified;
  public
    constructor Create(AOwner: TComponent); override;
    constructor CreateInterfaced; virtual;

    procedure AfterConstruction; override;
    procedure BeforeDestruction; override;
    class function NewInstance: TObject; override;

    property RefCount: Integer read FRefCount;
  published
    property OnAfterSave: TNotifyEvent read FOnAfterSave write FOnAfterSave;
    property OnBeforeSave: TNotifyEvent read FOnBeforeSave write FOnBeforeSave;
    property OnDestroyed: TNotifyEvent read FOnDestroyed write FOnDestroyed;
    property OnModified: TNotifyEvent read FOnModified write FOnModified;
  end;

implementation

uses
  SysConst,
  Types,
  WizardUtils;

{$R *.DFM}

//----------------------------------------------------------------------------------------------------------------------

{ TNotifierModule protected: IUnknown }

//----------------------------------------------------------------------------------------------------------------------

function TNotifierModule.QueryInterface(const IID: TGUID; out Obj): HResult;

begin
  if csDesigning in ComponentState then
    Result := inherited QueryInterface(IID, Obj)
  else
  begin
    if GetInterface(IID, Obj) then
      Result := 0
    else
      Result := E_NOINTERFACE;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

function TNotifierModule._AddRef: Integer;

begin
  if FUseRefCount then
    Result := InterlockedIncrement(FRefCount)
  else
    Result := -1;
end;

//----------------------------------------------------------------------------------------------------------------------

function TNotifierModule._Release: Integer;

begin
  if FUseRefCount then
    Result := InterlockedDecrement(FRefCount)
  else
    Result := -1;

  if Result = 0 then
    Destroy;
end;

//----------------------------------------------------------------------------------------------------------------------

{ TNotifierModule protected: IOTANotifier }

//----------------------------------------------------------------------------------------------------------------------

procedure TNotifierModule.AfterSave;

begin
  if Assigned(FOnAfterSave) then
    FOnAfterSave(Self);
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TNotifierModule.BeforeSave;

begin
  if Assigned(FOnBeforeSave) then
    FOnBeforeSave(Self);
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TNotifierModule.Destroyed;

begin
  if Assigned(FOnDestroyed) then
    FOnDestroyed(Self);
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TNotifierModule.Modified;

begin
  if Assigned(FOnModified) then
    FOnModified(Self);
end;

//----------------------------------------------------------------------------------------------------------------------

{ TNotifierModule public }

//----------------------------------------------------------------------------------------------------------------------

constructor TNotifierModule.Create(AOwner: TComponent);

begin
  inherited Create(AOwner);
  with DesignOffset do
  begin
    X := 200;
    Y := 100;
  end;
  with DesignSize do
  begin
    X := 300;
    Y := 200;
  end;
  FUseRefCount := False;
end;

//----------------------------------------------------------------------------------------------------------------------

constructor TNotifierModule.CreateInterfaced;

begin
  Create(nil);
  FUseRefCount := True;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TNotifierModule.AfterConstruction;

begin
  inherited AfterConstruction;
  // release the constructor's implicit refcount
  if FUseRefCount then
    InterlockedDecrement(FRefCount);
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TNotifierModule.BeforeDestruction;

begin
  if FUseRefCount then
  begin
    if FRefCount <> 0 then
      raise EInvalidPointer.Create(SInvalidPointer);
  end;
  inherited BeforeDestruction;
end;

//----------------------------------------------------------------------------------------------------------------------

class function TNotifierModule.NewInstance: TObject;

begin
  Result := inherited NewInstance;
  TNotifierModule(Result).FRefCount := 1;
end;

//----------------------------------------------------------------------------------------------------------------------

end.
