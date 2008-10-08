unit DMWizard;

interface

uses
  Windows, 
  Messages, 
  SysUtils, 
  Classes, 
  Graphics, 
  Controls, 
  Forms, 
  Dialogs,
  DMNotifier,
  ToolsAPI;

type
  TWizardStateEvent = procedure(Sender: TObject; var State: TWizardState) of object;

  TWizardModule = class(TNotifierModule, IOTAWizard)
  private
    FIDString: string;
    FState: TWizardState;
    FWizardName: string;
    FOnGetState: TWizardStateEvent;
    procedure SetIDString(const Value: string);
  protected
    { IOTAWizard }
    function GetIDString: string;
    function GetName: string;
    function GetState: TWizardState;
    procedure Execute; virtual;
    function GetDefaultAuthor: string;
    procedure Loaded; override;
    procedure SetName(const Value: TComponentName); override;
  public
    constructor Create(AOwner: TComponent); override;
  published
    property IDString: string read GetIDString write SetIDString;
    property State: TWizardState read GetState write FState default [wsEnabled];
    property WizardName: string read FWizardName write FWizardName;
    property OnGetState: TWizardStateEvent read FOnGetState write FOnGetState;
  end;

  TCreatorType = (ctNone, ctApplication, ctLibrary, ctConsole, ctPackage, ctUnit, ctForm, ctText);
  TCreatorOwnerEvent = procedure(Sender: TObject; var OwnerModule: IOTAModule) of object;
  TCreator = class(TComponent, IOTACreator)
  private
    FCreatorType: TCreatorType;
    FExisting: Boolean;
    FFileSystem: string;
    FUnnamed: Boolean;
    FWizardModule: TWizardModule;
    FOnGetOwner: TCreatorOwnerEvent;
    function IOTACreator.GetOwner = GetOwnerModule;
  protected
    { IOTACreator }
    function GetCreatorType: string;
    function GetExisting: Boolean;
    function GetFileSystem: string;
    function GetOwnerModule: IOTAModule; virtual; abstract;
    function GetUnnamed: Boolean;
  public
    constructor Create(AOwner: TComponent); override;
    property WizardModule: TWizardModule read FWizardModule;
  published
    property CreatorType: TCreatorType read FCreatorType write FCreatorType default ctNone;
    property Existing: Boolean read GetExisting write FExisting default False;
    property FileSystem: string read GetFileSystem write FFileSystem;
    property Unnamed: Boolean read GetUnnamed write FUnnamed default True;
    property OnGetOwner: TCreatorOwnerEvent read FOnGetOwner write FOnGetOwner;
  end;

  TModuleCreator = class;

  TModuleSourceType = (mstForm, mstImpl, mstIntf);
  TModuleFile = class(TInterfacedObject, IOTAFile)
  private
    FCreator: TModuleCreator;
    FSourceType: TModuleSourceType;
    FModuleIdent: string;
    FFormIdent: string;
    FAncestorIdent: string;
  protected
    { IOTAFile }
    function GetAge: TDateTime;
    function GetSource: string;
  public
    constructor Create(ACreator: TModuleCreator; ASourceType: TModuleSourceType; const AModuleIdent, AFormIdent, AAncestorIdent: string);
    property Creator: TModuleCreator read FCreator;
    property SourceType: TModuleSourceType read FSourceType;
    property ModuleIdent: string read FModuleIdent;
    property FormIdent: string read FFormIdent;
    property AncestorIdent: string read FAncestorIdent;
  end;

{
  TModuleCreator.GetOwnerModule by default returns the currently active project within the IDE (or nil if there's none).
  This is OK when used to add new modules to existing projects.

  However, when used within IOTAProjectCreator50.NewDefaultProjectModule (TProjectCreator.OnNewDefaultModule),
  the new project being just created is *not* yet active.

  You can resolve this problems in two ways:
  1. Keep a temporary reference to IOTAProject passed to your TProjectCreator.OnNewDefaultProjectModule event handler
  in a private variable and write your TModuleCreator.OnGetOwner to return it as the owner module. Don't forget to
  release its reference count when you're finished (by setting the variable to nil).
  An example of this can be found in TPackageWizardRepository.ProjectCreatorDefaultModule.

  2. Don't create the new modules in the OnNewDefaultProjectModule event handler, instead, create them in the
  Execute method of your wizard *after* the project has been created when it's already been activated by the IDE.
}

  TFormCreatedEvent = procedure(Sender: TObject; const FormEditor: IOTAFormEditor) of object;
  TModuleAgeEvent = procedure(Sender: TObject; var Age: TDateTime) of object;
  TModuleSourceEvent = procedure(Sender: TObject; SourceType: TModuleSourceType; var Source: string) of object;
  TModuleCreator = class(TCreator, IOTAModuleCreator)
  private
    FAncestorName: string;
    FImplFileName: string;
    FIntfFileName: string;
    FFormName: string;
    FMainForm: Boolean;
    FShowForm: Boolean;
    FShowSource: Boolean;
    FSourceForm: TStrings;
    FSourceImpl: TStrings;
    FSourceIntf: TStrings;
    FOnFormCreated: TFormCreatedEvent;
    FOnGetAge: TModuleAgeEvent;
    FOnGetSource: TModuleSourceEvent;
    function IOTAModuleCreator.GetOwner = GetOwnerModule;
    procedure SetSourceForm(Value: TStrings);
    procedure SetSourceImpl(Value: TStrings);
    procedure SetSourceIntf(Value: TStrings);
  protected
    { IOTAModuleCreator }
    function GetAncestorName: string;
    function GetImplFileName: string;
    function GetIntfFileName: string;
    function GetFormName: string;
    function GetMainForm: Boolean;
    function GetShowForm: Boolean;
    function GetShowSource: Boolean;
    function NewFormFile(const FormIdent, AncestorIdent: string): IOTAFile;
    function NewImplSource(const ModuleIdent, FormIdent, AncestorIdent: string): IOTAFile;
    function NewIntfSource(const ModuleIdent, FormIdent, AncestorIdent: string): IOTAFile;
    procedure FormCreated(const FormEditor: IOTAFormEditor);
    function GetOwnerModule: IOTAModule; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    property CreatorType default ctForm;
    property AncestorName: string read FAncestorName write FAncestorName;
    property ImplFileName: string read FImplFileName write FImplFileName;
    property IntfFileName: string read FIntfFileName write FIntfFileName;
    property FormName: string read FFormName write FFormName;
    property MainForm: Boolean read FMainForm write FMainForm default False;
    property ShowForm: Boolean read FShowForm write FShowForm default True;
    property ShowSource: Boolean read FShowSource write FShowSource default True;
    property SourceForm: TStrings read FSourceForm write SetSourceForm;
    property SourceImpl: TStrings read FSourceImpl write SetSourceImpl;
    property SourceIntf: TStrings read FSourceIntf write SetSourceIntf;
    property OnFormCreated: TFormCreatedEvent read FOnFormCreated write FOnFormCreated;
    property OnGetAge: TModuleAgeEvent read FOnGetAge write FOnGetAge;
    property OnGetSource: TModuleSourceEvent read FOnGetSource write FOnGetSource;
  end;

  TProjectEvent = procedure(Sender: TObject; const Project: IOTAProject) of object;
  TProjectCreator = class(TCreator, IOTAProjectCreator, IOTAProjectCreator50)
  private
    FFileName: string;
    FOptionFileName: string;
    FShowSource: Boolean;
    FOnNewDefaultModule: TProjectEvent;
    FOnNewProjectResource: TProjectEvent;
    function IOTAProjectCreator.GetOwner = GetOwnerModule;
    function IOTAProjectCreator50.GetOwner = GetOwnerModule;
  protected
    { IOTAProjectCreator }
    function GetFileName: string;
    function GetOptionFileName: string;
    function GetShowSource: Boolean;
    procedure NewDefaultModule;
    function NewOptionSource(const ProjectName: string): IOTAFile;
    procedure NewProjectResource(const Project: IOTAProject);
    function NewProjectSource(const ProjectName: string): IOTAFile;
    { IOTAProjectCreator50 }
    procedure NewDefaultProjectModule(const Project: IOTAProject);
    function GetOwnerModule: IOTAModule; override;
  public
    constructor Create(AOwner: TComponent); override;
  published
    property CreatorType default ctPackage;
    property FileName: string read FFileName write FFileName;
    property OptionFileName: string read FOptionFileName write FOptionFileName;
    property ShowSource: Boolean read FShowSource write FShowSource default False;
    property OnNewDefaultModule: TProjectEvent read FOnNewDefaultModule write FOnNewDefaultModule;
    property OnNewProjectResource: TProjectEvent read FOnNewProjectResource write FOnNewProjectResource;
  end;

implementation

uses
  Registry,
  WizardUtils;

{$R *.DFM}

resourcestring
  SEmptyIDString = 'IDString cannot be empty';

{ TWizardModule }

constructor TWizardModule.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FState := [wsEnabled];
end;

procedure TWizardModule.Execute;
begin
end;

function TWizardModule.GetDefaultAuthor: string;
var
  Reg: TRegistry;
begin
  Result := '';
  Reg := TRegistry.Create;
  try
    with BorlandIDEServices as IOTAServices do
      Reg.OpenKey(GetBaseRegistryKey + '\DMWizards', True); // do not localize
    Result := Reg.ReadString('DefaultAuthor'); // do not localize
    if Result = '' then
      Result := '<Author>'; // do not localize
  finally
    Reg.Free;
  end;
end;

function TWizardModule.GetIDString: string;
begin
  Result := FIDString;
end;

function TWizardModule.GetName: string;
begin
  Result := FWizardName;
end;

function TWizardModule.GetState: TWizardState;
begin
  Result := FState;
  if Assigned(FOnGetState) then
    FOnGetState(Self, Result);
end;

procedure TWizardModule.Loaded;
begin
  inherited Loaded;
  if FWizardName = '' then
    FWizardName := Name;
  if (FIDString = '') then
    FIDString := GetDefaultAuthor + '.' + Name;
end;

procedure TWizardModule.SetIDString(const Value: string);
begin
  if FIDString <> Value then
  begin
    if Value = '' then
      raise Exception.Create(SEmptyIDString);
    FIDString := Value;
  end;
end;

procedure TWizardModule.SetName(const Value: TComponentName);
var
  DefaultAuthor: string;
  OldName: string;
begin
  OldName := Name;
  inherited SetName(Value);
  if (FWizardName = '') or (FWizardName = OldName) then
    FWizardName := Value;
  DefaultAuthor := GetDefaultAuthor;
  if (FIDString = '') or (FIDString = DefaultAuthor + '.' + OldName) then
    FIDString := DefaultAuthor + '.' + Value;
end;

//----------------------------------------------------------------------------------------------------------------------

{ TCreator protected: IOTACreator }

//----------------------------------------------------------------------------------------------------------------------

function TCreator.GetCreatorType: string;

const
  CreatorTypes: array[TCreatorType] of string = (
    '',           // ctNone
    sApplication, // ctApplication
    sLibrary,     // ctLibrary
    sConsole,     // ctConsole
    sPackage,     // ctPackage
    sUnit,        // ctUnit
    sForm,        // ctForm
    sText         // ctText
  );

begin
  Result := CreatorTypes[FCreatorType];
end;

//----------------------------------------------------------------------------------------------------------------------

function TCreator.GetExisting: Boolean;

begin
  Result := FExisting;
end;

//----------------------------------------------------------------------------------------------------------------------

function TCreator.GetFileSystem: string;

begin
  Result := FFileSystem;
end;

//----------------------------------------------------------------------------------------------------------------------

function TCreator.GetUnnamed: Boolean;

begin
  Result := FUnnamed;
end;

//----------------------------------------------------------------------------------------------------------------------

{ TCreator public }

//----------------------------------------------------------------------------------------------------------------------

constructor TCreator.Create(AOwner: TComponent);

begin
  inherited Create(AOwner);
  FWizardModule := AOwner as TWizardModule;
  FUnnamed := True;
end;

//----------------------------------------------------------------------------------------------------------------------

{ TModuleFile protected: IOTAFile }

//----------------------------------------------------------------------------------------------------------------------

constructor TModuleFile.Create(ACreator: TModuleCreator; ASourceType: TModuleSourceType;
  const AModuleIdent, AFormIdent, AAncestorIdent: string);

begin
  inherited Create;
  FCreator := ACreator;
  FSourceType := ASourceType;
  FModuleIdent := AModuleIdent;
  FFormIdent := AFormIdent;
  FAncestorIdent := AAncestorIdent;
end;

//----------------------------------------------------------------------------------------------------------------------

function TModuleFile.GetAge: TDateTime;

begin
  Result := -1;
  if Assigned(FCreator) and Assigned(FCreator.OnGetAge) then
    FCreator.OnGetAge(FCreator, Result);
end;

//----------------------------------------------------------------------------------------------------------------------

function TModuleFile.GetSource: string;

begin
  Result := '';
  if Assigned(FCreator) then
  begin
    case FSourceType of
      mstForm:
        Result := Format(FCreator.FSourceForm.Text, [FModuleIdent, FFormIdent, FAncestorIdent]);
      mstImpl:
        Result := Format(FCreator.FSourceImpl.Text, [FModuleIdent, FFormIdent, FAncestorIdent]);
      mstIntf:
        Result := Format(FCreator.FSourceIntf.Text, [FModuleIdent, FFormIdent, FAncestorIdent]);
    end;
    // remove trailing CRLF
    if (Result[Length(Result) - 1] = #13) and (Result[Length(Result)] = #10) then
      Delete(Result, Length(Result) - 1, 2);
    if Assigned(FCreator.OnGetSource) then
      FCreator.OnGetSource(FCreator, FSourceType, Result);
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

{ TModuleCreator private }

//----------------------------------------------------------------------------------------------------------------------

procedure TModuleCreator.SetSourceForm(Value: TStrings);

begin
  FSourceForm.Assign(Value);
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TModuleCreator.SetSourceImpl(Value: TStrings);

begin
  FSourceImpl.Assign(Value);
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TModuleCreator.SetSourceIntf(Value: TStrings);

begin
  FSourceIntf.Assign(Value);
end;

//----------------------------------------------------------------------------------------------------------------------

{ TModuleCreator protected: IOTAModuleCreator }

//----------------------------------------------------------------------------------------------------------------------

function TModuleCreator.GetAncestorName: string;

begin
  Result := FAncestorName;
end;

//----------------------------------------------------------------------------------------------------------------------

function TModuleCreator.GetFormName: string;

begin
  Result := FFormName;
end;

//----------------------------------------------------------------------------------------------------------------------

function TModuleCreator.GetImplFileName: string;

begin
  Result := FImplFileName;
end;

//----------------------------------------------------------------------------------------------------------------------

function TModuleCreator.GetIntfFileName: string;

begin
  Result := FIntfFileName;
end;

//----------------------------------------------------------------------------------------------------------------------

function TModuleCreator.GetMainForm: Boolean;

begin
  Result := FMainForm;
end;

//----------------------------------------------------------------------------------------------------------------------

function TModuleCreator.GetShowForm: Boolean;

begin
  Result := FShowForm;
end;

//----------------------------------------------------------------------------------------------------------------------

function TModuleCreator.GetShowSource: Boolean;

begin
  Result := FShowSource;
end;

//----------------------------------------------------------------------------------------------------------------------

function TModuleCreator.NewFormFile(const FormIdent, AncestorIdent: string): IOTAFile;

begin
  if FSourceForm.Text = '' then
    Result := nil
  else
    Result := TModuleFile.Create(Self, mstForm, '', FormIdent, AncestorIdent);
end;

//----------------------------------------------------------------------------------------------------------------------

function TModuleCreator.NewImplSource(const ModuleIdent, FormIdent, AncestorIdent: string): IOTAFile;

begin
  if FSourceImpl.Text = '' then
    Result := nil
  else
    Result := TModuleFile.Create(Self, mstImpl, ModuleIdent, FormIdent, AncestorIdent);
end;

//----------------------------------------------------------------------------------------------------------------------

function TModuleCreator.NewIntfSource(const ModuleIdent, FormIdent, AncestorIdent: string): IOTAFile;

begin
  if FSourceIntf.Text = '' then
    Result := nil
  else
    Result := TModuleFile.Create(Self, mstIntf, ModuleIdent, FormIdent, AncestorIdent);
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TModuleCreator.FormCreated(const FormEditor: IOTAFormEditor);

begin
  if Assigned(FOnFormCreated) then
    FOnFormCreated(Self, FormEditor);
end;

//----------------------------------------------------------------------------------------------------------------------

function TModuleCreator.GetOwnerModule: IOTAModule;

begin
  Result := GetCurrentProject as IOTAModule;
  if Assigned(FOnGetOwner) then
    FOnGetOwner(Self, Result);
end;

//----------------------------------------------------------------------------------------------------------------------

{ TModuleCreator public }

//----------------------------------------------------------------------------------------------------------------------

constructor TModuleCreator.Create(AOwner: TComponent);

begin
  inherited Create(AOwner);
  FCreatorType := ctForm;
  FShowForm := True;
  FShowSource := True;
  FSourceForm := TStringList.Create;
  FSourceImpl := TStringList.Create;
  FSourceIntf := TStringList.Create;
end;

//----------------------------------------------------------------------------------------------------------------------

destructor TModuleCreator.Destroy;

begin
  FSourceForm.Free;
  FSourceImpl.Free;
  FSourceIntf.Free;
  inherited Destroy;
end;

//----------------------------------------------------------------------------------------------------------------------

{ TProjectCreator protected: IOTAProjectCreator}

//----------------------------------------------------------------------------------------------------------------------

function TProjectCreator.GetFileName: string;

begin
  Result := FFileName;
end;

//----------------------------------------------------------------------------------------------------------------------

function TProjectCreator.GetOptionFileName: string;

begin
  Result := FOptionFileName;
end;

//----------------------------------------------------------------------------------------------------------------------

function TProjectCreator.GetShowSource: Boolean;

begin
  Result := FShowSource;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TProjectCreator.NewDefaultModule;

begin
  // do nothing
end;

//----------------------------------------------------------------------------------------------------------------------

function TProjectCreator.NewOptionSource(const ProjectName: string): IOTAFile;

begin
  Result := nil;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TProjectCreator.NewProjectResource(const Project: IOTAProject);

begin
  if Assigned(FOnNewProjectResource) then
    FOnNewProjectResource(Self, Project);
end;

//----------------------------------------------------------------------------------------------------------------------

function TProjectCreator.NewProjectSource(const ProjectName: string): IOTAFile;

begin
  Result := nil;
end;

//----------------------------------------------------------------------------------------------------------------------

{ TProjectCreator protected: IOTAProjectCreator50 }

//----------------------------------------------------------------------------------------------------------------------

procedure TProjectCreator.NewDefaultProjectModule(const Project: IOTAProject);

begin
  if Assigned(FOnNewDefaultModule) then
    FOnNewDefaultModule(Self, Project);
end;

//----------------------------------------------------------------------------------------------------------------------

{ TProjectCreator protected }

//----------------------------------------------------------------------------------------------------------------------

function TProjectCreator.GetOwnerModule: IOTAModule;

begin
  Result := GetCurrentProjectGroup;
  if Assigned(FOnGetOwner) then
    FOnGetOwner(Self, Result);
end;

//----------------------------------------------------------------------------------------------------------------------

{ TProjectCreator public }

//----------------------------------------------------------------------------------------------------------------------

constructor TProjectCreator.Create(AOwner: TComponent);

begin
  inherited Create(AOwner);
  FCreatorType := ctPackage;
  FShowSource := True;
end;

//----------------------------------------------------------------------------------------------------------------------

end.
