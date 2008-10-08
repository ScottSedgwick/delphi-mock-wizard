unit MockGeneratorIDEAddIn;

interface

uses
  Menus,
  ToolsAPI;

type
  TMockGeneratorAddIn = class(TNotifierObject, INTAProjectMenuCreatorNotifier)
  private
    procedure MyContextMenuClickHandler(Sender: TObject);
  public
    function AddMenu(const Ident: string): TMenuItem;
    function CanHandle(const Ident: string): Boolean;
  end;

procedure Register;

implementation

uses
  Classes,
  Dialogs, 
  DMWizard,
  IntfParser,
  MockGenerator,
  SysUtils;

resourcestring
  StrMyContextMenu = 'Generate Mock';
  StrSelectedFileIs = 'Generate mocks for interfaces in:'#13#10'%s (%s)';

function ExtractUnitName(const Filename: string): string;
var
  sFileExt, sFileName: string;
begin
  sFileName := ExtractFileName(FileName);
  sFileExt := ExtractFileExt(sFileName);
  Result := Copy(sFileName, 1, Length(sFileName) - Length(sFileExt));
end;

{ TMockGeneratorAddIn }

function TMockGeneratorAddIn.AddMenu(const Ident: string): TMenuItem;
begin
  result := TMenuItem.Create(nil);
  result.Caption := StrMyContextMenu;
  result.OnClick := MyContextMenuClickHandler;
end;

function TMockGeneratorAddIn.CanHandle(const Ident: string): Boolean;
begin
  result := Ident = sFileContainer;
end;

procedure TMockGeneratorAddIn.MyContextMenuClickHandler(Sender: TObject);
var
  SelectedFilename: string;
  ModuleCreator: TModuleCreator;
  parser: TIntfParser;
  MockGen: TMockGenerator;
  sUnitName, sClassName, sFileName: string;
  Lines: TStringList;
begin
  (BorlandIDEServices as IOTAProjectManager).GetCurrentSelection(SelectedFilename);
  Lines := TStringList.Create;
  try
    parser := TIntfParser.Create;
    try                           
      parser.LoadAndRun(SelectedFilename);
      (BorlandIDEServices as IOTAModuleServices).GetNewModuleAndClassName('', sUnitName, sClassName, sFileName);
      MockGen := TMockGenerator.Create(sUnitName, ExtractUnitName(SelectedFilename), parser.AUnit);
      try
        MockGen.Output(Lines);
      finally
        FreeAndNil(MockGen);
      end;
    finally
      FreeAndNil(parser);
    end;
    ModuleCreator := TModuleCreator.Create(nil);
    try
      //Set up creator here
      ModuleCreator.CreatorType := ctUnit;
      ModuleCreator.SourceImpl.Assign(Lines);
      (BorlandIDEServices as IOTAModuleServices).CreateModule(ModuleCreator);
    finally
      FreeAndNil(ModuleCreator);
    end;
  finally
    FreeAndNil(Lines);
  end;
end;

var
  FNotifierIndex: Integer;

procedure Register;
begin
  FNotifierIndex := (BorlandIDEServices as IOTAProjectManager).AddMenuCreatorNotifier(TMockGeneratorAddIn.Create);
end;

initialization
  FNotifierIndex := -1;

finalization
  if FNotifierIndex > -1 then
    (BorlandIDEServices as IOTAProjectManager).RemoveMenuCreatorNotifier(FNotifierIndex);

end.
