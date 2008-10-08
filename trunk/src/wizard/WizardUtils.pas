unit WizardUtils;

interface

uses
  Classes, 
  SysUtils,
  Windows, 
  Forms, 
  Dialogs, 
  ActnList, 
  Graphics,
  ToolsAPI;

function FindAppBuilderForm: TCustomForm;
function FindDelphiAction(const AName: string): TContainedAction;
function FindModuleInterface(AInterface: TGUID): IUnknown;
function GetCurrentProject: IOTAProject;
function GetCurrentProjectGroup: IOTAProjectGroup;
procedure LogMessage(const MessageStr: string; const FileName: string = ''; const PrefixStr: string = ''; LineNumber: Integer = 0; ColumnNumber: Integer = 0);
procedure LogMessageFmt(const MessageStr: string; const Args: array of const; const FileName: string = ''; const PrefixStr: string = ''; LineNumber: Integer = 0; ColumnNumber: Integer = 0);
procedure RegisterActions(WizardActions: TCustomActionList);
function RegisterActionWithImageIndex(WizardAction: TCustomAction; const DelphiActionName: string): Boolean;
procedure ShowMessageView;

implementation

uses
  Consts, 
  Registry;         

function FindAppBuilderForm: TCustomForm;                  
begin
  Result := Application.MainForm;              
end;

function FindDelphiAction(const AName: string): TContainedAction;
var
  DelphiActions: TCustomActionList;
  I: Integer;
begin
  Result := nil;
  with BorlandIDEServices as INTAServices40 do
    DelphiActions := ActionList;
  if DelphiActions = nil then
    Exit;

  with DelphiActions do
    for I := 0 to ActionCount - 1 do
      if Actions[I].Name = AName then
      begin
        Result := Actions[I];
        Break;
      end;
end;

function FindModuleInterface(AInterface: TGUID): IUnknown;
var
  I: Integer;
begin
  Result := nil;
  with BorlandIDEServices as IOTAModuleServices do
    for I := 0 to ModuleCount - 1 do
      if (Modules[I].QueryInterface(AInterface, Result) = S_OK) then
        Break;
end;

function GetCurrentProject: IOTAProject;
var
  ProjectGroup: IOTAProjectGroup;
begin
  ProjectGroup := GetCurrentProjectGroup;
  if Assigned(ProjectGroup) then
    Result := ProjectGroup.ActiveProject
  else
    Result := FindModuleInterface(IOTAProject) as IOTAProject;
end;

function GetCurrentProjectGroup: IOTAProjectGroup;
begin
  Result := FindModuleInterface(IOTAProjectGroup) as IOTAProjectGroup;
end;

procedure LogMessage(const MessageStr: string; const FileName: string = ''; const PrefixStr: string = '';
  LineNumber: Integer = 0; ColumnNumber: Integer = 0);
begin
  with BorlandIDEServices as IOTAMessageServices40 do
    AddToolMessage(FileName, MessageStr, PrefixStr, LineNumber, ColumnNumber);
  ShowMessageView;
end;

procedure LogMessageFmt(const MessageStr: string; const Args: array of const; const FileName: string = '';
  const PrefixStr: string = ''; LineNumber: Integer = 0; ColumnNumber: Integer = 0);
begin
  LogMessage(Format(MessageStr, Args), FileName, PrefixStr, LineNumber, ColumnNumber);
end;

procedure RegisterActions(WizardActions: TCustomActionList);
var
  NTAServices: INTAServices;
  DelphiActions: TCustomActionList;
  NewImageIndex: Integer;
  Action: TCustomAction;
  Bitmap: TBitmap;
begin
  NTAServices := BorlandIDEServices as INTAServices;
  DelphiActions := NTAServices.ActionList;
  with WizardActions do
    if Assigned(Images) then
    begin
      while ActionCount > 0 do
      begin
        if Actions[0] is TCustomAction then
        begin
          Action := TCustomAction(Actions[0]);
          with Action do
          begin
            Bitmap := TBitmap.Create;
            try
              Bitmap.Height := Images.Height;
              Bitmap.Width := Images.Width;
              Images.GetBitmap(ImageIndex, Bitmap);
              NewImageIndex := NTAServices.AddMasked(Bitmap, clWhite, Name + 'Image');

              ActionList := DelphiActions;
              ImageIndex := NewImageIndex;
            finally
              Bitmap.Free;
            end;
          end;
        end;
      end;
    end
    else
      while ActionCount > 0 do
        Actions[0].ActionList := DelphiActions;
end;

function RegisterActionWithImageIndex(WizardAction: TCustomAction; const DelphiActionName: string): Boolean;
var
  DelphiAction: TContainedAction;
begin
  Result := False;
  DelphiAction := FindDelphiAction(DelphiActionName);
  if DelphiAction = nil then
    Exit;
  with WizardAction do
  begin
    ActionList := (BorlandIDEServices as INTAServices40).ActionList;
    if DelphiAction is TCustomAction then
    begin
      ImageIndex := TCustomAction(DelphiAction).ImageIndex;
      Result := True;
    end
    else
      ImageIndex := -1;
  end;
end;

procedure ShowMessageView;
var
  I: Integer;
begin
  for I := 0 to Screen.FormCount - 1 do
    if Screen.Forms[I].ClassNameIs('TMsgWindow') or Screen.Forms[I].ClassNameIs('TMessageViewForm') then // do not localize
    begin
      Screen.Forms[I].Show;
      Break;
    end;
end;

end.
