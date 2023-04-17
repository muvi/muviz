unit CommandListImpl2;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, CommandList, AdvFunc;

type
  TCommandListItem    = record
    Command: ShortString;
    Data   : Pointer;
  end;

  TCommandList        = class (TObject,ICommandList)
  private
    FElemSize  : Integer;
    FCommands  : array of TCommandListItem;

    function FStrBigger(const Index: Integer; var FindData): Boolean;
    function FStrSmaller(const Index: Integer; var FindData): Boolean;
    function FStrEqual(const Index: Integer; var FindData): Boolean;

    procedure DeleteCommand(const Index: Integer);
    function FindGetCommand(const Index: Integer): ShortString; stdcall;
  protected
    function QueryInterface(constref iid : tguid; out obj) : longint; stdcall;
    function _AddRef : longint;stdcall;
    function _Release : longint;stdcall;
    function FindCommand(const ACommand: ShortString): Integer;
    function GetCommand(const ACommand: ShortString): Pointer; overload; stdcall;
    function GetData(const Index: Integer): Pointer; overload; stdcall;
    function GetCount: Integer; stdcall;
    function GetItem(const Index: Integer): ShortString; stdcall;
    function AddCommand2(const ACommand: ShortString; const AData): Integer;
  public
    constructor Create(const ElemSize: Integer; const InvalidData); virtual;
    destructor Destroy; override;
    procedure AddCommand(const ACommand: ShortString; const AData); stdcall;
    procedure RemoveCommand(const ACommand: ShortString); stdcall;
    procedure OverrideCommand(const ACommand: ShortString; const AData; out OldData); stdcall;
    procedure GetCommand(const ACommand: ShortString; out AData); overload; stdcall;
    procedure GetData(const Index: Integer; out AData); overload; stdcall;
    procedure ClearCommands; stdcall;
    function CommandExists(const Command: ShortString): Boolean; stdcall;
    property Data[Index: Integer]: Pointer read GetData;
    property Items[Index: Integer]: ShortString read GetItem;
    property Commands[ACommand: ShortString]: Pointer read GetCommand; default;
    property Count: Integer read GetCount;
  end;

implementation

type
  TFindGetStrFunction  = function (const AIndex: Integer): ShortString of object; stdcall;
  TCommandListFindData = record
    FFindStr   : ShortString;
    FFindGetStr: TFindGetStrFunction;
  end;

{TCommandList}

constructor TCommandList.Create(const ElemSize: Integer; const InvalidData);
begin
  inherited Create;
  FElemSize:=ElemSize;
  AddCommand('',InvalidData);
end;

destructor TCommandList.Destroy;
begin
  ClearCommands;
  inherited Destroy;
end;

function TCommandList.FStrBigger(const Index: Integer; var FindData): Boolean;
var
  AFindData: TCommandListFindData absolute FindData;
begin
  with AFindData do Result:=(FFindStr>FFindGetStr(Index));
end;

function TCommandList.FStrSmaller(const Index: Integer; var FindData): Boolean;
var
  AFindData: TCommandListFindData absolute FindData;
begin
  with AFindData do Result:=(FFindStr<FFindGetStr(Index));
end;

function TCommandList.FStrEqual(const Index: Integer; var FindData): Boolean;
var
  AFindData: TCommandListFindData absolute FindData;
begin
  with AFindData do Result:=(FFindStr=FFindGetStr(Index));
end;

function TCommandList.FindCommand(const ACommand: ShortString): Integer;
var
  AFindData: TCommandListFindData;
begin
  with AFindData do begin
    FFindStr:=ACommand;
    FFindGetStr:=@FindGetCommand;
  end;
  Result:=BinarySearch(@FStrBigger,@FStrSmaller,@FStrEqual,AFindData,Length(FCommands)-1,0,0);
end;

procedure TCommandList.DeleteCommand(const Index: Integer);
var
  I,L: Integer;
begin
  FreeMem(FCommands[Index].Data,FElemSize);
  L:=Pred(Length(FCommands));
  for I:=Index to L-1 do FCommands[I]:=FCommands[I+1];
  SetLength(FCommands,L);
end;

function TCommandList.FindGetCommand(const Index: Integer): ShortString; stdcall;
begin
  Result:=FCommands[Index].Command;
end;

function TCommandList.AddCommand2(const ACommand: ShortString; const AData): Integer;
begin
  Result:=Length(FCommands);
  SetLength(FCommands,Result+1);
  if Result>0 then while FCommands[Result-1].Command>ACommand do begin
    FCommands[Result]:=FCommands[Result-1];
    Dec(Result);
    if Result=0 then break;
  end;
  with FCommands[Result] do begin
    Command:=ACommand;
    GetMem(Data,FElemSize);
    Move(AData,Data^,FElemSize);
  end;
end;

procedure TCommandList.AddCommand(const ACommand: ShortString; const AData); stdcall;
begin
  AddCommand2(ACommand,AData);
end;

procedure TCommandList.RemoveCommand(const ACommand: ShortString); stdcall;
begin
  DeleteCommand(FindCommand(ACommand));
end;

procedure TCommandList.OverrideCommand(const ACommand: ShortString; const AData; out OldData); stdcall;
var
  AData2: Pointer;
begin
  AData2:=FCommands[FindCommand(ACommand)].Data;
  Move(AData2^,OldData,FElemSize);
  Move(AData,AData2^,FElemSize);
end;

function TCommandList.GetCommand(const ACommand: ShortString): Pointer; stdcall;
begin
  Result:=FCommands[FindCommand(ACommand)].Data;
end;

procedure TCommandList.GetCommand(const ACommand: ShortString; out AData); stdcall;
begin
  Move(FCommands[FindCommand(ACommand)].Data^,AData,FElemSize);
end;

function TCommandList.GetData(const Index: Integer): Pointer; stdcall;
begin
  Result:=FCommands[Index].Data;
end;

procedure TCommandList.GetData(const Index: Integer; out AData); stdcall;
begin
  Move(FCommands[Index].Data^,AData,FElemSize);
end;

function TCommandList.GetCount: Integer; stdcall;
begin
  Result:=Length(FCommands);
end;

function TCommandList.GetItem(const Index: Integer): ShortString; stdcall;
begin
  Result:=FCommands[Index].Command;
end;

procedure TCommandList.ClearCommands; stdcall;
var
  I: Integer;
begin
  for I:=0 to Length(FCommands)-1 do FreeMem(FCommands[I].Data,FElemSize);
  SetLength(FCommands,0);
end;

function TCommandList.CommandExists(const Command: ShortString): Boolean; stdcall;
begin
  Result:=(FindCommand(Command)>0);
end;

function TCommandList.QueryInterface(constref iid : tguid; out obj) : longint; stdcall;
begin
  Result:=0;
end;

function TCommandList._AddRef : longint;stdcall;
begin
  Result:=1;
end;

function TCommandList._Release : longint;stdcall;
begin
 Result:=1;
end;

end.

