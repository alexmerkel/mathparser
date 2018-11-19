{ *********************************************************************** }
{                                                                         }
{ FileUtils                                                               }
{                                                                         }
{ Copyright (c) 2006 Pisarev Yuriy (post@pisarev.net)                     }
{                                                                         }
{ *********************************************************************** }

unit FileUtils;

{$B-}
{$I Directives.inc}

interface

uses
  {$IFDEF DELPHI_XE7}WinApi.Windows, {$ELSE}Windows, {$ENDIF}SysUtils, Classes, Types;

type
  TOpenType = (otCreate, otOpen, otRewrite);
  TFiles = array of ^TextFile;

function CheckIndex(const Index: Integer): Boolean;
function CreateFile: Integer;
function DisposeFile(const Index: Integer): Boolean;
function RemoveFile(const Index: Integer): Boolean;
procedure RemoveAll;

function ForceFile(const FileName: string): Boolean;
function ForceDirectories(const FileName: string): Boolean;
function DeleteFile(const FileName: string): Boolean;
procedure DeleteDirectory(const Path: string);
function OpenFile(const FileName: string; OpenType: TOpenType): Boolean;
function SaveFile: Boolean;
function CloseFile: Boolean;
function EmptyFile: Boolean;
function FileSize: Integer;
function ExactFileSize(const FileName: string): Integer;
function Write(const S: string): Boolean; overload;
function Write(const StringArray: TStringDynArray): Boolean; overload;
function WriteFile(const FileName: string): Boolean;

var
  Files: TFiles;

threadvar
  FileIndex: Integer;

implementation

uses
  MemoryUtils, SearchUtils;

function CheckIndex(const Index: Integer): Boolean;
begin
  Result := (Index >= 0) and (Index < Length(Files));
end;

function CreateFile: Integer;
var
  I: Integer;
begin
  Result := -1;
  for I := Low(Files) to High(Files) do
    if not Assigned(Files[I]) then
    begin
      Result := I;
      Break;
    end;
  if Result < 0 then
  begin
    Result := Length(Files);
    SetLength(Files, Result + 1);
  end;
  New(Files[Result]);
end;

function DisposeFile(const Index: Integer): Boolean;
begin
  Result := CheckIndex(Index) and Assigned(Files[Index]);
  if Result then
  begin
    {$I-}
    System.CloseFile(Files[Index]^);
    {$I+}
    Result := IOResult = 0;
    if Result then
    begin
      Dispose(Files[Index]);
      Files[Index] := nil;
    end;
  end;
end;

function RemoveFile(const Index: Integer): Boolean;
var
  I: Integer;
begin
  Result := CheckIndex(Index);
  if Result then
  begin
    DisposeFile(Index);
    I := Length(Files);
    Result := Delete(Files, Index * SizeOf(Pointer), SizeOf(Pointer), I * SizeOf(Pointer));
    if Result then SetLength(Files, I - 1);
  end;
end;

procedure RemoveAll;
var
  I: Integer;
begin
  for I := Low(Files) to High(Files) do DisposeFile(I);
  Files := nil;
end;

function ForceFile(const FileName: string): Boolean;
var
  F: TextFile;
begin
  Result := Trim(FileName) <> '';
  if Result then
  begin
    AssignFile(F, FileName);
    {$I-}
    Append(F);
    if IOResult <> 0 then
    begin
      ForceDirectories(FileName);
      Rewrite(F);
    end;
    {$I+}
    Result := IOResult = 0;
    if Result then System.CloseFile(F);
  end;
end;

function ForceDirectories(const FileName: string): Boolean;
var
  FilePath: string;
begin
  FilePath := ExtractFilePath(FileName);
  Result := (FilePath <> '') and SysUtils.ForceDirectories(FilePath);
end;

function DeleteFile(const FileName: string): Boolean;
begin
  SetFileAttributes(PChar(FileName), 0);
  Result := {$IFDEF DELPHI_XE7}WinApi.Windows{$ELSE}Windows{$ENDIF}.DeleteFile(PChar(FileName));
end;

procedure DeleteDirectory(const Path: string);
var
  FileList, PathList: TStringList;
  I: Integer;
begin
  FileList := TStringList.Create;
  try
    PathList := TStringList.Create;
    try
      Search(Path, FileList, PathList, True);
      for I := 0 to FileList.Count - 1 do DeleteFile(FileList[I]);
      for I := 0 to PathList.Count - 1 do
      begin
        SetFileAttributes(PChar(PathList[I]), 0);
        RemoveDirectory(PChar(PathList[I]));
      end;
    finally
      PathList.Free;
    end;
  finally
    FileList.Free;
  end;
end;

function OpenFile(const FileName: string; OpenType: TOpenType): Boolean;
begin
  AssignFile(Files[FileIndex]^, FileName);
  {$I-}
  case OpenType of
    otCreate:
      begin
        Append(Files[FileIndex]^);
        if IOResult <> 0 then
        begin
          ForceDirectories(FileName);
          Rewrite(Files[FileIndex]^);
        end;
      end;
    otOpen: Append(Files[FileIndex]^);
    otRewrite:
      begin
        ForceDirectories(FileName);
        Rewrite(Files[FileIndex]^);
      end;
  end;
  {$I+}
  Result := IOResult = 0;
end;

function SaveFile: Boolean;
begin
  {$I-}
  Flush(Files[FileIndex]^);
  {$I+}
  Result := IOResult = 0;
end;

function CloseFile: Boolean;
begin
  {$I-}
  System.CloseFile(Files[FileIndex]^);
  {$I+}
  Result := IOResult = 0;
end;

function EmptyFile: Boolean;
begin
  {$I-}
  Rewrite(Files[FileIndex]^);
  {$I+}
  Result := IOResult = 0;
end;

function FileSize: Integer;
begin
  {$I-}
  Result := System.FileSize(Files[FileIndex]^);
  {$I+}
  if IOResult <> 0 then Result := -1;
end;

function ExactFileSize(const FileName: string): Integer;
var
  Handle: THandle;
begin
  Handle := {$IFDEF DELPHI_XE7}WinApi.Windows{$ELSE}Windows{$ENDIF}.CreateFile(PChar(FileName), GENERIC_READ,
    FILE_SHARE_READ or FILE_SHARE_WRITE, nil, OPEN_EXISTING, FILE_ATTRIBUTE_NORMAL, 0);
  if Handle = INVALID_HANDLE_VALUE then Result := -1
  else
    try
      Result := GetFileSize(Handle, nil);
    finally
      CloseHandle(Handle);
    end;
end;

function Write(const S: string): Boolean;
begin
  {$I-}
  WriteLn(Files[FileIndex]^, S);
  {$I+}
  Result := IOResult = 0;
end;

function Write(const StringArray: TStringDynArray): Boolean;
var
  I: Integer;
begin
  for I := Low(StringArray) to High(StringArray) do
  begin
    Result := Write(StringArray[I]);
    if not Result then Exit;
  end;
  Result := True;
end;

function WriteFile(const FileName: string): Boolean;
var
  F: TextFile;
  S: string;
begin
  AssignFile(F, FileName);
  {$I-}
  Reset(F);
  {$I+}
  Result := IOResult = 0;
  if Result then
  try
    while not Eof(F) do
    begin
      ReadLn(F, S);
      WriteLn(Files[FileIndex]^, S);
    end;
  finally
    System.CloseFile(F);
  end;
end;

initialization

finalization
  RemoveAll;

end.
