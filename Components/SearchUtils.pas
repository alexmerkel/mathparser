{ *********************************************************************** }
{                                                                         }
{ SearchUtils                                                             }
{                                                                         }
{ Copyright (c) 2003-2004 Pisarev Yuriy (post@pisarev.net)                }
{                                                                         }
{ *********************************************************************** }

unit SearchUtils;

{$B-}
{$I Directives.inc}

interface

uses
  {$IFDEF DELPHI_XE7}WinApi.Windows, {$ELSE}Windows, {$ENDIF}SysUtils, Classes, Types, TextConsts;

type
  TDriveType = (dtUnknown, dtNoDrive, dtFloppy, dtFixed, dtNetwork, dtCDROM, dtRAM);
  TDriveTypes = set of TDriveType;

  TSearchOption = (soFile, soPath);
  TSearchOptions = set of TSearchOption;

const
  AnyFile = '*.*';

var
  MaskDelimiter: string = Semicolon;

function GetDriveArray(const DriveTypes: TDriveTypes): TStringDynArray;

function Search(const Target: string; const FileList: TStrings; const PathList: TStrings = nil;
  const Recurse: Boolean = False; const IncludeModuleName: Boolean = False;
  const FileOrderByDepth: Boolean = False; const PathOrderByDepth: Boolean = False;
  const Depth: Integer = -1; const FileLock: PRTLCriticalSection = nil;
  const PathLock: PRTLCriticalSection = nil): Boolean;

function Find(const Target: string; const List: TStrings; const Recurse: Boolean = False;
  const FileOrderByDepth: Boolean = False; const PathOrderByDepth: Boolean = False;
  const Options: TSearchOptions = [soFile]): Integer;

implementation

uses
  Math, MemoryUtils, TextUtils, ThreadUtils;

function GetDriveArray(const DriveTypes: TDriveTypes): TStringDynArray;
const
  DriveCount = 26;
  CharOffset = Ord('a');
  NameSuffix = ':\';
var
  Drives: set of 0..DriveCount - 1;
  Drive: Integer;
  DriveName: string;
begin
  Integer(Drives) := GetLogicalDrives;
  for Drive := 0 to DriveCount - 1 do
    if Drive in Drives then
    begin
      DriveName := Char(Drive + CharOffset) + NameSuffix;
      if TDriveType(GetDriveType(PChar(DriveName))) in DriveTypes then
        Add(Result, DriveName);
    end;
end;

procedure Reorder(const FileList: TStrings; const AIndex, BIndex: Pointer);
var
  I: Integer;
begin
  for I := 0 to FileList.Count - 1 do
    if FileList.Objects[I] = AIndex then
      FileList.Objects[I] := BIndex
    else if FileList.Objects[I] = BIndex then
      FileList.Objects[I] := AIndex;
end;
 
function FileCompare(const AIndex, BIndex: Integer; const Target, Data: Pointer): TValueRelationship;
const
  Min: Integer = - High(Integer) - 1;
var
  FileList: TStrings absolute Target;
  PathList: TStrings absolute Data;
  I, J: Integer;
begin
  I := Integer(FileList.Objects[BIndex]);
  J := Integer(FileList.Objects[AIndex]);
  if (I >= 0) and (I < PathList.Count) then
    I := Integer(PathList.Objects[I])
  else
    I := Min;
  if (J >= 0) and (J < PathList.Count) then
    J := Integer(PathList.Objects[J])
  else
    J := Min;
  Result := CompareValue(-J, -I);
end;
 
function PathCompare(const AIndex, BIndex: Integer; const Target, Data: Pointer): TValueRelationship;
var
  PathList: TStrings absolute Target;
  I, J: Integer;
begin
  I := Integer(PathList.Objects[BIndex]);
  J := Integer(PathList.Objects[AIndex]);
  Result := CompareValue(-I, -J);
end;
 
procedure FileExchange(const AIndex, BIndex: Integer; const Target, Data: Pointer);
var
  FileList: TStrings absolute Target;
  S: string;
  AObject: TObject;
begin
  S := FileList[AIndex];
  AObject := FileList.Objects[AIndex];
  FileList[AIndex] := FileList[BIndex];
  FileList.Objects[AIndex] := FileList.Objects[BIndex];
  FileList[BIndex] := S;
  FileList.Objects[BIndex] := AObject;
end;
 
procedure PathExchange(const AIndex, BIndex: Integer; const Target, Data: Pointer);
var
  PathList: TStrings absolute Target;
  FileList: TStrings absolute Data;
  S: string;
  AObject: TObject;
begin
  S := PathList[AIndex];
  AObject := PathList.Objects[AIndex];
  PathList[AIndex] := PathList[BIndex];
  PathList.Objects[AIndex] := PathList.Objects[BIndex];
  PathList[BIndex] := S;
  PathList.Objects[BIndex] := AObject;
  Reorder(FileList, Pointer(AIndex), Pointer(BIndex));
end;
 
function Search(const Target: string; const FileList, PathList: TStrings; const Recurse: Boolean;
  const IncludeModuleName, FileOrderByDepth, PathOrderByDepth: Boolean; const Depth: Integer;
  const FileLock, PathLock: PRTLCriticalSection): Boolean;
 
  function Add(const List: TStrings; const Data: Pointer; const Name: string; const Lock: PRTLCriticalSection): Integer;
  begin
    if Assigned(Lock) then Enter(Lock^);
    try
      Result := List.IndexOf(Name);
      if Result < 0 then Result := List.AddObject(Name, Data);
    finally
      if Assigned(Lock) then Leave(Lock^);
    end;
  end;
 
const
  cCFolder = '.';
  cPFolder = '..';
var
  List: TStrings;
  PathArray: TStringDynArray;
  I, J, K: Integer;
  Path, Name, S, ModuleName: string;
  R: TSearchRec;
begin
  if Assigned(PathList) then
    List := PathList
  else
    List := TStringList.Create;
  try
    Result := Write(Target, Semicolon, PathArray, False);
    if Result then
    try
      for I := Low(PathArray) to High(PathArray) do
      begin
        Path := Trim(ExtractFilePath(PathArray[I]));
        Name := Trim(ExtractFileName(PathArray[I]));
        if Name = '' then Name := AnyFile;
        J := Add(List, Pointer(Depth), Path, PathLock);
        K := List.Count;
        if FindFirst(Path + AnyFile, faAnyFile, R) = 0 then
        try
          repeat
            if (R.Attr and faDirectory = faDirectory) and not TextUtils.SameText(R.Name, cCFolder) and
              not TextUtils.SameText(R.Name, cPFolder) then
                Add(List, Pointer(Depth - 1), IncludeTrailingPathDelimiter(Path + R.Name), PathLock);
          until FindNext(R) <> 0;
        finally
          FindClose(R);
        end;
        if Assigned(FileList) then
        begin
          ModuleName := ParamStr(0);
          if FindFirst(Path + Name, faAnyFile, R) = 0 then
          try
            repeat
              if R.Attr and faDirectory = 0 then
              begin
                S := Path + R.Name;
                if IncludeModuleName or not TextUtils.SameText(ModuleName, S) then
                  Add(FileList, Pointer(J), S, FileLock);
              end;
            until FindNext(R) <> 0;
          finally
            FindClose(R);
          end;
        end;
        if Recurse then
          for J := K to List.Count - 1 do
            Search(List[J] + Name, FileList, List, Recurse, IncludeModuleName, False, False, Depth - 1, FileLock, PathLock);
        Result := Assigned(FileList) and (FileList.Count > 0) or Assigned(PathList) and (List.Count > 0);
      end;
      if Result and (Depth = -1) then
      begin
        if FileOrderByDepth and Assigned(FileList) then
          QSort(FileList, 0, FileList.Count - 1, FileCompare, FileExchange, PathList);
        if PathOrderByDepth and Assigned(PathList) then
          QSort(PathList, 0, PathList.Count - 1, PathCompare, PathExchange, FileList);
      end;
    finally
      PathArray := nil;
    end;
  finally
    if List <> PathList then List.Free;
  end;
end;
 
function Find(const Target: string; const List: TStrings; const Recurse, FileOrderByDepth, PathOrderByDepth: Boolean;
  const Options: TSearchOptions): Integer;
var
  FileList, PathList: TStringList;
  I, J: Integer;
begin
  if soFile in Options then
    FileList := TStringList.Create
  else
    FileList := nil;
  try
    if soPath in Options then
      PathList := TStringList.Create
    else
      PathList := nil;
    try
      Search(Target, FileList, PathList, Recurse, True, FileOrderByDepth, PathOrderByDepth);
      Result := 0;
      J := List.Count;
      if Assigned(PathList) then
        for I := 0 to PathList.Count - 1 do
        begin
          List.AddObject(PathList[I], PathList.Objects[I]);
          Inc(Result);
        end;
      if Assigned(FileList) then
        for I := 0 to FileList.Count - 1 do
        begin
          List.AddObject(FileList[I], Pointer(J + Integer(FileList.Objects[I])));
          Inc(Result);
        end;
    finally
      PathList.Free;
    end;
  finally
    FileList.Free;
  end;
end;

end.
