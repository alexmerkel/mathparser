{ *********************************************************************** }
{                                                                         }
{ Log                                                                     }
{                                                                         }
{ Copyright (c) 2007 Pisarev Yuriy (post@pisarev.net)                     }
{                                                                         }
{ *********************************************************************** }

unit Log;

{$B-}

interface

uses
  Windows, SysUtils, Classes, FileUtils;

type
  TLog = class
  private
    FImmediateSave: Boolean;
    FFileIndex: Integer;
    FMaxFileSize: Integer;
    FCapacity: Integer;
    FFileName: string;
    procedure SetFileName(const Value: string);
    procedure SetMaxFileSize(const Value: Integer);
  public
    constructor Create; virtual;
    destructor Destroy; override;
    procedure Log(const Text: string); overload; virtual;
    procedure Log(const Text: string; const Arguments: array of const); overload; virtual;
    property MaxFileSize: Integer read FMaxFileSize write SetMaxFileSize;
    property Capacity: Integer read FCapacity write FCapacity;
    property ImmediateSave: Boolean read FImmediateSave write FImmediateSave;
    property FileName: string read FFileName write SetFileName;
    property FileIndex: Integer read FFileIndex write FFileIndex;
  end;

const
  DefaultCapacity = 4096;

implementation

uses
  TextConsts;

{ TLog }

constructor TLog.Create;
begin
  FFileIndex := CreateFile;
  FCapacity := DefaultCapacity;
  FImmediateSave := True;
end;

destructor TLog.Destroy;
begin
  DisposeFile(FFileIndex);
  inherited;
end;

procedure TLog.Log(const Text: string);
var
  AFileName: string;
  FromStream, ToStream: TFileStream;
  KeepSize: Integer;
begin
  FileUtils.FileIndex := FFileIndex;
  if (FMaxFileSize > FCapacity) and (ExactFileSize(FFileName) + Length(Text) > FMaxFileSize) then
  begin
    CloseFile;
    try
      AFileName := ExtractFilePath(FFileName) + Tilde + ExtractFileName(FFileName);
      FromStream := TFileStream.Create(FFileName, fmOpenRead);
      try
        KeepSize := FMaxFileSize - FCapacity;
        FromStream.Position := FromStream.Size - KeepSize;
        ToStream := TFileStream.Create(AFileName, fmCreate);
        try
          ToStream.CopyFrom(FromStream, FromStream.Size - FromStream.Position);
        finally
          ToStream.Free;
        end;
      finally
        FromStream.Free;
      end;
      DeleteFile(FFileName);
      MoveFile(PChar(AFileName), PChar(FFileName));
    finally
      OpenFile(FFileName, otCreate);
    end;
  end;
  Write(Text);
  if FImmediateSave then SaveFile;
end;

procedure TLog.Log(const Text: string; const Arguments: array of const);
begin
  Log(Format(Text, Arguments));
end;

procedure TLog.SetFileName(const Value: string);
begin
  FFileName := Value;
  FileUtils.FileIndex := FFileIndex;
  OpenFile(FFileName, otCreate);
end;

procedure TLog.SetMaxFileSize(const Value: Integer);
begin
  FMaxFileSize := Value;
end;

end.
