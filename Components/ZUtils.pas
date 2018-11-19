{ *********************************************************************** }
{                                                                         }
{ ZUtils                                                                  }
{                                                                         }
{ Copyright (c) 2005 Pisarev Yuriy (post@pisarev.net)                     }
{                                                                         }
{ *********************************************************************** }

unit ZUtils;

{$B-}

interface

uses
  Windows, Classes, ZLib, NumberConsts;

type
  TBuffer = array[0..1024 - 1] of Char;

var
  CompressionLevel: TCompressionLevel = clMax;

function CompressStream(const Source, Target: TStream): Single;
function Compress(const Stream: TStream): Single;
procedure DecompressStream(const Source, Target: TStream);
procedure Decompress(const Stream: TStream);

implementation

function CompressStream(const Source, Target: TStream): Single;
var
  ZStream: TCompressionStream;
begin
  ZStream := TCompressionStream.Create(CompressionLevel, Target);
  try
    ZStream.CopyFrom(Source, 0);
    Result := ZStream.CompressionRate;
  finally
    ZStream.Free;
  end;
end;

function Compress(const Stream: TStream): Single;
var
  ZStream: TMemoryStream;
begin
  ZStream := TMemoryStream.Create;
  try
    Result := CompressStream(Stream, ZStream);
    Stream.Size := 0;
    Stream.CopyFrom(ZStream, 0);
  finally
    ZStream.Free;
  end;
end;

procedure DecompressStream(const Source, Target: TStream);
var
  ZStream: TDecompressionStream;
  Count: Integer;
  Buffer: TBuffer;
begin
  Source.Position := 0;
  ZStream := TDecompressionStream.Create(Source);
  try
    repeat
      Count := ZStream.Read(Buffer, SizeOf(Buffer));
      Target.Write(Buffer, Count);
    until Count = 0;
  finally
    ZStream.Free;
  end;
end;

procedure Decompress(const Stream: TStream);
var
  ZStream: TMemoryStream;
begin
  ZStream := TMemoryStream.Create;
  try
    DecompressStream(Stream, ZStream);
    Stream.Size := 0;
    Stream.CopyFrom(ZStream, 0);
  finally
    ZStream.Free;
  end;
end;

end.
