{ *********************************************************************** }
{                                                                         }
{ Parser                                                                  }
{                                                                         }
{ Copyright (c) 2010 Pisarev Yuriy (post@pisarev.net)                     }
{                                                                         }
{ *********************************************************************** }

unit License;

interface

uses
  Classes;

function GetLicense: TStrings;
function GetLicenseCount: Integer;

const
  LicenseError = 'Parser has reached maximum for amount of user-defined function within the current license';

implementation

uses
  Cipher, IniFiles, SearchUtils, SysUtils;

type
  TLicense = record
    Guid: string;
    LicenseCount: Integer;
  end;

const
  LicenseFileMask = '*.license';

  ASection = 'A';
  AIdent = 'A';
  BIdent = 'B';

  LicenseArray: array[0..4] of TLicense = ((Guid: '{B41FE89B-DE50-4077-9395-8832921710DD}'; LicenseCount: 5),
    (Guid: '{ABA75428-9883-4DB0-B5D2-8C83019A167B}'; LicenseCount: 100),
    (Guid: '{DF236438-4278-4265-9ABA-22F35C0CF951}'; LicenseCount: 500),
    (Guid: '{15BAF50B-323E-40FA-8846-DADEC0EC9025}'; LicenseCount: 1000),
    (Guid: '{EEE2E42D-DD48-4775-A830-E98130650306}'; LicenseCount: -1));

  DefaultLicenseCount = -1;

var
  ALicense: TStringList;
  LicenseCount: Integer = DefaultLicenseCount;
  FileList: TStringList;
  ACipher: TCipher;
  IniFile: TIniFile;
  S: string;
  I, J: Integer;

function GetLicense: TStrings;
begin
  Result := ALicense;
end;

function GetLicenseCount: Integer;
begin
  Result := LicenseCount;
end;

function ReadLicenseCount(const Text: string; out Count: Integer): Boolean;
var
  I, J: Integer;
begin
  Count := 0;
  for I := Low(LicenseArray) to High(LicenseArray) do
    if Pos(LicenseArray[I].Guid, Text) > 0 then
    begin
      J := LicenseArray[I].LicenseCount;
      if (J < 0) or (Count < J) then
      begin
        Count := J;
        if Count < 0 then Break;
      end;
    end;
  Result := Count <> 0;
end;

initialization
  ALicense := TStringList.Create;
  try
    FileList := TStringList.Create;
    try
      if Search(ExtractFilePath(ParamStr(0)) + LicenseFileMask, FileList) then
      begin
        ACipher := TCipher.Create(nil);
        try
          for I := 0 to FileList.Count - 1 do
          begin
            IniFile := TIniFile.Create(FileList[I]);
            try
              S := Trim(IniFile.ReadString(ASection, AIdent, ''));
              if S = '' then Continue;
              try
                ACipher.Key := S;
              except
                Continue;
              end;
              S := Trim(IniFile.ReadString(ASection, BIdent, ''));
              if S = '' then Continue;
              ACipher.Lines.Text := S;
            finally
              IniFile.Free;
            end;
            try
              ACipher.Decrypt;
            except
              Continue;
            end;
            if ReadLicenseCount(ACipher.Lines.Text, J) and ((J < 0) or (LicenseCount < J)) then
            begin
              ALicense.Assign(ACipher.Lines);
              LicenseCount := J;
              if LicenseCount < 0 then Break;
            end;
          end;
        finally
          ACipher.Free;
        end;
      end;
    finally
      FileList.Free;
    end;
  except
  end;

finalization
  ALicense.Free;

end.
