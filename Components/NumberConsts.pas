{ *********************************************************************** }
{                                                                         }
{ NumberConsts                                                            }
{                                                                         }
{ Copyright (c) 2007 Pisarev Yuriy (post@pisarev.net)                     }
{                                                                         }
{ *********************************************************************** }

unit NumberConsts;

{$B-}
{$I Directives.inc}

interface

uses
  SysUtils, TextConsts, Types;

type
  TNumberType = (ntZero, ntOne, ntTwo, ntThree, ntFour, ntFive, ntSix, ntSeven, ntEight, ntNine);
  TNumberTypes = set of TNumberType;

  TIndexFlag = (ifA, ifB, ifC, ifD, ifE, ifF, ifG, ifH, ifI, ifJ, ifK, ifL, ifM, ifN, ifO, ifP, ifQ, ifR, ifS, ifT, ifU, ifV, ifW, ifX, ifY, ifZ);
  TIndexFlags = set of TIndexFlag;

const
  NumberChar: array[TNumberType] of Char = '0123456789';
  LCaseIndexChar: array[TIndexFlag] of Char = 'abcdefghijklmnopqrstuvwxyz';
  UCaseIndexChar: array[TIndexFlag] of Char = 'ABCDEFGHIJKLMNOPQRSTUVWXYZ';

  AIndex = Ord(ifA);
  BIndex = Ord(ifB);
  CIndex = Ord(ifC);
  DIndex = Ord(ifD);
  EIndex = Ord(ifE);
  FIndex = Ord(ifF);
  GIndex = Ord(ifG);
  HIndex = Ord(ifH);
  IIndex = Ord(ifI);
  JIndex = Ord(ifJ);
  KIndex = Ord(ifK);
  LIndex = Ord(ifL);
  MIndex = Ord(ifM);
  NIndex = Ord(ifN);
  OIndex = Ord(ifO);
  PIndex = Ord(ifP);
  QIndex = Ord(ifQ);
  RIndex = Ord(ifR);
  SIndex = Ord(ifS);
  TIndex = Ord(ifT);
  UIndex = Ord(ifU);
  VIndex = Ord(ifV);
  WIndex = Ord(ifW);
  XIndex = Ord(ifX);
  YIndex = Ord(ifY);
  ZIndex = Ord(ifZ);

  Zero = Ord(ntZero);
  One = Ord(ntOne);
  Two = Ord(ntTwo);
  Three = Ord(ntThree);
  Four = Ord(ntFour);
  Five = Ord(ntFive);
  Six = Ord(ntSix);
  Seven = Ord(ntSeven);
  Eight = Ord(ntEight);
  Nine = Ord(ntNine);

  Kilobyte: LongWord = 1024;
  Megabyte: LongWord = 1048576;
  Gigabyte: LongWord = 1073741824;
  Terabyte: Int64 = 1099511627776;
  Petabyte: Int64 = 1125899906842624;

const
  Signs: TSysCharSet = [Minus, Plus];
  Digits: TSysCharSet = ['0', '1', '2', '3', '4', '5', '6', '7', '8', '9'];

function Number(const Text: string): Boolean;

implementation

{$IFNDEF UNICODE}
uses
  TextUtils;
{$ENDIF}

function Number(const Text: string): Boolean;
var
  I: Integer;
begin
  Result := Text <> '';
  if Result then
    for I := 1 to Length(Text) do
    begin
      Result := CharInSet(Text[I], Digits);
      if not Result then Break;
    end;
end;

end.
