{ *********************************************************************** }
{                                                                         }
{ GraphicTypes                                                            }
{                                                                         }
{ Copyright (c) 2007 Pisarev Yuriy (post@pisarev.net)                     }
{                                                                         }
{ *********************************************************************** }

unit GraphicTypes;

{$B-}

interface

uses
  Graphics, Types;

type
  PExactPoint = ^TExactPoint;
  TExactPoint = record
    X, Y, Angle: Extended;
  end;

  PExactSize = ^TExactSize;
  TExactSize = record
    cx, cy: Extended;
  end;

  PExactArea = ^TExactArea;
  TExactArea = record
    Min, Max: TExactPoint;
  end;

  PExactRect = ^TExactRect;
  TExactRect = record
    Left, Top, Right, Bottom: Extended;
  end;

  TExactPointDynArray = array of TExactPoint;
  TPointDynArray = array of TPoint;

  TChannelType = (ctBlue, ctGreen, ctRed);
  TPixel = array[TChannelType] of Byte;
  PPixel = ^TPixel;
  THeavyPixel = array[TChannelType] of Integer;

  TColorArray = array of TColor;

implementation

end.
