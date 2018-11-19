{ *********************************************************************** }
{                                                                         }
{ SystemTypes                                                             }
{                                                                         }
{ Copyright (c) 2013 Pisarev Yuriy (post@pisarev.net)                     }
{                                                                         }
{ *********************************************************************** }

unit SystemTypes;

{$B-}

interface

uses
  Windows;

const
  BufferSize = MAX_PATH;

type
  TBuffer = array[0..BufferSize - 1] of Char;

  PProductType = ^TProductType;
  TProductType = (ptUnknown, ptDomainController, ptServer, ptWorkstation);
  TVersionType = (vtUnknown, vt95, vt98, vtME, vtNT3, vtNT4, vtXP, vtVista, vt7, vt8, vt2000, vt2003, vt2003r2, vt2008, vt2008r2, vt2012, vtUp);

implementation

end.
