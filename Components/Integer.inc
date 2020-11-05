{$B-}
{$I Directives.inc}
{$IFDEF DELPHI_7}
type
  NativeInt = Integer;
{$ELSE}
  {$IFDEF CompilerVersion<=18.5}
  type
    NativeInt = Integer;
  {$ENDIF}
{$ENDIF}
