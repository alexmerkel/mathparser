{$B-}
{$I Directives.inc}
{$IFDEF VER150}
type
  NativeInt = Integer;
{$ELSE}
  {$IFDEF CompilerVersion<=18.5}
  type
    NativeInt = Integer;
  {$ENDIF}
{$ENDIF}
