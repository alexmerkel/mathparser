{ *********************************************************************** }
{                                                                         }
{ TextConsts                                                              }
{                                                                         }
{ Copyright (c) 2007 Pisarev Yuriy (post@pisarev.net)                     }
{                                                                         }
{ *********************************************************************** }

unit TextConsts;

interface

const
  Above = '>';
  Ampersand = '&';
  Asterisk = '*';
  AtSign = '@';
  Backslash = '\';
  Below = '<';
  Colon = ':';
  Comma = ',';
  CarriageReturn = #13;
  CR = CarriageReturn;
  Dollar = '$';
  Dot = '.';
  DoubleQuote = '"';
  DoubleSlash = '//';
  Equal = '=';
  Exclamation = '!';
  Inquiry = '?';
  LeftBrace = '{';
  LeftBracket = '[';
  LeftParenthesis = '(';
  LineFeed = #10;
  LF = LineFeed;
  Minus = '-';
  Percent = '%';
  Pipe = '|';
  Plus = '+';
  Pound = '#';
  Quote = '''';
  RightBrace = '}';
  RightBracket = ']';
  RightParenthesis = ')';
  Semicolon = ';';
  Slash = '/';
  Space = ' ';
  Tilde = '~';
  LineBreak = {$IFDEF LINUX} LF {$ENDIF} {$IFDEF MSWINDOWS} CR + LF {$ENDIF};
  LB = LineBreak;

  Blanks = [Space, CarriageReturn, LineFeed];
  Breaks = [CarriageReturn, LineFeed];

implementation

end.
