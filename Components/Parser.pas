{ *********************************************************************** }
{                                                                         }
{ Parser                                                                  }
{                                                                         }
{ Copyright (c) 2006 Pisarev Yuriy (post@pisarev.net)                     }
{                                                                         }
{ *********************************************************************** }

unit Parser;

{$B-}
{$I Directives.inc}

interface

uses
  {$IFDEF DELPHI_XE7}WinApi.Windows, WinApi.Messages, {$ELSE}Windows, Messages,
  {$ENDIF}SysUtils, SyncObjs, Classes, Types, FlexibleList, Math, Notifier, ParseCache, ParseConsts,
  ParseErrors, ParseMethod, ParseTypes, TextConsts, TextBuilder, ValueTypes;

{$I Integer.inc}

type
  TFunctionEvent = function(const AFunction: PFunction; const AType: PType; out Value: TValue;
    const LValue, RValue: TValue; const PA: TParameterArray): Boolean of object;

  TTextType = (ttUnknown, ttNumber, ttFunction, ttString, ttScript);
  TTextData = record
    TextType: TTextType;
    Value: TValue;
    Handle: Integer;
    Text: TString;
  end;

  TOperatorKind = (okNewest, okLatest, okNumber, okFunction, okScript, okParameter);
  TSyntax = record
    PriorKind: TOperatorKind;
    PriorHandle: Integer;
  end;

  PBracketData = ^TBracketData;
  TBracketData = record
    SA: PScriptArray;
    Parameter: Boolean;
    Text: TTextBuilder;
    Idle: PFunction;
  end;

  PParameterArrayData = ^TParameterArrayData;
  TParameterArrayData = record
    Fake: Boolean;
    PA: PParameterArray;
    Index: PInteger;
  end;

  PParameterOptimizationData = ^TParameterOptimizationData;
  TParameterOptimizationData = record
    ItemArray: TScriptArray;
    Optimal: Boolean;
  end;

  POptimizationData = ^TOptimizationData;
  TOptimizationData = record
    ItemArray: TScriptArray;
    Number: TNumber;
  end;

  TRetrieveMode = (rmNone, rmUser, rmFull);

  PDecompileData = ^TDecompileData;
  TDecompileData = record
    Text, ItemText: TTextBuilder;
    Delimiter: string;
    AFunction: PFunction;
    Parameter: Boolean;
    ParameterBracket: TBracket;
    TypeMode: TRetrieveMode;
  end;

const
  CacheSeparator = Pipe;

type
  TCacheType = (ctRawscript, ctScript, ctParameter, ctSubscript, ctSubparameter);
  TCacheArray = array[TCacheType] of TParseCache;

  TCustomParser = class;

  TCache = class(TComponent)
  private
    FSubscript: TParseCache;
    FRawscript: TParseCache;
    FScript: TParseCache;
    FSubparameter: TParseCache;
    {$IFDEF DELPHI_7}
    FNameValueSeparator: Char;
    {$ENDIF}
    FParser: TCustomParser;
    FParameter: TParseCache;
    FCacheArray: TCacheArray;
    function GetCache(CacheType: TCacheType): TParseCache;
    {$IFDEF DELPHI_7}
    procedure SetNameValueSeparator(const Value: Char);
    {$ENDIF}
  protected
    procedure SetCacheType(const Value: TListType); virtual;
    property CacheArray: TCacheArray read FCacheArray;
    property Parser: TCustomParser read FParser write FParser;
  public
    constructor Create(const AParser: TCustomParser; const AConnector: TComponent); reintroduce; overload; virtual;
    procedure Prepare; virtual;
    procedure Clear; virtual;
    {$IFDEF DELPHI_7}
    procedure WriteNameValueSeparator; virtual;
    {$ENDIF}
    property Cache[CacheType: TCacheType]: TParseCache read GetCache;
    property CacheType: TListType write SetCacheType;
  published
    {$IFDEF DELPHI_7}
    property NameValueSeparator: Char read FNameValueSeparator write SetNameValueSeparator default CacheSeparator;
    {$ENDIF}
    property Rawscript: TParseCache read FRawscript;
    property Script: TParseCache read FScript;
    property Parameter: TParseCache read FParameter;
    property Subscript: TParseCache read FSubscript;
    property Subparameter: TParseCache read FSubparameter;
  end;

  TParameterType = (ptParameter {TParameterKind = pkValue}, ptScript {TParameterKind = pkReference});

  TNotify = class(TObject)
  public
    NotifyType: TNotifyType;
    Component: TComponent;
  end;
  TNotifyArray = array of TNotify;

  TExecuteOption = (eoSubsequent);
  TExecuteOptions = set of TExecuteOption;

  TCustomParser = class(TComponent)
  private
    FWhileHandle: Integer;
    FOnFunction: TFunctionEvent;
    FRepeatHandle: Integer;
    FUpdateCount: Integer;
    FBeforeFunction: TFunctionEvent;
    FMethod: TMethod;
    FGetHandle: Integer;
    FDeleteHandle: Integer;
    FInternalHandle: Integer;
    FFData: PFunctionData;
    FConstantlist: TFlexibleList;
    FPrioritize: Boolean;
    FStringHandle: Integer;
    FFindHandle: Integer;
    FIgnoreType: array[TItemCode] of Boolean;
    FPositiveHandle: PInteger;
    FSetHandle: Integer;
    FForHandle: Integer;
    FDefaultTypeHandle: Integer;
    FParameterBracket: TBracket;
    FAddressee: TComponent;
    FTrueValue: Integer;
    FFalseValue: Integer;
    FTOHArray: TIntegerDynArray;
    FPData: PFunctionData;
    FNotifyArray: TNotifyArray;
    FExecuteOptions: TExecuteOptions;
    FNewHandle: Integer;
    FBracket: TBracket;
    FTData: PTypeData;
    FDefaultValueType: TValueType;
    FVoidHandle: Integer;
    FWindowHandle: THandle;
    FExceptionMask: TFPUExceptionMask;
    FScriptHandle: Integer;
    FNegativeHandle: PInteger;
    function GetIgnoreType(const ItemCode: TItemCode): Boolean;
    procedure SetIgnoreType(const ItemCode: TItemCode; const Value: Boolean);
    function InternalAddFunction(const AFunction: TFunction): Boolean; overload;
    function InternalAddFunction(const AName: string; var Handle: Integer; const Kind: TFunctionKind;
      const Method: TFunctionMethod; const Optimizable: Boolean;
      const ReturnType: TValueType = vtUnknown; const Priority: TFunctionPriority = fpNormal;
      const Coverage: TPriorityCoverage = pcLocal): Boolean; overload;
    function InternalAddVariable(const AName: string; var Variable: TValue; const Optimizable: Boolean;
      const ReturnType: TValueType = vtUnknown): Boolean; overload;
    function InternalAddVariable(const AName: string; const Variable: TLiveValue; const Optimizable: Boolean;
      const ReturnType: TValueType = vtUnknown): Boolean; overload;
    function InternalAddConstant(const AName: string; const Value: TValue): Boolean;
    function GetFInternal: PFunction;
  protected
    procedure Notification(Component: TComponent; Operation: TOperation); override;
    procedure WindowMethod(var Message: TMessage); virtual;
    function Notifiable(const NotifyType: TNotifyType): Boolean; virtual;
    procedure Notify; overload; virtual;
    function DoEvent(const Event: TFunctionEvent; const AFunction: PFunction; const AType: PType; out Value: TValue;
      const LValue, RValue: TValue; const PA: TParameterArray): Boolean; virtual;
    procedure ReadParameterArray(var AIndex: NativeInt; out AParameterArray: TParameterArray; const ParameterType: TParameterType = ptParameter;
      const AFake: Boolean = False); virtual; abstract;
    function GetTextData(const Text: string): TTextData; virtual;
    function CheckFHandle(const Handle: Integer): Boolean; virtual;
    function CheckTHandle(const Handle: Integer): Boolean; virtual;
    function Check(const Text: string): TError; overload; virtual;
    function Check(var Syntax: TSyntax; const Kind: TOperatorKind; const Text: string; const SA: TScriptArray; const Handle: Integer): TError; overload; virtual;
    function Check(const ItemArray: TTextItemArray; const Index: Integer; const Data: TItemData; const SA: TScriptArray; const Parameter: Boolean = False): TError; overload; virtual;
    function InternalCompile(const Text: string; var SA: TScriptArray; const Parameter: Boolean; var Idle: PFunction; out Error: TError): TScript; overload; virtual; abstract;
    function InternalCompile(const Text: string; var SA: TScriptArray; const Parameter: Boolean; var Idle: PFunction): TScript; overload; virtual; abstract;
    procedure InternalOptimize(const Index: Integer; out Script: TScript); virtual; abstract;
    function InternalDecompile(const Index: Integer; const ADelimiter: string; const AParameter: Boolean;
      const ABracket: TBracket; const ATypeMode: TRetrieveMode): string; virtual; abstract;
    property NotifyArray: TNotifyArray read FNotifyArray write FNotifyArray;
    property Method: TMethod read FMethod write FMethod;
    property TextData[const Text: string]: TTextData read GetTextData;
    property ExceptionMask: TFPUExceptionMask read FExceptionMask write FExceptionMask;
    property ConstantList: TFlexibleList read FConstantlist write FConstantList;
    property FInternal: PFunction read GetFInternal;
    property InternalHandle: Integer read FInternalHandle;
    property NegativeHandle: PInteger read FNegativeHandle;
    property PositiveHandle: PInteger read FPositiveHandle;
    property TOHArray: TIntegerDynArray read FTOHArray write FTOHArray;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function Connect(const ABeforeFunction: TFunctionEvent; const AAddressee: TCustomAddressee): TFunctionEvent; virtual;
    procedure Notify(const NotifyType: TNotifyType; const Sender: TComponent); overload; virtual;
    procedure BeginUpdate; virtual;
    procedure EndUpdate; virtual;
    function AddConstant(const AName: string; const Value: TValue): Boolean; overload; virtual;
    {$IFDEF DELPHI_2006}
    function AddConstant(const AName: string; const Value: Byte): Boolean; overload; virtual;
    function AddConstant(const AName: string; const Value: Shortint): Boolean; overload; virtual;
    function AddConstant(const AName: string; const Value: Word): Boolean; overload; virtual;
    function AddConstant(const AName: string; const Value: Smallint): Boolean; overload; virtual;
    function AddConstant(const AName: string; const Value: Longword): Boolean; overload; virtual;
    function AddConstant(const AName: string; const Value: Integer): Boolean; overload; virtual;
    {$ENDIF}
    function AddConstant(const AName: string; const Value: Int64): Boolean; overload; virtual;
    {$IFDEF DELPHI_2006}
    function AddConstant(const AName: string; const Value: Single): Boolean; overload; virtual;
    {$ENDIF}
    function AddConstant(const AName: string; const Value: Double): Boolean; overload; virtual;
    {$IFDEF DELPHI_2006}
    function AddConstant(const AName: string; const Value: Extended): Boolean; overload; virtual;
    {$ENDIF}
    function AddConstant(const AName: string; const Value: Boolean): Boolean; overload; virtual;
    function AddFunction(const AFunction: TFunction): Boolean; overload; virtual;
    function AddFunction(const AName: string; var Handle: Integer; const Kind: TFunctionKind;
      const Method: TFunctionMethod; const Optimizable: Boolean;
      const ReturnType: TValueType = vtUnknown; const Priority: TFunctionPriority = fpNormal;
      const Coverage: TPriorityCoverage = pcLocal): Boolean; overload; virtual;
    function AddVariable(const AName: string; var Variable: TValue; const Optimizable: Boolean;
      const ReturnType: TValueType = vtUnknown): Boolean; overload; virtual;
    function AddVariable(const AName: string; const Variable: TLiveValue; const Optimizable: Boolean;
      const ReturnType: TValueType = vtUnknown): Boolean; overload; virtual;
    function AddVariable(const AName: string; var Variable: TValue): Boolean; overload; virtual;
    function AddVariable(const AName: string; const Variable: TLiveValue): Boolean; overload; virtual;
    function AddVariable(const AName: string; var Variable: Byte): Boolean; overload; virtual;
    function AddVariable(const AName: string; var Variable: Shortint): Boolean; overload; virtual;
    function AddVariable(const AName: string; var Variable: Word): Boolean; overload; virtual;
    function AddVariable(const AName: string; var Variable: Smallint): Boolean; overload; virtual;
    function AddVariable(const AName: string; var Variable: Longword): Boolean; overload; virtual;
    function AddVariable(const AName: string; var Variable: Integer): Boolean; overload; virtual;
    function AddVariable(const AName: string; var Variable: Int64): Boolean; overload; virtual;
    function AddVariable(const AName: string; var Variable: Single): Boolean; overload; virtual;
    function AddVariable(const AName: string; var Variable: Double): Boolean; overload; virtual;
    function AddVariable(const AName: string; var Variable: Extended): Boolean; overload; virtual;
    function AddVariable(const AName: string; var Variable: Boolean): Boolean; overload; virtual;
    function DeleteConstant(const AName: string): Boolean; virtual;
    function DeleteFunction(const Handle: Integer): Boolean; overload; virtual;
    function DeleteFunction(const HandleArray: TIntegerDynArray): Integer; overload; virtual;
    function DeleteVariable(var Variable: TValue): Boolean; virtual;
    function FindConstant(const AName: string): PValue; overload; virtual;
    function FindConstant(const AName: string; out Value: PValue): Boolean; overload; virtual;
    function FindFunction(const AName: string): PFunction; overload; virtual;
    function GetFunction(const Handle: Integer; var AFunction: PFunction): Boolean; overload; virtual;
    function GetFunction(const Handle: Integer): PFunction; overload; virtual;
    function GetFunction(const Variable: PValue): PFunction; overload; virtual;
    function AddType(const AType: TType): Boolean; overload; virtual;
    function AddType(const AName: string; var Handle: Integer; const ValueType: TValueType): Boolean; overload; virtual;
    function DeleteType(const Handle: Integer): Boolean; overload; virtual;
    function DeleteType(const HandleArray: TIntegerDynArray): Integer; overload; virtual;
    function FindType(const AName: string): PType; overload; virtual;
    function GetType(const Handle: Integer; var AType: PType): Boolean; overload; virtual;
    function GetType(const Handle: Integer): PType; overload; virtual;
    procedure StringToScript(const Text: string; out Script: TScript; out Error: TError); overload; virtual; abstract;
    procedure StringToScript(const Text: string; out Script: TScript); overload; virtual; abstract;
    procedure Optimize(const Source: TScript; out Target: TScript); overload; virtual; abstract;
    procedure Optimize(var Script: TScript); overload; virtual; abstract;
    function ScriptToString(const Script: TScript; const TypeMode: TRetrieveMode = rmUser): string; overload; virtual; abstract;
    function ScriptToString(const Script: TScript; const Delimiter: string;
      const TypeMode: TRetrieveMode = rmUser): string; overload; virtual; abstract;
    function ScriptToString(const Script: TScript; const Delimiter: string; const ABracket: TBracket;
      const TypeMode: TRetrieveMode = rmUser): string; overload; virtual; abstract;
    function Execute(const Index: NativeInt): PValue; virtual; abstract;
    function ExecuteFunction(var Index: NativeInt; const ItemHeader: PItemHeader; const LValue: TValue;
      const Fake: Boolean = False): TValue; virtual; abstract;
    function AsValue(const Text: string): TValue; virtual;
    function AsByte(const Text: string): Byte; virtual;
    function AsShortint(const Text: string): Shortint; virtual;
    function AsWord(const Text: string): Word; virtual;
    function AsSmallint(const Text: string): Smallint; virtual;
    function AsLongword(const Text: string): Longword; virtual;
    function AsInteger(const Text: string): Integer; virtual;
    function AsInt64(const Text: string): Int64; virtual;
    function AsSingle(const Text: string): Single; virtual;
    function AsDouble(const Text: string): Double; virtual;
    function AsExtended(const Text: string): Extended; virtual;
    function AsBoolean(const Text: string): Boolean; virtual;
    function AsPointer(const Text: string): Pointer; virtual;
    function AsString(const Text: string): string; virtual;
    function DefaultFunction(const AFunction: PFunction; out Value: TValue; const LValue, RValue: TValue; const PA: TParameterArray): Boolean; virtual;
    procedure Prepare; virtual;
    property Addressee: TComponent read FAddressee write FAddressee;
    property IgnoreType[const ItemCode: TItemCode]: Boolean read GetIgnoreType write SetIgnoreType;
    property WindowHandle: THandle read FWindowHandle write FWindowHandle;
    property BeforeFunction: TFunctionEvent read FBeforeFunction write FBeforeFunction;
    property UpdateCount: Integer read FUpdateCount;
    property FData: PFunctionData read FFData write FFData;
    property TData: PTypeData read FTData write FTData;
    property DefaultTypeHandle: Integer read FDefaultTypeHandle write FDefaultTypeHandle;
    property PData: PFunctionData read FPData;
    property Bracket: TBracket read FBracket write FBracket;
    property ParameterBracket: TBracket read FParameterBracket write FParameterBracket;
    property VoidHandle: Integer read FVoidHandle;
    property NewHandle: Integer read FNewHandle;
    property DeleteHandle: Integer read FDeleteHandle;
    property FindHandle: Integer read FFindHandle;
    property GetHandle: Integer read FGetHandle;
    property SetHandle: Integer read FSetHandle;
    property ScriptHandle: Integer read FScriptHandle;
    property ForHandle: Integer read FForHandle;
    property RepeatHandle: Integer read FRepeatHandle;
    property WhileHandle: Integer read FWhileHandle;
    property StringHandle: Integer read FStringHandle;
  published
    property Prioritize: Boolean read FPrioritize write FPrioritize default True;
    property ExecuteOptions: TExecuteOptions read FExecuteOptions write FExecuteOptions default [eoSubsequent];
    property DefaultValueType: TValueType read FDefaultValueType write FDefaultValueType default vtInteger;
    property FalseValue: Integer read FFalseValue write FFalseValue default 0;
    property TrueValue: Integer read FTrueValue write FTrueValue default -1;
    property OnFunction: TFunctionEvent read FOnFunction write FOnFunction;
  end;

  TExceptionType = (etZeroDivide);
  TExceptionTypes = set of TExceptionType;

const
  DefaultExceptionTypes = [etZeroDivide];

type
  TParser = class(TCustomParser)
  private
    FIsZeroHandle: Integer;
    FConnector: TComponent;
    FShortintHandle: Integer;
    FExceptionTypes: TExceptionTypes;
    FSmallintHandle: Integer;
    FExtendedHandle: Integer;
    FEqualHandle: Integer;
    FStrToIntDefHandle: Integer;
    FMultiplyHandle: Integer;
    FNotEqualHandle: Integer;
    FInt64Handle: Integer;
    FBelowHandle: Integer;
    FAndHandle: Integer;
    FCache: TCache;
    FOrHandle: Integer;
    FSingleHandle: Integer;
    FLongwordHandle: Integer;
    FIfThenHandle: Integer;
    FAboveHandle: Integer;
    FShlHandle: Integer;
    FSetDecimalSeparatorHandle: Integer;
    FFalseHandle: Integer;
    FTrueHandle: Integer;
    FParseHandle: Integer;
    FParseManager: TObject;
    FIntegerHandle: Integer;
    FScript: TScript;
    FWordHandle: Integer;
    FByteHandle: Integer;
    FGetEpsilonHandle: Integer;
    FNotHandle: Integer;
    FSuccHandle: Integer;
    FDivideHandle: Integer;
    FCached: Boolean;
    FStrToFloatHandle: Integer;
    FDoubleHandle: Integer;
    FBelowOrEqualHandle: Integer;
    FEnsureRangeHandle: Integer;
    FText: string;
    FPredHandle: Integer;
    FSameValueHandle: Integer;
    FXorHandle: Integer;
    FEpsilonHandle: Integer;
    FIfHandle: Integer;
    FAboveOrEqualHandle: Integer;
    FStrToFloatDefHandle: Integer;
    FStrToIntHandle: Integer;
    FSetEpsilonHandle: Integer;
    FShrHandle: Integer;
  protected
    function ParseMethod(var Text: string; const FromIndex, TillIndex: Integer;
      const Data: Pointer): TError; virtual;
    function ExecuteMethod(var Index: NativeInt; const Header: PScriptHeader; const ItemHeader: PItemHeader;
      const Item: PScriptItem; const Data: Pointer): Boolean; virtual;
    function ScriptArrayMethod(var AIndex: NativeInt; const Header: PScriptHeader;
      const ItemHeader: PItemHeader; const Item: PScriptItem;
      const Data: Pointer): Boolean; virtual;
    function ParameterArrayMethod(var AIndex: NativeInt; const Header: PScriptHeader;
      const ItemHeader: PItemHeader; const Item: PScriptItem;
      const Data: Pointer): Boolean; virtual;
    function OptimizationMethod(var Index: NativeInt; const Header: PScriptHeader;
      const ItemHeader: PItemHeader; const Item: PScriptItem;
      const Data: Pointer): Boolean; virtual;
    function ParameterOptimizationMethod(var Index: NativeInt; const Header: PScriptHeader;
      const ItemHeader: PItemHeader; const Item: PScriptItem;
      const Data: Pointer): Boolean; virtual;
    function DecompileMethod(var Index: NativeInt; const Header: PScriptHeader;
      const ItemHeader: PItemHeader; const Item: PScriptItem;
      const Data: Pointer): Boolean; virtual;
    function Order(const Coverage: TPriorityCoverage; var ItemArray: TTextItemArray; var SA: TScriptArray; var Idle: PFunction): Boolean; virtual;
    function Morph(var AItemArray: TTextItemArray; out MItemArray: TTextItemMultiArray): Boolean;
    procedure Parse(var Text: string; var SA: TScriptArray; var Idle: PFunction; out Error: TError); overload; virtual;
    procedure Parse(var Text: string; var SA: TScriptArray; var Idle: PFunction); overload; virtual;
    function InternalCompile(const Text: string; var SA: TScriptArray; const Parameter: Boolean; var Idle: PFunction; out Error: TError): TScript; overload; override;
    function InternalCompile(const Text: string; var SA: TScriptArray; const Parameter: Boolean; var Idle: PFunction): TScript; overload; override;
    procedure ExecuteInternalScript(Index: Integer); virtual;
    procedure ReadParameterArray(var Index: NativeInt; out PA: TParameterArray; const ParameterType: TParameterType = ptParameter;
      const Fake: Boolean = False); override;
    procedure InternalOptimize(const Index: Integer; out Script: TScript); override;
    function OptimizeParameterArray(Index: Integer; out Script: TScript): Boolean; virtual;
    function Optimizable(var Index: NativeInt; const Number: Boolean; out Offset: Integer;
      out Parameter: Boolean; out Script: TScript): Boolean; virtual;
    function InternalDecompile(const Index: Integer; const Delimiter: string; const Parameter: Boolean;
      const ParameterBracket: TBracket; const TypeMode: TRetrieveMode): string; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function Connect(const ABeforeFunction: TFunctionEvent; const AAddressee: TCustomAddressee): TFunctionEvent; override;
    procedure StringToScript(const Text: string; out Script: TScript; out Error: TError); overload; override;
    procedure StringToScript(const Text: string; out Script: TScript); overload; override;
    procedure StringToScript(const AText: string); reintroduce; overload;
    procedure StringToScript; reintroduce; overload;
    procedure Optimize(const Source: TScript; out Target: TScript); overload; override;
    procedure Optimize(var Script: TScript); overload; override;
    procedure Optimize; reintroduce; overload;
    function ScriptToString(const Script: TScript; const TypeMode: TRetrieveMode = rmUser): string; override;
    function ScriptToString(const Script: TScript; const Delimiter: string; const TypeMode: TRetrieveMode = rmUser): string; override;
    function ScriptToString(const Script: TScript; const Delimiter: string; const ABracket: TBracket; const TypeMode: TRetrieveMode = rmUser): string; override;
    function ScriptToString(const TypeMode: TRetrieveMode = rmUser): string; reintroduce; overload;
    function Execute(const Index: NativeInt): PValue; overload; override;
    function Execute(const Script: TScript): PValue; reintroduce; overload;
    function Execute: PValue; reintroduce; overload;
    function ExecuteFunction(var Index: NativeInt; const ItemHeader: PItemHeader; const LValue: TValue; const Fake: Boolean = False): TValue; override;
    // Connector = Addressee
    property Connector: TComponent read FConnector write FConnector;
    property ParseManager: TObject read FParseManager write FParseManager;
    property Script: TScript read FScript write FScript;
    property MultiplyHandle: Integer read FMultiplyHandle;
    property DivideHandle: Integer read FDivideHandle;
    property SuccHandle: Integer read FSuccHandle;
    property PredHandle: Integer read FPredHandle;
    property NotHandle: Integer read FNotHandle;
    property AndHandle: Integer read FAndHandle;
    property OrHandle: Integer read FOrHandle;
    property XorHandle: Integer read FXorHandle;
    property ShlHandle: Integer read FShlHandle;
    property ShrHandle: Integer read FShrHandle;
    property SameValueHandle: Integer read FSameValueHandle;
    property IsZeroHandle: Integer read FIsZeroHandle;
    property IfHandle: Integer read FIfHandle;
    property IfThenHandle: Integer read FIfThenHandle;
    property EnsureRangeHandle: Integer read FEnsureRangeHandle;
    property StrToIntHandle: Integer read FStrToIntHandle;
    property StrToIntDefHandle: Integer read FStrToIntDefHandle;
    property StrToFloatHandle: Integer read FStrToFloatHandle;
    property StrToFloatDefHandle: Integer read FStrToFloatDefHandle;
    property ParseHandle: Integer read FParseHandle;
    property TrueHandle: Integer read FTrueHandle;
    property FalseHandle: Integer read FFalseHandle;
    property EqualHandle: Integer read FEqualHandle;
    property NotEqualHandle: Integer read FNotEqualHandle;
    property AboveHandle: Integer read FAboveHandle;
    property BelowHandle: Integer read FBelowHandle;
    property AboveOrEqualHandle: Integer read FAboveOrEqualHandle;
    property BelowOrEqualHandle: Integer read FBelowOrEqualHandle;
    property EpsilonHandle: Integer read FEpsilonHandle;
    property GetEpsilonHandle: Integer read FGetEpsilonHandle;
    property SetEpsilonHandle: Integer read FSetEpsilonHandle;
    property SetDecimalSeparatorHandle: Integer read FSetDecimalSeparatorHandle;
    property ShortintHandle: Integer read FShortintHandle;
    property ByteHandle: Integer read FByteHandle;
    property SmallintHandle: Integer read FSmallintHandle;
    property WordHandle: Integer read FWordHandle;
    property IntegerHandle: Integer read FIntegerHandle;
    property LongwordHandle: Integer read FLongwordHandle;
    property Int64Handle: Integer read FInt64Handle;
    property SingleHandle: Integer read FSingleHandle;
    property DoubleHandle: Integer read FDoubleHandle;
    property ExtendedHandle: Integer read FExtendedHandle;
  published
    property Cache: TCache read FCache;
    property Cached: Boolean read FCached write FCached default True;
    property Text: string read FText write FText;
    property ExceptionTypes: TExceptionTypes read FExceptionTypes write FExceptionTypes default DefaultExceptionTypes;
  end;

  TMathParser = class(TParser)
  private
    FCscHandle: Integer;
    FLnHandle: Integer;
    FDayHandle: Integer;
    FGetTickCountHandle: Integer;
    FMSecondHandle: Integer;
    FLog10Handle: Integer;
    FGradToRadHandle: Integer;
    FRoundHandle: Integer;
    FSqrHandle: Integer;
    FLdexpHandle: Integer;
    FRadToGradHandle: Integer;
    FCscHHandle: Integer;
    FAbsHandle: Integer;
    FCycleToRadHandle: Integer;
    FRadToCycleHandle: Integer;
    FCosHandle: Integer;
    FIntHandle: Integer;
    FMinIntValueHandle: Integer;
    FNormHandle: Integer;
    FCosHHandle: Integer;
    FGetDayOfWeekHandle: Integer;
    FRandomFromHandle: Integer;
    FArcSinHHandle: Integer;
    FArcSinHandle: Integer;
    FSqrtHandle: Integer;
    FFactorialHandle: Integer;
    FArcCosHHandle: Integer;
    FFloorHandle: Integer;
    FSecHHandle: Integer;
    FArcCosHandle: Integer;
    FTotalVarianceHandle: Integer;
    FSecHandle: Integer;
    FDayOfWeekHandle: Integer;
    FPopnStdDevHandle: Integer;
    FArcCscHandle: Integer;
    FLgHandle: Integer;
    FCycleToGradHandle: Integer;
    FGradToCycleHandle: Integer;
    FGetHourHandle: Integer;
    FDivHandle: Integer;
    FEncodeTimeHandle: Integer;
    FGetMinuteHandle: Integer;
    FArcTan2Handle: Integer;
    FTanHandle: Integer;
    FRoundToHandle: Integer;
    FMonthHandle: Integer;
    FLnXP1Handle: Integer;
    FEncodeDateTimeHandle: Integer;
    FHypotHandle: Integer;
    FMinHandle: Integer;
    FTanHHandle: Integer;
    FLog2Handle: Integer;
    FDegToGradHandle: Integer;
    FStdDevHandle: Integer;
    FArcSecHHandle: Integer;
    FExpHandle: Integer;
    FCoTanHHandle: Integer;
    FModHandle: Integer;
    FGetYearHandle: Integer;
    FGetSecondHandle: Integer;
    FHourHandle: Integer;
    FPopnVarianceHandle: Integer;
    FDegreeHandle: Integer;
    FRandomRangeHandle: Integer;
    FMaxIntValueHandle: Integer;
    FEncodeDateHandle: Integer;
    FMinuteHandle: Integer;
    FPolyHandle: Integer;
    FArcTanHHandle: Integer;
    FArcCscHHandle: Integer;
    FTimeHandle: Integer;
    FArcTanHandle: Integer;
    FFracHandle: Integer;
    FCoTanHandle: Integer;
    FMaxValueHandle: Integer;
    FArcSecHandle: Integer;
    FLogHandle: Integer;
    FDegToRadHandle: Integer;
    FRadToDegHandle: Integer;
    FRandomHandle: Integer;
    FVarianceHandle: Integer;
    FMaxHandle: Integer;
    FMinValueHandle: Integer;
    FIntPowerHandle: Integer;
    FArcCoTanHHandle: Integer;
    FYearHandle: Integer;
    FSecondHandle: Integer;
    FSumOfSquaresHandle: Integer;
    FGradToDegHandle: Integer;
    FMeanHandle: Integer;
    FGetMonthHandle: Integer;
    FDateHandle: Integer;
    FGetDayHandle: Integer;
    FCycleToDegHandle: Integer;
    FDegToCycleHandle: Integer;
    FSinHandle: Integer;
    FSumHandle: Integer;
    FRandGHandle: Integer;
    FCeilHandle: Integer;
    FSinHHandle: Integer;
    FGetMSecondHandle: Integer;
    FArcCoTanHandle: Integer;
    FSumIntHandle: Integer;
    FTruncHandle: Integer;
    FPowerHandle: Integer;
    FMathMethod: TMathMethod;
  protected
    property MathMethod: TMathMethod read FMathMethod write FMathMethod;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    property DivHandle: Integer read FDivHandle;
    property ModHandle: Integer read FModHandle;
    property DegreeHandle: Integer read FDegreeHandle;
    property FactorialHandle: Integer read FFactorialHandle;
    property SqrHandle: Integer read FSqrHandle;
    property SqrtHandle: Integer read FSqrtHandle;
    property IntHandle: Integer read FIntHandle;
    property RoundHandle: Integer read FRoundHandle;
    property RoundToHandle: Integer read FRoundToHandle;
    property TruncHandle: Integer read FTruncHandle;
    property AbsHandle: Integer read FAbsHandle;
    property FracHandle: Integer read FFracHandle;
    property LnHandle: Integer read FLnHandle;
    property LgHandle: Integer read FLgHandle;
    property LogHandle: Integer read FLogHandle;
    property ExpHandle: Integer read FExpHandle;
    property RandomHandle: Integer read FRandomHandle;
    property SinHandle: Integer read FSinHandle;
    property ArcSinHandle: Integer read FArcSinHandle;
    property SinHHandle: Integer read FSinHHandle;
    property ArcSinHHandle: Integer read FArcSinHHandle;
    property CosHandle: Integer read FCosHandle;
    property ArcCosHandle: Integer read FArcCosHandle;
    property CosHHandle: Integer read FCosHHandle;
    property ArcCosHHandle: Integer read FArcCosHHandle;
    property TanHandle: Integer read FTanHandle;
    property ArcTanHandle: Integer read FArcTanHandle;
    property TanHHandle: Integer read FTanHHandle;
    property ArcTanHHandle: Integer read FArcTanHHandle;
    property CoTanHandle: Integer read FCoTanHandle;
    property ArcCoTanHandle: Integer read FArcCoTanHandle;
    property CoTanHHandle: Integer read FCoTanHHandle;
    property ArcCoTanHHandle: Integer read FArcCoTanHHandle;
    property SecHandle: Integer read FSecHandle;
    property ArcSecHandle: Integer read FArcSecHandle;
    property SecHHandle: Integer read FSecHHandle;
    property ArcSecHHandle: Integer read FArcSecHHandle;
    property CscHandle: Integer read FCscHandle;
    property ArcCscHandle: Integer read FArcCscHandle;
    property CscHHandle: Integer read FCscHHandle;
    property ArcCscHHandle: Integer read FArcCscHHandle;
    property ArcTan2Handle: Integer read FArcTan2Handle;
    property HypotHandle: Integer read FHypotHandle;
    property RadToDegHandle: Integer read FRadToDegHandle;
    property RadToGradHandle: Integer read FRadToGradHandle;
    property RadToCycleHandle: Integer read FRadToCycleHandle;
    property DegToRadHandle: Integer read FDegToRadHandle;
    property DegToGradHandle: Integer read FDegToGradHandle;
    property DegToCycleHandle: Integer read FDegToCycleHandle;
    property GradToRadHandle: Integer read FGradToRadHandle;
    property GradToDegHandle: Integer read FGradToDegHandle;
    property GradToCycleHandle: Integer read FGradToCycleHandle;
    property CycleToRadHandle: Integer read FCycleToRadHandle;
    property CycleToDegHandle: Integer read FCycleToDegHandle;
    property CycleToGradHandle: Integer read FCycleToGradHandle;
    property LnXP1Handle: Integer read FLnXP1Handle;
    property Log10Handle: Integer read FLog10Handle;
    property Log2Handle: Integer read FLog2Handle;
    property IntPowerHandle: Integer read FIntPowerHandle;
    property PowerHandle: Integer read FPowerHandle;
    property LdexpHandle: Integer read FLdexpHandle;
    property CeilHandle: Integer read FCeilHandle;
    property FloorHandle: Integer read FFloorHandle;
    property PolyHandle: Integer read FPolyHandle;
    property MeanHandle: Integer read FMeanHandle;
    property SumHandle: Integer read FSumHandle;
    property SumIntHandle: Integer read FSumIntHandle;
    property SumOfSquaresHandle: Integer read FSumOfSquaresHandle;
    property MinValueHandle: Integer read FMinValueHandle;
    property MinIntValueHandle: Integer read FMinIntValueHandle;
    property MinHandle: Integer read FMinHandle;
    property MaxValueHandle: Integer read FMaxValueHandle;
    property MaxIntValueHandle: Integer read FMaxIntValueHandle;
    property MaxHandle: Integer read FMaxHandle;
    property StdDevHandle: Integer read FStdDevHandle;
    property PopnStdDevHandle: Integer read FPopnStdDevHandle;
    property VarianceHandle: Integer read FVarianceHandle;
    property PopnVarianceHandle: Integer read FPopnVarianceHandle;
    property TotalVarianceHandle: Integer read FTotalVarianceHandle;
    property NormHandle: Integer read FNormHandle;
    property RandGHandle: Integer read FRandGHandle;
    property RandomRangeHandle: Integer read FRandomRangeHandle;
    property RandomFromHandle: Integer read FRandomFromHandle;
    property YearHandle: Integer read FYearHandle;
    property MonthHandle: Integer read FMonthHandle;
    property DayHandle: Integer read FDayHandle;
    property DayOfWeekHandle: Integer read FDayOfWeekHandle;
    property HourHandle: Integer read FHourHandle;
    property MinuteHandle: Integer read FMinuteHandle;
    property SecondHandle: Integer read FSecondHandle;
    property MSecondHandle: Integer read FMSecondHandle;
    property TimeHandle: Integer read FTimeHandle;
    property DateHandle: Integer read FDateHandle;
    property GetYearHandle: Integer read FGetYearHandle;
    property GetMonthHandle: Integer read FGetMonthHandle;
    property GetDayHandle: Integer read FGetDayHandle;
    property GetDayOfWeekHandle: Integer read FGetDayOfWeekHandle;
    property GetHourHandle: Integer read FGetHourHandle;
    property GetMinuteHandle: Integer read FGetMinuteHandle;
    property GetSecondHandle: Integer read FGetSecondHandle;
    property GetMSecondHandle: Integer read FGetMSecondHandle;
    property EncodeTimeHandle: Integer read FEncodeTimeHandle;
    property EncodeDateHandle: Integer read FEncodeDateHandle;
    property EncodeDateTimeHandle: Integer read FEncodeDateTimeHandle;
    property GetTickCountHandle: Integer read FGetTickCountHandle;
  end;

const
  InternalRawscriptCacheName = 'RawscriptCache';
  InternalScriptCacheName = 'ScriptCache';
  InternalSubscriptCacheName = 'SubscriptCache';
  InternalParameterCacheName = 'ParameterCache';
  InternalSubparameterCacheName = 'SubparameterCache';
  InternalCacheName = 'Cache';
  InternalFlagCacheName = 'FlagCache';
  InternalPOperatorFlagCacheName = 'POperatorFlagCache';
  InternalItemCacheName = 'ItemCache';
  InternalPOperatorItemCacheName = 'POperatorItemCache';
  InternalConnectorName = 'Connector';
  InternalParseManagerName = 'ParseManager';

function Put(var NotifyArray: TNotifyArray; const Notify: TNotify): Integer;

function MakeParameterArrayData(const AFake: Boolean; const AParameterArray: PParameterArray; const AIndex: PInteger): TParameterArrayData;
function MakeDecompileData(const ADelimiter: string; const AParameter: Boolean; const AParameterBracket: TBracket; const ATypeMode: TRetrieveMode): TDecompileData;
function MakeBracketData(const ASA: PScriptArray; const AParameter: Boolean; const AIdle: PFunction): TBracketData;
function MakeNotify(const ANotifyType: TNotifyType; const AComponent: TComponent): TNotify;

procedure Register;

implementation

uses
  Connector, Dialogs, FlagCache, ItemCache, License, MemoryUtils, NumberConsts, NumberUtils,
  ParseCommon, ParseManager, ParseMessages, ParseUtils, ParseValidator, {$IFDEF DELPHI_XE3}System.UITypes, {$ENDIF}
  TextUtils, ThreadUtils, ValueConsts, ValueUtils, Variants;

procedure Register;
begin
  RegisterComponents('Samples', [TParser, TMathParser]);
end;

var
  WasteCount: Integer = 0;

function Put(var NotifyArray: TNotifyArray; const Notify: TNotify): Integer;
var
  I: Integer;
begin
  for I := Low(NotifyArray) to High(NotifyArray) do
    if NotifyArray[I].NotifyType = Notify.NotifyType then
    begin
      Result := I;
      NotifyArray[Result] := Notify;
      Exit;
    end;
  Result := Length(NotifyArray);
  SetLength(NotifyArray, Result + 1);
  NotifyArray[Result] := Notify;
end;

function MakeParameterArrayData(const AFake: Boolean; const AParameterArray: PParameterArray; const AIndex: PInteger): TParameterArrayData;
begin
  FillChar(Result, SizeOf(TParameterArrayData), 0);
  with Result do
  begin
    Fake := AFake;
    PA := AParameterArray;
    Index := AIndex;
  end;
end;

function MakeDecompileData(const ADelimiter: string; const AParameter: Boolean; const AParameterBracket: TBracket; const ATypeMode: TRetrieveMode): TDecompileData;
begin
  FillChar(Result, SizeOf(TDecompileData), 0);
  with Result do
  begin
    Delimiter := ADelimiter;
    Parameter := AParameter;
    ParameterBracket := AParameterBracket;
    TypeMode := ATypeMode;
  end;
end;

function MakeBracketData(const ASA: PScriptArray; const AParameter: Boolean; const AIdle: PFunction): TBracketData;
begin
  FillChar(Result, SizeOf(TBracketData), 0);
  with Result do
  begin
    SA := ASA;
    Parameter := AParameter;
    Idle := AIdle;
  end;
end;

function MakeNotify(const ANotifyType: TNotifyType; const AComponent: TComponent): TNotify;
begin
  FillChar(Result, SizeOf(TNotify), 0);
  with Result do
  begin
    NotifyType := ANotifyType;
    Component := AComponent;
  end;
end;

{ TCache }

procedure TCache.Clear;
var
  I: TCacheType;
begin
  for I := Low(TCacheType) to High(TCacheType) do
    if Available(FCacheArray[I]) then FCacheArray[I].Clear;
end;

constructor TCache.Create(const AParser: TCustomParser; const AConnector: TComponent);
begin
  inherited Create(AParser);
  FParser := AParser;
  FRawscript := TParseCache.Create(AParser);
  with FRawscript do
  begin
    LiteCache.Connector := AConnector;
    HardCache.Connector := AConnector;
    Name := InternalRawscriptCacheName;
    ScriptType := stScript;
    SetSubComponent(True);
  end;
  FScript := TParseCache.Create(AParser);
  with FScript do
  begin
    LiteCache.Connector := AConnector;
    HardCache.Connector := AConnector;
    Name := InternalScriptCacheName;
    ScriptType := stScript;
    SetSubComponent(True);
  end;
  FSubscript := TParseCache.Create(AParser);
  with FSubscript do
  begin
    LiteCache.Connector := AConnector;
    HardCache.Connector := AConnector;
    Name := InternalSubscriptCacheName;
    ScriptType := stScriptItem;
    HardCache.Enabled := False;
    SetSubComponent(True);
  end;
  FParameter := TParseCache.Create(AParser);
  with FParameter do
  begin
    LiteCache.Connector := AConnector;
    HardCache.Connector := AConnector;
    Name := InternalParameterCacheName;
    ScriptType := stScript;
    SetSubComponent(True);
  end;
  FSubparameter := TParseCache.Create(AParser);
  with FSubparameter do
  begin
    LiteCache.Connector := AConnector;
    HardCache.Connector := AConnector;
    Name := InternalSubparameterCacheName;
    ScriptType := stScriptItem;
    HardCache.Enabled := False;
    SetSubComponent(True);
  end;
  FCacheArray[ctRawscript] := FRawscript;
  FCacheArray[ctScript] := FScript;
  FCacheArray[ctSubscript] := FSubscript;
  FCacheArray[ctParameter] := FParameter;
  FCacheArray[ctSubparameter] := FSubparameter;
  {$IFDEF DELPHI_7}
  FNameValueSeparator := CacheSeparator;
  WriteNameValueSeparator;
  {$ENDIF}
end;

function TCache.GetCache(CacheType: TCacheType): TParseCache;
begin
  Result := FCacheArray[CacheType];
end;

procedure TCache.Prepare;
var
  I: TCacheType;
begin
  if Assigned(FParser) then FParser.BeginUpdate;
  try
    for I := Low(TCacheType) to High(TCacheType) do
    begin
      FCacheArray[I].LiteCache.Compile;
      FCacheArray[I].HardCache.Compile;
    end;
  finally
    if Assigned(FParser) then FParser.EndUpdate;
  end;
end;

procedure TCache.SetCacheType(const Value: TListType);
var
  I: TCacheType;
begin
  for I := Low(TCacheType) to High(TCacheType) do
  begin
    FCacheArray[I].LiteCache.CacheType := Value;
    FCacheArray[I].HardCache.CacheType := Value;
  end;
end;

{$IFDEF DELPHI_7}
procedure TCache.SetNameValueSeparator(const Value: Char);
begin
  if FNameValueSeparator <> Value then
  begin
    FNameValueSeparator := Value;
    WriteNameValueSeparator;
  end;
end;

procedure TCache.WriteNameValueSeparator;
var
  I: TCacheType;
begin
  for I := Low(TCacheType) to High(TCacheType) do
  begin
    FCacheArray[I].LiteCache.NameValueSeparator := FNameValueSeparator;
    FCacheArray[I].HardCache.NameValueSeparator := FNameValueSeparator;
  end;
end;
{$ENDIF}

{ TCustomParser }

function TCustomParser.AddConstant(const AName: string; const Value: TValue): Boolean;
var
  Item: PValue;
begin
  Result := not Assigned(FindFunction(AName));
  if Result then
  begin
    New(Item);
    try
      Item^ := Value;
      Result := AddVariable(AName, Item^, True);
      if Result then
        FConstantList.List.AddObject(AName, Pointer(Item))
      else
        Dispose(Item);
    except
      Dispose(Item);
      raise;
    end;
  end;
end;

{$IFDEF DELPHI_2006}
function TCustomParser.AddConstant(const AName: string; const Value: Byte): Boolean;
begin
  Result := AddConstant(AName, MakeValue(Value));
end;

function TCustomParser.AddConstant(const AName: string; const Value: Shortint): Boolean;
begin
  Result := AddConstant(AName, MakeValue(Value));
end;

function TCustomParser.AddConstant(const AName: string; const Value: Word): Boolean;
begin
  Result := AddConstant(AName, MakeValue(Value));
end;

function TCustomParser.AddConstant(const AName: string; const Value: Smallint): Boolean;
begin
  Result := AddConstant(AName, MakeValue(Value));
end;

function TCustomParser.AddConstant(const AName: string; const Value: Longword): Boolean;
begin
  Result := AddConstant(AName, MakeValue(Value));
end;

function TCustomParser.AddConstant(const AName: string; const Value: Integer): Boolean;
begin
  Result := AddConstant(AName, MakeValue(Value));
end;
{$ENDIF}

function TCustomParser.AddConstant(const AName: string; const Value: Int64): Boolean;
begin
  Result := AddConstant(AName, MakeValue(Value));
end;

{$IFDEF DELPHI_2006}
function TCustomParser.AddConstant(const AName: string; const Value: Single): Boolean;
begin
  Result := AddConstant(AName, MakeValue(Value));
end;
{$ENDIF}

function TCustomParser.AddConstant(const AName: string; const Value: Double): Boolean;
begin
  Result := AddConstant(AName, MakeValue(Value));
end;

{$IFDEF DELPHI_2006}
function TCustomParser.AddConstant(const AName: string; const Value: Extended): Boolean;
begin
  Result := AddConstant(AName, MakeValue(Value));
end;
{$ENDIF}

function TCustomParser.AddConstant(const AName: string; const Value: Boolean): Boolean;
begin
  Result := AddConstant(AName, MakeValue(Byte(Value)));
end;

function TCustomParser.AddFunction(const AFunction: TFunction): Boolean;
var
  LicenseCount: Integer;
begin
  LicenseCount := GetLicenseCount;
  Result := (LicenseCount < 0) or (WasteCount < LicenseCount);
  if Result then
  begin
    Result := InternalAddFunction(AFunction);
    if Result and (LicenseCount > 0) then Inc(WasteCount);
  end
  else MessageDlg(LicenseError, mtError, [mbOk], 0);
end;

function TCustomParser.AddFunction(const AName: string; var Handle: Integer;
  const Kind: TFunctionKind; const Method: TFunctionMethod; const Optimizable: Boolean;
  const ReturnType: TValueType; const Priority: TFunctionPriority;
  const Coverage: TPriorityCoverage): Boolean;
begin
  Result := AddFunction(MakeFunction(AName, Handle, ReturnType, Kind, Method, Optimizable, MakePriority(Priority, Coverage)));
end;

function TCustomParser.AddType(const AType: TType): Boolean;
var
  Error: TError;
begin
  Result := (AType.Name <> '') and not Assigned(FindType(AType.Name));
  if Result then
  begin
    Notify(ntBTA, Self);
    Error := Validator.Check(AType.Name, rtName);
    if Error.ErrorType <> etSuccess then raise ParseErrors.Error(Error.ErrorText);
    AType.Handle^ := Add(FTData^, AType);
    Notify(ntATA, Self);
  end
  else AType.Handle^ := -1;
end;

function TCustomParser.AddType(const AName: string; var Handle: Integer; const ValueType: TValueType): Boolean;
begin
  Result := AddType(MakeType(AName, Handle, ValueType));
end;

function TCustomParser.AddVariable(const AName: string; var Variable: TValue; const Optimizable: Boolean;
  const ReturnType: TValueType): Boolean;
begin
  Result := InternalAddVariable(AName, Variable, Optimizable, ReturnType);
end;

function TCustomParser.AddVariable(const AName: string; const Variable: TLiveValue; const Optimizable: Boolean;
  const ReturnType: TValueType): Boolean;
begin
  Result := InternalAddVariable(AName, Variable, Optimizable, ReturnType);
end;

function TCustomParser.AddVariable(const AName: string; var Variable: TValue): Boolean;
begin
  Result := AddVariable(AName, Variable, False);
end;

function TCustomParser.AddVariable(const AName: string; const Variable: TLiveValue): Boolean;
begin
  Result := AddVariable(AName, Variable, False);
end;

function TCustomParser.AddVariable(const AName: string; var Variable: Byte): Boolean;
begin
  Result := AddVariable(AName, MakeLiveValue(Variable));
end;

function TCustomParser.AddVariable(const AName: string; var Variable: Shortint): Boolean;
begin
  Result := AddVariable(AName, MakeLiveValue(Variable));
end;

function TCustomParser.AddVariable(const AName: string; var Variable: Word): Boolean;
begin
  Result := AddVariable(AName, MakeLiveValue(Variable));
end;

function TCustomParser.AddVariable(const AName: string; var Variable: Smallint): Boolean;
begin
  Result := AddVariable(AName, MakeLiveValue(Variable));
end;

function TCustomParser.AddVariable(const AName: string; var Variable: Longword): Boolean;
begin
  Result := AddVariable(AName, MakeLiveValue(Variable));
end;

function TCustomParser.AddVariable(const AName: string; var Variable: Integer): Boolean;
begin
  Result := AddVariable(AName, MakeLiveValue(Variable));
end;

function TCustomParser.AddVariable(const AName: string; var Variable: Int64): Boolean;
begin
  Result := AddVariable(AName, MakeLiveValue(Variable));
end;

function TCustomParser.AddVariable(const AName: string; var Variable: Single): Boolean;
begin
  Result := AddVariable(AName, MakeLiveValue(Variable));
end;

function TCustomParser.AddVariable(const AName: string; var Variable: Double): Boolean;
begin
  Result := AddVariable(AName, MakeLiveValue(Variable));
end;

function TCustomParser.AddVariable(const AName: string; var Variable: Extended): Boolean;
begin
  Result := AddVariable(AName, MakeLiveValue(Variable));
end;

function TCustomParser.AddVariable(const AName: string; var Variable: Boolean): Boolean;
begin
  Result := AddVariable(AName, MakeLiveValue(PByte(@Variable)^));
end;

function TCustomParser.AsBoolean(const Text: string): Boolean;
begin
  Result := Boolean(AsByte(Text));
end;

function TCustomParser.AsByte(const Text: string): Byte;
begin
  Result := Convert(AsValue(Text), vtByte).Unsigned8;
end;

function TCustomParser.AsDouble(const Text: string): Double;
begin
  Result := Convert(AsValue(Text), vtDouble).Float64;
end;

function TCustomParser.AsExtended(const Text: string): Extended;
begin
  Result := Convert(AsValue(Text), vtExtended).Float80;
end;

function TCustomParser.AsInt64(const Text: string): Int64;
begin
  Result := Convert(AsValue(Text), vtInt64).Signed64;
end;

function TCustomParser.AsInteger(const Text: string): Integer;
begin
  Result := Convert(AsValue(Text), vtInteger).Signed32;
end;

function TCustomParser.AsLongword(const Text: string): Longword;
begin
  Result := Convert(AsValue(Text), vtLongword).Unsigned32;
end;

function TCustomParser.AsPointer(const Text: string): Pointer;
begin
  Result := Pointer(AsInt64(Text));
end;

function TCustomParser.AsShortint(const Text: string): Shortint;
begin
  Result := Convert(AsValue(Text), vtShortint).Signed8;
end;

function TCustomParser.AsSingle(const Text: string): Single;
begin
  Result := Convert(AsValue(Text), vtSingle).Float32;
end;

function TCustomParser.AsSmallint(const Text: string): Smallint;
begin
  Result := Convert(AsValue(Text), vtSmallint).Signed16;
end;

function TCustomParser.AsString(const Text: string): string;
begin
  Result := ValueToText(AsValue(Text));
end;

function TCustomParser.AsValue(const Text: string): TValue;
var
  Script: TScript;
begin
  StringToScript(Text, Script);
  try
    Result := Execute(Integer(Script))^;
  finally
    Script := nil;
  end;
end;

function TCustomParser.AsWord(const Text: string): Word;
begin
  Result := Convert(AsValue(Text), vtWord).Unsigned16;
end;

procedure TCustomParser.BeginUpdate;
begin
  InterlockedIncrement(FUpdateCount);
end;

function TCustomParser.Check(const Text: string): TError;
var
  FlagArray: TFunctionFlagArray;
  I, J: Integer;
  AFunction: PFunction;
begin
  GetFlagArray(Text, ltChar, FFData, nil, FlagArray);
  try
    for I := Low(FlagArray) to High(FlagArray) do
      if GetFunction(FlagArray[I].Handle, AFunction) then
        for J := FlagArray[I].Index + Integer(StrLen(AFunction.Name)) to Length(Text) do
          if not CharInSet(Text[J], Blanks) then
            if Text[J] = FBracket[btLeft] then
              if not AFunction.Method.Parameter.RParameter and (AFunction.Method.Parameter.Count = 0) then
              begin
                Result := MakeError(etRTextExcessError, EText(RTextExcessError, [AFunction.Name]));
                Exit;
              end
              else Break
            else
              if AFunction.Method.Parameter.Count > 0 then
              begin
                Result := MakeError(etAParameterExpectError, EText(AParameterExpectError, [AFunction.Name]));
                Exit;
              end
              else Break;
  finally
    FlagArray := nil;
  end;
  FillChar(Result, SizeOf(TError), 0);
end;

{$WARNINGS OFF}
function TCustomParser.Check(const ItemArray: TTextItemArray; const Index: Integer; const Data: TItemData; const SA: TScriptArray; const Parameter: Boolean): TError;
var
  AFlag, BFlag: Boolean;
  AType: PType;
  AFunction: PFunction;
  Script: TScript;
begin
  if GetType(ItemArray[Index].THandle, AType) then
    case Data.Code of
      icNumber:
        begin
          if AType.Handle^ = FStringHandle then
          begin
            Result := MakeError(etStringTypeError, EText(StringTypeError, [ValueToText(Data.Number^)]));
            Exit;
          end;
          if Index > Low(ItemArray) then
          begin
            Result := MakeError(etNumberTypeError, EText(NumberTypeError, [AType.Name, ValueToText(Data.Number^), RestoreText(Self, ItemArray, SA, Parameter)]));
            Exit;
          end;
        end;
      icFunction:
        begin
          AFlag := AType.Handle^ = FStringHandle;
          BFlag := Index > Low(ItemArray);
          if (AFlag or BFlag) and GetFunction(Data.Handle^, AFunction) then
          begin
            if AFlag then
            begin
              Result := MakeError(etStringTypeError, EText(StringTypeError, [AFunction.Name]));
              Exit;
            end;
            if BFlag then
            begin
              Result := MakeError(etFunctionTypeError, EText(FunctionTypeError, [AType.Name, AFunction.Name, RestoreText(Self, ItemArray, SA, Parameter)]));
              Exit;
            end;
          end;
        end;
      icScript:
        begin
          AFlag := AType.Handle^ = FStringHandle;
          BFlag := Index > Low(ItemArray);
          if (AFlag or BFlag) and GetFunction(Data.Handle^, AFunction) then
          begin
            if AFlag then
            begin
              Result := MakeError(etStringTypeError, EText(StringTypeError, [ScriptToString(Data.Script^, rmUser)]));
              Exit;
            end;
            if BFlag then
            begin
              Result := MakeError(etScriptTypeError, EText(ScriptTypeError, [AType.Name, Embrace(ScriptToString(Data.Script^, rmUser), FBracket), RestoreText(Self, ItemArray, SA, Parameter)]));
              Exit;
            end;
          end;
        end;
      icParameter:
        if AType.Handle^ = FStringHandle then
        begin
          Script := ParameterToScript(Data.Script^);
          try
            Result := MakeError(etStringTypeError, EText(StringTypeError, [ScriptToString(Script, rmUser)]));
            Exit;
          finally
            Script := nil;
          end;
        end;
    end;
  FillChar(Result, SizeOf(TError), 0);
end;

function TCustomParser.Check(var Syntax: TSyntax; const Kind: TOperatorKind; const Text: string;
  const SA: TScriptArray; const Handle: Integer): TError;
var
  AFunction, PriorFunction: PFunction;
  Parameter, PriorParameter: PFunctionParameter;
begin
  if GetFunction(Handle, AFunction) then
    Parameter := @AFunction.Method.Parameter
  else
    Parameter := nil;
  if GetFunction(Syntax.PriorHandle, PriorFunction) then
    PriorParameter := @PriorFunction.Method.Parameter
  else
    PriorParameter := nil;
  try
    case Kind of
      okLatest:
        case Syntax.PriorKind of
          okFunction:
            if Assigned(PriorFunction) and Assigned(PriorParameter) then
            begin
              if PriorParameter.Count > 0 then
              begin
                Result := MakeError(etAParameterExpectError, EText(RestoreText(Self, Text, SA), AParameterExpectError, [PriorFunction.Name]));
                Exit;
              end;
              if PriorParameter.RParameter then
              begin
                Result := MakeError(etRTextExpectError, EText(RestoreText(Self, Text, SA), RTextExpectError, [PriorFunction.Name]));
                Exit;
              end;
            end;
        end;
      okNumber, okScript:
        case Syntax.PriorKind of
          okNumber, okScript, okParameter:
            begin
              Result := MakeError(etFunctionExpectError, EText(FunctionExpectError, [RestoreText(Self, Text, SA)]));
              Exit;
            end;
          okFunction:
            if Assigned(PriorFunction) and Assigned(PriorParameter) and ((PriorParameter.Count > 0) or not PriorParameter.RParameter) then
            begin
              Result := MakeError(etRTextExcessError, EText(RestoreText(Self, Text, SA), RTextExcessError, [PriorFunction.Name]));
              Exit;
            end;
        end;
      okFunction:
        begin
          case Syntax.PriorKind of
            okNewest:
              if Assigned(AFunction) and Assigned(Parameter) and Parameter.LParameter then
              begin
                Result := MakeError(etLTextExpectError, EText(LTextExpectError, [RestoreText(Self, Text, SA)]));
                Exit;
              end;
            okNumber, okScript, okParameter:
              if Assigned(AFunction) and Assigned(Parameter) and ((Parameter.Count > 0) or not Parameter.LParameter) then
              begin
                Result := MakeError(etLTextExcessError, EText(RestoreText(Self, Text, SA), LTextExcessError, [AFunction.Name]));
                Exit;
              end;
            okFunction:
              if Assigned(AFunction) and Assigned(PriorFunction) and Assigned(Parameter) and Assigned(PriorParameter) then
              begin
                if PriorParameter.RParameter and Parameter.LParameter then
                begin
                  Result := MakeError(etAMutualExcessError, EText(RestoreText(Self, Text, SA), AMutualExcessError, [PriorFunction.Name, AFunction.Name]));
                  Exit;
                end;
                if not PriorParameter.RParameter and not Parameter.LParameter then
                begin
                  Result := MakeError(etBMutualExcessError, EText(RestoreText(Self, Text, SA), BMutualExcessError, [PriorFunction.Name, AFunction.Name]));
                  Exit;
                end;
              end;
          end;
          Syntax.PriorHandle := Handle;
        end;
      okParameter:
        case Syntax.PriorKind of
          okNewest, okNumber, okScript, okParameter:
            begin
              Result := MakeError(etFunctionExpectError, EText(FunctionExpectError, [RestoreText(Self, Text, SA)]));
              Exit;
            end;
          okFunction:
            if Assigned(PriorFunction) and (PriorParameter.Count = 0) then
            begin
              Result := MakeError(etAParameterExcessError, EText(RestoreText(Self, Text, SA), AParameterExcessError, [PriorFunction.Name]));
              Exit;
            end;
        end;
    end;
  finally
    Syntax.PriorKind := Kind;
  end;
  FillChar(Result, SizeOf(TError), 0);
end;

function TCustomParser.CheckFHandle(const Handle: Integer): Boolean;
begin
  Result := ParseUtils.CheckFHandle(FFData, Handle);
end;

function TCustomParser.CheckTHandle(const Handle: Integer): Boolean;
begin
  Result := ParseUtils.CheckTHandle(FTData, Handle);
end;
{$WARNINGS ON}

function TCustomParser.Connect(const ABeforeFunction: TFunctionEvent; const AAddressee: TCustomAddressee): TFunctionEvent;
begin
  Result := FBeforeFunction;
  FBeforeFunction := ABeforeFunction;
  FAddressee := AAddressee;
end;

constructor TCustomParser.Create(AOwner: TComponent);

  function MakeData(const Value: array of string): PFunctionData;
  var
    AFunction: TFunction;
    I: Integer;
  begin
    New(Result);
    ZeroMemory(Result, SizeOf(TFunctionData));
    FillChar(AFunction, SizeOf(TFunction), 0);
    for I := Low(Value) to High(Value) do
    begin
      StrLCopy(AFunction.Name, PChar(Value[I]), SizeOf(TString) - 1);
      Add(Result^, AFunction);
    end;
    MemoryUtils.Add(Result.FOrder, 0, Length(Result.FArray));
  end;

var
  I: TTextOperator;
begin
  inherited;
  FMethod := TMethod.Create(Self);
  FWindowHandle := AllocateHWnd(WindowMethod);
  New(FFData);
  ZeroMemory(FFData, SizeOf(TFunctionData));
  New(FTData);
  ZeroMemory(FTData, SizeOf(TTypeData));
  FPData := MakeData(POperatorArray);
  FExceptionMask := GetExceptionMask;
  SetExceptionMask([exInvalidOp, exDenormalized, exZeroDivide, exOverflow, exUnderflow, exPrecision]);
  FConstantList := TFlexibleList.Create(Self);
  BeginUpdate;
  try
    InternalAddFunction(InternalFunctionName, FInternalHandle, fkHandle, MakeFunctionMethod(False, False, 0), False);
    SetLength(FTOHArray, Length(TOperatorArray));
    for I := Low(TOperatorArray) to High(TOperatorArray) do
      InternalAddFunction(TOperatorArray[I], FTOHArray[Ord(I)], fkHandle, MakeFunctionMethod(True, True, 0), False);
    FNegativeHandle := @FTOHArray[Integer(toNegative)];
    FPositiveHandle := @FTOHArray[Integer(toPositive)];
    InternalAddFunction(VoidFunctionName, FVoidHandle, fkMethod, MakeFunctionMethod(FMethod.VoidMethod), True);
    InternalAddFunction(NewFunctionName, FNewHandle, fkMethod, MakeFunctionMethod(FMethod.NewMethod, NewMinParameterCount, pkReference), False);
    InternalAddFunction(DeleteFunctionName, FDeleteHandle, fkMethod, MakeFunctionMethod(FMethod.DeleteMethod, DeleteParameterCount, pkReference), False);
    InternalAddFunction(FindFunctionName, FFindHandle, fkMethod, MakeFunctionMethod(FMethod.FindMethod, FindParameterCount, pkReference), False);
    InternalAddFunction(GetFunctionName, FGetHandle, fkMethod, MakeFunctionMethod(FMethod.GetMethod, GetParameterCount, pkReference), False);
    InternalAddFunction(SetFunctionName, FSetHandle, fkMethod, MakeFunctionMethod(FMethod.SetMethod, SetParameterCount, pkReference), False);
    InternalAddFunction(ScriptFunctionName, FScriptHandle, fkMethod, MakeFunctionMethod(FMethod.ScriptMethod, ScriptMinParameterCount, pkReference), False);
    InternalAddFunction(ForFunctionName, FForHandle, fkMethod, MakeFunctionMethod(FMethod.ForMethod, ForParameterCount, pkReference), False);
    InternalAddFunction(RepeatFunctionName, FRepeatHandle, fkMethod, MakeFunctionMethod(FMethod.RepeatMethod, RepeatParameterCount, pkReference), False);
    InternalAddFunction(WhileFunctionName, FWhileHandle, fkMethod, MakeFunctionMethod(FMethod.WhileMethod, WhileParameterCount, pkReference), False);
    AddType(StringTypeName, FStringHandle, vtUnknown);
  finally
    EndUpdate;
  end;
  FBracket := BracketArray[btParenthesis];
  FPrioritize := True;
  FExecuteOptions := [eoSubsequent];
  FDefaultValueType := vtInteger;
  FFalseValue := 0;
  FTrueValue := -1;
  FParameterBracket := BracketArray[btBracket];
end;

function TCustomParser.DefaultFunction(const AFunction: PFunction; out Value: TValue;
  const LValue, RValue: TValue; const PA: TParameterArray): Boolean;
begin
  Result := False;
end;

function TCustomParser.DeleteFunction(const Handle: Integer): Boolean;
var
  I: Integer;
begin
  Result := CheckFHandle(Handle);
  if Result then
  begin
    try
      Notify(ntBFD, Self);
      I := Length(FFData.FArray);
      Result := MemoryUtils.Delete(FFData.FArray, Handle * SizeOf(TFunction), SizeOf(TFunction), I * SizeOf(TFunction));
      if Result then
      begin
        SetLength(FFData.FArray, I - 1);
        FFData.Prepared := False;
      end;
      Notify(ntAFD, Self);
      Notify(ntCompile, Self);
    finally
      if WasteCount > 0 then Dec(WasteCount);
    end;
  end;
end;

function TCustomParser.DeleteFunction(const HandleArray: TIntegerDynArray): Integer;
var
  Handle: TIntegerDynArray;
  I: Integer;
begin
  Handle := Copy(HandleArray);
  try
    SortHandleArray(Handle);
    Result := 0;
    for I := Low(Handle) to High(Handle) do if DeleteFunction(Handle[I]) then Inc(Result);
  finally
    Handle := nil;
  end;
end;

function TCustomParser.DeleteType(const HandleArray: TIntegerDynArray): Integer;
var
  Handle: TIntegerDynArray;
  I: Integer;
begin
  Handle := Copy(HandleArray);
  try
    SortHandleArray(Handle);
    Result := 0;
    for I := Low(Handle) to High(Handle) do if DeleteType(Handle[I]) then Inc(Result);
  finally
    Handle := nil;
  end;
end;

function TCustomParser.DeleteConstant(const AName: string): Boolean;
var
  I: Integer;
  Value: PValue;
begin
  Result := Find(FConstantList.List, AName, False, I);
  if Result then
  begin
    Value := PValue(FConstantList.List.Objects[I]);
    DeleteVariable(Value^);
    FConstantList.List.Delete(I);
    Dispose(Value);
  end;
end;

function TCustomParser.DeleteType(const Handle: Integer): Boolean;
var
  I: Integer;
begin
  Result := CheckTHandle(Handle);
  if Result then
  begin
    Notify(ntBTD, Self);
    I := Length(FTData.TArray);
    Result := MemoryUtils.Delete(FTData.TArray, Handle * SizeOf(TType), SizeOf(TType), I * SizeOf(TType));
    if Result then
    begin
      SetLength(FTData.TArray, I - 1);
      FTData.Prepared := False;
    end;
    Notify(ntATD, Self);
    Notify(ntCompile, Self);
  end;
end;

function TCustomParser.DeleteVariable(var Variable: TValue): Boolean;
var
  I: Integer;
  AVariable: PFunctionVariable;
begin
  for I := Low(FFData.FArray) to High(FFData.FArray) do
  begin
    AVariable := @FFData.FArray[I].Method.Variable;
    if (AVariable.VariableType = vtValue) and (AVariable.Variable = @Variable) then
    begin
      Result := DeleteFunction(I);
      Exit;
    end;
  end;
  Result := False;
end;

destructor TCustomParser.Destroy;
var
  I: Integer;
  Item: PValue;
begin
  Notify(ntDisconnect, Self);
  with FFData^ do
  begin
    FArray := nil;
    FOrder := nil;
    NameList.Free;
  end;
  with FTData^ do
  begin
    TArray := nil;
    TOrder := nil;
    NameList.Free;
  end;
  with FPData^ do
  begin
    FArray := nil;
    FOrder := nil;
    NameList.Free;
  end;
  FTOHArray := nil;
  FNotifyArray := nil;
  SetExceptionMask(FExceptionMask);
  DeallocateHWnd(FWindowHandle);
  Dispose(FFData);
  Dispose(FTData);
  for I := 0 to FConstantList.List.Count - 1 do
  begin
    Item := PValue(FConstantList.List.Objects[I]);
    if Assigned(Item) then Dispose(Item);
  end;
  inherited;
end;

function TCustomParser.DoEvent(const Event: TFunctionEvent; const AFunction: PFunction; const AType: PType; out Value: TValue;
  const LValue, RValue: TValue; const PA: TParameterArray): Boolean;
begin
  Result := Assigned(Event) and Event(AFunction, AType, Value, LValue, RValue, PA);
end;

procedure TCustomParser.EndUpdate;
begin
  InterlockedDecrement(FUpdateCount);
end;

function TCustomParser.FindConstant(const AName: string): PValue;
var
  I: Integer;
begin
  I := ParseCommon.IndexOf(FConstantList.List, AName, False);
  if I < 0 then
    Result := nil
  else
    Result := Pointer(FConstantList.List.Objects[I]);
end;

function TCustomParser.FindConstant(const AName: string; out Value: PValue): Boolean;
begin
  Value := FindConstant(AName);
  Result := Assigned(Value);
end;

function TCustomParser.FindFunction(const AName: string): PFunction;
var
  I: Integer;
begin
  if FFData.Prepared then
    if Assigned(FFData.NameList) then
    begin
      I := FFData.NameList.List.IndexOf(AName);
      if I < 0 then
        Result := nil
      else
        GetFunction(Integer(FFData.NameList.List.Objects[I]), Result);
    end
    else Result := nil
  else begin
    for I := Low(FFData.FArray) to High(FFData.FArray) do
      if TextUtils.SameText(FFData.FArray[I].Name, AName) then
      begin
        Result := @FFData.FArray[I];
        Exit;
      end;
    Result := nil;
  end;
end;

function TCustomParser.FindType(const AName: string): PType;
var
  I: Integer;
begin
  if FTData.Prepared then
    if Assigned(FTData.NameList) then
    begin
      I := FTData.NameList.List.IndexOf(AName);
      if I < 0 then
        Result := nil
      else
        GetType(Integer(FTData.NameList.List.Objects[I]), Result);
    end
    else
      Result := nil
  else begin
    for I := Low(FTData.TArray) to High(FTData.TArray) do
      if TextUtils.SameText(FTData.TArray[I].Name, AName) then
      begin
        Result := @FTData.TArray[I];
        Exit;
      end;
    Result := nil;
  end;
end;

function TCustomParser.GetFunction(const Handle: Integer; var AFunction: PFunction): Boolean;
begin
  Result := CheckFHandle(Handle);
  if Result then AFunction := @FFData.FArray[Handle];
end;

function TCustomParser.GetFunction(const Handle: Integer): PFunction;
begin
  if not GetFunction(Handle, Result) then Result := nil;
end;

function TCustomParser.GetFInternal: PFunction;
begin
  Result := @FFData.FArray[FInternalHandle];
end;

function TCustomParser.GetFunction(const Variable: PValue): PFunction;
var
  I: Integer;
begin
  for I := Low(FFData.FArray) to High(FFData.FArray) do
  begin
    Result := @FFData.FArray[I];
    if (Result.Kind = fkMethod) and (Result.Method.MethodType = mtVariable) and (Result.Method.Variable.Variable = Variable) then Exit;
  end;
  Result := nil;
end;

function TCustomParser.GetIgnoreType(const ItemCode: TItemCode): Boolean;
begin
  Result := FIgnoreType[ItemCode];
end;

function TCustomParser.GetTextData(const Text: string): TTextData;
var
  AFunction: PFunction;
begin
  FillChar(Result, SizeOf(TTextData), 0);
  if InBrace(Text, FBracket) then Result.TextType := ttScript
  else if InQuote(Text) then
  begin
    Result.TextType := ttString;
    StrLCopy(Result.Text, PChar(Quote(Text)), SizeOf(TString) - 1);
  end
  else begin
    Result.Value := TextToValue(Text);
    case Result.Value.ValueType of
      vtUnknown:
        begin
          AFunction := FindFunction(Text);
          if Assigned(AFunction) then
          begin
            Result.Handle := AFunction.Handle^;
            Result.TextType := ttFunction;
          end
          else Result.TextType := ttUnknown;
        end;
    else Result.TextType := ttNumber;
    end;
  end;
end;

function TCustomParser.GetType(const Handle: Integer; var AType: PType): Boolean;
begin
  Result := CheckTHandle(Handle);
  if Result then AType := @FTData.TArray[Handle];
end;

function TCustomParser.GetType(const Handle: Integer): PType;
begin
  if not GetType(Handle, Result) then Result := nil;
end;

function TCustomParser.Notifiable(const NotifyType: TNotifyType): Boolean;
begin
  Result := NotifyAttribute[NotifyType].Permanent and (NotifyAttribute[NotifyType].Reliable or Available(Self)) or
    (FUpdateCount = 0) and (NotifyAttribute[NotifyType].Reliable or Available(Self));
end;

procedure TCustomParser.Notification(Component: TComponent; Operation: TOperation);
begin
  inherited;
  if (Operation = opRemove) and (Component = FAddressee) then FAddressee := nil;
end;

{$WARNINGS OFF}
procedure TCustomParser.Notify;
var
  ANotifyArray: TNotifyArray;
  I: Integer;
begin
  if Assigned(FNotifyArray) then
  begin
    ANotifyArray[0] := TInterlocked.Exchange<TNotify>(FNotifyArray[0], ANotifyArray[0]);
    try
      for I := Low(ANotifyArray) to High(ANotifyArray) do
        Notify(ANotifyArray[I].NotifyType, ANotifyArray[I].Component);
    finally
      ANotifyArray := nil;
    end;
  end;
end;
{$WARNINGS ON}

procedure TCustomParser.Notify(const NotifyType: TNotifyType; const Sender: TComponent);
begin
  if Notifiable(NotifyType) and Available(FAddressee) then TCustomAddressee(FAddressee).Notify(NotifyType, Sender);
end;

procedure TCustomParser.Prepare;
begin
  ParseUtils.Prepare(FFData);
  if ParseUtils.Prepare(FTData) then FDefaultTypeHandle := MakeTypeHandle(FTData^, FDefaultValueType, -1);
  Notify;
end;

procedure TCustomParser.SetIgnoreType(const ItemCode: TItemCode; const Value: Boolean);
begin
  FIgnoreType[ItemCode] := Value;
end;

procedure TCustomParser.WindowMethod(var Message: TMessage);
var
  Data: PVariableData;
begin
  case Message.Msg of
    WM_NOTIFY:
      Put(FNotifyArray, MakeNotify(TNotifyType(Message.WParam), TComponent(Message.LParam)));
    WM_ADDFUNCTION:
      Message.Result := Integer(InternalAddFunction(PFunction(Message.WParam)^));
    WM_ADDVARIABLE:
      begin
        Data := PVariableData(Message.WParam);
        Message.Result := Integer(InternalAddVariable(Data.Name, PValue(Message.LParam)^, Data.Optimizable, Data.ReturnType));
      end;
  else Message.Result := DefWindowProc(FWindowHandle, Message.Msg, Message.WParam, Message.LParam);
  end
end;

{ TParser }

function TParser.Connect(const ABeforeFunction: TFunctionEvent; const AAddressee: TCustomAddressee): TFunctionEvent;
begin
  if AAddressee <> FConnector then TCustomConnector(FConnector).Parser := nil;
  Result := inherited Connect(ABeforeFunction, AAddressee);
end;

constructor TParser.Create(AOwner: TComponent);
begin
  inherited;
  FConnector := TConnector.Create(Self);
  TConnector(FConnector).Parser := Self;
  TConnector(FConnector).Name := InternalConnectorName;
  FParseManager := TParseManager.Create(Self);
  TParseManager(FParseManager).Connector := TCustomConnector(FConnector);
  TParseManager(FParseManager).Name := InternalParseManagerName;
  TParseManager(FParseManager).SetSubComponent(True);
  FCache := TCache.Create(Self, TCustomAddressee(FConnector));
  FCache.Name := InternalCacheName;
  FCache.SetSubComponent(True);
  FFData.FlagCache := TFlagCache.Create(Self);
  TFlagCache(FFData.FlagCache).Connector := TCustomConnector(FConnector);
  TFlagCache(FFData.FlagCache).Name := InternalFlagCacheName;
  TFlagCache(FFData.FlagCache).SetSubComponent(True);
  FFData.ItemCache := TItemCache.Create(Self);
  TItemCache(FFData.ItemCache).Connector := TCustomConnector(FConnector);
  TItemCache(FFData.ItemCache).Name := InternalItemCacheName;
  TItemCache(FFData.ItemCache).SetSubComponent(True);
  FPData.FlagCache := TFlagCache.Create(Self);
  TFlagCache(FPData.FlagCache).Connector := TCustomConnector(FConnector);
  TFlagCache(FPData.FlagCache).Name := InternalPOperatorFlagCacheName;
  TFlagCache(FPData.FlagCache).SetSubComponent(True);
  FPData.ItemCache := TItemCache.Create(Self);
  TItemCache(FPData.ItemCache).Connector := TCustomConnector(FConnector);
  TItemCache(FPData.ItemCache).Name := InternalPOperatorItemCacheName;
  TItemCache(FPData.ItemCache).SetSubComponent(True);
  BeginUpdate;
  try
    InternalAddFunction(MultiplyFunctionName, FMultiplyHandle, fkMethod, MakeFunctionMethod(FMethod.MultiplyMethod), True);
    InternalAddFunction(DivideFunctionName, FDivideHandle, fkMethod, MakeFunctionMethod(FMethod.DivideMethod), True);
    InternalAddFunction(SuccFunctionName, FSuccHandle, fkMethod, MakeFunctionMethod(FMethod.SuccMethod, False, True), True);
    InternalAddFunction(PredFunctionName, FPredHandle, fkMethod, MakeFunctionMethod(FMethod.PredMethod, False, True), True);
    InternalAddFunction(BitwiseNegationFunctionName, FNotHandle, fkMethod, MakeFunctionMethod(FMethod.NotMethod, False, True), True);
    InternalAddFunction(BitwiseAndFunctionName, FAndHandle, fkMethod, MakeFunctionMethod(FMethod.AndMethod), True);
    InternalAddFunction(BitwiseOrFunctionName, FOrHandle, fkMethod, MakeFunctionMethod(FMethod.OrMethod), True);
    InternalAddFunction(BitwiseXorFunctionName, FXorHandle, fkMethod, MakeFunctionMethod(FMethod.XorMethod), True);
    InternalAddFunction(BitwiseShiftLeftFunctionName, FShlHandle, fkMethod, MakeFunctionMethod(FMethod.ShlMethod), True);
    InternalAddFunction(BitwiseShiftRightFunctionName, FShrHandle, fkMethod, MakeFunctionMethod(FMethod.ShrMethod), True);
    InternalAddFunction(SameValueFunctionName, FSameValueHandle, fkMethod, MakeFunctionMethod(FMethod.SameValueMethod, SameValueMaxParameterCount), True);
    InternalAddFunction(IsZeroFunctionName, FIsZeroHandle, fkMethod, MakeFunctionMethod(FMethod.IsZeroMethod, IsZeroMaxParameterCount), True);
    InternalAddFunction(IfFunctionName, FIfHandle, fkMethod, MakeFunctionMethod(FMethod.IfMethod, IfMaxParameterCount, pkReference), True);
    InternalAddFunction(IfThenFunctionName, FIfThenHandle, fkMethod, MakeFunctionMethod(FMethod.IfThenMethod, IfThenParameterCount), True);
    InternalAddFunction(EnsureRangeFunctionName, FEnsureRangeHandle, fkMethod, MakeFunctionMethod(FMethod.EnsureRangeMethod, EnsureRangeParameterCount), True);
    InternalAddFunction(StrToIntFunctionName, FStrToIntHandle, fkMethod, MakeFunctionMethod(FMethod.StrToIntMethod, StrToIntParameterCount), True);
    InternalAddFunction(StrToIntDefFunctionName, FStrToIntDefHandle, fkMethod, MakeFunctionMethod(FMethod.StrToIntDefMethod, StrToIntDefParameterCount), True);
    InternalAddFunction(StrToFloatFunctionName, FStrToFloatHandle, fkMethod, MakeFunctionMethod(FMethod.StrToFloatMethod, StrToFloatParameterCount), True);
    InternalAddFunction(StrToFloatDefFunctionName, FStrToFloatDefHandle, fkMethod, MakeFunctionMethod(FMethod.StrToFloatDefMethod, StrToFloatDefParameterCount), True);
    InternalAddFunction(ParseFunctionName, FParseHandle, fkMethod, MakeFunctionMethod(FMethod.ParseMethod, ParseMaxParameterCount), False);
    InternalAddFunction(FalseFunctionName, FFalseHandle, fkMethod, MakeFunctionMethod(FMethod.FalseMethod), True);
    InternalAddFunction(TrueFunctionName, FTrueHandle, fkMethod, MakeFunctionMethod(FMethod.TrueMethod), True);
    InternalAddFunction(EqualFunctionName, FEqualHandle, fkMethod, MakeFunctionMethod(FMethod.EqualMethod), True, vtUnknown, fpLower, pcTotal);
    InternalAddFunction(NotEqualFunctionName, FNotEqualHandle, fkMethod, MakeFunctionMethod(FMethod.NotEqualMethod), True, vtUnknown, fpLower, pcTotal);
    InternalAddFunction(AboveFunctionName, FAboveHandle, fkMethod, MakeFunctionMethod(FMethod.AboveMethod), True, vtUnknown, fpLower, pcTotal);
    InternalAddFunction(BelowFunctionName, FBelowHandle, fkMethod, MakeFunctionMethod(FMethod.BelowMethod), True, vtUnknown, fpLower, pcTotal);
    InternalAddFunction(AboveOrEqualFunctionName, FAboveOrEqualHandle, fkMethod, MakeFunctionMethod(FMethod.AboveOrEqualMethod), True, vtUnknown, fpLower, pcTotal);
    InternalAddFunction(BelowOrEqualFunctionName, FBelowOrEqualHandle, fkMethod, MakeFunctionMethod(FMethod.BelowOrEqualMethod), True, vtUnknown, fpLower, pcTotal);
    InternalAddFunction(GetEpsilonFunctionName, FGetEpsilonHandle, fkMethod, MakeFunctionMethod(FMethod.GetEpsilonMethod), False);
    InternalAddFunction(SetEpsilonFunctionName, FSetEpsilonHandle, fkMethod, MakeFunctionMethod(FMethod.SetEpsilonMethod, SetEpsilonParameterCount), False);
    InternalAddFunction(SetDecimalSeparatorFunctionName, FSetDecimalSeparatorHandle, fkMethod, MakeFunctionMethod(FMethod.SetDecimalSeparatorMethod, SetDecimalSeparatorParameterCount), False);
    AddType(ShortintTypeName, FShortintHandle, vtShortint);
    AddType(ByteTypeName, FByteHandle, vtByte);
    AddType(SmallintTypeName, FSmallintHandle, vtSmallint);
    AddType(WordTypeName, FWordHandle, vtWord);
    AddType(IntegerTypeName, FIntegerHandle, vtInteger);
    AddType(LongwordTypeName, FLongwordHandle, vtLongword);
    AddType(Int64TypeName, FInt64Handle, vtInt64);
    AddType(SingleTypeName, FSingleHandle, vtSingle);
    AddType(DoubleTypeName, FDoubleHandle, vtDouble);
    AddType(ExtendedTypeName, FExtendedHandle, vtExtended);
  finally
    EndUpdate;
  end;
  FExceptionTypes := [etZeroDivide];
  FCached := True;
end;

function TParser.DecompileMethod(var Index: NativeInt; const Header: PScriptHeader;
  const ItemHeader: PItemHeader; const Item: PScriptItem;
  const Data: Pointer): Boolean;
var
  S: string;
  AData: PDecompileData absolute Data;
  Start: Integer absolute ItemHeader;
  AType: PType;
begin
  case Item.Code of
    NumberCode:
      begin
        S := ValueToText(Item.ScriptNumber.Value);
        if not AData.Parameter and LessZero(Item.ScriptNumber.Value) then
          AData.ItemText.Append(Embrace(S, FBracket), AData.Delimiter)
        else
          AData.ItemText.Append(S, AData.Delimiter);
        Inc(Index, SizeOf(TCode) + SizeOf(TScriptNumber));
        AData.AFunction := nil;
      end;
    FunctionCode:
      begin
        Read(Self, Index, AData.AFunction);
        AData.ItemText.Append(AData.AFunction.Name, AData.Delimiter);
      end;
    StringCode:
      begin
        AData.ItemText.Append(LockChar, AData.Delimiter);
        AData.ItemText.Append(PAnsiChar(Index) + SizeOf(TCode) + SizeOf(TScriptString), Item.ScriptString.Size div SizeOf(Char));
        AData.ItemText.Append(LockChar);
        Inc(Index, SizeOf(TCode) + SizeOf(TScriptString) + Item.ScriptString.Size);
        AData.AFunction := nil;
      end;
    ScriptCode:
      begin
        S := InternalDecompile(Index + SizeOf(TCode), AData.Delimiter, False, AData.ParameterBracket, AData.TypeMode);
        if AData.Parameter then
          AData.ItemText.Append(S, AData.Delimiter)
        else
          AData.ItemText.Append(Embrace(S, FBracket), AData.Delimiter);
        Inc(Index, SizeOf(TCode) + Item.Script.Header.ScriptSize);
        AData.AFunction := nil;
      end;
    ParameterCode:
      begin
        S := InternalDecompile(Index + SizeOf(TCode), AData.Delimiter, True, AData.ParameterBracket, AData.TypeMode);
        AData.ItemText.Append(Embrace(S, AData.ParameterBracket), AData.Delimiter);
        Inc(Index, SizeOf(TCode) + Item.Script.Header.ScriptSize);
        AData.AFunction := nil;
      end;
  else raise Error(ScriptError);
  end;
  if Index - Start >= ItemHeader.Size then
  begin
    if ((AData.TypeMode = rmUser) and ItemHeader.UserType.Active or (AData.TypeMode = rmFull)) and GetType(ItemHeader.UserType.Handle, AType) then
      AData.ItemText.Insert(string(AType.Name) + Space);
    if not AData.Parameter and Boolean(ItemHeader.Sign) then AData.ItemText.Insert(TOperatorArray[toNegative] + Space);
    if AData.Text.Size > 0 then
      if AData.Parameter then
        AData.ItemText.Insert(POperatorArray[poComma] + Space)
      else
        if not Boolean(ItemHeader.Sign) then AData.ItemText.Insert(TOperatorArray[toPositive] + Space);
    if AData.Parameter then
      AData.Text.Append(AData.ItemText.Text)
    else
      if AData.ItemText.Size > 0 then AData.Text.Append(AData.ItemText.Text, AData.Delimiter);
    AData.ItemText.Clear;
  end;
  Result := True;
end;

destructor TParser.Destroy;
begin
  FMethod.Free;
  FScript := nil;
  FCache.Clear;
  inherited;
end;

function TParser.Execute(const Index: NativeInt): PValue;
var
  Header: PScriptHeader absolute Index;
  Value: TValue;
begin
  Notify;
  if not (eoSubsequent in FExecuteOptions) and (Header.ScriptCount > 0) then ExecuteInternalScript(Index);
  Result := @Header.Value;
  Header.Value := EmptyValue;
  Value := EmptyValue;
  ParseScript(Index, ExecuteMethod, @Value);
end;

function TParser.Execute(const Script: TScript): PValue;
begin
  Result := Execute(Integer(Script));
end;

function TParser.Execute: PValue;
begin
  Result := Execute(FScript);
end;

function TParser.ExecuteFunction(var Index: NativeInt; const ItemHeader: PItemHeader; const LValue: TValue; const Fake: Boolean): TValue;
var
  AFunction: PFunction;
  Parameter: PFunctionParameter;
  PA: TParameterArray;
  RValue: TValue;
  Item: PScriptItem absolute Index;
  AType: PType;
  ValueType: TValueType;
begin
  Read(Self, Index, AFunction);
  try
    Parameter := @AFunction.Method.Parameter;
    if Parameter.Count > 0 then
    begin
      Inc(Index, SizeOf(TCode));
      ReadParameterArray(Index, PA, TParameterType(Parameter.Kind = pkReference));
      RValue := EmptyValue;
    end
    else begin
      RValue := EmptyValue;
      if Parameter.RParameter then
        case Item.Code of
          NumberCode:
            begin
              if not Fake then RValue := Item.ScriptNumber.Value;
              Inc(Index, SizeOf(TCode) + SizeOf(TScriptNumber));
            end;
          FunctionCode:
            if Fake then
              ExecuteFunction(Index, ItemHeader, EmptyValue, Fake)
            else
              RValue := ExecuteFunction(Index, ItemHeader, EmptyValue, Fake);
          ScriptCode:
            begin
              if not Fake then
                if eoSubsequent in FExecuteOptions then
                  RValue := Execute(Integer(@Item.Script.Header))^
                else
                  RValue := Item.Script.Header.Value;
              Inc(Index, SizeOf(TCode) + Item.Script.Header.ScriptSize);
            end;
        else raise Error(ScriptError);
        end;
    end;
    if Fake then Result := EmptyValue
    else begin
      AType := ParseUtils.GetType(Self, ItemHeader);
      case AFunction.Kind of
        fkHandle:
          if not DoEvent(FBeforeFunction, AFunction, AType, Result, LValue, RValue, PA) and
            not DefaultFunction(AFunction, Result, LValue, RValue, PA) and
            not DoEvent(FOnFunction, AFunction, AType, Result, LValue, RValue, PA) then
              raise Error(FunctionHandleError, [AFunction.Name]);
        fkMethod:
          case AFunction.Method.MethodType of
            mtParameterless: Result := AFunction.Method.AMethod(AFunction, AType);
            mtSingleParameter:
              if Parameter.LParameter then
                Result := AFunction.Method.BMethod(AFunction, AType, LValue)
              else
                Result := AFunction.Method.BMethod(AFunction, AType, RValue);
            mtDoubleParameter: Result := AFunction.Method.CMethod(AFunction, AType, LValue, RValue);
            mtParameterArray: Result := AFunction.Method.DMethod(AFunction, AType, PA);
            mtVariable:
              case AFunction.Method.Variable.VariableType of
                vtValue: Result := AFunction.Method.Variable.Variable^;
                vtLiveValue: Result := MakeValue(AFunction.Method.Variable.LiveVariable);
              else
                Result := EmptyValue;
              end;
          else Result := EmptyValue;
          end;
        else Result := EmptyValue;
        end;
    end;
    if (Result.ValueType <> vtUnknown) and Assigned(AFunction) and (AFunction.ReturnType <> vtUnknown) then
    begin
      if ItemHeader.UserType.Active and GetType(ItemHeader.UserType.Handle, AType) then
        ValueType := AType.ValueType
      else
        if FIgnoreType[icFunction] then
          ValueType := vtUnknown
        else
          ValueType := AFunction.ReturnType;
      if ValueType <> vtUnknown then Result := Convert(Result, ValueType);
    end;
  finally
    PA := nil;
  end;
end;

procedure TParser.ExecuteInternalScript(Index: Integer);
var
  Header: PScriptHeader absolute Index;
  I, J, K: Integer;
begin
  if Header.ScriptCount > 0 then
  begin
    I := Index + SizeOf(TScriptHeader);
    J := I + Header.ScriptCount * SizeOf(Integer);
    while I < J do
    begin
      K := Index + PInteger(I)^;
      Execute(K);
      Inc(I, SizeOf(Integer));
    end;
  end;
end;

function TParser.ExecuteMethod(var Index: NativeInt; const Header: PScriptHeader; const ItemHeader: PItemHeader;
  const Item: PScriptItem; const Data: Pointer): Boolean;
var
  Value: PValue absolute Data;
  Start: Integer absolute ItemHeader;
  AType: PType;
begin
  case Item.Code of
    NumberCode:
      begin
        Value^ := Item.ScriptNumber.Value;
        Inc(Index, SizeOf(TCode) + SizeOf(TScriptNumber));
      end;
    FunctionCode:
      Value^ := ExecuteFunction(Index, ItemHeader, Value^);
    ScriptCode:
      begin
        if eoSubsequent in FExecuteOptions then
          Value^ := Execute(Integer(@Item.Script.Header))^
        else
          Value^ := Item.Script.Header.Value;
        Inc(Index, SizeOf(TCode) + Item.Script.Header.ScriptSize);
      end;
    ParameterCode:
      Inc(Index, SizeOf(TCode) + Item.Script.Header.ScriptSize);
  else raise Error(ScriptError);
  end;
  if Index - Start >= ItemHeader.Size then
  begin
    if not FIgnoreType[icScript] and ItemHeader.UserType.Active and GetType(ItemHeader.UserType.Handle, AType) then
      Value^ := Convert(Value^, AType.ValueType);
    Header.Value := Operation(Header.Value, Value^, TOperationType(Ord(otAdd) + ItemHeader.Sign));
  end;
  Result := True;
end;

function TCustomParser.InternalAddConstant(const AName: string; const Value: TValue): Boolean;
var
  Item: PValue;
begin
  Result := not Assigned(FindFunction(AName));
  if Result then
  begin
    New(Item);
    try
      Item^ := Value;
      Result := InternalAddVariable(AName, Item^, True, Value.ValueType);
      if Result then
        FConstantList.List.AddObject(AName, Pointer(Item))
      else
        Dispose(Item);
    except
      Dispose(Item);
      raise;
    end;
  end;
end;

function TCustomParser.InternalAddFunction(const AFunction: TFunction): Boolean;
var
  Error: TError;
begin
  Result := (AFunction.Name <> '') and not Assigned(FindFunction(AFunction.Name));
  if Result then
  begin
    Notify(ntBFA, Self);
    if AFunction.Handle^ <> FInternalHandle then
    begin
      Error := Validator.Check(AFunction.Name, rtName);
      if Error.ErrorType <> etSuccess then raise ParseErrors.Error(Error.ErrorText);
    end;
    AFunction.Handle^ := Add(FFData^, AFunction);
    Notify(ntAFA, Self);
  end
  else AFunction.Handle^ := -1;
end;

function TCustomParser.InternalAddFunction(const AName: string; var Handle: Integer; const Kind: TFunctionKind;
  const Method: TFunctionMethod; const Optimizable: Boolean; const ReturnType: TValueType; const Priority: TFunctionPriority;
  const Coverage: TPriorityCoverage): Boolean;
begin
  Result := InternalAddFunction(MakeFunction(AName, Handle, ReturnType, Kind, Method, Optimizable, MakePriority(Priority, Coverage)));
end;

function TCustomParser.InternalAddVariable(const AName: string; var Variable: TValue; const Optimizable: Boolean;
  const ReturnType: TValueType): Boolean;
var
  AFunction: TFunction;
begin
  AFunction := MakeFunction(AName, AFunction.Method.Variable.Handle, ReturnType, fkMethod, MakeFunctionMethod(@Variable), Optimizable, MakePriority(fpNormal, pcLocal));
  Result := InternalAddFunction(AFunction);
end;

function TCustomParser.InternalAddVariable(const AName: string; const Variable: TLiveValue; const Optimizable: Boolean;
  const ReturnType: TValueType): Boolean;
var
  AFunction: TFunction;
begin
  AFunction := MakeFunction(AName, AFunction.Method.Variable.Handle, ReturnType, fkMethod, MakeFunctionMethod(Variable), Optimizable, MakePriority(fpNormal, pcLocal));
  Result := AddFunction(AFunction);
end;

//1) Set ( "Pi", Sum (1) + 2 )
//2) 1
//3) Sum {#0} + 2
//4) Pi, Sum {#0} + 2
//5) Pi, {1}
//6) Set {#2}

function TParser.InternalCompile(const Text: string; var SA: TScriptArray; const Parameter: Boolean; var Idle: PFunction;
  out Error: TError): TScript;
var
  AText, S: string;
  AFlag, BFlag, CFlag, DFlag, Sign: Boolean;
  ACache, BCache: TParseCache;
  AItemArray, BItemArray: TTextItemArray;
  Script: TScript;
  I, J, K, ItemIndex: Integer;
  MItemArray: TTextItemMultiArray;
  AItem, BItem: PTextItem;
  IHType: TValueType;
  Syntax: TSyntax;
  Data: TTextData;
  Header: PScriptHeader absolute Result;

  procedure WriteMArray(const Item: PTextItem);
  const
    Size = AIndex + 1;
  begin
    if I > High(MItemArray) then SetLength(MItemArray, I + 1);
    SetLength(MItemArray[I], Size);
    MItemArray[I][AIndex] := Item^;
  end;

  procedure WriteType(const Code: TItemCode; const ItemType: TValueType);
  var
    UserType: PType;
  begin
    // AItem.THandle - пользовательский тип текущего элемента, IHType - текущий тип TItemHeader (по умолчанию - FDefaultValueType)
    // ItemType - тип текущего элемента, например, в случае если текущий элемент - функция, ItemType = TFunction.ReturnType
    if ItemType <> vtUnknown then
    begin
      if not GetType(AItem.THandle, UserType) then UserType := nil;
      if Assigned(UserType) then
        if Assignable(UserType.ValueType, ItemType) then
          IHType := UserType.ValueType
        else
          IHType := ItemType
      else
        IHType := ItemType;
      AssignUserType(Self, @Result[ItemIndex], Assigned(UserType), IHType);
    end;
  end;

  procedure Write(const Code: TItemCode);
  var
    AType: PType;
  begin
    case Code of
      icNumber:
        begin
          if not FIgnoreType[icNumber] and GetType(AItem.THandle, AType) then Data.Value := Convert(Data.Value, AType.ValueType);
          WriteType(Code, Data.Value.ValueType);
          WriteNumber(Result, Data.Value);
        end;
      icFunction:
        begin
          if GetType(AItem.THandle, AType) then WriteType(Code, AType.ValueType);
          WriteFunction(Result, BItem.FHandle);
        end;
      icString:
        begin
          PItemHeader(@Result[ItemIndex]).UserType.Active := GetType(AItem.THandle, AType);
          PItemHeader(@Result[ItemIndex]).UserType.Handle := FStringHandle;
          WriteString(Result, Data.Text);
        end;
      icScript, icParameter:
        begin
          if GetType(AItem.THandle, AType) then WriteType(Code, AType.ValueType);
          WriteScript(Result, Script, Code = icParameter, @ItemIndex);
        end;
    end;
  end;

begin
  FillChar(Error, SizeOf(TError), 0);
  Result := nil;
  AText := Text;
  if AText = '' then
  begin
    Error := MakeError(etEmptyTextError, EmptyTextError);
    Exit;
  end;
  if Assigned(FCache) then
    if Parameter then
      ACache := FCache.CacheArray[ctParameter]
    else
      ACache := FCache.CacheArray[ctScript]
  else
    ACache := nil;
  AFlag := Pos(FInternal.Name, AText) = 0;
  if FCached and Assigned(ACache) and AFlag then
  begin
    Enter(ACache.Lock^);
    try
      Result := ACache.Find(AText);
    finally
      Leave(ACache.Lock^);
    end;
  end;
  if not Assigned(Result) then
  begin
    Parse(AText, SA, Idle, Error);
    if Error.ErrorType <> etSuccess then Exit;
    CFlag := FPrioritize and not Parameter;
    try
      if Parameter then
        GetItemArray(AText, FPData, FTData, AItemArray)
      else
        GetItemArray(AText, FFData, FTData, AItemArray, FInternalHandle);
      try
        if CFlag then Order(pcTotal, AItemArray, SA, Idle);
        if not Parameter then Morph(AItemArray, MItemArray);
        if Assigned(FCache) then
          if Parameter then
            BCache := FCache.CacheArray[ctSubparameter]
          else
            BCache := FCache.CacheArray[ctSubscript]
        else
          BCache := nil;
        Resize(Result, SizeOf(TScriptHeader));
        for I := Low(AItemArray) to High(AItemArray) do
        begin
          AItem := @AItemArray[I];
          // Знак параметра не важен
          if Parameter then
          begin
            TrimPOperator(AItem.Text);
            Sign := False;
          end
          else
            Sign := GetSign(AItem^, FTOHArray);
          try
            BFlag := Pos(FInternal.Name, AItem.Text) = 0;
            if FCached and Assigned(BCache) and BFlag then
            begin
              Enter(BCache.Lock^);
              try
                if Sign then
                  Script := BCache.Find(TOperatorArray[toNegative] + AItem.Text)
                else
                  Script := BCache.Find(AItem.Text);
              finally
                Leave(BCache.Lock^);
              end;
            end
            else Script := nil;
            if Assigned(Script) then Add(Result, Script)
            else begin
              ItemIndex := Length(Result);
              Resize(Result, ItemIndex + SizeOf(TItemHeader));
              WriteItemHeader(PItemHeader(@Result[ItemIndex]), Sign, AItem.THandle, FDefaultTypeHandle);
              IHType := FDefaultValueType;
              FillChar(Syntax, SizeOf(TSyntax), 0);
              Syntax.PriorHandle := -1;
              Data := TextData[AItem.Text];
              // Если у параметра функции указан тип то такой параметр рассматривается как вложенное выражение, соответственно параметр
              // (AItem.Text) может содержать или число без типа или функцию без типа или строку или вложенное выражение
              if Data.TextType = ttNumber then Write(icNumber)
              else begin
                if AItem.Text = '' then
                begin
                  Error := MakeError(etTextError, Format(TextError, [AText]));
                  Exit;
                end;
                // Если Parameter = True, выражение разделяется по FPData, любой TTextItem.FHandle связан с "чужой" PFunctionData,
                // PTypeData в разделении выражения не участвует
                if Parameter then
                begin
                  AItem.Text := SetSign(AItem.Text);
                  AItem.FHandle := -1;
                  case Data.TextType of
                    ttUnknown:
                      begin
                        try
                          if FCached and Assigned(ACache) and (Pos(FInternal.Name, AItem.Text) = 0) then
                          begin
                            Enter(ACache.Lock^);
                            try
                              Script := ACache.Find(AItem.Text);
                              if not Assigned(Script) then
                              begin
                                Script := InternalCompile(AItem.Text, SA, False, Idle, Error);
                                if Error.ErrorType <> etSuccess then Exit;
                                ACache.Add(AItem.Text, Script);
                              end;
                            finally
                              Leave(ACache.Lock^);
                            end;
                          end
                          else begin
                            Script := InternalCompile(AItem.Text, SA, False, Idle, Error);
                            if Error.ErrorType <> etSuccess then Exit;
                          end;
                          AItem^ := MakeTextItem(Embrace(IntToStr(Add(SA, Script)), BracketArray[btBrace]), FInternal.Handle^, -1);
                        finally
                          Script := nil;
                        end;
                      end;
                    ttFunction:
                      AItem^ := MakeTextItem(AItem.Text, Data.Handle, -1);
                  end;
                  WriteMArray(AItem);
                end;
                if (I < Length(MItemArray)) and Assigned(MItemArray[I]) then
                  BItemArray := MItemArray[I]
                else begin
                  Error := MakeError(etScriptError, ScriptError);
                  Exit;
                end;
                try
                  if CFlag then Order(pcLocal, BItemArray, SA, Idle);
                  for J := Low(BItemArray) to High(BItemArray) do
                  begin
                    BItem := @BItemArray[J];
                    if BItem.FHandle < 0 then
                    begin
                      Data := TextData[BItem.Text];
                      case Data.TextType of
                        ttNumber:
                          begin
                            Error := Check(Syntax, okNumber, AText, SA, -1);
                            if Error.ErrorType <> etSuccess then Exit;
                            Error := Check(BItemArray, J, MakeItemData(Data.Value), SA, Parameter);
                            if Error.ErrorType <> etSuccess then Exit;
                            Write(icNumber);
                          end;
                        ttString:
                          if Parameter then
                            Write(icString)
                          else begin
                            Error := MakeError(etStringError, StringError);
                            Exit;
                          end;
                      else
                        if BItem.Text <> '' then
                        begin
                          Error := MakeError(etElementError, Format(ElementError, [BItem.Text]));
                          Exit;
                        end;
                      end;
                    end
                    else if BItem.FHandle = FInternalHandle then
                    begin
                      Script := GetIS(BItem.Text, SA, DFlag);
                      if Assigned(Script) then
                      begin
                        if DFlag then
                        begin
                          Error := Check(Syntax, okParameter, AText, SA, -1);
                          if Error.ErrorType <> etSuccess then Exit;
                        end
                        else begin
                          Error := Check(Syntax, okScript, AText, SA, -1);
                          if Error.ErrorType <> etSuccess then Exit;
                        end;
                        Error := Check(BItemArray, J, MakeItemData(Script, DFlag), SA, Parameter);
                        if Error.ErrorType <> etSuccess then Exit;
                        if DFlag then
                          Write(icParameter)
                        else
                          Write(icScript);
                      end
                      else begin
                        Error := MakeError(etScriptError, ScriptError);
                        Exit;
                      end;
                      K := NextBracket(BItem.Text, BracketArray[btBrace], 1);
                      if K > 0 then
                      begin
                        S := Trim(Copy(BItem.Text, K + 1, MaxInt));
                        if S <> '' then
                        begin
                          Error := MakeError(etElementError, Format(ElementError, [S]));
                          Exit;
                        end;
                      end;
                    end
                    else begin
                      Error := Check(Syntax, okFunction, AText, SA, BItem.FHandle);
                      if Error.ErrorType <> etSuccess then Exit;
                      Error := Check(BItemArray, J, MakeItemData(BItem.FHandle), SA, Parameter);
                      if Error.ErrorType <> etSuccess then Exit;
                      Write(icFunction);
                    end;
                  end;
                finally
                  BItemArray := nil;
                end;
                Error := Check(Syntax, okLatest, AText, SA, -1);
                if Error.ErrorType <> etSuccess then Exit;
              end;
              PItemHeader(@Result[ItemIndex]).Size := Length(Result) - ItemIndex;
              if FCached and Assigned(BCache) and BFlag then
              begin
                Enter(BCache.Lock^);
                try
                  Script := Segment(Result, ItemIndex, Length(Result) - ItemIndex);
                  try
                    if Sign then
                      BCache.Add(TOperatorArray[toNegative] + AItem.Text, Script)
                    else
                      BCache.Add(AItem.Text, Script);
                  finally
                    Script := nil;
                  end;
                finally
                  Leave(BCache.Lock^);
                end;
              end;
            end;
          finally
            Script := nil;
          end;
        end;
      finally
        for I := Low(MItemArray) to High(MItemArray) do MItemArray[I] := nil;
        MItemArray := nil;
      end;
    finally
      AItemArray := nil;
    end;
    Header.ScriptSize := Length(Result);
    Header.HeaderSize := SizeOf(TScriptHeader) + Header.ScriptCount * SizeOf(Integer);
    if FCached and Assigned(ACache) and AFlag then
    begin
      Enter(ACache.Lock^);
      try
        ACache.Add(Text, Result);
      finally
        Leave(ACache.Lock^);
      end;
    end;
  end;
end;

function TParser.InternalCompile(const Text: string; var SA: TScriptArray; const Parameter: Boolean; var Idle: PFunction): TScript;
var
  Error: TError;
begin
  Result := InternalCompile(Text, SA, Parameter, Idle, Error);
  if Error.ErrorType <> etSuccess then raise ParseErrors.Error(Error.ErrorText);
end;

function TParser.InternalDecompile(const Index: Integer; const Delimiter: string; const Parameter: Boolean;
  const ParameterBracket: TBracket; const TypeMode: TRetrieveMode): string;
var
  Data: TDecompileData;
begin
  Data := MakeDecompileData(Delimiter, Parameter, ParameterBracket, TypeMode);
  Data.Text := TTextBuilder.Create;
  try
    Data.ItemText := TTextBuilder.Create;
    try
      ParseScript(Index, DecompileMethod, @Data);
      Result := Data.Text.Text;
    finally
      Data.ItemText.Free;
    end;
  finally
    Data.Text.Free;
  end;
end;

procedure TParser.InternalOptimize(const Index: Integer; out Script: TScript);
var
  Data: TOptimizationData;
begin
  FillChar(Data, SizeOf(TOptimizationData), 0);
  try
    ParseScript(Index, OptimizationMethod, @Data);
    BuildScript(Self, Data.ItemArray, Script, True);
  finally
    Delete(Data.ItemArray);
  end;
end;

function TParser.Morph(var AItemArray: TTextItemArray; out MItemArray: TTextItemMultiArray): Boolean;

  function New: Integer;
  begin
    Result := Length(MItemArray);
    SetLength(MItemArray, Result + 1);
  end;

var
  I, J, K, L: Integer;
  AItem: PTextItem;
  BItemArray: TTextItemArray;
begin
  Result := Length(AItemArray) > 0;
  if Result then
  begin
    J := -1;
    K := -1;
    L := -1;
    for I := Low(AItemArray) to High(AItemArray) do
    begin
      AItem := @AItemArray[I];
      if (AItem.FHandle < 0) or (MemoryUtils.IndexOf(FTOHArray, AItem.FHandle) < 0) then
        if J < 0 then
        begin
          if K < 0 then K := New;
          Add(MItemArray[K], AItem^);
          if L < 0 then
            L := Add(BItemArray, AItem.Text, AItem.FHandle, AItem.THandle)
          else
            BItemArray[L].Text := BItemArray[L].Text + Space + AItem.Text;
        end
        else begin
          K := New;
          Add(MItemArray[K], AItem^);
          if AItem.THandle < 0 then
            L := Add(BItemArray, AItem.Text, AItemArray[J].FHandle, AItemArray[J].THandle)
          else
            L := Add(BItemArray, AItem.Text, AItemArray[J].FHandle, AItem.THandle);
          J := -1;
        end
      else begin
        if (J >= 0) and (AItemArray[J].FHandle = FNegativeHandle^) then
          if AItemArray[I].FHandle = FNegativeHandle^ then
            AItemArray[I].FHandle := FPositiveHandle^
          else
            AItemArray[I].FHandle := FNegativeHandle^;
        J := I
      end;
    end;
    AItemArray := BItemArray;
  end;
end;

function TParser.Optimizable(var Index: NativeInt; const Number: Boolean; out Offset: Integer;
  out Parameter: Boolean; out Script: TScript): Boolean;
var
  AFunction: PFunction;
  Item: PScriptItem absolute Index;
begin
  Read(Self, Index, AFunction);
  Parameter := AFunction.Method.Parameter.Count > 0;
  if Parameter then
    if Item.Code = ParameterCode then
    begin
      Result := OptimizeParameterArray(Index + SizeOf(TCode), Script) and AFunction.Optimizable;
      Offset := SizeOf(TCode) + Item.Script.Header.ScriptSize;
      if Result then
      begin
        Script := nil;
        Inc(Index, Offset);
      end;
    end
    else raise Error(ScriptError)
  else begin
    Result := (AFunction.Method.Parameter.LParameter xor not Number) and AFunction.Optimizable;
    if Result and AFunction.Method.Parameter.RParameter then
      case Item.Code of
        NumberCode:
          Inc(Index, SizeOf(TCode) + SizeOf(TScriptNumber));
        FunctionCode:
          Result := Optimizable(Index, False, Offset, Parameter, Script);
        ScriptCode:
          begin
            InternalOptimize(Integer(@Item.Script.Header), Script);
            Result := Optimal(Script, stScript);
            Offset := SizeOf(TCode) + Item.Script.Header.ScriptSize;
            if Result then
            begin
              Item.Script.Header.Value := Execute(Script)^;
              Script := nil;
              Inc(Index, Offset);
            end;
          end;
      else raise Error(ScriptError);
      end;
  end;
end;

function TParser.OptimizationMethod(var Index: NativeInt; const Header: PScriptHeader; const ItemHeader: PItemHeader;
  const Item: PScriptItem; const Data: Pointer): Boolean;
var
  Start: Integer absolute ItemHeader;
  I, J: NativeInt;
  Offset: Integer;
  AData: POptimizationData absolute Data;
  Parameter: Boolean;
  Script: TScript;
begin
  if (Index - Start = SizeOf(TItemHeader)) or not Assigned(AData.ItemArray) then
  begin
    I := Add(AData.ItemArray, nil);
    Resize(AData.ItemArray[I], SizeOf(TItemHeader));
  end
  else I := High(AData.ItemArray);
  case Item.Code of
    NumberCode:
      begin
        AData.Number := MakeNumber(Item.ScriptNumber.Value, True);
        Inc(Index, SizeOf(TCode) + SizeOf(TScriptNumber));
      end;
    FunctionCode:
      begin
        J := Index;
        try
          if Optimizable(Index, AData.Number.Alive, Offset, Parameter, Script) then
          begin
            AData.Number.Value := ExecuteFunction(J, ItemHeader, AData.Number.Value);
            AData.Number.Alive := True;
          end
          else begin
            if AData.Number.Alive then
            begin
              WriteNumber(AData.ItemArray[I], AData.Number.Value);
              AData.Number.Alive := False;
            end;
            Add(AData.ItemArray[I], PAnsiChar(J), Index - J);
            if Assigned(Script) then
            begin
              WriteScript(AData.ItemArray[I], Script, Parameter);
              Inc(Index, Offset);
            end;
          end;
        finally
          Script := nil;
        end;
      end;
    ScriptCode:
      begin
        InternalOptimize(Integer(@Item.Script.Header), Script);
        try
          AData.Number.Alive := Optimal(Script, stScript);
          if AData.Number.Alive then
            AData.Number.Value := Execute(Script)^
          else
            WriteScript(AData.ItemArray[I], Script, False);
        finally
          Script := nil;
        end;
        Inc(Index, SizeOf(TCode) + Item.Script.Header.ScriptSize);
      end;
  else raise Error(ScriptError);
  end;
  if Index - Start >= ItemHeader.Size then
  begin
    if AData.Number.Alive then
    begin
      WriteNumber(AData.ItemArray[I], AData.Number.Value);
      AData.Number.Alive := False;
    end;
    PItemHeader(AData.ItemArray[I])^ := ItemHeader^;
    PItemHeader(AData.ItemArray[I]).Size := Length(AData.ItemArray[I]);
  end;
  Result := True;
end;

procedure TParser.Optimize(const Source: TScript; out Target: TScript);
begin
  InternalOptimize(Integer(Source), Target);
end;

procedure TParser.Optimize;
begin
  Optimize(FScript);
end;

procedure TParser.Optimize(var Script: TScript);
var
  AScript: TScript;
begin
  Optimize(Script, AScript);
  Script := AScript;
end;

function TParser.OptimizeParameterArray(Index: Integer; out Script: TScript): Boolean;
var
  Data: TParameterOptimizationData;
begin
  FillChar(Data, SizeOf(TParameterOptimizationData), 0);
  Data.Optimal := True;
  try
    ParseScript(Index, ParameterOptimizationMethod, @Data);
    Result := Data.Optimal;
    BuildScript(Self, Data.ItemArray, Script, False);
  finally
    Delete(Data.ItemArray);
  end;
end;

function TParser.Order(const Coverage: TPriorityCoverage; var ItemArray: TTextItemArray; var SA: TScriptArray; var Idle: PFunction): Boolean;

  function Peek(const FHandle: Integer; const IgnorePriority: Boolean; out AFunction: PFunction): Boolean;
  begin
    if FHandle < 0 then
      AFunction := nil
    else
      AFunction := GetFunction(FHandle);
    Result := Assigned(AFunction) and (IgnorePriority or (AFunction.Priority.Priority <> fpNormal) and (AFunction.Priority.Coverage = Coverage) and
      (not Assigned(Idle) or (Idle <> AFunction))) and (AFunction.Method.Parameter.LParameter or AFunction.Method.Parameter.RParameter);
  end;

  function Same(const AItemArray, BItemArray: TTextItemArray): Boolean;
  var
    I: Integer;
  begin
    Result := (Length(AItemArray) = Length(BItemArray));
    if Result then
      for I := Low(AItemArray) to High(AItemArray) do
      begin
        Result := TextUtils.SameText(AItemArray[I].Text, BItemArray[I].Text) and (AItemArray[I].FHandle = BItemArray[I].FHandle);
        if not Result then Break;
      end;
  end;

  function ItemText(const Item: PTextItem): string;
  var
    AType: PType;
  begin
    if GetType(Item.THandle, AType) then
      Result := string(AType.Name) + Space + Item.Text
    else
      Result := Item.Text;
  end;

var
  I, J, K, Index: Integer;
  L: array[Boolean] of Integer;
  Flag: Boolean;
  AFunction, BFunction: PFunction;
  S, AItem, BItem: string;
  AItemArray, BItemArray: TTextItemArray;
begin
  Result := Assigned(ItemArray);
  if Result then
  begin
    FillChar(L, SizeOf(L), 0);
    for I := Low(ItemArray) to High(ItemArray) do
      if ItemArray[I].FHandle >= 0 then
      begin
        for Flag := False to True do if Peek(ItemArray[I].FHandle, Flag, AFunction) then
        begin
          Inc(L[Flag]);
          Break;
        end;
        if (L[False] > 0) and (L[True] > 0) or (L[False] > 1) then Break;
      end;
    Result := (L[False] > 0) and (L[True] > 0) or (L[False] > 1);
    if Result then
    begin
      I := 0;
      while I < Length(ItemArray) do
      begin
        if Peek(ItemArray[I].FHandle, False, AFunction) then
        begin
          AItemArray := nil;
          case AFunction.Priority.Priority of
            fpLower:
              begin
                if AFunction.Method.Parameter.LParameter then
                  if (I - 1 = Low(ItemArray)) and ((ItemArray[I - 1].FHandle < 0) or (ItemArray[I - 1].FHandle = FInternalHandle)) then
                    Add(AItemArray, ItemArray[I - 1])
                  else begin
                    S := '';
                    for J := Low(ItemArray) to I - 1 do
                      if J = Low(ItemArray) then
                        S := ItemText(@ItemArray[J])
                      else
                        S := S + Space + ItemText(@ItemArray[J]);
                    J := AFunction.Handle^;
                    S := Embrace(IntToStr(Add(SA, InternalCompile(Trim(S), SA, False, Idle))), BracketArray[btBrace]);
                    if not GetFunction(J, AFunction) then raise Error(ScriptError);
                    Add(AItemArray, S, FInternalHandle, -1);
                  end
                else for J := Low(ItemArray) to I - 1 do Add(AItemArray, ItemArray[J]);
                Index := Add(AItemArray, ItemArray[I]);
                if AFunction.Method.Parameter.RParameter then
                begin
                  if (I + 1 = High(ItemArray)) and ((ItemArray[I + 1].FHandle < 0) or (ItemArray[I + 1].FHandle = FInternalHandle)) then
                    Add(AItemArray, ItemArray[I + 1])
                  else begin
                    S := '';
                    for J := I + 1 to High(ItemArray) do
                      if J = I + 1 then
                        S := ItemText(@ItemArray[J])
                      else
                        S := S + Space + ItemText(@ItemArray[J]);
                    J := AFunction.Handle^;
                    S := Embrace(IntToStr(Add(SA, InternalCompile(Trim(S), SA, False, Idle))), BracketArray[btBrace]);
                    if not GetFunction(J, AFunction) then raise Error(ScriptError);
                    Add(AItemArray, S, FInternalHandle, -1);
                  end;
                  ItemArray := AItemArray;
                  Break;
                end
                else for J := I + 1 to High(ItemArray) do Add(AItemArray, ItemArray[J]);
                I := Index + 1;
              end;
          else
            AItem := '';
            BItem := '';
            try
              if AFunction.Method.Parameter.LParameter then
              begin
                for J := I - 1 downto Low(ItemArray) do
                begin
                  if J = I - 1 then
                    AItem := ItemText(@ItemArray[J])
                  else
                    AItem := ItemText(@ItemArray[J]) + Space + AItem;
                  Add(BItemArray, ItemArray[J]);
                  if not GetFunction(ItemArray[J].FHandle, BFunction) or not BFunction.Method.Parameter.LParameter then
                  begin
                    for K := Low(ItemArray) to J - 1 do Add(AItemArray, ItemArray[K]);
                    Break;
                  end;
                end;
                AItem := Trim(AItem);
              end
              else for J := Low(ItemArray) to I - 1 do Add(AItemArray, ItemArray[J]);
              Index := Add(AItemArray, '', -1, -1);
              Add(BItemArray, ItemArray[I]);
              if AFunction.Method.Parameter.RParameter then
              begin
                for J := I + 1 to High(ItemArray) do
                begin
                  if J = I + 1 then
                    BItem := ItemText(@ItemArray[J])
                  else
                    BItem := BItem + Space + ItemText(@ItemArray[J]);
                  Add(BItemArray, ItemArray[J]);
                  if not GetFunction(ItemArray[J].FHandle, BFunction) or not BFunction.Method.Parameter.RParameter then
                  begin
                    for K := J + 1 to High(ItemArray) do Add(AItemArray, ItemArray[K]);
                    Break;
                  end;
                end;
                BItem := Trim(BItem);
              end
              else for J := I + 1 to High(ItemArray) do Add(AItemArray, ItemArray[J]);
              S := AFunction.Name;
              if AItem <> '' then S := AItem + Space + S;
              if BItem <> '' then S := S + Space + BItem;
              if Same(ItemArray, BItemArray) then Break
              else begin
                S := Embrace(IntToStr(Add(SA, InternalCompile(S, SA, False, AFunction))), BracketArray[btBrace]);
                AItemArray[Index] := MakeTextItem(S, FInternalHandle, -1);
              end;
            finally
              BItemArray := nil;
            end;
          end;
          ItemArray := AItemArray;
        end
        else Inc(I);
      end;
    end;
  end;
end;

function TParser.ParameterArrayMethod(var AIndex: NativeInt; const Header: PScriptHeader;
  const ItemHeader: PItemHeader; const Item: PScriptItem;
  const Data: Pointer): Boolean;
var
  AData: PParameterArrayData absolute Data;
  I, J: Integer;
  Start: Integer absolute ItemHeader;
begin
  I := -1;
  case Item.Code of
    NumberCode:
      begin
        if not AData.Fake then
          I := Add(AData.PA^, ItemHeader.UserType.Handle, Item.ScriptNumber.Value);
        Inc(AIndex, SizeOf(TCode) + SizeOf(TScriptNumber));
      end;
    FunctionCode:
      begin
        if AData.Fake then
          ExecuteFunction(AIndex, ItemHeader, EmptyValue, AData.Fake)
        else
          I := Add(AData.PA^, ItemHeader.UserType.Handle, ExecuteFunction(AIndex, ItemHeader, EmptyValue));
      end;
    StringCode:
      begin
        if not AData.Fake then
        begin
          I := Add(AData.PA^, ItemHeader.UserType.Handle, '');
          J := Item.ScriptString.Size;
          if J > SizeOf(TString) then J := SizeOf(TString);
          ZeroMemory(@AData.PA^[I].Text, SizeOf(TString));
          CopyMemory(@AData.PA^[I].Text, PAnsiChar(AIndex) + SizeOf(TCode) + SizeOf(TScriptString), J);
        end;
        Inc(AIndex, SizeOf(TCode) + SizeOf(TScriptString) + Item.ScriptString.Size);
      end;
    ScriptCode:
      begin
        if not AData.Fake then
          I := Add(AData.PA^, ItemHeader.UserType.Handle, Execute(Integer(@Item.Script.Header))^);
        Inc(AIndex, SizeOf(TCode) + Item.Script.Header.ScriptSize);
      end;
  else raise Error(ScriptError);
  end;
  if AIndex - Start >= ItemHeader.Size then
  begin
    if (I >= 0) and Boolean(ItemHeader.Sign) then AData.PA^[I].Value := Negative(AData.PA^[I].Value);
    AData.Index^ := AIndex;
  end;
  Result := True;
end;

function TParser.ParameterOptimizationMethod(var Index: NativeInt; const Header: PScriptHeader;
  const ItemHeader: PItemHeader; const Item: PScriptItem;
  const Data: Pointer): Boolean;
var
  Start: Integer absolute ItemHeader;
  I, J: NativeInt;
  Offset: Integer;
  AData: PParameterOptimizationData absolute Data;
  Parameter: Boolean;
  Script: TScript;
begin
  if (Index - Start = SizeOf(TItemHeader)) or not Assigned(AData.ItemArray) then
  begin
    I := Add(AData.ItemArray, nil);
    Resize(AData.ItemArray[I], SizeOf(TItemHeader));
  end
  else I := High(AData.ItemArray);
  case Item.Code of
    NumberCode:
      begin
        WriteNumber(AData.ItemArray[I], Item.ScriptNumber.Value);
        Inc(Index, SizeOf(TCode) + SizeOf(TScriptNumber));
      end;
    FunctionCode:
      begin
        J := Index;
        try
          if Optimizable(Index, False, Offset, Parameter, Script) then
            WriteNumber(AData.ItemArray[I], ExecuteFunction(J, ItemHeader, EmptyValue))
          else begin
            Add(AData.ItemArray[I], PAnsiChar(J), Index - J);
            if Assigned(Script) then
            begin
              WriteScript(AData.ItemArray[I], Script, Parameter);
              Inc(Index, Offset);
            end;
            AData.Optimal := False;
          end;
        finally
          Script := nil;
        end;
      end;
    StringCode:
      begin
        J := SizeOf(TCode) + SizeOf(TScriptString);
        WriteString(AData.ItemArray[I], PAnsiChar(Index) + J, Item.ScriptString.Size);
        Inc(Index, J + Item.ScriptString.Size);
      end;
    ScriptCode:
      begin
        InternalOptimize(Integer(@Item.Script.Header), Script);
        try
          if Optimal(Script, stScript) then
            WriteNumber(AData.ItemArray[I], Execute(Script)^)
          else begin
            WriteScript(AData.ItemArray[I], Script, False);
            AData.Optimal := False;
          end;
        finally
          Script := nil;
        end;
        Inc(Index, SizeOf(TCode) + Item.Script.Header.ScriptSize);
      end;
  else raise Error(ScriptError);
  end;
  if Index - Integer(ItemHeader) >= ItemHeader.Size then
  begin
    PItemHeader(AData.ItemArray[I])^ := ItemHeader^;
    PItemHeader(AData.ItemArray[I]).Size := Length(AData.ItemArray[I]);
  end;
  Result := True;
end;

procedure TParser.Parse(var Text: string; var SA: TScriptArray; var Idle: PFunction; out Error: TError);
var
  Data: TBracketData;
  I: Boolean;
begin
  FillChar(Error, SizeOf(TError), 0);
  Data := MakeBracketData(@SA, False, Idle);
  Data.Text := TTextBuilder.Create;
  try
    for I := Low(Boolean) to High(Boolean) do
    begin
      Data.Parameter := I;
      if Data.Parameter then
        ParseBracket(Text, FParameterBracket, ParseMethod, @Data, Error)
      else
        ParseBracket(Text, FBracket, ParseMethod, @Data, Error);
      if Error.ErrorType <> etSuccess then Break;
    end;
  finally
    Data.Text.Free;
  end;
end;

procedure TParser.Parse(var Text: string; var SA: TScriptArray; var Idle: PFunction);
var
  Error: TError;
begin
  Parse(Text, SA, Idle, Error);
  if Error.ErrorType <> etSuccess then raise ParseErrors.Error(Error.ErrorText);
end;

function TParser.ParseMethod(var Text: string; const FromIndex, TillIndex: Integer; const Data: Pointer): TError;
var
  S: string;
  AData: PBracketData absolute Data;
  I: Integer;
begin
  S := Trim(Copy(Text, FromIndex + 1, TillIndex - FromIndex - 1));
  if S <> '' then
  begin
    I := Add(AData.SA^, InternalCompile(S, AData.SA^, AData.Parameter, AData.Idle, Result));
    if Result.ErrorType <> etSuccess then Exit;
    if AData.Parameter then
      S := ParameterPrefix
    else
      S := '';
    try
      AData.Text.Append(Copy(Text, 1, FromIndex - 1));
      AData.Text.Append(Embrace(S + IntToStr(I), BracketArray[btBrace]));
      AData.Text.Append(Copy(Text, TillIndex + 1, MaxInt));
      Text := AData.Text.Text;
    finally
      AData.Text.Clear;
    end;
  end
  else begin
    Result := MakeError(etTextError, EText(TextError, [Text]));
    Exit;
  end;
  FillChar(Result, SizeOf(TError), 0);
end;

procedure TParser.ReadParameterArray(var Index: NativeInt; out PA: TParameterArray; const ParameterType: TParameterType;
  const Fake: Boolean);
var
  Data: TParameterArrayData;
begin
  Data := MakeParameterArrayData(Fake, @PA, @Index);
  case ParameterType of
    ptParameter: ParseScript(Index, ParameterArrayMethod, @Data);
    ptScript: ParseScript(Index, ScriptArrayMethod, @Data);
  end;
end;

function TParser.ScriptArrayMethod(var AIndex: NativeInt; const Header: PScriptHeader;
  const ItemHeader: PItemHeader; const Item: PScriptItem;
  const Data: Pointer): Boolean;
var
  AData: PParameterArrayData absolute Data;
  Value: TValue;
  Start: Integer absolute ItemHeader;
begin
  if not AData.Fake then
  begin
    with Value do
    begin
      Int64Rec.Lo := Integer(Item);
      Int64Rec.Hi := Integer(ItemHeader);
      ValueType := vtInt64;
    end;
    Add(AData.PA^, ItemHeader.UserType.Handle, Value);
  end;
  case Item.Code of
    NumberCode:
      Inc(AIndex, SizeOf(TCode) + SizeOf(TScriptNumber));
    FunctionCode:
      ExecuteFunction(AIndex, ItemHeader, EmptyValue, True);
    StringCode:
      Inc(AIndex, SizeOf(TCode) + SizeOf(TScriptString) + Item.ScriptString.Size);
    ScriptCode:
      Inc(AIndex, SizeOf(TCode) + Item.Script.Header.ScriptSize);
  else raise Error(ScriptError);
  end;
  if AIndex - Start >= ItemHeader.Size then AData.Index^ := AIndex;
  Result := True;
end;

function TParser.ScriptToString(const Script: TScript; const Delimiter: string; const TypeMode: TRetrieveMode): string;
begin
  Result := ScriptToString(Script, Delimiter, FBracket, TypeMode);
end;

function TParser.ScriptToString(const Script: TScript; const TypeMode: TRetrieveMode): string;
begin
  Result := ScriptToString(Script, Space, FBracket, TypeMode);
end;

function TParser.ScriptToString(const TypeMode: TRetrieveMode): string;
begin
  Result := ScriptToString(FScript, TypeMode);
end;

function TParser.ScriptToString(const Script: TScript; const Delimiter: string; const ABracket: TBracket;
  const TypeMode: TRetrieveMode): string;
begin
  Result := InternalDecompile(Integer(Script), Delimiter, False, ABracket, TypeMode);
end;

procedure TParser.StringToScript(const Text: string; out Script: TScript);
var
  Error: TError;
begin
  StringToScript(Text, Script, Error);
  if Error.ErrorType <> etSuccess then raise ParseErrors.Error(Error.ErrorText);
end;

procedure TParser.StringToScript(const Text: string; out Script: TScript; out Error: TError);
const
  ScriptSize = SizeOf(TScriptHeader) + SizeOf(TItemHeader) + SizeOf(TCode);
var
  AText: string;
  Idle: PFunction;
  SA: TScriptArray;
begin
  FillChar(Error, SizeOf(TError), 0);
  Script := nil;
  if FCached and Assigned(FCache.Rawscript) then
  begin
    Enter(FCache.Rawscript.Lock^);
    try
      Script := FCache.Rawscript.Find(Text);
    finally
      Leave(FCache.Rawscript.Lock^);
    end;
  end;
  if not Assigned(Script) then
  begin
    AText := Trim(Text);
    Error := Validator.Check(AText, rtText);
    if Error.ErrorType <> etSuccess then Exit;
    ParseUtils.Prepare(FFData);
    if ParseUtils.Prepare(FTData) then FDefaultTypeHandle := MakeTypeHandle(FTData^, FDefaultValueType, -1);
    Error := Check(AText);
    if Error.ErrorType <> etSuccess then Exit;
    ChangeBracket(AText, FFData, FBracket, FParameterBracket);
    Idle := nil;
    try
      try
        Script := InternalCompile(AText, SA, False, Idle, Error);
      except
        on E: Exception do Error := MakeError(etUnknown, E.Message);
      end;
      if Error.ErrorType <> etSuccess then Exit;
    finally
      Delete(SA);
    end;
    if Length(Script) < ScriptSize then
    begin
      Error := MakeError(etSizeError, SizeError);
      Exit;
    end;
    if FCached and Assigned(FCache.Rawscript) then
    begin
      Enter(FCache.Rawscript.Lock^);
      try
        FCache.Rawscript.Add(Text, Script);
      finally
        Leave(FCache.Rawscript.Lock^);
      end;
    end;
  end
  else FillChar(Error, SizeOf(TError), 0);
end;

procedure TParser.StringToScript;
begin
  StringToScript(FText, FScript);
end;

procedure TParser.StringToScript(const AText: string);
begin
  StringToScript(AText, FScript);
end;

{ TMathParser }

constructor TMathParser.Create(AOwner: TComponent);
begin
  inherited;
  FMathMethod := TMathMethod.Create(Self);
  BeginUpdate;
  try
    InternalAddFunction(IntegerDivideFunctionName, FDivHandle, fkMethod, MakeFunctionMethod(FMathMethod.DivMethod), True);
    InternalAddFunction(ReminderFunctionName, FModHandle, fkMethod, MakeFunctionMethod(FMathMethod.ModMethod), True);
    InternalAddFunction(DegreeFunctionName, FDegreeHandle, fkMethod, MakeFunctionMethod(FMathMethod.DegreeMethod), True, vtUnknown, fpHigher, pcLocal);
    InternalAddFunction(FactorialFunctionName, FFactorialHandle, fkMethod, MakeFunctionMethod(FMathMethod.FactorialMethod, True, False), True);
    InternalAddFunction(SqrFunctionName, FSqrHandle, fkMethod, MakeFunctionMethod(FMathMethod.SqrMethod, False, True), True);
    InternalAddFunction(SqrtFunctionName, FSqrtHandle, fkMethod, MakeFunctionMethod(FMathMethod.SqrtMethod, False, True), True);
    InternalAddFunction(IntFunctionName, FIntHandle, fkMethod, MakeFunctionMethod(FMathMethod.IntMethod, False, True), True);
    InternalAddFunction(RoundFunctionName, FRoundHandle, fkMethod, MakeFunctionMethod(FMathMethod.RoundMethod, False, True), True);
    InternalAddFunction(RoundToFunctionName, FRoundToHandle, fkMethod, MakeFunctionMethod(FMathMethod.RoundToMethod, RoundToParameterCount), True);
    InternalAddFunction(TruncFunctionName, FTruncHandle, fkMethod, MakeFunctionMethod(FMathMethod.TruncMethod, False, True), True);
    InternalAddFunction(AbsFunctionName, FAbsHandle, fkMethod, MakeFunctionMethod(FMathMethod.AbsMethod, False, True), True);
    InternalAddFunction(FracFunctionName, FFracHandle, fkMethod, MakeFunctionMethod(FMathMethod.FracMethod, False, True), True);
    InternalAddFunction(LnFunctionName, FLnHandle, fkMethod, MakeFunctionMethod(FMathMethod.LnMethod, False, True), True);
    InternalAddFunction(LgFunctionName, FLgHandle, fkMethod, MakeFunctionMethod(FMathMethod.LgMethod, False, True), True);
    InternalAddFunction(LogFunctionName, FLogHandle, fkMethod, MakeFunctionMethod(FMathMethod.LogMethod), True);
    InternalAddFunction(ExpFunctionName, FExpHandle, fkMethod, MakeFunctionMethod(FMathMethod.ExpMethod, False, True), True);
    InternalAddFunction(RandomFunctionName, FRandomHandle, fkMethod, MakeFunctionMethod(FMathMethod.RandomMethod, False, True), False);
    InternalAddFunction(SinFunctionName, FSinHandle, fkMethod, MakeFunctionMethod(FMathMethod.SinMethod, False, True), True);
    InternalAddFunction(ArcSinFunctionName, FArcSinHandle, fkMethod, MakeFunctionMethod(FMathMethod.ArcSinMethod, False, True), True);
    InternalAddFunction(SinHFunctionName, FSinHHandle, fkMethod, MakeFunctionMethod(FMathMethod.SinHMethod, False, True), True);
    InternalAddFunction(ArcSinHFunctionName, FArcSinHHandle, fkMethod, MakeFunctionMethod(FMathMethod.ArcSinHMethod, False, True), True);
    InternalAddFunction(CosFunctionName, FCosHandle, fkMethod, MakeFunctionMethod(FMathMethod.CosMethod, False, True), True);
    InternalAddFunction(ArcCosFunctionName, FArcCosHandle, fkMethod, MakeFunctionMethod(FMathMethod.ArcCosMethod, False, True), True);
    InternalAddFunction(CosHFunctionName, FCosHHandle, fkMethod, MakeFunctionMethod(FMathMethod.CosHMethod, False, True), True);
    InternalAddFunction(ArcCosHFunctionName, FArcCosHHandle, fkMethod, MakeFunctionMethod(FMathMethod.ArcCosHMethod, False, True), True);
    InternalAddFunction(TanFunctionName, FTanHandle, fkMethod, MakeFunctionMethod(FMathMethod.TanMethod, False, True), True);
    InternalAddFunction(ArcTanFunctionName, FArcTanHandle, fkMethod, MakeFunctionMethod(FMathMethod.ArcTanMethod, False, True), True);
    InternalAddFunction(TanHFunctionName, FTanHHandle, fkMethod, MakeFunctionMethod(FMathMethod.TanHMethod, False, True), True);
    InternalAddFunction(ArcTanHFunctionName, FArcTanHHandle, fkMethod, MakeFunctionMethod(FMathMethod.ArcTanHMethod, False, True), True);
    InternalAddFunction(CoTanFunctionName, FCoTanHandle, fkMethod, MakeFunctionMethod(FMathMethod.CoTanMethod, False, True), True);
    InternalAddFunction(ArcCotanFunctionName, FArcCoTanHandle, fkMethod, MakeFunctionMethod(FMathMethod.ArcCoTanMethod, False, True), True);
    InternalAddFunction(CotanHFunctionName, FCoTanHHandle, fkMethod, MakeFunctionMethod(FMathMethod.CoTanHMethod, False, True), True);
    InternalAddFunction(ArcCotanHFunctionName, FArcCoTanHHandle, fkMethod, MakeFunctionMethod(FMathMethod.ArcCoTanHMethod, False, True), True);
    InternalAddFunction(SecFunctionName, FSecHandle, fkMethod, MakeFunctionMethod(FMathMethod.SecMethod, False, True), True);
    InternalAddFunction(ArcSecFunctionName, FArcSecHandle, fkMethod, MakeFunctionMethod(FMathMethod.ArcSecMethod, False, True), True);
    InternalAddFunction(SecHFunctionName, FSecHHandle, fkMethod, MakeFunctionMethod(FMathMethod.SecHMethod, False, True), True);
    InternalAddFunction(ArcSecHFunctionName, FArcSecHHandle, fkMethod, MakeFunctionMethod(FMathMethod.ArcSecHMethod, False, True), True);
    InternalAddFunction(CscFunctionName, FCscHandle, fkMethod, MakeFunctionMethod(FMathMethod.CscMethod, False, True), True);
    InternalAddFunction(ArcCscFunctionName, FArcCscHandle, fkMethod, MakeFunctionMethod(FMathMethod.ArcCscMethod, False, True), True);
    InternalAddFunction(CscHFunctionName, FCscHHandle, fkMethod, MakeFunctionMethod(FMathMethod.CscHMethod, False, True), True);
    InternalAddFunction(ArcCscHFunctionName, FArcCscHHandle, fkMethod, MakeFunctionMethod(FMathMethod.ArcCscHMethod, False, True), True);
    InternalAddFunction(ArcTan2FunctionName, FArcTan2Handle, fkMethod, MakeFunctionMethod(FMathMethod.ArcTan2Method, ArcTan2ParameterCount), True);
    InternalAddFunction(HypotFunctionName, FHypotHandle, fkMethod, MakeFunctionMethod(FMathMethod.HypotMethod, HypotParameterCount), True);
    InternalAddFunction(RadToDegFunctionName, FRadToDegHandle, fkMethod, MakeFunctionMethod(FMathMethod.RadToDegMethod, False, True), True);
    InternalAddFunction(RadToGradFunctionName, FRadToGradHandle, fkMethod, MakeFunctionMethod(FMathMethod.RadToGradMethod, False, True), True);
    InternalAddFunction(RadToCycleFunctionName, FRadToCycleHandle, fkMethod, MakeFunctionMethod(FMathMethod.RadToCycleMethod, False, True), True);
    InternalAddFunction(DegToRadFunctionName, FDegToRadHandle, fkMethod, MakeFunctionMethod(FMathMethod.DegToRadMethod, False, True), True);
    InternalAddFunction(DegToGradFunctionName, FDegToGradHandle, fkMethod, MakeFunctionMethod(FMathMethod.DegToGradMethod, False, True), True);
    InternalAddFunction(DegToCycleFunctionName, FDegToCycleHandle, fkMethod, MakeFunctionMethod(FMathMethod.DegToCycleMethod, False, True), True);
    InternalAddFunction(GradToRadFunctionName, FGradToRadHandle, fkMethod, MakeFunctionMethod(FMathMethod.GradToRadMethod, False, True), True);
    InternalAddFunction(GradToDegFunctionName, FGradToDegHandle, fkMethod, MakeFunctionMethod(FMathMethod.GradToDegMethod, False, True), True);
    InternalAddFunction(GradToCycleFunctionName, FGradToCycleHandle, fkMethod, MakeFunctionMethod(FMathMethod.GradToCycleMethod, False, True), True);
    InternalAddFunction(CycleToRadFunctionName, FCycleToRadHandle, fkMethod, MakeFunctionMethod(FMathMethod.CycleToRadMethod, False, True), True);
    InternalAddFunction(CycleToDegFunctionName, FCycleToDegHandle, fkMethod, MakeFunctionMethod(FMathMethod.CycleToDegMethod, False, True), True);
    InternalAddFunction(CycleToGradFunctionName, FCycleToGradHandle, fkMethod, MakeFunctionMethod(FMathMethod.CycleToGradMethod, False, True), True);
    InternalAddFunction(LnXP1FunctionName, FLnXP1Handle, fkMethod, MakeFunctionMethod(FMathMethod.LnXP1Method, False, True), True);
    InternalAddFunction(Log10FunctionName, FLog10Handle, fkMethod, MakeFunctionMethod(FMathMethod.Log10Method, False, True), True);
    InternalAddFunction(Log2FunctionName, FLog2Handle, fkMethod, MakeFunctionMethod(FMathMethod.Log2Method, False, True), True);
    InternalAddFunction(IntPowerFunctionName, FIntPowerHandle, fkMethod, MakeFunctionMethod(FMathMethod.IntPowerMethod, IntPowerParameterCount), True);
    InternalAddFunction(PowerFunctionName, FPowerHandle, fkMethod, MakeFunctionMethod(FMathMethod.PowerMethod, PowerParameterCount), True);
    InternalAddFunction(LdexpFunctionName, FLdexpHandle, fkMethod, MakeFunctionMethod(FMathMethod.LdexpMethod, LdexpParameterCount), True);
    InternalAddFunction(CeilFunctionName, FCeilHandle, fkMethod, MakeFunctionMethod(FMathMethod.CeilMethod, False, True), True);
    InternalAddFunction(FloorFunctionName, FFloorHandle, fkMethod, MakeFunctionMethod(FMathMethod.FloorMethod, False, True), True);
    InternalAddFunction(PolyFunctionName, FPolyHandle, fkMethod, MakeFunctionMethod(FMathMethod.PolyMethod, PolyParameterCount), True);
    InternalAddFunction(MeanFunctionName, FMeanHandle, fkMethod, MakeFunctionMethod(FMathMethod.MeanMethod, MeanParameterCount), True);
    InternalAddFunction(SumFunctionName, FSumHandle, fkMethod, MakeFunctionMethod(FMathMethod.SumMethod, SumParameterCount), True);
    InternalAddFunction(SumIntFunctionName, FSumIntHandle, fkMethod, MakeFunctionMethod(FMathMethod.SumIntMethod, SumIntParameterCount), True);
    InternalAddFunction(SumOfSquaresFunctionName, FSumOfSquaresHandle, fkMethod, MakeFunctionMethod(FMathMethod.SumOfSquaresMethod, SumOfSquaresParameterCount), True);
    InternalAddFunction(MinValueFunctionName, FMinValueHandle, fkMethod, MakeFunctionMethod(FMathMethod.MinValueMethod, MinValueParameterCount), True);
    InternalAddFunction(MinIntValueFunctionName, FMinIntValueHandle, fkMethod, MakeFunctionMethod(FMathMethod.MinIntValueMethod, MinIntValueParameterCount), True);
    InternalAddFunction(MinFunctionName, FMinHandle, fkMethod, MakeFunctionMethod(FMathMethod.MinMethod, MinParameterCount), True);
    InternalAddFunction(MaxValueFunctionName, FMaxValueHandle, fkMethod, MakeFunctionMethod(FMathMethod.MaxValueMethod, MaxValueParameterCount), True);
    InternalAddFunction(MaxIntValueFunctionName, FMaxIntValueHandle, fkMethod, MakeFunctionMethod(FMathMethod.MaxIntValueMethod, MaxIntValueParameterCount), True);
    InternalAddFunction(MaxFunctionName, FMaxHandle, fkMethod, MakeFunctionMethod(FMathMethod.MaxMethod, MaxParameterCount), True);
    InternalAddFunction(StdDevFunctionName, FStdDevHandle, fkMethod, MakeFunctionMethod(FMathMethod.StdDevMethod, StdDevParameterCount), True);
    InternalAddFunction(PopnStdDevFunctionName, FPopnStdDevHandle, fkMethod, MakeFunctionMethod(FMathMethod.PopnStdDevMethod, PopnStdDevParameterCount), True);
    InternalAddFunction(VarianceFunctionName, FVarianceHandle, fkMethod, MakeFunctionMethod(FMathMethod.VarianceMethod, VarianceParameterCount), True);
    InternalAddFunction(PopnVarianceFunctionName, FPopnVarianceHandle, fkMethod, MakeFunctionMethod(FMathMethod.PopnVarianceMethod, PopnVarianceParameterCount), True);
    InternalAddFunction(TotalVarianceFunctionName, FTotalVarianceHandle, fkMethod, MakeFunctionMethod(FMathMethod.TotalVarianceMethod, TotalVarianceParameterCount), True);
    InternalAddFunction(NormFunctionName, FNormHandle, fkMethod, MakeFunctionMethod(FMathMethod.NormMethod, NormParameterCount), True);
    InternalAddFunction(RandGFunctionName, FRandGHandle, fkMethod, MakeFunctionMethod(FMathMethod.RandGMethod, RandGParameterCount), True);
    InternalAddFunction(RandomRangeFunctionName, FRandomRangeHandle, fkMethod, MakeFunctionMethod(FMathMethod.RandomRangeMethod, RandomRangeParameterCount), False);
    InternalAddFunction(RandomFromFunctionName, FRandomFromHandle, fkMethod, MakeFunctionMethod(FMathMethod.RandomFromMethod, RandomFromParameterCount), False);
    InternalAddFunction(YearFunctionName, FYearHandle, fkMethod, MakeFunctionMethod(FMathMethod.YearMethod), False);
    InternalAddFunction(MonthFunctionName, FMonthHandle, fkMethod, MakeFunctionMethod(FMathMethod.MonthMethod), False);
    InternalAddFunction(DayFunctionName, FDayHandle, fkMethod, MakeFunctionMethod(FMathMethod.DayMethod), False);
    InternalAddFunction(DayOfWeekFunctionName, FDayOfWeekHandle, fkMethod, MakeFunctionMethod(FMathMethod.DayOfWeekMethod), False);
    InternalAddFunction(HourFunctionName, FHourHandle, fkMethod, MakeFunctionMethod(FMathMethod.HourMethod), False);
    InternalAddFunction(MinuteFunctionName, FMinuteHandle, fkMethod, MakeFunctionMethod(FMathMethod.MinuteMethod), False);
    InternalAddFunction(SecondFunctionName, FSecondHandle, fkMethod, MakeFunctionMethod(FMathMethod.SecondMethod), False);
    InternalAddFunction(MSecondFunctionName, FMSecondHandle, fkMethod, MakeFunctionMethod(FMathMethod.MSecondMethod), False);
    InternalAddFunction(TimeFunctionName, FTimeHandle, fkMethod, MakeFunctionMethod(FMathMethod.TimeMethod), False);
    InternalAddFunction(DateFunctionName, FDateHandle, fkMethod, MakeFunctionMethod(FMathMethod.DateMethod), False);
    InternalAddFunction(GetYearFunctionName, FGetYearHandle, fkMethod, MakeFunctionMethod(FMathMethod.GetYearMethod, GetYearParameterCount), True);
    InternalAddFunction(GetMonthFunctionName, FGetMonthHandle, fkMethod, MakeFunctionMethod(FMathMethod.GetMonthMethod, GetMonthParameterCount), True);
    InternalAddFunction(GetDayFunctionName, FGetDayHandle, fkMethod, MakeFunctionMethod(FMathMethod.GetDayMethod, GetDayParameterCount), True);
    InternalAddFunction(GetDayOfWeekFunctionName, FGetDayOfWeekHandle, fkMethod, MakeFunctionMethod(FMathMethod.GetDayOfWeekMethod, GetDayOfWeekParameterCount), True);
    InternalAddFunction(GetHourFunctionName, FGetHourHandle, fkMethod, MakeFunctionMethod(FMathMethod.GetHourMethod, GetHourParameterCount), True);
    InternalAddFunction(GetMinuteFunctionName, FGetMinuteHandle, fkMethod, MakeFunctionMethod(FMathMethod.GetMinuteMethod, GetMinuteParameterCount), True);
    InternalAddFunction(GetSecondFunctionName, FGetSecondHandle, fkMethod, MakeFunctionMethod(FMathMethod.GetSecondMethod, GetSecondParameterCount), True);
    InternalAddFunction(GetMSecondFunctionName, FGetMSecondHandle, fkMethod, MakeFunctionMethod(FMathMethod.GetMSecondMethod, GetMSecondParameterCount), True);
    InternalAddFunction(EncodeTimeFunctionName, FEncodeTimeHandle, fkMethod, MakeFunctionMethod(FMathMethod.EncodeTimeMethod, EncodeTimeParameterCount), True);
    InternalAddFunction(EncodeDateFunctionName, FEncodeDateHandle, fkMethod, MakeFunctionMethod(FMathMethod.EncodeDateMethod, EncodeDateParameterCount), True);
    InternalAddFunction(EncodeDateTimeFunctionName, FEncodeDateTimeHandle, fkMethod, MakeFunctionMethod(FMathMethod.EncodeDateTimeMethod, EncodeDateTimeParameterCount), True);
    InternalAddFunction(GetTickCountFunctionName, FGetTickCountHandle, fkMethod, MakeFunctionMethod(FMathMethod.GetTickCount), False, vtLongword);
    InternalAddConstant(PiConstantName, MakeValue(Pi));
    InternalAddConstant(KilobyteConstantName, MakeValue(Kilobyte));
    InternalAddConstant(MegabyteConstantName, MakeValue(Megabyte));
    InternalAddConstant(GigabyteConstantName, MakeValue(Gigabyte));
    InternalAddConstant(TerabyteConstantName, MakeValue(Terabyte));
    InternalAddConstant(PetabyteConstantName, MakeValue(Petabyte));
    InternalAddConstant(MinShortintConstantName, MakeValue(- High(Shortint) - 1));
    InternalAddConstant(MaxShortintConstantName, MakeValue(High(Shortint)));
    InternalAddConstant(MinByteConstantName, MakeValue(Byte(0)));
    InternalAddConstant(MaxByteConstantName, MakeValue(High(Byte)));
    InternalAddConstant(MinSmallintConstantName, MakeValue(- High(Smallint) - 1));
    InternalAddConstant(MaxSmallintConstantName, MakeValue(High(Smallint)));
    InternalAddConstant(MinWordConstantName, MakeValue(Byte(0)));
    InternalAddConstant(MaxWordConstantName, MakeValue(High(Word)));
    InternalAddConstant(MinIntegerConstantName, MakeValue(- High(Integer) - 1));
    InternalAddConstant(MaxIntegerConstantName, MakeValue(High(Integer)));
    InternalAddConstant(MinLongwordConstantName, MakeValue(Byte(0)));
    InternalAddConstant(MaxLongwordConstantName, MakeValue(High(Longword)));
    InternalAddConstant(MinInt64ConstantName, MakeValue(- High(Int64) - 1));
    InternalAddConstant(MaxInt64ConstantName, MakeValue(High(Int64)));
    InternalAddConstant(MinSingleConstantName, MakeValue(MinSingle));
    InternalAddConstant(MaxSingleConstantName, MakeValue(MaxSingle));
    InternalAddConstant(MinDoubleConstantName, MakeValue(MinDouble));
    InternalAddConstant(MaxDoubleConstantName, MakeValue(MaxDouble));
    InternalAddConstant(TinyConstantName, MakeValue(1E-16));
    InternalAddConstant(HugeConstantName, MakeValue(1 / 1E-16));
  finally
    EndUpdate;
  end;
end;

destructor TMathParser.Destroy;
begin
  FMathMethod.Free;
  inherited;
end;

initialization
  {$IFDEF DELPHI_XE}
  FormatSettings.DecimalSeparator := Dot;
  {$ELSE}
  DecimalSeparator := Dot;
  {$ENDIF}

end.
