unit FileTime;

{$B-}

interface

uses
  Windows, SysUtils, Classes, DateUtils;

type
  TCustomFileTime = class
  private
    FFileName: string;
    FATime: TDateTime;
    FCTime: TDateTime;
    FWTime: TDateTime;
  public
    constructor Create(const AFileName: string = ''); virtual;
    function UpdateFile: Boolean; virtual;
    function UpdateTime: Boolean; virtual;
    property FileName: string read FFileName write FFileName;
    property CTime: TDateTime read FCTime write FCTime;
    property ATime: TDateTime read FATime write FATime;
    property WTime: TDateTime read FWTime write FWTime;
  end;

  TTimeType = (ttCTime, ttATime, ttWTime);

  TFileTime = class(TCustomFileTime)
  private
    function GetDay(const TimeType: TTimeType): Word;
    function GetHour(const TimeType: TTimeType): Word;
    function GetMillisecond(const TimeType: TTimeType): Word;
    function GetMinute(const TimeType: TTimeType): Word;
    function GetMonth(const TimeType: TTimeType): Word;
    function GetSecond(const TimeType: TTimeType): Word;
    function GetYear(const TimeType: TTimeType): Word;
    procedure SetDay(const TimeType: TTimeType; const Value: Word);
    procedure SetHour(const TimeType: TTimeType; const Value: Word);
    procedure SetMillisecond(const TimeType: TTimeType; const Value: Word);
    procedure SetMinute(const TimeType: TTimeType; const Value: Word);
    procedure SetMonth(const TimeType: TTimeType; const Value: Word);
    procedure SetSecond(const TimeType: TTimeType; const Value: Word);
    procedure SetYear(const TimeType: TTimeType; const Value: Word);
  public
    property Year[const TimeType: TTimeType]: Word read GetYear write SetYear;
    property Month[const TimeType: TTimeType]: Word read GetMonth write SetMonth;
    property Day[const TimeType: TTimeType]: Word read GetDay write SetDay;
    property Hour[const TimeType: TTimeType]: Word read GetHour write SetHour;
    property Minute[const TimeType: TTimeType]: Word read GetMinute write SetMinute;
    property Second[const TimeType: TTimeType]: Word read GetSecond write SetSecond;
    property MilliSecond[const TimeType: TTimeType]: Word read GetMillisecond write SetMillisecond;
  end;

implementation

{ TCustomFileTime }

constructor TCustomFileTime.Create(const AFileName: string);
begin
  FFileName := AFileName;
  UpdateTime;
end;

function TCustomFileTime.UpdateTime: Boolean;
var
  Handle: THandle;
  DosCTime, DosATime, DosWTime: Integer;
  LocalCTime, LocalATime, LocalWTime, ACTime, AATime, AWTime: Windows.TFileTime;
begin
  Result := FileExists(FFileName);
  if Result then
  begin
    Handle := CreateFile(PChar(FFileName), GENERIC_READ, FILE_SHARE_READ or FILE_SHARE_WRITE, nil, OPEN_EXISTING, FILE_ATTRIBUTE_NORMAL, 0);
    Result := Handle <> INVALID_HANDLE_VALUE;
    if Result then
    try
      Result := GetFileTime(Handle, @ACTime, @AATime, @AWTime) and
        FileTimeToLocalFileTime(ACTime, LocalCTime) and
        FileTimeToDosDateTime(LocalCTime, LongRec(DosCTime).Hi, LongRec(DosCTime).Lo) and
        FileTimeToLocalFileTime(AATime, LocalATime) and
        FileTimeToDosDateTime(LocalATime, LongRec(DosATime).Hi, LongRec(DosATime).Lo) and
        FileTimeToLocalFileTime(AWTime, LocalWTime) and
        FileTimeToDosDateTime(LocalWTime, LongRec(DosWTime).Hi, LongRec(DosWTime).Lo);
      if Result then
      begin
        FCTime := FileDateToDateTime(DosCTime);
        FATime := FileDateToDateTime(DosATime);
        FWTime := FileDateToDateTime(DosWTime);
      end;
    finally
      CloseHandle(Handle);
    end;
  end;
end;

function TCustomFileTime.UpdateFile: Boolean;
var
  Handle: THandle;
  DosCTime, DosATime, DosWTime: Integer;
  LocalCTime, LocalATime, LocalWTime, ACTime, AATime, AWTime: Windows.TFileTime;
begin
  Result := FileExists(FFileName);
  if Result then
  begin
    Handle := CreateFile(PChar(FFileName), GENERIC_READ or GENERIC_WRITE, FILE_SHARE_READ or FILE_SHARE_WRITE, nil, OPEN_EXISTING, FILE_ATTRIBUTE_NORMAL, 0);
    Result := Handle <> INVALID_HANDLE_VALUE;
    if Result then
    try
      DosCTime := DateTimeToFileDate(FCTime);
      DosATime := DateTimeToFileDate(FATime);
      DosWTime := DateTimeToFileDate(FWTime);
      Result := DosDateTimeToFileTime(LongRec(DosCTime).Hi, LongRec(DosCTime).Lo, LocalCTime) and
        LocalFileTimeToFileTime(LocalCTime, ACTime) and
        DosDateTimeToFileTime(LongRec(DosATime).Hi, LongRec(DosATime).Lo, LocalATime) and
        LocalFileTimeToFileTime(LocalATime, AATime) and
        DosDateTimeToFileTime(LongRec(DosWTime).Hi, LongRec(DosWTime).Lo, LocalWTime) and
        LocalFileTimeToFileTime(LocalWTime, AWTime) and
        SetFileTime(Handle, @ACTime, @AATime, @AWTime);
    finally
      CloseHandle(Handle);
    end;
  end;
end;

{ TFileTime }

function TFileTime.GetDay(const TimeType: TTimeType): Word;
begin
  case TimeType of
    ttCTime: Result := DayOf(FCTime);
    ttATime: Result := DayOf(FATime);
    ttWTime: Result := DayOf(FWTime);
  else Result := 0;
  end;
end;

function TFileTime.GetHour(const TimeType: TTimeType): Word;
begin
  case TimeType of
    ttCTime: Result := HourOf(FCTime);
    ttATime: Result := HourOf(FATime);
    ttWTime: Result := HourOf(FWTime);
  else Result := 0;
  end;
end;

function TFileTime.GetMillisecond(const TimeType: TTimeType): Word;
begin
  case TimeType of
    ttCTime: Result := MillisecondOf(FCTime);
    ttATime: Result := MillisecondOf(FATime);
    ttWTime: Result := MillisecondOf(FWTime);
  else Result := 0;
  end;
end;

function TFileTime.GetMinute(const TimeType: TTimeType): Word;
begin
  case TimeType of
    ttCTime: Result := MinuteOf(FCTime);
    ttATime: Result := MinuteOf(FATime);
    ttWTime: Result := MinuteOf(FWTime);
  else Result := 0;
  end;
end;

function TFileTime.GetMonth(const TimeType: TTimeType): Word;
begin
  case TimeType of
    ttCTime: Result := MonthOf(FCTime);
    ttATime: Result := MonthOf(FATime);
    ttWTime: Result := MonthOf(FWTime);
  else Result := 0;
  end;
end;

function TFileTime.GetSecond(const TimeType: TTimeType): Word;
begin
  case TimeType of
    ttCTime: Result := SecondOf(FCTime);
    ttATime: Result := SecondOf(FATime);
    ttWTime: Result := SecondOf(FWTime);
  else Result := 0;
  end;
end;

function TFileTime.GetYear(const TimeType: TTimeType): Word;
begin
  case TimeType of
    ttCTime: Result := YearOf(FCTime);
    ttATime: Result := YearOf(FATime);
    ttWTime: Result := YearOf(FWTime);
  else Result := 0;
  end;
end;

procedure TFileTime.SetDay(const TimeType: TTimeType; const Value: Word);
begin
  case TimeType of
    ttCTime: FCTime := RecodeDay(FCTime, Value);
    ttATime: FATime := RecodeDay(FATime, Value);
    ttWTime: FWTime := RecodeDay(FWTime, Value);
  end;
end;

procedure TFileTime.SetHour(const TimeType: TTimeType; const Value: Word);
begin
  case TimeType of
    ttCTime: FCTime := RecodeHour(FCTime, Value);
    ttATime: FATime := RecodeHour(FATime, Value);
    ttWTime: FWTime := RecodeHour(FWTime, Value);
  end;
end;

procedure TFileTime.SetMillisecond(const TimeType: TTimeType; const Value: Word);
begin
  case TimeType of
    ttCTime: FCTime := RecodeMillisecond(FCTime, Value);
    ttATime: FATime := RecodeMillisecond(FATime, Value);
    ttWTime: FWTime := RecodeMillisecond(FWTime, Value);
  end;
end;

procedure TFileTime.SetMinute(const TimeType: TTimeType; const Value: Word);
begin
  case TimeType of
    ttCTime: FCTime := RecodeMinute(FCTime, Value);
    ttATime: FATime := RecodeMinute(FATime, Value);
    ttWTime: FWTime := RecodeMinute(FWTime, Value);
  end;
end;

procedure TFileTime.SetMonth(const TimeType: TTimeType; const Value: Word);
begin
  case TimeType of
    ttCTime: FCTime := RecodeMonth(FCTime, Value);
    ttATime: FATime := RecodeMonth(FATime, Value);
    ttWTime: FWTime := RecodeMonth(FWTime, Value);
  end;
end;

procedure TFileTime.SetSecond(const TimeType: TTimeType; const Value: Word);
begin
  case TimeType of
    ttCTime: FCTime := RecodeSecond(FCTime, Value);
    ttATime: FATime := RecodeSecond(FATime, Value);
    ttWTime: FWTime := RecodeSecond(FWTime, Value);
  end;
end;

procedure TFileTime.SetYear(const TimeType: TTimeType; const Value: Word);
begin
  case TimeType of
    ttCTime: FCTime := RecodeYear(FCTime, Value);
    ttATime: FATime := RecodeYear(FATime, Value);
    ttWTime: FWTime := RecodeYear(FWTime, Value);
  end;
end;

end.
