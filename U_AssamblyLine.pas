unit U_AssamblyLine;

interface
uses SysUtils,Vcl.ExtCtrls, Math, CodeSiteLogging, DateUtils, mTXTController;

type TAssamblyLine=class(TObject)
  type direction=(Up,Down,Stop);

  private
    Line:TTXTController;

    DriveTimer:TTimer;
    RunTimer:TTimer;
    AliveTimer:TTimer;

    AliTime:integer;
    RunTime:integer;
  public
    DrivingFlag:direction;

    constructor Create(Ip_Adresse:string);
    destructor Destroy();Override;
  private
    procedure drive(Side:direction);

    procedure DriveTimerTimer(Sender:TObject);
    procedure RunTimerTimer(Sender:TObject);
    procedure AliveTimerTimer(Sender:TObject);
    function AliveTime(seconds:integer):integer;
end;

implementation

{ AssamblyLine }

procedure TAssamblyLine.AliveTimerTimer(Sender: TObject);
begin
  Line.HoleStatus;
end;

function TAssamblyLine.AliveTime(seconds: integer): integer;
begin
  AliTime:=round(seconds*62.5);
  result:=AliTime;
end;

constructor TAssamblyLine.Create(Ip_Adresse:string);
begin
  RunTime:=0;
  AliTime:=AliveTime(1);//seconds

  AliveTimer:=TTimer.Create(nil);
  AliveTimer.OnTimer:=AliveTimerTimer;
  AliveTimer.Interval:=250;
  AliveTimer.Enabled:=True;

  RunTimer:=TTimer.Create(nil);
  RunTimer.OnTimer:=RunTimerTimer;
  RunTimer.Interval:=1;
  DrivingFlag:=Stop;

  DriveTimer:=TTimer.Create(nil);
  DriveTimer.OnTimer:=DriveTimerTimer;
  DriveTimer.Interval:=1;
  DriveTimer.Enabled:=True;

  Try
  Line:=TTXTController.Create(Ip_Adresse);
  Line.Verbinden;
  CodeSiteLogging.CodeSite.Send('Connected Line');
  Except
  CodeSiteLogging.CodeSite.Send('Can not connect');
  end;
end;

destructor TAssamblyLine.Destroy;
begin
  Line.Trennen;
  Line.Destroy;
  FreeAndNil(Line);
  FreeAndNil(DriveTimer);
  FreeAndNil(AliveTimer);
  FreeAndNil(RunTimer);
  inherited;
end;

procedure TAssamblyLine.drive(Side: direction);
const maxSpeed=512;
var   Speed:integer;
begin
  case Side of
    Up: Speed:=round(maxSpeed*(-1));
    Down: Speed:=maxSpeed;
    Stop: Speed:=0;
    end;

  Line.Steuerbefehl
    .SetzeMotorgeschwindigkeit(1, Speed)
    .SetzeMotorgeschwindigkeit(2, 512)
    .Senden;

end;

procedure TAssamblyLine.DriveTimerTimer(Sender: TObject);
begin
  //CodeSite.Send('RunTime:'+IntToStr(RunTime)+' AliveTime:'+IntToStr(AliTime));
  if (DrivingFlag=Up) or (DrivingFlag=Down) then
    if (RunTime<=AliTime) then
      begin
      inc(RunTime);
      if AliTime=RunTime then
        DrivingFlag:=Stop;
      end
  else
    RunTime:=0;
end;

procedure TAssamblyLine.RunTimerTimer(Sender: TObject);
begin
  if Line.LiesAnalog(1)=1 then
    begin
    DriveTimer.Enabled:=True;
    drive(DrivingFlag);
    end
  else
    begin
    DriveTimer.Enabled:=False;
    drive(Stop);
    end;
end;


end.
