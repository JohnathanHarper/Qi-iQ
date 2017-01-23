unit U_Arm;

interface
uses SysUtils,Vcl.ExtCtrls ,Classes , Math,Generics.Collections, CodeSiteLogging, DateUtils, mTXTController;

 type TArm=class(TObject)
  type  TAllEngines = array [1..4] of integer;
  type  TDirection =  (Up,Down,Stop);
  type  TMotorType =  (Seight,Width,Angle,Hold);
  type  TFlagSwitch = procedure(Sender:Tobject;_Engine:TMotorType) of Object;
  type  TPosiSwitch = procedure(Sender:Tobject;_Engine:TMotorType) of Object;


  private
    Arm:              TTXTController;

    RunTimer:         array [1..4] of TTimer;
    AliveTimer:       TTimer;

    FlagSwitch:       TFlagSwitch;
    PosiSwitch:       TPosiSwitch;

    AxisLength:       TAllEngines;

    GTemp:            integer;

    //To Remind the driven dinstances
    Distance:         array [1..4]of TList<Integer>;

    // links the different engines and their indexes
    function          MType(Engine:TMotorType):integer; Overload;
    //function          MType(Index:Integer):TMotorType;

    function          Stopping(engine:TMotorType):boolean;
    procedure         drive(side: TDirection; engine: TMotorType);

    procedure         OnFlagSwitch_Switch(Sender:TObject;_Engine:TMotorType);
    procedure OnPosiSwitch_Switch(Sender: TObject; _Engine: TArm.TMotorType);
    procedure ClearList(_Engine: TMotorType;Flag:TDirection);

    property          OnFlagSwitch: TFlagSwitch read FlagSwitch write FlagSwitch;
    property          OnPosiSwitch: TPosiSwitch read PosiSwitch write PosiSwitch;

   {Timer} {$region // Timer}
    procedure         RunTimerATimer(Sender:TObject);
    procedure         RunTimerBTimer(Sender:TObject);
    procedure         RunTimerCTimer(Sender:TObject);
    procedure         RunTimerDTimer(Sender:TObject);

    procedure         AliveTimerTimer(Sender:TObject);
    {$endregion}
//    procedure         calibrate(engine:TMotorType; MillimeterDist:Integer);

  public
    DrivingFlags:     array [1..4] of TDirection;
    Positions:        TAllEngines;
    PositionCall:     TAllEngines;
    procedure         setDistance(engine: TMotorType; side:TDirection; ticks: integer);
    constructor       Create(Ip_Adresse: string);
    destructor        Destroy(); Override;
 end;
implementation

// Nicht fertig!!!!
//procedure TArm.calibrate(engine: TMotorType; MillimeterDist: Integer);
//const M=1;
//begin
//    if Arm.LiesAnalog(M+1)=1 then
//      drive(Down,Engine)
//    else if Arm.LiesAnalog(M+1)=0 then
//      begin
//      AxisLength[engine];
//      drive(Up,Engine);
//      end
//    else
//    drive(Stop,Engine);
//end;

{Constr/Destr}{$region Constr/Destr}
constructor TArm.Create(Ip_Adresse:string);
var
  I: integer;
begin
  OnFlagSwitch:=OnFlagSwitch_Switch;
  OnPosiSwitch:=OnPosiSwitch_Switch;

  for I := Low(Distance) to High(Distance) do
  begin
    Distance[I]:=TList<Integer>.Create;
  end;

  AliveTimer:=TTimer.Create(nil);
  AliveTimer.OnTimer:=AliveTimerTimer;
  AliveTimer.Interval:=250;
  AliveTimer.Enabled:=True;

  for I := Low(RunTimer) to High(RunTimer) do
  begin
    RunTimer[I]:=TTimer.Create(nil);
    case I of
      0:RunTimer[I].OnTimer:=RunTimerATimer;
      1:RunTimer[I].OnTimer:=RunTimerBTimer;
      2:RunTimer[I].OnTimer:=RunTimerCTimer;
      3:RunTimer[I].OnTimer:=RunTimerDTimer;
    end;
    RunTimer[I].Interval:=1;
    RunTimer[I].Enabled:=True;
  end;
  for I := Low(DrivingFlags) to High(DrivingFlags) do
    DrivingFlags[I]:=Stop;

  Try
  Arm:=TTXTController.Create(Ip_Adresse);
  Arm.Verbinden;
  CodeSiteLogging.CodeSite.Send('Connected Arm');
  Except
  CodeSiteLogging.CodeSite.Send('Can not connect');
  end;
end;

destructor TArm.Destroy;
var
  i: integer;
begin
  Arm.Trennen;
  Arm.Destroy;
  FreeAndNil(Arm);
  for I := High(DrivingFlags) downto Low(DrivingFlags) do
    FreeAndNil(DrivingFlags[I]);
  for I := High(RunTimer) downto Low(RunTimer) do
    FreeAndNil(RunTimer[I]);
  inherited;
end;
{$endregion}


procedure TArm.setDistance(engine:TMotorType; side:TDirection; ticks: integer);
begin
  PositionCall[MType(engine)]:=Positions[MType(engine)]+ticks;
  DrivingFlags[MType(engine)]:=side;
end;

procedure TArm.drive(side: TDirection; engine: TMotorType);
const maxSpeed=512;
var   Speed:integer;
begin
  Speed:=0;
  case Side of
    Up:   Speed:=round(maxSpeed*(-1));
    Down: Speed:=maxSpeed;
    Stop: Speed:=0;
    end;
  case Engine of
    Seight: Speed:=round(Speed*(1));
    Width:  Speed:=round(Speed*(1));
    Angle:  Speed:=round(Speed*(-1));
    Hold:   Speed:=round(Speed*(-1));
    end;
  DrivingFlags[MType(engine)]:=side;
  Distance[MType(engine)].Add(Arm.LiesCounter(MType(engine)));

  Arm.Steuerbefehl
    .SetzeMotorgeschwindigkeit(MType(engine), Speed)
    .Senden;
end;

function TArm.Stopping(engine: TMotorType): boolean;
var temp:integer;
begin
  if DrivingFlags[MType(engine)] <> Stop then
  begin
    // distance Stop - Sekundär
    temp:=0;
    if Distance[MType(engine)].Count>=2 then
    begin
    temp:= Distance[MType(engine)].Last-Distance[MType(engine)].First;
    if DrivingFlags[MType(engine)] = Down then temp:=round(temp*-1);
    end;
    if (PositionCall[MType(engine)]=Positions[MType(engine)]+temp) or
        (PositionCall[MType(engine)]=Positions[MType(engine)]) then
      result:=True
    else
      result:=False;
    // Taster Stop - Primär
    if Arm.LiesAnalog(MType(engine))=1 then
      result:=True;
  end
  else
    result:=True;

  if result=True then
  begin
     if Assigned(FlagSwitch) then
        FlagSwitch(self,engine);
      DrivingFlags[MType(engine)]:=Stop;
  end;
end;


{TMotorType Translation}{$region TMotorType Translation}
//function TArm.MType(Index: Integer): TMotorType;
//begin
//  case Index of
//    1: result:=Angle;
//    2: result:=Seight;
//    3: result:=Width;
//    4: result:=Hold;
//    end;
//end;

function TArm.MType(Engine: TMotorType): integer;
begin
  case Engine of
    Seight: result:=2;
    Width:  result:=3;
    Angle:  result:=1;
    Hold:   result:=4;
    end;
end;
{$endregion}
procedure TArm.OnPosiSwitch_Switch(Sender: TObject; _Engine: TArm.TMotorType);
begin
  if Positions[MType(_Engine)]<PositionCall[MType(_Engine)] then
    DrivingFlags[MType(_Engine)]:=Up
  else if Positions[MType(_Engine)]<PositionCall[MType(_Engine)] then
    DrivingFlags[MType(_Engine)]:=Down
  else
    begin
    DrivingFlags[MType(_Engine)]:=Stop;
    //PositionCall[MType(_Engine)]:=nil
    end;

  Distance[MType(_engine)].Add(Arm.LiesCounter(MType(_engine)));

  if (not (DrivingFlags[MType(_Engine)]=Stop)) and (Assigned(FlagSwitch)) then
    FlagSwitch(self,_Engine);
end;

procedure TArm.ClearList(_Engine:TMotorType;Flag:TDirection);
var dif:integer;
begin
  if Distance[MType(_Engine)].Count>2 then
    begin
    dif:= Distance[MType(_Engine)].Last-Distance[MType(_Engine)].First;
    if Flag = Down then
      dif:=round(dif*-1);
    Positions[MType(_Engine)]:=Positions[MType(_Engine)]+dif;
    Distance[MType(_Engine)].Clear;
    end;
end;

procedure TArm.OnFlagSwitch_Switch(Sender: TObject;_Engine:TMotorType);
var i:integer;
temp:integer;
begin
  case DrivingFlags[MType(_engine)] of
    up:   codesite.SendNote('Flag: Up');
    down: codesite.SendNote('Flag: Down');
    stop: codesite.SendNote('Flag: Stop');
  end;

  if (DrivingFlags[MType(_Engine)]=Up) or (DrivingFlags[MType(_Engine)]=Down) then
  begin
    Distance[MType(_Engine)].Sort;
    temp:=Distance[MType(_Engine)].Last-Distance[MType(_Engine)].First;
    if DrivingFlags[MType(_Engine)]=Down then
      temp:=round(temp*-1);
    Positions[MType(_Engine)]:=Positions[MType(_Engine)]+temp;
    Distance[MType(_Engine)].Clear;
  end
  else
    Distance[MType(_Engine)].Clear;
end;

{MotionTimer}{$region MotionTimer}
procedure TArm.RunTimerATimer(Sender: TObject);
const M=Angle;
begin
//    if (Arm.LiesAnalog(M)=0) or ((Arm.LiesAnalog(M)=1) and (DrivingFlags[M]=Down)) then
//      drive(DrivingFlags[M],MT)
//    else
//    drive(Stop,MT);

  if not Stopping(M) then
    drive(DrivingFlags[MType(M)],M)
  else
    drive(stop,M);
end;

procedure TArm.RunTimerBTimer(Sender: TObject);
const M=Seight;
begin
  if not Stopping(M) then
    drive(DrivingFlags[MType(M)],M)
  else
    drive(stop,M);
end;

procedure TArm.RunTimerCTimer(Sender: TObject);
const M=Width;
begin
  if not Stopping(M) then
    drive(DrivingFlags[MType(M)],M)
  else
    drive(stop,M);
end;

procedure TArm.RunTimerDTimer(Sender: TObject);
const M=Hold;
begin
  if not Stopping(M) then
    drive(DrivingFlags[MType(M)],M)
  else
    drive(stop,M);
end;


{$endregion}
{AliveTimer}{$region: AliveTimer}
procedure TArm.AliveTimerTimer(Sender: TObject);
var
  I: Integer;
begin
  Arm.HoleStatus;
  case GTemp of
    0:begin inc(GTemp); end;
    1:begin dec(GTemp); end;
  end;
    Codesite.sendMsg(IntToStr(Positions[2])+' '+IntToStr(PositionCall[2]));
 // codesite.send(intTostr(Distance[2].Last));
  end;

{$endregion}
end.
