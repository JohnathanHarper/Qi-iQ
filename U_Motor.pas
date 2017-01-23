unit U_Motor;

interface
uses SysUtils,Vcl.ExtCtrls ,Classes , Math,Generics.Collections, CodeSiteLogging, DateUtils, mTXTController;

type TMotor=class(TObject)
    type  TDirection =  (Up,Down,Stop);
    type  TMotorType =  (Heights,Widths,Angles,Holds);

    Private
      _PhysMotorOwner:       TTXTController;
      _MotorType:TMotorType;
      _rotationalDirection:  Boolean;
      _Position:             integer;
      _Call:                 integer;
      _drivingFlag:          TDirection;
      _RunTimer:             TTimer;
      procedure driver(const Value: integer);
      procedure Motor(state:TDirection);
      function  stopCondition:Boolean;
    procedure   _RunTimerTimer(Sender: TObject);
    Public
      property Position:integer read _Position write driver();
      constructor Create(var PhysMotorOwner:TTXTController_Konfiguration; MType:TMotorType);
      destructor Destroy();Override;
 end;

implementation

{ TMotor }

constructor TMotor.Create(var PhysMotorOwner:TTXTController_Konfiguration; MType:TMotorType);
begin
   _PhysMotorOwner:=PhysMotorOwner;
   _MotorType:=MType;

   _RunTimer:=TTimer.Create(nil);
   _RunTimer.Interval:=1;
   _RunTimer.OnTimer:=_RunTimerTimer;
   _RunTimer.Enabled:=False;
end;

destructor TMotor.Destroy;
begin

  inherited;
end;

procedure TMotor.driver(const Value: integer);
begin
  if Value=_Position then
    _drivingFlag:=Stop
  else if Value>_Position then
    _drivingFlag:=Up
  else
    _drivingFlag:=Down;
  _Call:=Value;
  while not stopCondition do
    _RunTimer.Enabled;
end;
procedure TMotor.Motor(state: TDirection);
const maxSpeed=512;
var   Speed:integer;
begin
  Speed:=0;
  case state of
    Up:   Speed:=round(maxSpeed*(-1));
    Down: Speed:=maxSpeed;
    Stop: Speed:=0;
    end;
  case _MotorType of
    Heights: Speed:=round(Speed*(1));
    Widths:  Speed:=round(Speed*(1));
    Angles:  Speed:=round(Speed*(-1));
    Holds:   Speed:=round(Speed*(-1));
    end;

  _PhysMotorOwner.Steuerbefehl
    .SetzeMotorgeschwindigkeit(TMotorType(_MotorType), Speed)
    .Senden;
end;

function TMotor.stopCondition: Boolean;
begin
  if _Call<>nil then
    begin
      if _Call=Position then
         begin
           _Call:=Nil;
           result:=True;
         end
      else
        result:=False;
    end;
end;

procedure TMotor._RunTimerTimer(Sender:TObject);
begin

end;
end.
