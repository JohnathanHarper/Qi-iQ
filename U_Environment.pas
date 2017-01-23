unit U_Environment;

interface
 uses SysUtils,Vcl.ExtCtrls, Math, CodeSiteLogging, DateUtils, mTXTController, U_AssamblyLine, U_Arm;
 type TEnvironment=class(TObject)
   private
    const ArmAdresses:array[0..1] of String=('192.168.0.24','');
    const Bandadresses:array[0..1] of String=('192.168.0.23','');
   public
    Arm:array of TArm;
    Band:array of TAssamblyLine;
    constructor Create();
    destructor Destroy();Override;
 end;
implementation

{ TEnvironment }

constructor TEnvironment.Create;
var
  I: Integer;
begin
  setLength(Band,1);
  setLength(Arm,1);
  Band[0]:=TAssamblyLine.Create(BandAdresses[0]);
  Arm[0]:=TArm.Create(ArmAdresses[0]);
end;

destructor TEnvironment.Destroy;
begin
  Arm[0].Destroy;
  Band[0].Destroy;
  FreeAndNil(Arm[0]);
  FreeAndNil(Band[0]);
  inherited;
end;

end.
