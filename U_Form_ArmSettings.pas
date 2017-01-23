unit U_Form_ArmSettings;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.ComCtrls, Vcl.ExtCtrls,CodeSiteLogging, U_Arm;

type
  TForm_AdvancedArmSettings = class(TForm)
    PBx_Arm_Top: TPaintBox;
    PBx_Arm_Profile: TPaintBox;
    Label1: TLabel;
    TEd_Arm_Angle: TEdit;
    UpD_Arm_Angle: TUpDown;
    Label2: TLabel;
    TEd_Arm_Hight: TEdit;
    UpDown_Arm_Height: TUpDown;
    Label3: TLabel;
    TEd_Arm_Width: TEdit;
    UpDown3_Arm_Width: TUpDown;
    BtN_Calibrate_Arm_Angle: TButton;
    BtN_Back: TButton;
    BtN_Calibrate_Arm_Height: TButton;
    BtN_Calibrate_Arm_Width: TButton;
    GroupBox1: TGroupBox;
    GroupBox2: TGroupBox;
    GroupBox3: TGroupBox;
    Edit_Angle: TEdit;
    Edit_Height: TEdit;
    Edit_Width: TEdit;
    Edit_Hand: TEdit;
    Button1: TButton;
    ListBox1: TListBox;
    constructor Create(AOwner:TComponent; var XArm:TArm);
    procedure Button1Click(Sender: TObject);
  private
    _Arm:TArm;
    { Private-Deklarationen }
  public

    { Public-Deklarationen }
  end;

var
  Form_AdvancedArmSettings: TForm_AdvancedArmSettings;
  int:integer;
implementation

{$R *.dfm}

{ TForm_AdvancedArmSettings }


procedure TForm_AdvancedArmSettings.Button1Click(Sender: TObject);
var
  I: Integer;
begin
  I:=StrToInt(Edit_Height.Text);
  if I<0 then
  _Arm.SetDistance(Seight,down,I)
  else
  _Arm.SetDistance(Seight,up,I);
  codesite.send('B Pressed');
end;

constructor TForm_AdvancedArmSettings.Create(AOwner: TComponent;
  var XArm: TArm);
begin
  inherited Create(AOwner);
   _Arm:=XArm;
end;



end.
