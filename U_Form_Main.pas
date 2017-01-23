unit U_Form_Main;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.Menus, Vcl.StdCtrls,mTxtController, Vcl.ExtCtrls,CodeSiteLogging,
  U_Environment, U_Form_LineSettings, U_Form_ArmSettings ;

type
  TForm_MainWindow = class(TForm)
    LbL_Process: TLabel;
    MainMenu1: TMainMenu;
    Data1: TMenuItem;
    Help1: TMenuItem;
    BtN_ProcessStartStop: TButton;
    GBx_Alignments: TGroupBox;
    CBx_advancedSettings: TCheckBox;
    BtN_SettingRobotArm: TButton;
    BtN_SettingAssemblyLine: TButton;
    BtN_SettingDistances: TButton;
    PB_Arm_A: TPaintBox;
    PB_Arm_B: TPaintBox;
    PB_Line_FB: TPaintBox;
    PB_Line_FA: TPaintBox;
    procedure BtN_SettingAssemblyLineClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure BtN_SettingRobotArmClick(Sender: TObject);
  private
    { Private-Deklarationen }
    Robots:TEnvironment;
    LineSettings:TForm_AdvancedLineSettings;
    ArmSettings:TForm_AdvancedArmSettings;
  public

    { Public-Deklarationen }
  end;

var
  Form_MainWindow: TForm_MainWindow;

implementation

{$R *.dfm}

procedure TForm_MainWindow.BtN_SettingAssemblyLineClick(Sender: TObject);
begin
    LineSettings.ShowModal;
end;

procedure TForm_MainWindow.BtN_SettingRobotArmClick(Sender: TObject);
begin
   ArmSettings.ShowModal;
end;

procedure TForm_MainWindow.FormCreate(Sender: TObject);
begin
  Robots:=TEnvironment.Create;

  LineSettings:=TForm_AdvancedLineSettings.Create(self,Robots.Band[0]);
  LineSettings.PopupMode := pmExplicit;
  LineSettings.PopupParent:=self;

  ArmSettings:=TForm_AdvancedArmSettings.Create(self,Robots.Arm[0]);
  ArmSettings.PopupMode := pmExplicit;
  ArmSettings.PopupParent:=self;

end;

end.
