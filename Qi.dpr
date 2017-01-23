program Qi;

uses
  Vcl.Forms,
  U_Form_Main in 'U_Form_Main.pas' {Form_MainWindow},
  U_Form_Connect in 'U_Form_Connect.pas' {Form_Connect},
  U_Form_ArmSettings in 'U_Form_ArmSettings.pas' {Form_AdvancedArmSettings},
  U_Form_LineSettings in 'U_Form_LineSettings.pas' {Form_AdvancedLineSettings},
  U_Form_AssamblySettings in 'U_Form_AssamblySettings.pas' {Form_AssamblyDistanceSettings},
  U_AssamblyLine in 'U_AssamblyLine.pas',
  U_Environment in 'U_Environment.pas',
  U_Arm in 'U_Arm.pas',
  U_Motor in 'U_Motor.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TForm_MainWindow, Form_MainWindow);
  Application.CreateForm(TForm_Connect, Form_Connect);
  Application.CreateForm(TForm_AdvancedArmSettings, Form_AdvancedArmSettings);
  Application.CreateForm(TForm_AdvancedLineSettings, Form_AdvancedLineSettings);
  Application.CreateForm(TForm_AssamblyDistanceSettings, Form_AssamblyDistanceSettings);
  Application.Run;
end.
