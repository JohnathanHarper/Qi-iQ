unit U_Form_AssamblySettings;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.ExtCtrls, Vcl.StdCtrls;

type
  TForm_AssamblyDistanceSettings = class(TForm)
    TEd_Assembly_Distance: TEdit;
    Label1: TLabel;
    BtN_Save: TButton;
    PBx_HelpPicture: TPaintBox;
    BtN_Cancel: TButton;
    TEd_Assembly_Angle: TEdit;
    Label2: TLabel;
    TEd_Assembly_Height: TEdit;
    Label3: TLabel;
  private
    { Private-Deklarationen }
  public
    { Public-Deklarationen }
  end;

var
  Form_AssamblyDistanceSettings: TForm_AssamblyDistanceSettings;

implementation

{$R *.dfm}

end.
