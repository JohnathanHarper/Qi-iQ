unit U_Form_Connect;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.ExtCtrls;

type
  TForm_Connect = class(TForm)
    Label1: TLabel;
    TEd_IpArm_A: TEdit;
    Label2: TLabel;
    TEd_IpLine_FA: TEdit;
    Label3: TLabel;
    TEd_IpArm_B: TEdit;
    Label4: TLabel;
    TEd_IpLine_FB: TEdit;
    Label5: TLabel;
    BtN_Connect: TButton;
    PBx_A: TPaintBox;
    PBx_B: TPaintBox;
    PBx_FB: TPaintBox;
    PBx_FA: TPaintBox;
    Label6: TLabel;
    Label7: TLabel;
    Label8: TLabel;
    Label9: TLabel;
    CBx_Limited: TCheckBox;
  private
    { Private-Deklarationen }
  public
    { Public-Deklarationen }
  end;

var
  Form_Connect: TForm_Connect;

implementation

{$R *.dfm}

end.
