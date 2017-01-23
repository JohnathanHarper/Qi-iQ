unit U_Form_LineSettings;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.ComCtrls, Vcl.ExtCtrls, U_AssamblyLine;

type
  TForm_AdvancedLineSettings = class(TForm)
    PaintBox1: TPaintBox;
    BtN_Calibrate: TButton;
    BtN_Back: TButton;
    BtN_Up: TButton;
    BtN_Halt: TButton;
    BtN_Down: TButton;
    constructor Create(AOwner:TComponent; var Band:TAssamblyLine);
    procedure BtN_UpClick(Sender: TObject);
    procedure BtN_HaltClick(Sender: TObject);
    procedure BtN_DownClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure BtN_BackClick(Sender: TObject);
  private
    { Private-Deklarationen }
    _Band:TAssamblyLine;
  public
    { Public-Deklarationen }
  end;

var
  Form_AdvancedLineSettings: TForm_AdvancedLineSettings;

implementation

{$R *.dfm}

procedure TForm_AdvancedLineSettings.BtN_BackClick(Sender: TObject);
begin
  ModalResult:=mrClose;
end;

procedure TForm_AdvancedLineSettings.BtN_DownClick(Sender: TObject);
begin
   _Band.DrivingFlag:=Down;
end;

procedure TForm_AdvancedLineSettings.BtN_HaltClick(Sender: TObject);
begin
  _Band.DrivingFlag:=Stop;
end;

procedure TForm_AdvancedLineSettings.BtN_UpClick(Sender: TObject);
begin
   _Band.DrivingFlag:=Up;
end;

constructor TForm_AdvancedLineSettings.Create(AOwner: TComponent; var Band: TAssamblyLine);
begin
  inherited Create(AOwner);
  _Band:=Band;
end;

procedure TForm_AdvancedLineSettings.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  _Band.DrivingFlag:=Stop;
end;

procedure TForm_AdvancedLineSettings.FormCreate(Sender: TObject);
begin
  //
end;

end.
