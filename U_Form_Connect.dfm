object Form_Connect: TForm_Connect
  Left = 0
  Top = 0
  Caption = 'Connect'
  ClientHeight = 214
  ClientWidth = 388
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  PixelsPerInch = 96
  TextHeight = 13
  object Label1: TLabel
    Left = 16
    Top = 40
    Width = 113
    Height = 16
    Caption = 'Roboter Arm        A'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -13
    Font.Name = 'Tahoma'
    Font.Style = []
    ParentFont = False
  end
  object Label2: TLabel
    Left = 16
    Top = 67
    Width = 112
    Height = 16
    Caption = 'Assembly Line    FA'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -13
    Font.Name = 'Tahoma'
    Font.Style = []
    ParentFont = False
  end
  object Label3: TLabel
    Left = 16
    Top = 94
    Width = 112
    Height = 16
    Caption = 'Roboter Arm        B'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -13
    Font.Name = 'Tahoma'
    Font.Style = []
    ParentFont = False
  end
  object Label4: TLabel
    Left = 16
    Top = 121
    Width = 111
    Height = 16
    Caption = 'Assembly Line    FB'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -13
    Font.Name = 'Tahoma'
    Font.Style = []
    ParentFont = False
  end
  object Label5: TLabel
    Left = 152
    Top = 17
    Width = 65
    Height = 16
    Caption = 'ip adresses'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -13
    Font.Name = 'Tahoma'
    Font.Style = []
    ParentFont = False
  end
  object PBx_A: TPaintBox
    Left = 296
    Top = 32
    Width = 24
    Height = 24
  end
  object PBx_B: TPaintBox
    Left = 296
    Top = 113
    Width = 24
    Height = 24
  end
  object PBx_FB: TPaintBox
    Left = 328
    Top = 50
    Width = 24
    Height = 72
  end
  object PBx_FA: TPaintBox
    Left = 266
    Top = 50
    Width = 24
    Height = 72
  end
  object Label6: TLabel
    Left = 304
    Top = 13
    Width = 7
    Height = 13
    Caption = 'A'
  end
  object Label7: TLabel
    Left = 304
    Top = 143
    Width = 6
    Height = 13
    Caption = 'B'
  end
  object Label8: TLabel
    Left = 359
    Top = 79
    Width = 12
    Height = 13
    Caption = 'FB'
  end
  object Label9: TLabel
    Left = 248
    Top = 79
    Width = 13
    Height = 13
    Caption = 'FA'
  end
  object TEd_IpArm_A: TEdit
    Left = 152
    Top = 39
    Width = 81
    Height = 21
    Hint = '192.168.0.0-255'
    ParentShowHint = False
    ShowHint = False
    TabOrder = 0
    Text = '192.168.0.'
    TextHint = '192.168.0.0-255'
  end
  object TEd_IpLine_FA: TEdit
    Left = 152
    Top = 66
    Width = 81
    Height = 21
    Hint = '192.168.0.0-255'
    TabOrder = 1
    Text = '192.168.0.'
    TextHint = '192.168.0.0-255'
  end
  object TEd_IpArm_B: TEdit
    Left = 152
    Top = 93
    Width = 81
    Height = 21
    Hint = '192.168.0.0-255'
    TabOrder = 2
    Text = '192.168.0.'
    TextHint = '192.168.0.0-255'
  end
  object TEd_IpLine_FB: TEdit
    Left = 152
    Top = 120
    Width = 81
    Height = 21
    Hint = '192.168.0.0-255'
    TabOrder = 3
    Text = '192.168.0.'
    TextHint = '192.168.0.0-255'
  end
  object BtN_Connect: TButton
    Left = 300
    Top = 181
    Width = 75
    Height = 25
    Caption = 'Connect'
    TabOrder = 4
  end
  object CBx_Limited: TCheckBox
    Left = 16
    Top = 185
    Width = 97
    Height = 17
    Caption = 'Limited'
    TabOrder = 5
  end
end
