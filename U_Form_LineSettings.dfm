object Form_AdvancedLineSettings: TForm_AdvancedLineSettings
  Left = 0
  Top = 0
  Caption = 'Advanced Line Settings'
  ClientHeight = 328
  ClientWidth = 252
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  OnClose = FormClose
  PixelsPerInch = 96
  TextHeight = 13
  object PaintBox1: TPaintBox
    Left = 32
    Top = 32
    Width = 105
    Height = 225
  end
  object BtN_Calibrate: TButton
    Left = 80
    Top = 288
    Width = 75
    Height = 25
    Caption = 'Calibrate'
    TabOrder = 0
  end
  object BtN_Back: TButton
    Left = 161
    Top = 288
    Width = 75
    Height = 25
    Caption = 'Back'
    TabOrder = 1
    OnClick = BtN_BackClick
  end
  object BtN_Up: TButton
    Left = 152
    Top = 88
    Width = 75
    Height = 25
    Caption = 'Up'
    TabOrder = 2
    OnClick = BtN_UpClick
  end
  object BtN_Halt: TButton
    Left = 152
    Top = 119
    Width = 75
    Height = 25
    Caption = 'Halt'
    TabOrder = 3
    OnClick = BtN_HaltClick
  end
  object BtN_Down: TButton
    Left = 152
    Top = 150
    Width = 75
    Height = 25
    Caption = 'Down'
    TabOrder = 4
    OnClick = BtN_DownClick
  end
end
