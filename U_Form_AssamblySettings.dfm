object Form_AssamblyDistanceSettings: TForm_AssamblyDistanceSettings
  Left = 0
  Top = 0
  Caption = 'Assambly Distance Settings'
  ClientHeight = 205
  ClientWidth = 353
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
    Left = 24
    Top = 35
    Width = 48
    Height = 16
    Caption = 'Distance'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -13
    Font.Name = 'Tahoma'
    Font.Style = []
    ParentFont = False
  end
  object PBx_HelpPicture: TPaintBox
    Left = 224
    Top = 24
    Width = 105
    Height = 105
  end
  object Label2: TLabel
    Left = 24
    Top = 62
    Width = 32
    Height = 16
    Caption = 'Angle'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -13
    Font.Name = 'Tahoma'
    Font.Style = []
    ParentFont = False
  end
  object Label3: TLabel
    Left = 24
    Top = 89
    Width = 36
    Height = 16
    Caption = 'Height'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -13
    Font.Name = 'Tahoma'
    Font.Style = []
    ParentFont = False
  end
  object TEd_Assembly_Distance: TEdit
    Left = 96
    Top = 34
    Width = 65
    Height = 21
    Alignment = taCenter
    TabOrder = 0
    TextHint = '(cm)'
  end
  object BtN_Save: TButton
    Left = 240
    Top = 160
    Width = 75
    Height = 25
    Caption = 'Save'
    TabOrder = 1
  end
  object BtN_Cancel: TButton
    Left = 159
    Top = 160
    Width = 75
    Height = 25
    Caption = 'Cancel'
    TabOrder = 2
  end
  object TEd_Assembly_Angle: TEdit
    Left = 96
    Top = 61
    Width = 65
    Height = 21
    Alignment = taCenter
    TabOrder = 3
    TextHint = '(Degree)'
  end
  object TEd_Assembly_Height: TEdit
    Left = 96
    Top = 88
    Width = 65
    Height = 21
    Alignment = taCenter
    TabOrder = 4
    TextHint = '(cm)'
  end
end
