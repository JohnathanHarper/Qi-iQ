object Form_MainWindow: TForm_MainWindow
  Left = 0
  Top = 0
  Caption = 'Main Window'
  ClientHeight = 291
  ClientWidth = 425
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  Menu = MainMenu1
  OldCreateOrder = False
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object LbL_Process: TLabel
    Left = 24
    Top = 24
    Width = 44
    Height = 16
    Caption = 'Process'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -13
    Font.Name = 'Tahoma'
    Font.Style = []
    ParentFont = False
  end
  object PB_Arm_A: TPaintBox
    Left = 265
    Top = 25
    Width = 57
    Height = 57
  end
  object PB_Arm_B: TPaintBox
    Left = 265
    Top = 203
    Width = 57
    Height = 57
  end
  object PB_Line_FB: TPaintBox
    Left = 328
    Top = 67
    Width = 57
    Height = 155
  end
  object PB_Line_FA: TPaintBox
    Left = 202
    Top = 67
    Width = 57
    Height = 155
  end
  object BtN_ProcessStartStop: TButton
    Left = 40
    Top = 46
    Width = 75
    Height = 25
    Caption = 'start'
    TabOrder = 0
  end
  object GBx_Alignments: TGroupBox
    Left = 24
    Top = 111
    Width = 129
    Height = 130
    Caption = 'Alignments'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -13
    Font.Name = 'Tahoma'
    Font.Style = []
    ParentFont = False
    TabOrder = 1
    object BtN_SettingRobotArm: TButton
      Left = 16
      Top = 24
      Width = 97
      Height = 25
      Caption = 'Robot arm'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'Tahoma'
      Font.Style = []
      ParentFont = False
      TabOrder = 0
      OnClick = BtN_SettingRobotArmClick
    end
    object BtN_SettingAssemblyLine: TButton
      Left = 16
      Top = 55
      Width = 97
      Height = 25
      Caption = 'Assembly line'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'Tahoma'
      Font.Style = []
      ParentFont = False
      TabOrder = 1
      OnClick = BtN_SettingAssemblyLineClick
    end
    object BtN_SettingDistances: TButton
      Left = 16
      Top = 86
      Width = 97
      Height = 25
      Caption = 'Distances'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'Tahoma'
      Font.Style = []
      ParentFont = False
      TabOrder = 2
    end
  end
  object CBx_advancedSettings: TCheckBox
    Left = 40
    Top = 88
    Width = 113
    Height = 17
    Caption = 'advanced Settings'
    TabOrder = 2
  end
  object MainMenu1: TMainMenu
    Left = 144
    Top = 16
    object Data1: TMenuItem
      Caption = 'Data'
    end
    object Help1: TMenuItem
      Caption = 'Help'
    end
  end
end
