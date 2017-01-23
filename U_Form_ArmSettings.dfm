object Form_AdvancedArmSettings: TForm_AdvancedArmSettings
  Left = 0
  Top = 0
  Caption = 'Advanced Arm Settings'
  ClientHeight = 583
  ClientWidth = 563
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  PixelsPerInch = 96
  TextHeight = 13
  object PBx_Arm_Top: TPaintBox
    Left = 24
    Top = 24
    Width = 105
    Height = 105
  end
  object PBx_Arm_Profile: TPaintBox
    Left = 152
    Top = 24
    Width = 105
    Height = 105
  end
  object BtN_Back: TButton
    Left = 226
    Top = 280
    Width = 75
    Height = 25
    Caption = 'Back'
    TabOrder = 0
  end
  object GroupBox1: TGroupBox
    Left = 17
    Top = 135
    Width = 116
    Height = 122
    Caption = 'Calibration'
    TabOrder = 1
    object BtN_Calibrate_Arm_Width: TButton
      Left = 11
      Top = 80
      Width = 102
      Height = 25
      Caption = 'Width'
      TabOrder = 0
    end
    object BtN_Calibrate_Arm_Height: TButton
      Left = 11
      Top = 49
      Width = 102
      Height = 25
      Caption = 'Hight'
      TabOrder = 1
    end
    object BtN_Calibrate_Arm_Angle: TButton
      Left = 11
      Top = 18
      Width = 102
      Height = 25
      Caption = 'Angle'
      TabOrder = 2
    end
  end
  object GroupBox2: TGroupBox
    Left = 139
    Top = 135
    Width = 162
    Height = 122
    Caption = 'GroupBox2'
    TabOrder = 2
    object Label1: TLabel
      Left = 12
      Top = 30
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
    object Label2: TLabel
      Left = 12
      Top = 54
      Width = 40
      Height = 16
      Caption = 'Height '
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -13
      Font.Name = 'Tahoma'
      Font.Style = []
      ParentFont = False
    end
    object Label3: TLabel
      Left = 12
      Top = 78
      Width = 33
      Height = 16
      Caption = 'Width'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -13
      Font.Name = 'Tahoma'
      Font.Style = []
      ParentFont = False
    end
    object TEd_Arm_Angle: TEdit
      AlignWithMargins = True
      Left = 58
      Top = 28
      Width = 63
      Height = 21
      Alignment = taCenter
      NumbersOnly = True
      TabOrder = 0
      TextHint = ' (Degree)'
    end
    object TEd_Arm_Hight: TEdit
      AlignWithMargins = True
      Left = 58
      Top = 53
      Width = 63
      Height = 21
      Alignment = taCenter
      NumbersOnly = True
      TabOrder = 1
      TextHint = '(cm)'
    end
    object TEd_Arm_Width: TEdit
      AlignWithMargins = True
      Left = 58
      Top = 77
      Width = 63
      Height = 21
      Alignment = taCenter
      NumbersOnly = True
      TabOrder = 2
      TextHint = '(cm)'
    end
    object UpD_Arm_Angle: TUpDown
      Left = 127
      Top = 28
      Width = 17
      Height = 21
      TabOrder = 3
    end
    object UpDown_Arm_Height: TUpDown
      Left = 127
      Top = 55
      Width = 17
      Height = 21
      TabOrder = 4
    end
    object UpDown3_Arm_Width: TUpDown
      Left = 127
      Top = 80
      Width = 17
      Height = 21
      TabOrder = 5
    end
  end
  object GroupBox3: TGroupBox
    Left = 68
    Top = 352
    Width = 453
    Height = 185
    Caption = 'GroupBox3'
    TabOrder = 3
    object Edit_Angle: TEdit
      Left = 116
      Top = 22
      Width = 76
      Height = 21
      TabOrder = 0
      Text = 'Edit_Angle'
    end
    object Edit_Height: TEdit
      Left = 116
      Top = 49
      Width = 76
      Height = 21
      TabOrder = 1
      Text = 'Edit2'
    end
    object Edit_Width: TEdit
      Left = 116
      Top = 76
      Width = 76
      Height = 21
      TabOrder = 2
      Text = 'Edit2'
    end
    object Edit_Hand: TEdit
      Left = 116
      Top = 103
      Width = 76
      Height = 21
      TabOrder = 3
      Text = 'Edit2'
    end
    object Button1: TButton
      Left = 288
      Top = 128
      Width = 75
      Height = 25
      Caption = 'Button1'
      TabOrder = 4
      OnClick = Button1Click
    end
  end
  object ListBox1: TListBox
    Left = 424
    Top = 24
    Width = 121
    Height = 347
    ItemHeight = 13
    TabOrder = 4
  end
end
