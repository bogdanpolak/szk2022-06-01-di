object FormMain: TFormMain
  Left = 0
  Top = 0
  Caption = 'FormMain'
  ClientHeight = 331
  ClientWidth = 406
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object GroupBox1: TGroupBox
    AlignWithMargins = True
    Left = 3
    Top = 3
    Width = 334
    Height = 325
    Align = alLeft
    Caption = 'Feature: Forms'
    Padding.Top = 10
    TabOrder = 0
    ExplicitLeft = 8
    ExplicitTop = 8
    object btnCreateForms: TButton
      AlignWithMargins = True
      Left = 5
      Top = 28
      Width = 324
      Height = 25
      Align = alTop
      Caption = 'btnCreateForms'
      TabOrder = 0
      OnClick = btnCreateFormsClick
      ExplicitLeft = 8
      ExplicitTop = 8
      ExplicitWidth = 137
    end
    object Button2: TButton
      AlignWithMargins = True
      Left = 5
      Top = 106
      Width = 324
      Height = 25
      Align = alTop
      Caption = 'Button2'
      TabOrder = 2
      ExplicitLeft = 110
      ExplicitTop = 80
      ExplicitWidth = 75
    end
    object Button3: TButton
      AlignWithMargins = True
      Left = 5
      Top = 137
      Width = 324
      Height = 25
      Align = alTop
      Caption = 'Button3'
      TabOrder = 3
      ExplicitLeft = 70
      ExplicitTop = 111
      ExplicitWidth = 75
    end
    object GridPanel1: TGridPanel
      AlignWithMargins = True
      Left = 5
      Top = 59
      Width = 324
      Height = 41
      Align = alTop
      Caption = 'GridPanel1'
      ColumnCollection = <
        item
          Value = 50.000000000000000000
        end
        item
          Value = 50.000000000000000000
        end>
      ControlCollection = <
        item
          Column = 0
          Control = Edit1
          Row = 0
        end
        item
          Column = 1
          Control = Edit2
          Row = 0
        end>
      RowCollection = <
        item
          Value = 100.000000000000000000
        end>
      TabOrder = 1
      object Edit1: TEdit
        AlignWithMargins = True
        Left = 4
        Top = 4
        Width = 155
        Height = 21
        Align = alTop
        TabOrder = 0
        Text = 'Edit1'
        ExplicitLeft = 17
        ExplicitTop = 9
        ExplicitWidth = 121
      end
      object Edit2: TEdit
        AlignWithMargins = True
        Left = 165
        Top = 4
        Width = 155
        Height = 21
        Align = alTop
        TabOrder = 1
        Text = 'Edit2'
        ExplicitLeft = 104
        ExplicitTop = 8
        ExplicitWidth = 121
      end
    end
  end
end
