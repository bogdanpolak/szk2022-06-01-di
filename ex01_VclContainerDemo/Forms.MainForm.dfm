object FormMain: TFormMain
  Left = 0
  Top = 0
  Caption = 'FormMain'
  ClientHeight = 212
  ClientWidth = 262
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
    Width = 254
    Height = 206
    Align = alLeft
    Caption = 'Feature: Forms'
    Padding.Top = 10
    TabOrder = 0
    ExplicitHeight = 237
    object btnCreateForms: TButton
      AlignWithMargins = True
      Left = 5
      Top = 28
      Width = 244
      Height = 25
      Align = alTop
      Caption = 'btnCreateForms'
      TabOrder = 0
      OnClick = btnCreateFormsClick
      ExplicitWidth = 324
    end
    object btnArangePosition: TButton
      AlignWithMargins = True
      Left = 5
      Top = 106
      Width = 244
      Height = 25
      Align = alTop
      Caption = 'btnArangePosition'
      TabOrder = 2
      OnClick = btnArangePositionClick
      ExplicitWidth = 324
    end
    object btnApplyStyle: TButton
      AlignWithMargins = True
      Left = 5
      Top = 137
      Width = 244
      Height = 25
      Align = alTop
      Caption = 'btnApplyStyle'
      TabOrder = 3
      OnClick = btnApplyStyleClick
      ExplicitWidth = 324
    end
    object GridPanel1: TGridPanel
      AlignWithMargins = True
      Left = 5
      Top = 59
      Width = 244
      Height = 41
      Align = alTop
      Caption = ' '
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
          Control = edtRangeFrom
          Row = 0
        end
        item
          Column = 1
          Control = edtRangeTo
          Row = 0
        end>
      RowCollection = <
        item
          Value = 100.000000000000000000
        end>
      TabOrder = 1
      ExplicitWidth = 324
      object edtRangeFrom: TEdit
        AlignWithMargins = True
        Left = 4
        Top = 4
        Width = 115
        Height = 21
        Align = alTop
        Alignment = taCenter
        TabOrder = 0
        Text = '1'
        ExplicitWidth = 155
      end
      object edtRangeTo: TEdit
        AlignWithMargins = True
        Left = 125
        Top = 4
        Width = 115
        Height = 21
        Align = alTop
        Alignment = taCenter
        TabOrder = 1
        Text = '4'
        ExplicitLeft = 165
        ExplicitWidth = 155
      end
    end
  end
end
