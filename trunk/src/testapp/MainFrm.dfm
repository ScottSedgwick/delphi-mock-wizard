object Form1: TForm1
  Left = 0
  Top = 0
  Caption = 'Form1'
  ClientHeight = 778
  ClientWidth = 803
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  PixelsPerInch = 96
  TextHeight = 13
  object Memo1: TMemo
    Left = 0
    Top = 0
    Width = 712
    Height = 778
    Align = alClient
    TabOrder = 0
  end
  object Panel1: TPanel
    Left = 712
    Top = 0
    Width = 91
    Height = 778
    Align = alRight
    TabOrder = 1
    object btnGenerate: TButton
      Left = 8
      Top = 8
      Width = 75
      Height = 25
      Caption = 'Generate'
      TabOrder = 0
      OnClick = btnGenerateClick
    end
  end
end
