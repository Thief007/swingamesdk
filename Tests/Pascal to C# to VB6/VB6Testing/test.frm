VERSION 5.00
Begin VB.Form Form1 
   Caption         =   "Form1"
   ClientHeight    =   3090
   ClientLeft      =   60
   ClientTop       =   450
   ClientWidth     =   4680
   LinkTopic       =   "Form1"
   ScaleHeight     =   3090
   ScaleWidth      =   4680
   StartUpPosition =   3  'Windows Default
   Begin VB.CommandButton Command2 
      Caption         =   "Command2"
      Height          =   915
      Left            =   2760
      TabIndex        =   1
      Top             =   2040
      Width           =   1095
   End
   Begin VB.CommandButton Command1 
      Caption         =   "Command1"
      Height          =   615
      Left            =   2880
      TabIndex        =   0
      Top             =   960
      Width           =   1095
   End
End
Attribute VB_Name = "Form1"
Attribute VB_GlobalNameSpace = False
Attribute VB_Creatable = False
Attribute VB_PredeclaredId = True
Attribute VB_Exposed = False
Private Sub Command1_Click()
Dim testing As CSharpLibrary.VBTestClass
Set testing = New CSharpLibrary.VBTestClass
Dim number As Integer
Dim thing As CSharpLibrary.structtest
Set thing = New CSharpLibrary.structtest
thing.serTemp ("twietr")
Print (testing.ReciveWords(thing))
number = 10
Print (number)
testing.Mess (number)
Print (number)
Print (testing.ReciveInt(987))
Print ("int from VB6 to Pascal and back")
Print (testing.SendInt())
Print ("int from Pascal")
Print (testing.ReciveString("From VB6"))
Print (testing.SendString())
End Sub
