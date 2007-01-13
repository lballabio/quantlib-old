Public Class FormNameEnvironment

    ''''''''''''''''''''''''''''''''''''''''''
    ' members
    ''''''''''''''''''''''''''''''''''''''''''

    Private environments_ As QuantLibXL.EnvironmentList
    Private originalValue_ As String

    ''''''''''''''''''''''''''''''''''''''''''
    ' properties
    ''''''''''''''''''''''''''''''''''''''''''

    Public ReadOnly Property ValueChanged() As Boolean
        Get
            ValueChanged = Not originalValue_ = txtNameEnvironment.Text
        End Get
    End Property

    Public ReadOnly Property NewEnvironmentName() As String
        Get
            NewEnvironmentName = txtNameEnvironment.Text
        End Get
    End Property

    ''''''''''''''''''''''''''''''''''''''''''
    ' initialization
    ''''''''''''''''''''''''''''''''''''''''''

    Public Sub New(ByRef environments As QuantLibXL.EnvironmentList, ByVal text As String)
        InitializeComponent()
        environments_ = environments
        txtNameEnvironment.Text = text
        originalValue_ = text
    End Sub

    Private Sub btnOK_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles btnOK.Click
        If ValueChanged And environments_.nameInUse(txtNameEnvironment.Text) Then
            MsgBox("There is already an environment with the name " _
            & txtNameEnvironment.Text & " - please choose another name.")
        Else
            DialogResult = Windows.Forms.DialogResult.OK
            Close()
        End If
    End Sub

    Private Sub btnCancel_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles btnCancel.Click
        Close()
    End Sub

End Class