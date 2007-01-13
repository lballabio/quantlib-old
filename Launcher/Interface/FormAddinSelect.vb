Public Class FormAddinSelect

    Private processingEvents_ As Boolean = True

    Public Property AddinName() As String
        Get
            Return lstAddinNames.SelectedItem
        End Get
        Set(ByVal value As String)
            If lstAddinNames.Items.Contains(value) Then
                lstAddinNames.SelectedItem = value
            End If
        End Set
    End Property

    Public Sub New()

        InitializeComponent()

        lstAddinNames.Items.Add("QuantLibXL-vc80-mt-s-0_4_0.xll")
        lstAddinNames.Items.Add("QuantLibXL-vc80-mt-0_4_0.xll")
        lstAddinNames.Items.Add("QuantLibXL-vc80-mt-sgd-0_4_0.xll")
        lstAddinNames.Items.Add("QuantLibXL-vc80-mt-gd-0_4_0.xll")
        lstAddinNames.Items.Add("QuantLibXL-vc71-mt-s-0_4_0.xll")
        lstAddinNames.Items.Add("QuantLibXL-vc71-mt-0_4_0.xll")
        lstAddinNames.Items.Add("QuantLibXL-vc71-s-0_4_0.xll")
        lstAddinNames.Items.Add("QuantLibXL-vc71-mt-sgd-0_4_0.xll")
        lstAddinNames.Items.Add("QuantLibXL-vc71-mt-gd-0_4_0.xll")
        lstAddinNames.Items.Add("QuantLibXL-vc71-sgd-0_4_0.xll")

        lstAddinNames.SelectedItem = "QuantLibXL-vc80-mt-s-0_4_0.xll"

    End Sub

    Private Sub lstAddinNames_SelectedIndexChanged(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles lstAddinNames.SelectedIndexChanged
        If Not processingEvents_ Then Exit Sub
        processingEvents_ = False
        Dim addinName As String = lstAddinNames.SelectedItem
        If addinName = "QuantLibXL-vc71-sgd-0_4_0.xll" Then
            optVC7.Checked = True
            optSingle.Checked = True
            optStatic.Checked = True
            optDebug.Checked = True
        ElseIf addinName = "QuantLibXL-vc71-s-0_4_0.xll" Then
            optVC7.Checked = True
            optSingle.Checked = True
            optStatic.Checked = True
            optRelease.Checked = True
        ElseIf addinName = "QuantLibXL-vc71-mt-sgd-0_4_0.xll" Then
            optVC7.Checked = True
            optMulti.Checked = True
            optStatic.Checked = True
            optDebug.Checked = True
        ElseIf addinName = "QuantLibXL-vc71-mt-s-0_4_0.xll" Then
            optVC7.Checked = True
            optMulti.Checked = True
            optStatic.Checked = True
            optRelease.Checked = True
        ElseIf addinName = "QuantLibXL-vc71-mt-gd-0_4_0.xll" Then
            optVC7.Checked = True
            optMulti.Checked = True
            optDynamic.Checked = True
            optDebug.Checked = True
        ElseIf addinName = "QuantLibXL-vc71-mt-0_4_0.xll" Then
            optVC7.Checked = True
            optMulti.Checked = True
            optDynamic.Checked = True
            optRelease.Checked = True
        ElseIf addinName = "QuantLibXL-vc80-mt-sgd-0_4_0.xll" Then
            optVC8.Checked = True
            optStatic.Checked = True
            optDebug.Checked = True
        ElseIf addinName = "QuantLibXL-vc80-mt-s-0_4_0.xll" Then
            optVC8.Checked = True
            optStatic.Checked = True
            optRelease.Checked = True
        ElseIf addinName = "QuantLibXL-vc80-mt-gd-0_4_0.xll" Then
            optVC8.Checked = True
            optDynamic.Checked = True
            optDebug.Checked = True
        ElseIf addinName = "QuantLibXL-vc80-mt-0_4_0.xll" Then
            optVC8.Checked = True
            optDynamic.Checked = True
            optRelease.Checked = True
        End If
        processingEvents_ = True
    End Sub

    Private Sub optVC_CheckedChanged(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles optVC7.CheckedChanged, optVC8.CheckedChanged
        Dim processingEvents As Boolean = processingEvents_
        processingEvents_ = False
        If optVC7.Checked Then
            optSingle.Enabled = True
            optMulti.Enabled = True
        Else
            optMulti.Checked = True
            optSingle.Enabled = False
            optMulti.Enabled = False
        End If
        processingEvents_ = processingEvents
        If processingEvents_ Then deriveAddinName()
    End Sub

    Private Sub optThreading_CheckedChanged(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles optVC7.CheckedChanged, optVC8.CheckedChanged, optSingle.CheckedChanged, optMulti.CheckedChanged
        Dim processingEvents As Boolean = processingEvents_
        processingEvents_ = False
        If optMulti.Checked Then
            optStatic.Enabled = True
            optDynamic.Enabled = True
        Else
            optStatic.Checked = True
            optStatic.Enabled = False
            optDynamic.Enabled = False
        End If
        processingEvents_ = processingEvents
        If processingEvents_ Then deriveAddinName()
    End Sub

    Private Sub optAll_CheckedChanged(ByVal sender As Object, ByVal e As System.EventArgs) Handles optStatic.CheckedChanged, optRelease.CheckedChanged, optDynamic.CheckedChanged, optDebug.CheckedChanged
        If processingEvents_ Then deriveAddinName()
    End Sub

    Private Sub deriveAddinName()

        Dim compiler As String
        Dim threading As String
        Dim linkage As String
        Dim environment As String
        Dim lastHyphen As String = ""
        Dim addinName As String

        If optVC7.Checked Then
            compiler = "vc71"
        Else
            compiler = "vc80"
        End If

        If optSingle.Checked Then
            threading = ""
        Else
            threading = "mt-"
        End If

        If optStatic.Checked Then
            linkage = "s"
            lastHyphen = "-"
        Else
            linkage = ""
        End If

        If optDebug.Checked Then
            environment = "gd"
            lastHyphen = "-"
        Else
            environment = ""
        End If

        addinName = "QuantLibXL-" & compiler & "-" _
            & threading & linkage & environment _
            & lastHyphen & "0_4_0.xll"

        processingEvents_ = False
        lstAddinNames.SelectedItem = addinName
        processingEvents_ = True

    End Sub

    Private Sub btnOK_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles btnOK.Click
        DialogResult = Windows.Forms.DialogResult.OK
        Close()
    End Sub

    Private Sub btnCancel_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles btnCancel.Click
        Close()
    End Sub

End Class