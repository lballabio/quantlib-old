
Partial Public Class FormMain

    Private Sub setAddinsEnabled(ByVal enabled As Boolean)

        lbComponents.Enabled = enabled
        btnAddinInsert.Enabled = enabled
        tstAddins.Enabled = enabled
        If enabled Then
            Call enableAddinButtons()
        Else
            btnAddinUp.Enabled = False
            btnAddinDown.Enabled = False
            btnAddinDelete.Enabled = False
            btnAddinRename.Enabled = False
        End If

    End Sub

    ''''''''''''''''''''''''''''''''''''''''''
    ' Events - Addins
    ''''''''''''''''''''''''''''''''''''''''''

    ' Sub enableAddinButtons() - Based on the current selection,
    ' enable or disable the following buttons: Delete, Rename, Up, Down

    Private Sub enableAddinButtons()

        If lbComponents.SelectedIndex = -1 Then
            ' Nothing selected - disable all buttons
            btnAddinDelete.Enabled = False
            btnAddinRename.Enabled = False
            btnAddinUp.Enabled = False
            btnAddinDown.Enabled = False
            Exit Sub
        End If

        ' Something is selected, so enable Delete/Rename
        btnAddinDelete.Enabled = True
        btnAddinRename.Enabled = True

        If lbComponents.Items.Count <= 1 Then
            ' Only one item in list - disable Up/Down
            btnAddinUp.Enabled = False
            btnAddinDown.Enabled = False
            Exit Sub
        End If

        If lbComponents.SelectedIndex = 0 Then
            ' Top item selected - disable Up, enable Down
            btnAddinUp.Enabled = False
            btnAddinDown.Enabled = True
        ElseIf lbComponents.SelectedIndex = (lbComponents.Items.Count - 1) Then
            ' Bottom item selected - enable Up, disable Down
            btnAddinUp.Enabled = True
            btnAddinDown.Enabled = False
        Else
            ' Middle item selected - enable Up & Down
            btnAddinUp.Enabled = True
            btnAddinDown.Enabled = True
        End If

    End Sub

End Class
