
Partial Public Class FormMain

    Private Sub enableVariableMenu()

        If lvVariables.SelectedIndices.Count = 0 Then
            miVariableDelete.Enabled = False
            miVariableEdit.Enabled = False
        Else
            miVariableDelete.Enabled = True
            miVariableEdit.Enabled = True
        End If

    End Sub

    Private Sub initializeVariableList()

        lvVariables.Items.Clear()
        For Each variable As XL_Launcher.EnvironmentVariable In SelectedFramework.EnvironmentVariables
            Dim listItem As New ListViewItem(variable.Name)
            listItem.SubItems.Add(variable.Value)
            lvVariables.Items.Add(listItem)
        Next

    End Sub

End Class
