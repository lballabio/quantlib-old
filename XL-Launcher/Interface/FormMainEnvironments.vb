
Partial Public Class FormMain

    Private Function inputEnvironmentName(ByVal initialValue As String) As String

        inputEnvironmentName = initialValue
        Dim invalidEntry As Boolean = True
        While invalidEntry
            inputEnvironmentName = InputBox("Edit Environment Name:", _
                "Environment Name", inputEnvironmentName)
            If 0 = inputEnvironmentName.Length Then Exit Function
            invalidEntry = envUserconfigured_.nameInUse(inputEnvironmentName)
            If invalidEntry Then
                XL_Launcher.displayError("The name '" & inputEnvironmentName & "' is already in use - " _
                    & "please enter a different name.")
            End If
        End While

    End Function

End Class
