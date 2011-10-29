
Namespace XL_Launcher

    ''' <summary>
    ''' Central processing to be used for all run time errors which are thrown
    ''' by the application.
    ''' </summary>
    ''' <remarks></remarks>
    Module Errors

        Public Const INFO_MESSAGE As String = "XL-Launcher"
        Public Const ERROR_MESSAGE As String = "XL-Launcher Error"

        Public Sub displayError(ByVal errorMessage As String, _
            Optional ByVal messageBoxStyle As MsgBoxStyle = MsgBoxStyle.OkOnly + MsgBoxStyle.Exclamation)

            MsgBox(errorMessage, messageBoxStyle, ERROR_MESSAGE)

        End Sub

        Public Sub displayError(ByVal errorMessage As String, ByVal ex As Exception, _
            Optional ByVal messageBoxStyle As MsgBoxStyle = MsgBoxStyle.OkOnly + MsgBoxStyle.Exclamation)

            displayError(errorMessage & ":" & vbCrLf & vbCrLf & ex.Message, messageBoxStyle)

        End Sub

    End Module

End Namespace
