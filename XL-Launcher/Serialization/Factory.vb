
Namespace XL_Launcher

    ''' <summary>
    ''' A Singleton utility for converting a type string into an instance
    ''' of the corresponding object.
    ''' </summary>
    ''' <remarks></remarks>
    Public Class Factory

        Public Shared Function make(ByVal className As String) As Object

            Select Case className

                Case "Authentication"
                    make = New Authentication()
                Case "Domain"
                    make = New Domain()
                Case "User"
                    make = New User()
                Case "Machine"
                    make = New Machine()

                Case "EnvironmentConfig"
                    make = New Environment()
                Case "LauncherConfig"
                    make = New EnvironmentList()
                Case "Framework"
                    make = New Framework()

                Case "GroupBox"
                    make = New GroupBox()
                Case "KeyValuePair"
                    make = New KeyValuePair()
                Case "CheckBox"
                    make = New CheckBox()
                Case "TextBox"
                    make = New TextBox()
                Case "ComboBox"
                    make = New ComboBox()
                Case "DateTimePicker"
                    make = New DateTimePicker()

                Case "Component"
                    make = New Component()
                Case "Paths"
                    make = New Paths()
                Case "GlobalAddin"
                    make = New GlobalAddin()
                Case "EnvironmentVariable"
                    make = New EnvironmentVariable()

                Case Else
                    Throw New Exception("invalid class name: " & className)

            End Select

        End Function

    End Class

End Namespace
