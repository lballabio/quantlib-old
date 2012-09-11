
''' <summary>
''' A collection of generic utility functions required by the XL-Launcher application.
''' </summary>
''' <remarks></remarks>
Module Utilities

    Private Const EXCEL_10_PATH As String = "C:\Program Files\Microsoft Office\OFFICE10\EXCEL.EXE"
    Private Const EXCEL_11_PATH As String = "C:\Program Files\Microsoft Office\OFFICE11\EXCEL.EXE"
    Private Const EXCEL_12_PATH As String = "C:\Program Files\Microsoft Office\OFFICE12\EXCEL.EXE"
    Public Const THIS_VERSION As Integer = 0
    Public Declare Function GetCurrentProcessId Lib "kernel32" () As Long

    Function deriveDefaultExcelPath() As String

        If System.IO.File.Exists(EXCEL_12_PATH) Then
            deriveDefaultExcelPath = EXCEL_12_PATH
        ElseIf System.IO.File.Exists(EXCEL_11_PATH) Then
            deriveDefaultExcelPath = EXCEL_11_PATH
        ElseIf System.IO.File.Exists(EXCEL_10_PATH) Then
            deriveDefaultExcelPath = EXCEL_10_PATH
        Else
            deriveDefaultExcelPath = ""
        End If

    End Function

    Public Structure AuthenticationParameters

        Dim userName As String
        Dim domainName As String
        Dim machineName As String

    End Structure

    Public Function deriveAuthenticationParameters() As AuthenticationParameters

        Static authenticationParameters_ As AuthenticationParameters = Nothing
        Static done_ As Boolean = False

        If Not done_ Then

            authenticationParameters_ = New AuthenticationParameters
            authenticationParameters_.userName = System.Environment.UserName
            authenticationParameters_.domainName = System.Environment.UserDomainName
            authenticationParameters_.machineName = System.Environment.MachineName

            done_ = True

        End If

        deriveAuthenticationParameters = authenticationParameters_

    End Function

End Module
