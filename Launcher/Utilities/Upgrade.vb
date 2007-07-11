
'Copyright (C) 2006, 2007 Eric Ehlers

'This file is part of QuantLib, a free-software/open-source library
'for financial quantitative analysts and developers - http://quantlib.org/

'QuantLib is free software: you can redistribute it and/or modify it
'under the terms of the QuantLib license.  You should have received a
'copy of the license along with this program; if not, please email
'<quantlib-dev@lists.sf.net>. The license is also available online at
'<http://quantlib.org/license.shtml>.

'This program is distributed in the hope that it will be useful, but WITHOUT
'ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
'FOR A PARTICULAR PURPOSE.  See the license for more details.

' Module Upgrade - make any necessary changes to the user's environment
' following an upgrade of the QuantLibXL Launcher

Module Upgrade

    Private r_ As QuantLibXL.RegistryEditor
    Private registryVersion_ As Integer
    Public Const THIS_VERSION As Integer = 9

    Public Sub run()

        Try

            r_ = New QuantLibXL.RegistryEditor
            getVersionNumber()
            updateRegistry()
            r_ = Nothing
            Exit Sub

        Catch ex As Exception

            Dim msg As String = "QuantLibXL Launcher encountered an error " _
                & "while attempting to upgrade user preferences: " _
                & vbCrLf & vbCrLf & ex.Message & vbCrLf & vbCrLf _
                & "Do you want to restore default preferences?" & vbCrLf & vbCrLf _
                & "Click Yes to restore default preferences and continue processing." & vbCrLf & vbCrLf _
                & "Click No to exit the application."

            Dim res As MsgBoxResult = MsgBox(msg, MsgBoxStyle.YesNo + MsgBoxStyle.Critical, _
                "QuantLibXL Launcher Error")

            If res = MsgBoxResult.Yes Then

                Call restoreDefaults()

            Else

                Environment.Exit(1)

            End If

        End Try

    End Sub

    Private Sub restoreDefaults()

        Try

            r_ = New QuantLibXL.RegistryEditor
            r_.deleteKey("QuantLibXL Launcher")
            initializeRegistryLatestVersion()
            r_ = Nothing

            MsgBox("Default preferences successfully restored.")

        Catch ex As Exception

            MsgBox("QuantLibXL Launcher encountered an error " _
                & "while attempting to restore default preferences: " _
                & vbCrLf & vbCrLf & ex.Message & vbCrLf & vbCrLf _
                & "The application will now close, please contact support.")

            Environment.Exit(1)

        End Try

    End Sub

    Private Sub getVersionNumber()

        If r_.keyExists("QuantLibXL Launcher\LauncherVersion10") Then
            registryVersion_ = 10
        ElseIf r_.keyExists("QuantLibXL Launcher\LauncherVersion9") Then
            registryVersion_ = 9
        ElseIf r_.keyExists("QuantLibXL Launcher\LauncherVersion8") Then
            registryVersion_ = 8
        ElseIf r_.keyExists("QuantLibXL Launcher\LauncherVersion7") Then
            registryVersion_ = 7
        ElseIf r_.keyExists("QuantLibXL Launcher\LauncherVersion6") Then
            registryVersion_ = 6
        Else
            registryVersion_ = 0
        End If

    End Sub

    Private Sub updateRegistry()

        If registryVersion_ = 0 Then
            initializeRegistryLatestVersion()
            Exit Sub
        End If

        If registryVersion_ = 6 Then upgradeVersion6to7()
        If registryVersion_ = 7 Then upgradeVersion7to8()
        If registryVersion_ = 8 Then upgradeVersion8to9()
        If registryVersion_ = 9 Then upgradeVersion9to10()

    End Sub

    ''''''''''''''''''''''''''''''''''''''''''''''''''
    ' upgrade registry from previous launcher versions
    ''''''''''''''''''''''''''''''''''''''''''''''''''

    Private Sub upgradeVersion9to10()
        r_.copyKey("QuantLibXL Launcher\LauncherVersion9", "QuantLibXL Launcher\LauncherVersion10")
        ' The settings just copied from version 8 may or may not contain value ExcelPath
        ' depending on whether the user picked up an incremental upgrade of version 8.
        ' If the new registry key doesn't contain value ExcelPath then initialize it.
        If Not r_.valueExists("QuantLibXL Launcher\LauncherVersion10\Configuration", "ExcelPath") Then
            r_.setValue("QuantLibXL Launcher\LauncherVersion10\Configuration", _
                "ExcelPath", deriveDefaultExcelPath())
        End If

        Dim envKeyName As String
        Dim rootName As String
        rootName = "QuantLibXL Launcher\LauncherVersion10\Configuration\StartupActionsList"
        For Each keyName As String In r_.subKeyNames(rootName)
            envKeyName = rootName & "\" & keyName & "\SetEvaluationDate"
            r_.createKey(envKeyName)
            r_.setValue(rootName & "\" & keyName, "SetEvaluationDate", False)
        Next
        rootName = "QuantLibXL Launcher\LauncherVersion10\Environments"
        For Each keyName As String In r_.subKeyNames(rootName)
            envKeyName = rootName & "\" & keyName & "\StartupActions\" & "\SetEvaluationDate"
            r_.createKey(envKeyName)
            r_.setValue(rootName & "\" & keyName & "\StartupActions", "SetEvaluationDate", False)
        Next
        registryVersion_ = 10

    End Sub

    Private Sub upgradeVersion8to9()
        r_.copyKey("QuantLibXL Launcher\LauncherVersion8", "QuantLibXL Launcher\LauncherVersion9")

        ' The settings just copied from version 8 may or may not contain value ExcelPath
        ' depending on whether the user picked up an incremental upgrade of version 8.
        ' If the new registry key doesn't contain value ExcelPath then initialize it.
        If Not r_.valueExists("QuantLibXL Launcher\LauncherVersion9\Configuration", "ExcelPath") Then
            r_.setValue("QuantLibXL Launcher\LauncherVersion9\Configuration", _
                "ExcelPath", deriveDefaultExcelPath())
        End If

        Dim envKeyName As String
        Dim rootName As String
        rootName = "QuantLibXL Launcher\LauncherVersion9\Configuration\StartupActionsList"
        For Each keyName As String In r_.subKeyNames(rootName)
            envKeyName = rootName & "\" & keyName & "\MainChecks"
            r_.createKey(envKeyName)
            r_.setValue(rootName & "\" & keyName, "MainChecks", False)
        Next
        rootName = "QuantLibXL Launcher\LauncherVersion9\Environments"
        For Each keyName As String In r_.subKeyNames(rootName)
            envKeyName = rootName & "\" & keyName & "\StartupActions\" & "\MainChecks"
            r_.createKey(envKeyName)
            r_.setValue(rootName & "\" & keyName & "\StartupActions", "MainChecks", False)
        Next
        registryVersion_ = 9

    End Sub

    Private Sub upgradeVersion7to8()
        r_.copyKey("QuantLibXL Launcher\LauncherVersion7", "QuantLibXL Launcher\LauncherVersion8")
        Dim envKeyName As String
        Dim rootName As String
        rootName = "QuantLibXL Launcher\LauncherVersion8\Configuration\StartupActionsList"
        For Each keyName As String In r_.subKeyNames(rootName)
            envKeyName = rootName & "\" & keyName & "\CalibrateCMS"
            r_.createKey(envKeyName)
            r_.setValue(rootName & "\" & keyName, "CalibrateCMS", False)
        Next
        rootName = "QuantLibXL Launcher\LauncherVersion8\Environments"
        For Each keyName As String In r_.subKeyNames(rootName)
            envKeyName = rootName & "\" & keyName & "\StartupActions\" & "\CalibrateCMS"
            r_.createKey(envKeyName)
            r_.setValue(rootName & "\" & keyName & "\StartupActions", "CalibrateCMS", False)
        Next
        registryVersion_ = 8

    End Sub

    Private Sub upgradeVersion6to7()

        r_.copyKey("QuantLibXL Launcher\LauncherVersion6", "QuantLibXL Launcher\LauncherVersion7")
        r_.setValue("QuantLibXL Launcher\LauncherVersion7\Configuration", "ReutersPath", _
            QuantLibXL.Configuration.REUTERS_PATH_DEFAULT & "\" & QuantLibXL.Configuration.REUTERS_XLA_DEFAULT)
        r_.setValue("QuantLibXL Launcher\LauncherVersion7\Configuration", "BloombergPath", _
            QuantLibXL.Configuration.BLOOMBERG_PATH_DEFAULT & "\" & QuantLibXL.Configuration.BLOOMBERG_XLA_DEFAULT)
        r_.setValue("QuantLibXL Launcher\LauncherVersion7\Configuration", "ReutersSelected", False)
        r_.setValue("QuantLibXL Launcher\LauncherVersion7\Configuration", "BloombergSelected", False)

        registryVersion_ = 7

    End Sub

    ''''''''''''''''''''''''''''''''''''''''''
    ' initialize registry for first use
    ''''''''''''''''''''''''''''''''''''''''''
    Private Sub initializeRegistryLatestVersion()

        Dim path As String
        path = "QuantLibXL Launcher\LauncherVersion" & THIS_VERSION & "\Configuration"
        r_.createKey(path)
        r_.setValue(path, "SelectedEnvConfig", "")
        r_.setValue(path, "SelectedEnvName", "")
        r_.setValue(path, "ReutersPath", QuantLibXL.Configuration.REUTERS_PATH_DEFAULT & "\" & QuantLibXL.Configuration.REUTERS_XLA_DEFAULT)
        r_.setValue(path, "BloombergPath", QuantLibXL.Configuration.BLOOMBERG_PATH_DEFAULT & "\" & QuantLibXL.Configuration.BLOOMBERG_XLA_DEFAULT)
        r_.setValue(path, "ReutersSelected", False)
        r_.setValue(path, "BloombergSelected", False)
        r_.setValue(path, "ExcelPath", deriveDefaultExcelPath())
        r_.createKey(path & "\StartupActionsList")
        r_.createKey(path & "\Environments")

    End Sub

    'Private Sub initializeRegistryVersion8()

    '    Dim path As String
    '    path = "QuantLibXL Launcher\LauncherVersion" & THIS_VERSION & "\Configuration"
    '    r_.createKey(path)
    '    r_.setValue(path, "SelectedEnvConfig", "")
    '    r_.setValue(path, "SelectedEnvName", "")
    '    r_.setValue(path, "ReutersPath", QuantLibXL.Configuration.REUTERS_PATH_DEFAULT & "\" & QuantLibXL.Configuration.REUTERS_XLA_DEFAULT)
    '    r_.setValue(path, "BloombergPath", QuantLibXL.Configuration.BLOOMBERG_PATH_DEFAULT & "\" & QuantLibXL.Configuration.BLOOMBERG_XLA_DEFAULT)
    '    r_.setValue(path, "ReutersSelected", False)
    '    r_.setValue(path, "BloombergSelected", False)
    '    r_.setValue(path, "ExcelPath", deriveDefaultExcelPath())
    '    r_.createKey(path & "\StartupActionsList")
    '    r_.createKey(path & "\Environments")

    'End Sub

    'Private Sub initializeRegistryVersion7()

    '    r_.createKey("QuantLibXL Launcher\LauncherVersion7\Configuration")
    '    r_.setValue("QuantLibXL Launcher\LauncherVersion7\Configuration", "SelectedEnvConfig", "")
    '    r_.setValue("QuantLibXL Launcher\LauncherVersion7\Configuration", "SelectedEnvName", "")
    '    r_.setValue("QuantLibXL Launcher\LauncherVersion7\Configuration", "ReutersPath", _
    '        QuantLibXL.Configuration.REUTERS_PATH_DEFAULT)
    '    r_.setValue("QuantLibXL Launcher\LauncherVersion7\Configuration", "BloombergPath", _
    '        QuantLibXL.Configuration.BLOOMBERG_PATH_DEFAULT)
    '    r_.setValue("QuantLibXL Launcher\LauncherVersion7\Configuration", "ReutersSelected", False)
    '    r_.setValue("QuantLibXL Launcher\LauncherVersion7\Configuration", "BloombergSelected", False)
    '    r_.createKey("QuantLibXL Launcher\LauncherVersion7\Configuration\StartupActionsList")
    '    r_.createKey("QuantLibXL Launcher\LauncherVersion7\Environments")

    'End Sub


    'Private Sub initializeRegistryVersion6()

    '    r_.createKey("QuantLibXL Launcher\LauncherVersion6\Configuration")
    '    r_.setValue("QuantLibXL Launcher\LauncherVersion6\Configuration", "SelectedEnvConfig", "")
    '    r_.setValue("QuantLibXL Launcher\LauncherVersion6\Configuration", "SelectedEnvName", "")
    '    r_.createKey("QuantLibXL Launcher\LauncherVersion6\Configuration\StartupActionsList")
    '    r_.createKey("QuantLibXL Launcher\LauncherVersion6\Environments")

    'End Sub

End Module
