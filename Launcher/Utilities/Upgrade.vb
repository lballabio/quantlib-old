
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
    Public Const THIS_VERSION As Integer = 11

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
            initializeRegistryVersion11()
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

        If r_.keyExists("QuantLibXL Launcher\LauncherVersion11") Then
            registryVersion_ = 11
        ElseIf r_.keyExists("QuantLibXL Launcher\LauncherVersion10") Then
            registryVersion_ = 10
        ElseIf r_.keyExists("QuantLibXL Launcher\LauncherVersion9") Then
            registryVersion_ = 9
        ElseIf r_.keyExists("QuantLibXL Launcher\LauncherVersion8") Then
            registryVersion_ = 8
        Else
            registryVersion_ = 0
        End If

    End Sub

    Private Sub updateRegistry()

        If registryVersion_ = 0 Then
            initializeRegistryVersion11()
            Exit Sub
        End If

        If registryVersion_ = 8 Then upgradeVersion8to9()
        If registryVersion_ = 9 Then upgradeVersion9to10()
        If registryVersion_ = 10 Then upgradeVersion10to11()

    End Sub

    ''''''''''''''''''''''''''''''''''''''''''''''''''
    ' upgrade registry from previous launcher versions
    ''''''''''''''''''''''''''''''''''''''''''''''''''

    Private Sub upgradeVersion10to11()

        r_.copyKey("QuantLibXL Launcher\LauncherVersion10", "QuantLibXL Launcher\LauncherVersion11")
        r_.setValue("QuantLibXL Launcher\LauncherVersion11\Configuration", "FeedUse", "Reuters")

        Dim rootName As String
        rootName = "QuantLibXL Launcher\LauncherVersion11\Configuration\StartupActionsList"
        For Each keyName As String In r_.subKeyNames(rootName)
            r_.deleteValue(rootName & "\" & keyName, "LoadMurexYieldCurve")
            r_.setValue(rootName & "\" & keyName, "InitSource", "Excel")
        Next keyName
        rootName = "QuantLibXL Launcher\LauncherVersion11\Environments"
        For Each keyName As String In r_.subKeyNames(rootName)
            Dim subKeyName As String = rootName & "\" & keyName
            r_.deleteValue(rootName & "\" & keyName & "\StartupActions", "LoadMurexYieldCurve")
            r_.setValue(subKeyName & "\StartupActions", "InitSource", "Excel")
            r_.setValue(subKeyName, "FeedUse", "Reuters")
            r_.createKey(subKeyName & "\Variables")
            Dim addinKeyName As String = subKeyName & "\AddinList"
            For Each addinName As String In r_.valueNames(addinKeyName)
                Dim Path As String = r_.getValue(addinKeyName, addinName)
                r_.deleteValue(addinKeyName, addinName)
                r_.createKey(addinKeyName & "\" & addinName)
                r_.setValue(addinKeyName & "\" & addinName, "Path", Path)
                r_.setValue(addinKeyName & "\" & addinName, "DeliveredByLauncher", True)
            Next
        Next keyName

        registryVersion_ = 11

    End Sub

    Private Sub upgradeVersion9to10()

        r_.copyKey("QuantLibXL Launcher\LauncherVersion9", "QuantLibXL Launcher\LauncherVersion10")

        Dim rootName As String
        rootName = "QuantLibXL Launcher\LauncherVersion10\Configuration\StartupActionsList"
        For Each keyName As String In r_.subKeyNames(rootName)
            r_.createKey(rootName & "\" & keyName & "\SetEvaluationDate")
            r_.setValue(rootName & "\" & keyName, "SetEvaluationDate", False)
            r_.createKey(rootName & "\" & keyName & "\EvaluationDate")
            r_.setValue(rootName & "\" & keyName, "EvaluationDate", 0)
        Next
        rootName = "QuantLibXL Launcher\LauncherVersion10\Environments"
        For Each keyName As String In r_.subKeyNames(rootName)
            r_.createKey(rootName & "\" & keyName & "\StartupActions\SetEvaluationDate")
            r_.setValue(rootName & "\" & keyName & "\StartupActions", "SetEvaluationDate", False)
            r_.createKey(rootName & "\" & keyName & "\StartupActions\EvaluationDate")
            r_.setValue(rootName & "\" & keyName & "\StartupActions", "EvaluationDate", 0)
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
            envKeyName = rootName & "\" & keyName & "\StartupActions\MainChecks"
            r_.createKey(envKeyName)
            r_.setValue(rootName & "\" & keyName & "\StartupActions", "MainChecks", False)
        Next
        registryVersion_ = 9

    End Sub

    ''''''''''''''''''''''''''''''''''''''''''
    ' initialize registry for first use
    ''''''''''''''''''''''''''''''''''''''''''
    Private Sub initializeRegistryVersion11()

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
        r_.setValue(path, "InitSource", "Excel")
        r_.setValue(path, "FeedUse", "Reuters")
        r_.createKey(path & "\StartupActionsList")
        r_.createKey(path & "\Environments")

    End Sub

End Module
