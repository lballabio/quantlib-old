
'Copyright (C) 2006, 2007 Eric Ehlers

'This file is part of QuantLib, a free-software/open-source library
'for financial quantitative analysts and developers - http://quantlib.org/

'QuantLib is free software: you can redistribute it and/or modify it
'under the terms of the QuantLib license.  You should have received a
'copy of the license along with this program; if not, please email
'<quantlib-dev@lists.sf.net>. The license is also available online at
'<http://quantlib.org/reference/license.html>.

'This program is distributed in the hope that it will be useful, but WITHOUT
'ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
'FOR A PARTICULAR PURPOSE.  See the license for more details.

' Module Upgrade - make any necessary changes to the user's environment
' following an upgrade of the QuantLibXL Launcher

Module Upgrade

    Private r_ As QuantLibXL.RegistryEditor
    Private registryVersion_ As Integer
    Public Const THIS_VERSION As Integer = 6

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
            r_.deleteKey("QuantLibXL Launcher\LauncherVersion6")
            initializeRegistryVersion6()
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

        If r_.keyExists("QuantLibXL Launcher") Then
            If r_.keyExists("QuantLibXL Launcher\LauncherVersion6") Then
                registryVersion_ = 6
            Else
                registryVersion_ = 5
            End If
        ElseIf r_.keyExists("QuantLibXL") Then
            If r_.valueExists("QuantLibXL\Configuration", "Version") Then
                registryVersion_ = r_.getValue("QuantLibXL\Configuration", "Version")
            Else
                registryVersion_ = 1
            End If
        Else
            registryVersion_ = 0
        End If

    End Sub

    Private Sub updateRegistry()

        If registryVersion_ = 0 Then
            initializeRegistryVersion6()
            Exit Sub
        End If

        'If registryVersion_ > THIS_VERSION Then

        '    Dim msg As String = "The preferences were saved with version " & registryVersion_ _
        '        & " of the Launcher." & vbCrLf & vbCrLf _
        '        & "You are running version " & THIS_VERSION _
        '        & " of the Launcher." & vbCrLf & vbCrLf _
        '        & "The Launcher which saved the preferences " _
        '        & "is newer than the Launcher which is loading the preferences."

        '    Throw New Exception(msg)

        'End If

        If registryVersion_ = 1 Then upgradeVersion1to2()
        If registryVersion_ = 2 Then upgradeVersion2to3()
        If registryVersion_ = 3 Then upgradeVersion3to4()
        If registryVersion_ = 4 Then upgradeVersion4to5()
        If registryVersion_ = 5 Then upgradeVersion5to6()

    End Sub

    ''''''''''''''''''''''''''''''''''''''''''''''''''
    ' upgrade registry from previous launcher versions
    ''''''''''''''''''''''''''''''''''''''''''''''''''

    Private Sub upgradeVersion5to6()

        r_.createKey("QuantLibXL Launcher")
        r_.copyKey("QuantLibXL", "QuantLibXL Launcher\LauncherVersion6")
        r_.deleteValue("QuantLibXL Launcher\LauncherVersion6\Configuration\", "Version")

        For Each environmentName As String In r_.subKeyNames("QuantLibXL Launcher\LauncherVersion6\Environments")
            Dim environmentKey As String = "QuantLibXL Launcher\LauncherVersion6\Environments\" & environmentName
            Dim addinDirectory As String = r_.getValue(environmentKey, "AddinDirectory")
            Dim addinName As String = r_.getValue(environmentKey, "AddinName")
            Dim addinFull As String
            If Len(addinDirectory) > 0 Then
                addinFull = addinDirectory & "\" & addinName
            Else
                addinFull = addinName
            End If
            r_.createKey(environmentKey & "\AddinList")
            r_.setValue(environmentKey & "\AddinList", "Addin0", addinFull)
            r_.deleteValue(environmentKey, "AddinDirectory")
            r_.deleteValue(environmentKey, "AddinName")

            r_.setValue(environmentKey, "FrameWorkVersion", 5)
        Next

        registryVersion_ = THIS_VERSION

    End Sub

    Private Sub upgradeVersion4to5()

        Dim envKeyName As String

        For Each keyName As String In r_.subKeyNames("QuantLibXL\Configuration\Environments")
            envKeyName = "QuantLibXL\Configuration\Environments\" & keyName & "\"
            r_.deleteValue(envKeyName, "AddinDirectory")
            r_.deleteValue(envKeyName, "AddinName")
            r_.deleteValue(envKeyName, "Framework")
            r_.deleteValue(envKeyName, "HelpFile")
            r_.deleteValue(envKeyName, "name")
            r_.deleteValue(envKeyName, "Workbooks")
        Next

        envKeyName = "QuantLibXL\Configuration\"
        r_.moveKey(envKeyName & "Environments", envKeyName & "StartupActionsList")

        For Each keyName As String In r_.subKeyNames("QuantLibXL\Environments")

            envKeyName = "QuantLibXL\Environments\" & keyName

            r_.setValue(envKeyName, "UserConfigurationFile", "C:\projects\Launcher\Users\users.xml")
            r_.setValue(envKeyName, "FunctionMetadata", "C:\projects\QuantLibAddin\gensrc\metadata")

            r_.createKey(envKeyName & "\StartupActions")
            r_.moveValue(envKeyName, envKeyName & "\StartupActions", "CapVolBootstrap")
            r_.moveValue(envKeyName, envKeyName & "\StartupActions", "FitCMS")
            r_.moveValue(envKeyName, envKeyName & "\StartupActions", "IndexesTimeSeries")
            r_.moveValue(envKeyName, envKeyName & "\StartupActions", "LoadBonds")
            r_.moveValue(envKeyName, envKeyName & "\StartupActions", "LoadMurexYieldCurve")
            r_.moveValue(envKeyName, envKeyName & "\StartupActions", "StaticData")
            r_.moveValue(envKeyName, envKeyName & "\StartupActions", "SwapSmileBootstrap")
            r_.moveValue(envKeyName, envKeyName & "\StartupActions", "SwapVolBootstrap")
            r_.moveValue(envKeyName, envKeyName & "\StartupActions", "YieldCurveBootstrap")

        Next

        r_.setValue("QuantLibXL\Configuration", "Version", 5)
        registryVersion_ = 5

    End Sub

    Private Sub upgradeVersion3to4()

        Dim envKeyName As String
        For Each keyName As String In r_.subKeyNames("QuantLibXL\Environments")
            envKeyName = "QuantLibXL\Environments\" & keyName
            r_.setValue(envKeyName, "StaticData", False)
        Next
        For Each keyName As String In r_.subKeyNames("QuantLibXL\Configuration\Environments")
            envKeyName = "QuantLibXL\Configuration\Environments\" & keyName
            r_.setValue(envKeyName, "StaticData", False)
        Next
        r_.setValue("QuantLibXL\Configuration", "Version", 4)
        registryVersion_ = 4

    End Sub

    Private Sub upgradeVersion2to3()

        Dim envKeyName As String
        For Each keyName As String In r_.subKeyNames("QuantLibXL\Environments")
            envKeyName = "QuantLibXL\Environments\" & keyName
            r_.setValue(envKeyName, "HelpFile", "")
            r_.setValue(envKeyName, "SwapSmileBootstrap", False)
            r_.setValue(envKeyName, "FitCMS", False)
            r_.setValue(envKeyName, "LoadBonds", False)
        Next
        r_.createKey("QuantLibXL\Configuration\Environments")
        r_.setValue("QuantLibXL\Configuration", "Version", 3)
        registryVersion_ = 3

    End Sub

    Private Sub upgradeVersion1to2()

        r_.moveKey("QuantLibXL\Configuration\Environments", "QuantLibXL\Environments")
        r_.deleteKey("QuantLibXL\Configuration\Users")
        r_.deleteKey("QuantLibXL\Launch")
        Dim envKeyName As String
        For Each keyName As String In r_.subKeyNames("QuantLibXL\Environments")
            envKeyName = "QuantLibXL\Environments\" & keyName
            r_.renameValue(envKeyName, "Bootstrap", "YieldCurveBootstrap")
            r_.setValue(envKeyName, "LoadMurexYieldCurve", False)
            r_.setValue(envKeyName, "CapVolBootstrap", False)
            r_.setValue(envKeyName, "SwapVolBootstrap", False)
            r_.setValue(envKeyName, "IndexesTimeSeries", False)
        Next
        r_.setValue("QuantLibXL\Configuration", "Version", 2)
        registryVersion_ = 2

    End Sub

    ''''''''''''''''''''''''''''''''''''''''''
    ' initialize registry for first use
    ''''''''''''''''''''''''''''''''''''''''''

    Private Sub initializeRegistryVersion6()

        r_.createKey("QuantLibXL Launcher\LauncherVersion6\Configuration")
        r_.setValue("QuantLibXL Launcher\LauncherVersion6\Configuration", "SelectedEnvConfig", "")
        r_.setValue("QuantLibXL Launcher\LauncherVersion6\Configuration", "SelectedEnvName", "")
        r_.createKey("QuantLibXL Launcher\LauncherVersion6\Configuration\StartupActionsList")
        r_.createKey("QuantLibXL Launcher\LauncherVersion6\Environments")

    End Sub

    'Private Sub initializeRegistryVersion5()

    '    r_.createKey("QuantLibXL\Configuration")
    '    r_.setValue("QuantLibXL\Configuration", "SelectedEnvConfig", "")
    '    r_.setValue("QuantLibXL\Configuration", "SelectedEnvName", "")
    '    r_.setValue("QuantLibXL\Configuration", "Version", 5)
    '    r_.createKey("QuantLibXL\Configuration\StartupActionsList")
    '    r_.createKey("QuantLibXL\Environments")

    'End Sub

    'Private Sub initializeRegistryVersion4()
    '
    '    r_.createKey("QuantLibXL\Configuration")
    '    r_.setValue("QuantLibXL\Configuration", "SelectedEnvConfig", "")
    '    r_.setValue("QuantLibXL\Configuration", "SelectedEnvName", "")
    '    r_.setValue("QuantLibXL\Configuration", "Version", 4)
    '    r_.createKey("QuantLibXL\Configuration\Environments")
    '    r_.createKey("QuantLibXL\Environments")
    '
    'End Sub

    'Private Sub initializeRegistryVersion3()
    '
    '    r_.createKey("QuantLibXL\Configuration")
    '    r_.setValue("QuantLibXL\Configuration", "SelectedEnvConfig", "")
    '    r_.setValue("QuantLibXL\Configuration", "SelectedEnvName", "")
    '    r_.setValue("QuantLibXL\Configuration", "Version", 3)
    '    r_.createKey("QuantLibXL\Configuration\Environments")
    '    r_.createKey("QuantLibXL\Environments")
    '
    'End Sub

    'Private Sub initializeRegistryVersion2()
    '
    '    r_.createKey("QuantLibXL\Configuration")
    '    r_.setValue("QuantLibXL\Configuration", "SelectedEnvConfig", "")
    '    r_.setValue("QuantLibXL\Configuration", "SelectedEnvName", "")
    '    r_.setValue("QuantLibXL\Configuration", "Version", 2)
    '    r_.createKey("QuantLibXL\Environments")
    '
    'End Sub

End Module
