
'Copyright (C) 2006 Eric Ehlers

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
    Private version_ As Integer

    Private Sub getVersionNumber()
        If r_.keyExists("QuantLibXL") Then
            If r_.valueExists("QuantLibXL\Configuration", "Version") Then
                version_ = r_.getValue("QuantLibXL\Configuration", "Version")
            Else
                version_ = 1
            End If
        Else
            version_ = 0
        End If
    End Sub

    'Private Sub initializeRegistryVersion2()
    '    r_.createKey("QuantLibXL\Configuration")
    '    r_.setValue("QuantLibXL\Configuration", "SelectedEnvConfig", "")
    '    r_.setValue("QuantLibXL\Configuration", "SelectedEnvName", "")
    '    r_.setValue("QuantLibXL\Configuration", "Version", 2)
    '    r_.createKey("QuantLibXL\Environments")
    'End Sub

    'Private Sub initializeRegistryVersion3()
    '    r_.createKey("QuantLibXL\Configuration")
    '    r_.setValue("QuantLibXL\Configuration", "SelectedEnvConfig", "")
    '    r_.setValue("QuantLibXL\Configuration", "SelectedEnvName", "")
    '    r_.setValue("QuantLibXL\Configuration", "Version", 3)
    '    r_.createKey("QuantLibXL\Configuration\Environments")
    '    r_.createKey("QuantLibXL\Environments")
    'End Sub

    Private Sub initializeRegistryVersion4()
        r_.createKey("QuantLibXL\Configuration")
        r_.setValue("QuantLibXL\Configuration", "SelectedEnvConfig", "")
        r_.setValue("QuantLibXL\Configuration", "SelectedEnvName", "")
        r_.setValue("QuantLibXL\Configuration", "Version", 4)
        r_.createKey("QuantLibXL\Configuration\Environments")
        r_.createKey("QuantLibXL\Environments")
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
        version_ = 2
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
        version_ = 3
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
        version_ = 4
    End Sub

    Private Sub updateRegistry()
        If version_ = 0 Then
            initializeRegistryVersion4()
            Exit Sub
        End If

        If version_ = 1 Then upgradeVersion1to2()
        If version_ = 2 Then upgradeVersion2to3()
        If version_ = 3 Then upgradeVersion3to4()

    End Sub

    Public Sub run()
        Try
            r_ = New QuantLibXL.RegistryEditor
            getVersionNumber()
            updateRegistry()
            r_ = Nothing
            Exit Sub
        Catch ex As Exception
        End Try

        Try
            r_ = New QuantLibXL.RegistryEditor
            r_.deleteKey("QuantLibXL")
            initializeRegistryVersion4()
            r_ = Nothing
        Catch ex As Exception
        End Try

        MsgBox("QuantLibXL Launcher encountered an error " _
            & "while attempting to upgrade user preferences.")

    End Sub

End Module
