
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

Imports System.IO
Imports System.Deployment.Application

Public Class FormMain

    ''''''''''''''''''''''''''''''''''''''''''
    ' private members
    ''''''''''''''''''''''''''''''''''''''''''

    Private config_ As QuantLibXL.Config
    Private envUserconfigured_ As QuantLibXL.EnvironmentList
    Private envPreconfigured_ As QuantLibXL.EnvironmentList
    Private selectedEnvironment_ As QuantLibXL.Environment
    Private processingEvents_ As Boolean = True
    Private qlxlDir_ As String

    Private Const PRECONFIGURED = "Preconfigured"
    Private Const USERCONFIGURED = "UserConfigured"

    ''''''''''''''''''''''''''''''''''''''''''
    ' properties
    ''''''''''''''''''''''''''''''''''''''''''

    Private Property SelectedEnvironment() As QuantLibXL.Environment
        Get
            If selectedEnvironment_ Is Nothing Then
                Throw New Exception("No environment selected.")
            Else
                SelectedEnvironment = selectedEnvironment_
            End If
        End Get
        Set(ByVal value As QuantLibXL.Environment)
            If value Is Nothing Then
                Throw New Exception("No environment selected.")
            Else
                selectedEnvironment_ = value
                txtFramework.Text = SelectedEnvironment.Framework
                txtWorkbooks.Text = SelectedEnvironment.Workbooks
                txtAddinDir.Text = SelectedEnvironment.AddinDirectory
                txtAddinName.Text = SelectedEnvironment.AddinName
                txtHelpPath.Text = SelectedEnvironment.HelpPath
                cbYCBootstrap.Checked = SelectedEnvironment.YieldCurveBootstrap
                cbLoadMurexYC.Checked = SelectedEnvironment.LoadMurexYieldCurve
                cbCapVolBootstrap.Checked = SelectedEnvironment.CapVolBootstrap
                cbSwapVolBootstrap.Checked = SelectedEnvironment.SwapVolBootstrap
                cbSwapSmileBootstrap.Checked = SelectedEnvironment.SwapSmileBootstrap
                cbFitCMS.Checked = SelectedEnvironment.FitCMS
                cbIndexesTimeSeries.Checked = SelectedEnvironment.IndexesTimeSeries
                cbLoadBonds.Checked = SelectedEnvironment.LoadBonds
                cbStaticData.Checked = SelectedEnvironment.StaticData
            End If
        End Set
    End Property

    ''''''''''''''''''''''''''''''''''''''''''
    ' initialization
    ''''''''''''''''''''''''''''''''''''''''''

    Public Sub New()
        qlxlDir_ = Environ(QUANTLIBXL_DIR)  ' not fatal if invalid

        InitializeComponent()

        Try
            Upgrade.run()
            initializeConfig()
            overrideActions()
            initializeLists()
        Catch ex As Exception
            MsgBox(ex.Message, MsgBoxStyle.OkOnly + MsgBoxStyle.Critical, _
                "QuantLibXL Fatal Error")
            Environment.Exit(1)
        End Try

    End Sub

    Private Sub initializeLists()
        For Each environment As QuantLibXL.Environment In envPreconfigured_.Environments
            lstPreconfigured.Items.Add(environment.Name)
        Next
        For Each environment As QuantLibXL.Environment In envUserconfigured_.Environments
            lstUserconfigured.Items.Add(environment.Name)
        Next
        selectEnvironment()
    End Sub

    Private Sub selectEnvironment()
        ' Attempt to select the environment that was active
        ' last time the app shut down.  The index that was saved before
        ' may be invalid if the configuration has been changed.
        Try
            If config_.SelectedEnvConfig = PRECONFIGURED Then
                If lstPreconfigured.Items.Contains(config_.SelectedEnvName) Then
                    lstPreconfigured.SelectedIndex = lstPreconfigured.Items.IndexOf(config_.SelectedEnvName)
                    Exit Sub
                End If
            ElseIf config_.SelectedEnvConfig = USERCONFIGURED Then
                If lstUserconfigured.Items.Contains(config_.SelectedEnvName) Then
                    lstUserconfigured.SelectedIndex = lstUserconfigured.Items.IndexOf(config_.SelectedEnvName)
                    Exit Sub
                End If
            End If
            ' if none of the above worked then try for a safe option
            If lstPreconfigured.Items.Count >= 1 Then
                lstPreconfigured.SelectedIndex = 0
            Else
                Throw New Exception("Could not identify an environment to activate")
            End If
        Catch ex As Exception
            Throw New Exception("Error activating environment " _
                & ex.Message)
        End Try

    End Sub

    Private Function deriveConfigPath() As String

        deriveConfigPath = configPath() & "\Configuration\config.xml"

        If Not fileExists(deriveConfigPath) Then
            Throw New Exception("Error: this application loads configuration information " & _
            "from the following location:" & vbCrLf & vbCrLf & deriveConfigPath & vbCrLf & vbCrLf & _
            "The path appears to be invalid.")
        End If

    End Function

    Private Sub initializeConfig()

        If Not fileExists(EXCEL_PATH) Then
            Throw New Exception("Error: this application is configured to load Excel " & _
            "from the following location:" & vbCrLf & vbCrLf & EXCEL_PATH & vbCrLf & vbCrLf & _
            "the path appears to be invalid")
        End If

        ' Derive the location of the launcher configuration file config.xml:
        ' - If this application is being run across the network (a ClickOnce installation)
        '   then get the config file from the local ClickOnce directory
        ' - If this application is being run from the command line then
        '   look for config.xml in the location specified by QUANTLIBXL_CONFIG_PATH
        Dim configPath As String
        If (ApplicationDeployment.IsNetworkDeployed) Then
            configPath = ApplicationDeployment.CurrentDeployment.DataDirectory & "\Configuration\config.xml"
        Else
            configPath = deriveConfigPath()
        End If

        Dim userList As QuantLibXL.UserList = Nothing
        Try
            Dim xmlReader As New QuantLibXL.XmlReader(configPath, "Configuration")
            xmlReader.serializeObject(envPreconfigured_, "Environments")
            xmlReader.serializeObject(userList, "Users")
        Catch ex As Exception
            Throw New Exception("Error processing config file config.xml:" _
                & vbcrlf & vbcrlf & ex.Message)
        End Try

        userList.validate()
        envPreconfigured_.validate()
        envPreconfigured_.setConfigured()

        Try
            Dim registryReader As New QuantLibXL.RegistryReader()
            registryReader.serializeObject(config_, "Configuration")
            registryReader.serializeObject(envUserconfigured_, "Environments")
        Catch ex As Exception
            Throw New Exception("Error accessing the Windows registry:" _
                 & vbCrLf & vbCrLf & ex.Message)
        End Try

    End Sub

    Private Sub overrideActions()
        ' Preconfigured environments have been loaded from the configuration file.
        ' For each of these, override the startup actions with values saved to
        ' the user's registry from previous session (if any)
        Try

            Dim envPreconfigured As QuantLibXL.Environment
            Dim envOverride As QuantLibXL.Environment
            For Each envOverride In config_.EnvOverrides.Environments
                If envPreconfigured_.nameInUse(envOverride.Name) Then
                    envPreconfigured = envPreconfigured_.nameToEnvironment(envOverride.Name)
                    envPreconfigured.overrideActions(envOverride)
                End If
            Next

        Catch ex As Exception
            Throw New Exception("Error deriving startup actions: " & ex.Message)
        End Try

    End Sub

    ''''''''''''''''''''''''''''''''''''''''''
    ' events - main form
    ''''''''''''''''''''''''''''''''''''''''''

    Private Sub btnClose_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles btnClose.Click
        Try
            config_.EnvOverrides = envPreconfigured_
            Dim registryWriter As New QuantLibXL.RegistryWriter()
            registryWriter.deleteKey("Configuration")
            registryWriter.serializeObject(config_, "Configuration")
            registryWriter.deleteKey("Environments")
            registryWriter.serializeObject(envUserconfigured_, "Environments")
        Catch ex As Exception
            MsgBox("Error while closing launcher:" & vbCrLf & vbCrLf & ex.Message, _
                MsgBoxStyle.OkOnly + MsgBoxStyle.Exclamation, "QuantLibXL Error")
        End Try
        Close()
    End Sub

    Private Sub btnLaunch_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles btnLaunch.Click
        Try
            SelectedEnvironment.launch()
        Catch ex As Exception
            MsgBox("Error on launch command:" _
                 & vbcrlf & vbcrlf & ex.Message, _
                 MsgBoxStyle.OkOnly + MsgBoxStyle.Exclamation, _
                "QuantLibXL Error")
        End Try
    End Sub

    ''''''''''''''''''''''''''''''''''''''''''
    ' events - environment - lists
    ''''''''''''''''''''''''''''''''''''''''''

    Private Sub lstPreconfigured_SelectedIndexChanged(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles lstPreconfigured.SelectedIndexChanged
        Try
            If lstPreconfigured.SelectedIndex = -1 Then Exit Sub
            If processingEvents_ Then
                processingEvents_ = False
                SelectedEnvironment = envPreconfigured_.nameToEnvironment(lstPreconfigured.Text)
                lstUserconfigured.SelectedIndex = -1
                setEnabled(False)
                processingEvents_ = True
                config_.SelectedEnvConfig = PRECONFIGURED
                config_.SelectedEnvName = lstPreconfigured.Text
            End If
        Catch ex As Exception
            MsgBox("Error processing environment selection:" _
                 & vbcrlf & vbcrlf & ex.Message, _
                 MsgBoxStyle.OkOnly + MsgBoxStyle.Exclamation, _
                "QuantLibXL Error")
        End Try
    End Sub

    Private Sub lstUserconfigured_SelectedIndexChanged(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles lstUserconfigured.SelectedIndexChanged
        Try
            If lstUserconfigured.SelectedIndex = -1 Then Exit Sub
            If processingEvents_ Then
                processingEvents_ = False
                SelectedEnvironment = envUserconfigured_.nameToEnvironment(lstUserconfigured.Text)
                lstPreconfigured.SelectedIndex = -1
                setEnabled(True)
                processingEvents_ = True
                config_.SelectedEnvConfig = USERCONFIGURED
                config_.SelectedEnvName = lstUserconfigured.Text
            End If
        Catch ex As Exception
            MsgBox("Error processing environment selection:" _
                 & vbcrlf & vbcrlf & ex.Message, _
                 MsgBoxStyle.OkOnly + MsgBoxStyle.Exclamation, _
                "QuantLibXL Error")
        End Try
    End Sub

    ''''''''''''''''''''''''''''''''''''''''''
    ' events - environment - buttons
    ''''''''''''''''''''''''''''''''''''''''''

    Private Sub btnNew_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles btnNew.Click
        Try
            Dim text As String = envUserconfigured_.deriveNewName()
            Dim frm As New FormNameEnvironment(envUserconfigured_, text)
            If frm.ShowDialog() = System.Windows.Forms.DialogResult.OK Then
                SelectedEnvironment = envUserconfigured_.createEnvironment(frm.NewEnvironmentName)
                Dim newIndex As Integer = lstUserconfigured.Items.Add(frm.NewEnvironmentName)
                lstUserconfigured.SelectedIndex = newIndex
            End If
        Catch ex As Exception
            MsgBox("Error creating new environment:" _
                 & vbcrlf & vbcrlf & ex.Message, _
                 MsgBoxStyle.OkOnly + MsgBoxStyle.Exclamation, _
                "QuantLibXL Error")
        End Try
    End Sub

    Private Sub btnCopy_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles btnCopy.Click
        Try
            Dim text As String = envUserconfigured_.deriveCopyName(SelectedEnvironment.Name)
            Dim frm As New FormNameEnvironment(envUserconfigured_, text)
            If frm.ShowDialog() = System.Windows.Forms.DialogResult.OK Then
                SelectedEnvironment = envUserconfigured_.copyEnvironment(SelectedEnvironment, frm.NewEnvironmentName)
                Dim newIndex As Integer = lstUserconfigured.Items.Add(frm.NewEnvironmentName)
                lstUserconfigured.SelectedIndex = newIndex
            End If
        Catch ex As Exception
            MsgBox("Error copying environment:" _
                 & vbcrlf & vbcrlf & ex.Message, _
                 MsgBoxStyle.OkOnly + MsgBoxStyle.Exclamation, _
                "QuantLibXL Error")
        End Try
    End Sub

    Private Sub btnDelete_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles btnDelete.Click
        Try
            If lstUserconfigured.SelectedIndex = -1 Then
                MsgBox("No environment selected", _
                    MsgBoxStyle.OkOnly + MsgBoxStyle.Exclamation, _
                    "QuantLibXL Error")
                Exit Sub
            End If
            envUserconfigured_.deleteEnvironment(lstUserconfigured.Text)
            Dim deletedIndex As Long = lstUserconfigured.SelectedIndex
            lstUserconfigured.Items.RemoveAt(lstUserconfigured.SelectedIndex)
            If lstUserconfigured.Items.Count Then
                If deletedIndex Then
                    lstUserconfigured.SelectedIndex = deletedIndex - 1
                Else
                    lstUserconfigured.SelectedIndex = deletedIndex
                End If
            Else
                lstPreconfigured.SelectedIndex = 0
            End If
        Catch ex As Exception
            MsgBox("Error deleting environment:" _
                 & vbcrlf & vbcrlf & ex.Message, _
                 MsgBoxStyle.OkOnly + MsgBoxStyle.Exclamation, _
                "QuantLibXL Error")
        End Try
    End Sub

    Private Sub btnClear_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles btnClear.Click
        Try
            clear()
        Catch ex As Exception
            MsgBox("Error clearing environment:" _
                 & vbcrlf & vbcrlf & ex.Message, _
                 MsgBoxStyle.OkOnly + MsgBoxStyle.Exclamation, _
                "QuantLibXL Error")
        End Try
    End Sub

    Private Sub btnRename_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles btnRename.Click
        If lstUserconfigured.SelectedIndex = -1 Then Exit Sub
        Try
            Dim text As String = SelectedEnvironment.Name
            Dim frm As New FormNameEnvironment(envUserconfigured_, text)
            If frm.ShowDialog() = System.Windows.Forms.DialogResult.OK _
            And frm.ValueChanged Then
                envUserconfigured_.renameEnvironment(text, frm.NewEnvironmentName)
                SelectedEnvironment.Name = frm.NewEnvironmentName
                config_.SelectedEnvName = frm.NewEnvironmentName
                processingEvents_ = False
                lstUserconfigured.Items.Remove(text)
                Dim newIndex As Integer = lstUserconfigured.Items.Add(frm.NewEnvironmentName)
                lstUserconfigured.SelectedIndex = newIndex
                processingEvents_ = True
            End If
        Catch ex As Exception
            MsgBox("Error renaming environment:" _
                 & vbcrlf & vbcrlf & ex.Message, _
                 MsgBoxStyle.OkOnly + MsgBoxStyle.Exclamation, _
                "QuantLibXL Error")
        End Try
    End Sub

    ''''''''''''''''''''''''''''''''''''''''''
    ' events - startup actions
    ''''''''''''''''''''''''''''''''''''''''''

    Private Sub cbYCBootstrap_CheckedChanged(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles cbYCBootstrap.CheckedChanged
        SelectedEnvironment.YieldCurveBootstrap = cbYCBootstrap.Checked
    End Sub

    Private Sub cbLoadMurexYC_CheckedChanged(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles cbLoadMurexYC.CheckedChanged
        SelectedEnvironment.LoadMurexYieldCurve = cbLoadMurexYC.Checked
    End Sub

    Private Sub cbCapVolBootstrap_CheckedChanged(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles cbCapVolBootstrap.CheckedChanged
        SelectedEnvironment.CapVolBootstrap = cbCapVolBootstrap.Checked
    End Sub

    Private Sub cbSwapVolBootstrap_CheckedChanged(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles cbSwapVolBootstrap.CheckedChanged
        SelectedEnvironment.SwapVolBootstrap = cbSwapVolBootstrap.Checked
    End Sub

    Private Sub cbSwapSmileBootstrap_CheckedChanged(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles cbSwapSmileBootstrap.CheckedChanged
        SelectedEnvironment.SwapSmileBootstrap = cbSwapSmileBootstrap.Checked
    End Sub

    Private Sub cbFitCMS_CheckedChanged(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles cbFitCMS.CheckedChanged
        SelectedEnvironment.FitCMS = cbFitCMS.Checked
    End Sub

    Private Sub cbIndexesTimeSeries_CheckedChanged(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles cbIndexesTimeSeries.CheckedChanged
        SelectedEnvironment.IndexesTimeSeries = cbIndexesTimeSeries.Checked
    End Sub

    Private Sub cbLoadBonds_CheckedChanged(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles cbLoadBonds.CheckedChanged
        SelectedEnvironment.LoadBonds = cbLoadBonds.Checked
    End Sub
    Private Sub cbStaticData_CheckedChanged(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles cbStaticData.CheckedChanged
        SelectedEnvironment.StaticData = cbStaticData.Checked
    End Sub

    ''''''''''''''''''''''''''''''''''''''''''
    ' events - path - buttons
    ''''''''''''''''''''''''''''''''''''''''''

    Private Function deriveDefaultDir(ByVal testPath As String, ByVal subDir As String) As String
        Try
            If dirExists(testPath) Then
                deriveDefaultDir = testPath
            ElseIf dirExists(qlxlDir_ & "\" & subDir) Then
                deriveDefaultDir = qlxlDir_ & "\" & subDir
            Else
                deriveDefaultDir = ""
            End If
            Exit Function
        Catch ex As Exception
            deriveDefaultDir = ""
        End Try
    End Function

    Private Sub btnFrameworkSelect_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles btnFrameworkSelect.Click
        Try
            Dim dlg As New OpenFileDialog()
            dlg.InitialDirectory = deriveDefaultDir(Path.GetFileName(txtFramework.Text), "framework")
            dlg.FileName = "QuantLibXL.xla"
            dlg.Filter = "Excel VBA Addins (*.xla)|*.xla"
            dlg.Title = "Select QuantLibXL VBA Framework"
            If dlg.ShowDialog() = System.Windows.Forms.DialogResult.OK Then
                txtFramework.Text = dlg.FileName
            End If
        Catch ex As Exception
            MsgBox("Error processing framework path:" _
                 & vbCrLf & vbCrLf & ex.Message, _
                 MsgBoxStyle.OkOnly + MsgBoxStyle.Exclamation, _
                "QuantLibXL Error")
        End Try
    End Sub

    Private Sub btnWorkbooks_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles btnWorkbooks.Click
        Try
            Dim dlg As New FolderBrowserDialog()
            dlg.SelectedPath = deriveDefaultDir(txtWorkbooks.Text, "Workbooks")
            dlg.Description = "Select QuantLibXL Workbook root folder"
            dlg.ShowNewFolderButton = False
            If dlg.ShowDialog() = System.Windows.Forms.DialogResult.OK Then
                txtWorkbooks.Text = dlg.SelectedPath
            End If
        Catch ex As Exception
            MsgBox("Error processing workbooks path:" _
                 & vbCrLf & vbCrLf & ex.Message, _
                 MsgBoxStyle.OkOnly + MsgBoxStyle.Exclamation, _
                "QuantLibXL Error")
        End Try
    End Sub

    Private Sub btnAddinDirSelect_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles btnAddinDirSelect.Click
        Try
            Dim dlg As New OpenFileDialog()
            dlg.InitialDirectory = deriveDefaultDir(txtAddinDir.Text, "xll")
            dlg.Filter = "Excel XLL Addins (*.xll)|*.xll"
            dlg.Title = "Select QuantLibXL Addin Directory and Name"
            If dlg.ShowDialog() = System.Windows.Forms.DialogResult.OK Then
                txtAddinDir.Text = System.IO.Path.GetDirectoryName(dlg.FileName)
                txtAddinName.Text = System.IO.Path.GetFileName(dlg.FileName)
            End If
        Catch ex As Exception
            MsgBox("Error processing addin path:" _
                 & vbCrLf & vbCrLf & ex.Message, _
                 MsgBoxStyle.OkOnly + MsgBoxStyle.Exclamation, _
                "QuantLibXL Error")
        End Try
    End Sub

    Private Sub btnAddinSelect_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles btnAddinNameSelect.Click
        Try
            Dim formAddinSelect As New FormAddinSelect
            formAddinSelect.AddinName = txtAddinName.Text
            If formAddinSelect.ShowDialog() = System.Windows.Forms.DialogResult.OK Then
                txtAddinName.Text = formAddinSelect.AddinName
                SelectedEnvironment.AddinName = formAddinSelect.AddinName
            End If
        Catch ex As Exception
            MsgBox("Error processing addin path:" _
                 & vbCrLf & vbCrLf & ex.Message, _
                 MsgBoxStyle.OkOnly + MsgBoxStyle.Exclamation, _
                "QuantLibXL Error")
        End Try
    End Sub

    Private Sub btnHelpFile_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles btnHelpFile.Click
        Try
            Dim dlg As New FolderBrowserDialog()
            dlg.SelectedPath = deriveDefaultDir(txtHelpPath.Text, "Docs")
            dlg.Description = "Select QuantLibXL Help folder"
            dlg.ShowNewFolderButton = False
            If dlg.ShowDialog() = System.Windows.Forms.DialogResult.OK Then
                txtHelpPath.Text = dlg.SelectedPath
            End If
        Catch ex As Exception
            MsgBox("Error processing helpfile path:" _
                 & vbCrLf & vbCrLf & ex.Message, _
                 MsgBoxStyle.OkOnly + MsgBoxStyle.Exclamation, _
                "QuantLibXL Error")
        End Try
    End Sub

    ''''''''''''''''''''''''''''''''''''''''''
    ' events - path - text
    ''''''''''''''''''''''''''''''''''''''''''

    Private Sub txtFramework_TextChanged(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles txtFramework.TextChanged
        SelectedEnvironment.Framework = txtFramework.Text
    End Sub

    Private Sub txtWorkbooks_TextChanged(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles txtWorkbooks.TextChanged
        SelectedEnvironment.Workbooks = txtWorkbooks.Text
    End Sub

    Private Sub txtAddinDir_TextChanged(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles txtAddinDir.TextChanged
        SelectedEnvironment.AddinDirectory = txtAddinDir.Text
    End Sub

    Private Sub txtAddinName_TextChanged(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles txtAddinName.TextChanged
        SelectedEnvironment.AddinName = txtAddinName.Text
    End Sub

    Private Sub txtHelpFile_TextChanged(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles txtHelpPath.TextChanged
        SelectedEnvironment.HelpPath = txtHelpPath.Text
    End Sub

    ''''''''''''''''''''''''''''''''''''''''''
    ' private functions
    ''''''''''''''''''''''''''''''''''''''''''

    Private Sub setEnabled(ByVal enabled As Boolean)
        btnDelete.Enabled = enabled
        btnClear.Enabled = enabled
        btnRename.Enabled = enabled
        btnFrameworkSelect.Enabled = enabled
        btnWorkbooks.Enabled = enabled
        btnAddinDirSelect.Enabled = enabled
        btnAddinNameSelect.Enabled = enabled
        btnHelpFile.Enabled = enabled
        txtFramework.Enabled = enabled
        txtWorkbooks.Enabled = enabled
        txtAddinDir.Enabled = enabled
        txtAddinName.Enabled = enabled
        txtHelpPath.Enabled = enabled
    End Sub

    Private Sub clear()
        txtFramework.Clear()
        txtWorkbooks.Clear()
        txtAddinDir.Clear()
        txtAddinName.Clear()
        txtHelpPath.Clear()
        cbYCBootstrap.Checked = False
        cbLoadMurexYC.Checked = False
        cbCapVolBootstrap.Checked = False
        cbSwapVolBootstrap.Checked = False
        cbSwapSmileBootstrap.Checked = False
        cbFitCMS.Checked = False
        cbIndexesTimeSeries.Checked = False
        cbLoadBonds.Checked = False
        cbStaticData.Checked = False
    End Sub
End Class
