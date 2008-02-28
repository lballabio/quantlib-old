
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

Imports System.IO
Imports System.Deployment.Application

Public Class FormMain

    ''''''''''''''''''''''''''''''''''''''''''
    ' Private members
    ''''''''''''''''''''''''''''''''''''''''''

    Private config_ As QuantLibXL.Configuration
    Private envUserconfigured_ As QuantLibXL.EnvironmentList
    Private envPreconfigured_ As QuantLibXL.EnvironmentList
    Private selectedEnvironment_ As QuantLibXL.Environment
    Private processingEvents_ As Boolean = True
    Private qlxlDir_ As String

    Private Const PRECONFIGURED As String = "Preconfigured"
    Private Const USERCONFIGURED As String = "UserConfigured"
    Private Const QUANTLIBXL_DIR As String = "QUANTLIBXL_DIR"

    ''''''''''''''''''''''''''''''''''''''''''
    ' Properties
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
                resetControls()
                Me.Text = "QuantLibXL Launcher 2008 - " & selectedEnvironment_.Name
            End If
        End Set

    End Property

    ''''''''''''''''''''''''''''''''''''''''''
    ' Initialization
    ''''''''''''''''''''''''''''''''''''''''''

    Public Sub New()

        qlxlDir_ = Environ(QUANTLIBXL_DIR)  ' not fatal if invalid

        InitializeComponent()

        Try

            Upgrade.run()
            initializeConfig()
            overrideActions()
            initializeLists()
            selectEnvironment()
            initializeAboutTab()
            setToolTips()
            enableVariableMenu()
            initializeVariableList()

            lblBuildNumber.Text = "version " & buildNumber()

        Catch ex As Exception

            MsgBox(ex.Message, MsgBoxStyle.OkOnly + MsgBoxStyle.Critical, _
                "QuantLibXL Fatal Error")
            Environment.Exit(1)

        End Try

    End Sub

    ''''''''''''''''''''''''''''''''''''''''''
    ' Private functions
    ''''''''''''''''''''''''''''''''''''''''''

    Private Sub setEnabled(ByVal enabled As Boolean)

        ' Environments
        btnDelete.Enabled = enabled
        btnClear.Enabled = enabled
        btnRename.Enabled = enabled

        ' Paths - text boxes
        btnFrameworkSelect.Enabled = enabled
        btnWorkbooks.Enabled = enabled
        btnHelpFile.Enabled = enabled
        btnXmlPath.Enabled = enabled
        btnUserConfig.Enabled = enabled

        ' Paths - buttons
        txtFramework.Enabled = enabled
        txtWorkbooks.Enabled = enabled
        txtHelpPath.Enabled = enabled
        txtXmlPath.Enabled = enabled
        txtUserConfig.Enabled = enabled

        ' Environment properties
        cbFrameworkVersion.Enabled = enabled

        ' Addins
        lbAddins.Enabled = enabled
        btnAddinInsert.Enabled = enabled
        If enabled Then
            Call enableAddinButtons()
        Else
            btnAddinUp.Enabled = False
            btnAddinDown.Enabled = False
            btnAddinDelete.Enabled = False
            btnAddinRename.Enabled = False
        End If

        ' Variables
        lvVariables.Enabled = enabled

    End Sub

    Private Sub clear()

        ' Paths - text boxes
        txtFramework.Clear()
        txtWorkbooks.Clear()
        txtHelpPath.Clear()
        txtXmlPath.Clear()
        txtUserConfig.Clear()

        ' Startup actions
        cbSetEvaluationDate.Checked = False
        dtEvaluationDate.Value = System.DateTime.Today
        cbYCBootstrap.Checked = False
        cbCapVolBootstrap.Checked = False
        cbSwapSmileBootstrap.Checked = False
        cbCalibrateCms.Checked = False
        cbFitCMS.Checked = False
        cbIndexesTimeSeries.Checked = False
        cbLoadBonds.Checked = False
        cbMainChecks.Checked = False
        cbStaticData.Checked = False
        ' We must choose a data source so force Excel as the default
        rbExcel.Checked = True
        ' We must choose a feed so force Reuters as the default
        rbReuters.Checked = True

        ' Environment properties
        cbFrameworkVersion.SelectedIndex = -1

        ' Addins

    End Sub

    ' After changing the selected environment, call this sub
    ' to synch up the controls.

    Private Sub resetControls()

        ' Paths - text boxes
        txtFramework.Text = SelectedEnvironment.FrameworkName
        txtWorkbooks.Text = SelectedEnvironment.Workbooks
        txtHelpPath.Text = SelectedEnvironment.HelpPath
        txtXmlPath.Text = SelectedEnvironment.XmlPath
        txtUserConfig.Text = SelectedEnvironment.UserConfig

        ' Startup actions
        cbSetEvaluationDate.Checked = SelectedEnvironment.StartupActions.SetEvaluationDate
        dtEvaluationDate.Enabled = SelectedEnvironment.StartupActions.SetEvaluationDate
        dtEvaluationDate.Value = SelectedEnvironment.StartupActions.EvaluationDate
        cbYCBootstrap.Checked = SelectedEnvironment.StartupActions.YieldCurveBootstrap
        cbCapVolBootstrap.Checked = SelectedEnvironment.StartupActions.CapVolBootstrap
        cbSwapSmileBootstrap.Checked = SelectedEnvironment.StartupActions.SwapSmileBootstrap
        cbCalibrateCms.Checked = SelectedEnvironment.StartupActions.CalibrateCMS
        cbFitCMS.Checked = SelectedEnvironment.StartupActions.FitCMS
        cbIndexesTimeSeries.Checked = SelectedEnvironment.StartupActions.IndexesTimeSeries
        cbLoadBonds.Checked = SelectedEnvironment.StartupActions.LoadBonds
        cbMainChecks.Checked = SelectedEnvironment.StartupActions.MainChecks
        cbStaticData.Checked = SelectedEnvironment.StartupActions.StaticData

        ' Initialization Data Source
        If UCase(SelectedEnvironment.StartupActions.InitSource) = "EXCEL" Then
            rbExcel.Checked = True
        ElseIf UCase(SelectedEnvironment.StartupActions.InitSource) = "XML" Then
            rbXML.Checked = True
        Else
            Throw New Exception("Invalid value for initialization source: '" _
                & SelectedEnvironment.StartupActions.InitSource & "'")
        End If

        ' Environment properties
        Dim frameworkVersion As String = CStr(SelectedEnvironment.FrameworkVersion)
        If cbFrameworkVersion.Items.Contains(frameworkVersion) Then
            cbFrameworkVersion.Text = frameworkVersion
        Else
            cbFrameworkVersion.SelectedIndex = -1
        End If

        ' Addins
        initializeAddinList()

        ' Variables
        initializeVariableList()

    End Sub

    Private Sub initializeAboutTab()

        Me.lblVersionValue.Text = buildNumber()
        Me.lblUserNameValue.Text = System.Environment.UserName
        Me.lblDomainValue.Text = System.Environment.UserDomainName
        Me.lblHardDiskValue.Text = getSerialNumber()

    End Sub

    Private Sub setToolTips()

        Dim toolTip1 As New ToolTip()
        toolTip1.ShowAlways = True
        toolTip1.SetToolTip(Me.btnLaunchExcel, "Launch an empty Excel session")

    End Sub

    Private Sub saveConfiguration()

        config_.OverrideActions.clear()
        For Each environment As QuantLibXL.Environment In envPreconfigured_.Environments
            config_.OverrideActions.add(environment.StartupActions, environment.Name)
        Next

        Dim registryWriter As New QuantLibXL.RegistryWriter()
        registryWriter.deleteKey("Configuration")
        registryWriter.serializeObject(config_, "Configuration", THIS_VERSION)
        registryWriter.deleteKey("Environments")
        registryWriter.serializeObject(envUserconfigured_, "Environments", THIS_VERSION)

    End Sub

    Private Function buildNumber() As String

        If (ApplicationDeployment.IsNetworkDeployed) Then
            buildNumber = My.Application.Deployment.CurrentVersion.ToString()
        Else
            buildNumber = "?.?.? (local build)"
        End If

    End Function

    Private Sub initializeLists()

        For Each environment As QuantLibXL.Environment In envPreconfigured_.Environments
            lstPreconfigured.Items.Add(environment.Name)
        Next

        For Each environment As QuantLibXL.Environment In envUserconfigured_.Environments
            lstUserconfigured.Items.Add(environment.Name)
        Next

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

            ' If none of the above worked then try for a safe option

            If lstPreconfigured.Items.Count >= 1 Then
                lstPreconfigured.SelectedIndex = 0
            Else
                Throw New Exception("Could not identify an environment to activate")
            End If

        Catch ex As Exception

            Throw New Exception("Error activating environment " & ex.Message)

        End Try

    End Sub

    Private Function deriveConfigPath() As String

        deriveConfigPath = configPath() & "\site\data\config.xml"

        If Not fileExists(deriveConfigPath) Then
            Throw New Exception("Error: this application loads configuration information " & _
            "from the following location:" & vbCrLf & vbCrLf & deriveConfigPath & vbCrLf & vbCrLf & _
            "The path is invalid.")
        End If

    End Function

    Private Sub initializeConfig()

        ' Derive the location of the launcher configuration file config.xml:
        ' - If this application is being run across the network (a ClickOnce installation)
        '   then get the config file from the local ClickOnce directory
        ' - If this application is being run from the command line then
        '   look for config.xml in the location specified by QUANTLIBXL_CONFIG_PATH

        Dim configPath As String
        If (ApplicationDeployment.IsNetworkDeployed) Then
            configPath = ApplicationDeployment.CurrentDeployment.DataDirectory & "\site\data\config.xml"
        Else
            configPath = deriveConfigPath()
        End If

        Try
            Dim xmlReader As New QuantLibXL.XmlReader(configPath, "Configuration")
            xmlReader.serializeObject(envPreconfigured_, "Environments", THIS_VERSION)
        Catch ex As Exception
            Throw New Exception("Error processing configuration file " & configPath & ":" _
                & vbCrLf & vbCrLf & ex.Message)
        End Try

        ' Call the setDotNetParameters() method of the Environment objects to give
        ' them a chance to initialize properties derived from the .NET environment

        envPreconfigured_.setDotNetParameters()

        envPreconfigured_.validate()

        ' Load the user's preferences from the Windows registry.  The config_
        ' object allows the user to override certain characteristics of the
        ' preconfigured environments loaded above, and the envUserconfigured_
        ' object contains additional environments configured by the user.

        Try
            Dim registryReader As New QuantLibXL.RegistryReader()
            registryReader.serializeObject(config_, "Configuration", THIS_VERSION)
            registryReader.serializeObject(envUserconfigured_, "Environments", THIS_VERSION)
        Catch ex As Exception
            Throw New Exception("Error accessing the Windows registry:" _
                 & vbCrLf & vbCrLf & ex.Message)
        End Try

        If Not fileExists(config_.ExcelPath) Then
            MsgBox("Warning: The Launcher is unable to determine the path to Microsoft Excel." _
                & vbCrLf & "Before launching QuantLibXL you must go to the 'Paths' tab" _
                & vbCrLf & "and specify the path to Excel.", _
                MsgBoxStyle.OkOnly + MsgBoxStyle.Information, "QuantLibXL Warning")
        End If

        txtReuters.Text = config_.ReutersPath
        txtBloomberg.Text = config_.BloombergPath
        cbReuters.Checked = config_.ReutersSelected
        cbBloomberg.Checked = config_.BloombergSelected
        txtExcelPath.Text = config_.ExcelPath
        If UCase(config_.FeedUse) = "REUTERS" Then
            rbReuters.Checked = True
        ElseIf UCase(config_.FeedUse) = "BLOOMBERG" Then
            rbBloomberg.Checked = True
        Else
            Throw New Exception("Invalid value for feed : '" & config_.FeedUse & "'")
        End If
        setReutersPathEnabled()
        setBloombergPathEnabled()

    End Sub

    Private Sub overrideActions()

        ' Preconfigured environments have been loaded from the configuration file.
        ' For each of these, override the startup actions with values saved to
        ' the user's registry from previous session (if any)

        Try

            Dim envPreconfigured As QuantLibXL.Environment
            Dim startupActions As QuantLibXL.StartupActions
            For Each startupActions In config_.OverrideActions.StartupActionsList
                If envPreconfigured_.nameInUse(startupActions.Name) Then
                    envPreconfigured = envPreconfigured_.nameToEnvironment(startupActions.Name)
                    envPreconfigured.StartupActions = CType(startupActions, QuantLibXL.StartupActions).Clone
                End If
            Next

        Catch ex As Exception
            Throw New Exception("Error deriving startup actions: " & ex.Message)
        End Try

    End Sub

    ''''''''''''''''''''''''''''''''''''''''''
    ' Events - Main Form
    ''''''''''''''''''''''''''''''''''''''''''

    Private Sub btnClose_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles btnClose.Click

        Try

            saveConfiguration()

        Catch ex As Exception

            MsgBox("Error while closing launcher:" & vbCrLf & vbCrLf & ex.Message, _
                MsgBoxStyle.OkOnly + MsgBoxStyle.Exclamation, "QuantLibXL Error")

        End Try

        Close()

    End Sub

    Private Sub btnLaunch_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles btnLaunch.Click

        Try

            SelectedEnvironment.launch(config_.FeedList(), config_.ExcelPath, config_.FeedUse)
            saveConfiguration()

        Catch ex As Exception

            MsgBox("Error on launch command:" _
                 & vbCrLf & vbCrLf & ex.Message, _
                 MsgBoxStyle.OkOnly + MsgBoxStyle.Exclamation, _
                "QuantLibXL Error")

        End Try

    End Sub

    ''''''''''''''''''''''''''''''''''''''''''
    ' Events - Environment - Lists
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
                 & vbCrLf & vbCrLf & ex.Message, _
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
                 & vbCrLf & vbCrLf & ex.Message, _
                 MsgBoxStyle.OkOnly + MsgBoxStyle.Exclamation, _
                "QuantLibXL Error")

        End Try

    End Sub

    ''''''''''''''''''''''''''''''''''''''''''
    ' Events - Environments - Buttons
    ''''''''''''''''''''''''''''''''''''''''''

    Private Sub btnNew_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles btnNew.Click

        Try

            Dim newName As String = inputEnvironmentName(envUserconfigured_.deriveNewName())
            If Len(newName) < 1 Then Exit Sub

            SelectedEnvironment = envUserconfigured_.createEnvironment(newName)
            Dim newIndex As Integer = lstUserconfigured.Items.Add(newName)
            lstUserconfigured.SelectedIndex = newIndex

        Catch ex As Exception

            MsgBox("Error creating new environment:" _
                 & vbCrLf & vbCrLf & ex.Message, _
                 MsgBoxStyle.OkOnly + MsgBoxStyle.Exclamation, _
                "QuantLibXL Error")

        End Try

    End Sub

    Private Sub btnCopy_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles btnCopy.Click

        Try

            Dim newName As String = inputEnvironmentName(envUserconfigured_.deriveCopyName(SelectedEnvironment.Name))
            If Len(newName) < 1 Then Exit Sub

            SelectedEnvironment = envUserconfigured_.copyEnvironment(SelectedEnvironment, newName)
            Dim newIndex As Integer = lstUserconfigured.Items.Add(newName)
            lstUserconfigured.SelectedIndex = newIndex

        Catch ex As Exception

            MsgBox("Error copying environment:" _
                 & vbCrLf & vbCrLf & ex.Message, _
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
            Dim deletedIndex As Integer = lstUserconfigured.SelectedIndex
            lstUserconfigured.Items.RemoveAt(lstUserconfigured.SelectedIndex)

            If lstUserconfigured.Items.Count > 0 Then
                lstUserconfigured.SelectedIndex = Math.Min(deletedIndex, lstUserconfigured.Items.Count - 1)
            Else
                lstPreconfigured.SelectedIndex = 0
            End If

        Catch ex As Exception

            MsgBox("Error deleting environment:" _
                 & vbCrLf & vbCrLf & ex.Message, _
                 MsgBoxStyle.OkOnly + MsgBoxStyle.Exclamation, _
                "QuantLibXL Error")

        End Try

    End Sub

    Private Sub btnClear_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles btnClear.Click

        Try

            clear()

        Catch ex As Exception

            MsgBox("Error clearing environment:" _
                 & vbCrLf & vbCrLf & ex.Message, _
                 MsgBoxStyle.OkOnly + MsgBoxStyle.Exclamation, _
                "QuantLibXL Error")

        End Try

    End Sub

    Private Sub btnRename_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles btnRename.Click

        If lstUserconfigured.SelectedIndex = -1 Then Exit Sub

        Try

            Dim oldName As String = SelectedEnvironment.Name
            Dim newName As String = inputEnvironmentName(oldName)
            If Len(newName) < 1 Or newName = oldName Then Exit Sub

            envUserconfigured_.renameEnvironment(oldName, newName)
            SelectedEnvironment.Name = newName
            config_.SelectedEnvName = newName
            processingEvents_ = False
            lstUserconfigured.Items.Remove(oldName)
            Dim newIndex As Integer = lstUserconfigured.Items.Add(newName)
            lstUserconfigured.SelectedIndex = newIndex
            processingEvents_ = True

        Catch ex As Exception

            MsgBox("Error renaming environment:" & vbCrLf & vbCrLf & ex.Message, _
                MsgBoxStyle.OkOnly + MsgBoxStyle.Exclamation, "QuantLibXL Error")

        End Try

    End Sub

    ''''''''''''''''''''''''''''''''''''''''''
    ' Events - Startup Actions
    ''''''''''''''''''''''''''''''''''''''''''

    Private Sub cbSetEvaluationDate_CheckedChanged(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles cbSetEvaluationDate.CheckedChanged
        SelectedEnvironment.StartupActions.SetEvaluationDate = cbSetEvaluationDate.Checked
        dtEvaluationDate.Enabled = cbSetEvaluationDate.Checked
    End Sub

    Private Sub dtEvaluationDate_ValueChanged(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles dtEvaluationDate.ValueChanged
        SelectedEnvironment.StartupActions.EvaluationDate = dtEvaluationDate.Value
    End Sub

    Private Sub cbYCBootstrap_CheckedChanged(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles cbYCBootstrap.CheckedChanged
        SelectedEnvironment.StartupActions.YieldCurveBootstrap = cbYCBootstrap.Checked
    End Sub

    Private Sub cbCapVolBootstrap_CheckedChanged(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles cbCapVolBootstrap.CheckedChanged
        SelectedEnvironment.StartupActions.CapVolBootstrap = cbCapVolBootstrap.Checked
    End Sub

    Private Sub cbSwapSmileBootstrap_CheckedChanged(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles cbSwapSmileBootstrap.CheckedChanged
        SelectedEnvironment.StartupActions.SwapSmileBootstrap = cbSwapSmileBootstrap.Checked
    End Sub

    Private Sub cbCalibrateCms_CheckedChanged(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles cbCalibrateCms.CheckedChanged
        SelectedEnvironment.StartupActions.CalibrateCMS = cbCalibrateCms.Checked
    End Sub

    Private Sub cbFitCMS_CheckedChanged(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles cbFitCMS.CheckedChanged
        SelectedEnvironment.StartupActions.FitCMS = cbFitCMS.Checked
    End Sub

    Private Sub cbIndexesTimeSeries_CheckedChanged(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles cbIndexesTimeSeries.CheckedChanged
        SelectedEnvironment.StartupActions.IndexesTimeSeries = cbIndexesTimeSeries.Checked
    End Sub

    Private Sub cbLoadBonds_CheckedChanged(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles cbLoadBonds.CheckedChanged
        SelectedEnvironment.StartupActions.LoadBonds = cbLoadBonds.Checked
    End Sub

    Private Sub cbMainChecks_CheckedChanged(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles cbMainChecks.CheckedChanged
        SelectedEnvironment.StartupActions.MainChecks = cbMainChecks.Checked
    End Sub

    Private Sub cbStaticData_CheckedChanged(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles cbStaticData.CheckedChanged
        SelectedEnvironment.StartupActions.StaticData = cbStaticData.Checked
    End Sub

    ' Initialization Data Source

    Private Sub rbExcel_CheckedChanged(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles rbExcel.CheckedChanged
        Call setInitSource()
    End Sub

    Private Sub rbXML_CheckedChanged(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles rbXML.CheckedChanged
        Call setInitSource()
    End Sub

    Private Sub rbReuters_CheckedChanged(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles rbReuters.CheckedChanged
        Call setFeedUse()
    End Sub

    Private Sub rbBloomberg_CheckedChanged(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles rbBloomberg.CheckedChanged
        Call setFeedUse()
    End Sub

    ''''''''''''''''''''''''''''''''''''''''''
    ' Events - Paths - Buttons
    ''''''''''''''''''''''''''''''''''''''''''

    Private Sub btnFrameworkSelect_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles btnFrameworkSelect.Click

        Try

            Dim dlg As New OpenFileDialog()
            dlg.InitialDirectory = deriveDefaultFile(txtFramework.Text, "framework")
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

    Private Sub cbFrameworkVersion_SelectedIndexChanged(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles cbFrameworkVersion.SelectedIndexChanged

        If cbFrameworkVersion.SelectedIndex = -1 Then
            SelectedEnvironment.FrameworkVersion = 0
        Else
            SelectedEnvironment.FrameworkVersion = CInt(cbFrameworkVersion.Text)
        End If

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

            MsgBox("Error processing workbooks path:" & vbCrLf & vbCrLf & ex.Message, _
                MsgBoxStyle.OkOnly + MsgBoxStyle.Exclamation, "QuantLibXL Error")

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

            MsgBox("Error processing helpfile path:" & vbCrLf & vbCrLf & ex.Message, _
                MsgBoxStyle.OkOnly + MsgBoxStyle.Exclamation, "QuantLibXL Error")

        End Try

    End Sub

    Private Sub btnXmlPath_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles btnXmlPath.Click

        Try

            Dim dlg As New FolderBrowserDialog()
            dlg.SelectedPath = deriveDefaultDir(txtXmlPath.Text, "..\QuantLibAddin\gensrc\metadata")
            dlg.Description = "Select QuantLibXL Function Metadata folder"
            dlg.ShowNewFolderButton = False
            If dlg.ShowDialog() = System.Windows.Forms.DialogResult.OK Then
                txtXmlPath.Text = dlg.SelectedPath
            End If

        Catch ex As Exception

            MsgBox("Error processing Function Metadata folder:" & vbCrLf & vbCrLf & ex.Message, _
                MsgBoxStyle.OkOnly + MsgBoxStyle.Exclamation, "QuantLibXL Error")

        End Try

    End Sub

    Private Sub btnUserConfig_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles btnUserConfig.Click

        Try

            Dim dlg As New OpenFileDialog()
            dlg.InitialDirectory = deriveDefaultFile(txtUserConfig.Text, "")
            If Len(Path.GetFileName(txtUserConfig.Text)) > 0 Then
                dlg.FileName = Path.GetFileName(txtUserConfig.Text)
            Else
                dlg.FileName = "users.xml"
            End If
            dlg.Filter = "XML Configuration Files (*.xml)|*.xml"
            dlg.Title = "Select QuantLibXL User Configuration File"
            If dlg.ShowDialog() = System.Windows.Forms.DialogResult.OK Then
                txtUserConfig.Text = dlg.FileName
            End If

        Catch ex As Exception

            MsgBox("Error processing User Configuration File path:" & vbCrLf & vbCrLf & ex.Message, _
                MsgBoxStyle.OkOnly + MsgBoxStyle.Exclamation, "QuantLibXL Error")

        End Try

    End Sub

    Private Sub btnExcelPath_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles btnExcelPath.Click

        Try

            Dim dlg As New OpenFileDialog()
            Dim defaultPath As String = deriveDefaultFile(txtExcelPath.Text)
            If defaultPath = "" Then defaultPath = deriveDefaultExcelPath()
            dlg.InitialDirectory = defaultPath
            dlg.FileName = "EXCEL.EXE"
            dlg.Filter = "Excel executable (EXCEL.EXE)|EXCEL.EXE"
            dlg.Title = "Select Excel executable"
            If dlg.ShowDialog() = System.Windows.Forms.DialogResult.OK Then
                txtExcelPath.Text = dlg.FileName
            End If

        Catch ex As Exception

            MsgBox("Error processing path to Excel:" & vbCrLf & vbCrLf & ex.Message, _
                MsgBoxStyle.OkOnly + MsgBoxStyle.Exclamation, "QuantLibXL Error")

        End Try

    End Sub

    Private Sub btnLaunchExcel_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles btnLaunchExcel.Click

        Try

            If Not fileExists(config_.ExcelPath) Then
                Throw New Exception("The specified Excel path:" & vbCrLf & vbCrLf & config_.ExcelPath & _
                vbCrLf & vbCrLf & "is invalid.")
            End If

            Shell(config_.ExcelPath, AppWinStyle.NormalFocus)

        Catch ex As Exception

            MsgBox("Error starting Excel:" & vbCrLf & vbCrLf & ex.Message, _
                MsgBoxStyle.OkOnly + MsgBoxStyle.Exclamation, "QuantLibXL Error")

        End Try

    End Sub

    ''''''''''''''''''''''''''''''''''''''''''
    ' Events - Paths - text
    ''''''''''''''''''''''''''''''''''''''''''

    Private Sub txtFramework_TextChanged(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles txtFramework.TextChanged
        SelectedEnvironment.FrameworkName = txtFramework.Text
    End Sub

    Private Sub txtWorkbooks_TextChanged(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles txtWorkbooks.TextChanged
        SelectedEnvironment.Workbooks = txtWorkbooks.Text
    End Sub

    Private Sub txtHelpFile_TextChanged(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles txtHelpPath.TextChanged
        SelectedEnvironment.HelpPath = txtHelpPath.Text
    End Sub

    Private Sub txtXmlPath_TextChanged(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles txtXmlPath.TextChanged
        SelectedEnvironment.XmlPath = txtXmlPath.Text
    End Sub

    Private Sub txtUserConfig_TextChanged(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles txtUserConfig.TextChanged
        SelectedEnvironment.UserConfig = txtUserConfig.Text
    End Sub

    Private Sub txtExcelPath_TextChanged(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles txtExcelPath.TextChanged
        config_.ExcelPath = txtExcelPath.Text
    End Sub

    ''''''''''''''''''''''''''''''''''''''''''
    ' Events - Addins
    ''''''''''''''''''''''''''''''''''''''''''

    Private Sub lbAddins_SelectedIndexChanged(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles lbAddins.SelectedIndexChanged
        Call enableAddinButtons()
    End Sub

    Private Sub btnAddinInsert_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles btnAddinInsert.Click

        Try

            Dim dlg As New OpenFileDialog()
            dlg.InitialDirectory = deriveDefaultFile(lbAddins.SelectedValue, "addin")
            dlg.FileName = ""
            dlg.Filter = "Excel Addins (*.xll;*.xla)|*.xll;*.xla"
            dlg.Title = "Select Addin"

            If dlg.ShowDialog() = System.Windows.Forms.DialogResult.OK Then
                Dim before As Integer = Math.Max(lbAddins.SelectedIndex, 0)
                SelectedEnvironment.AddinList.insert(dlg.FileName, before + 1)
                lbAddins.Items.Insert(before, dlg.FileName)
                'SelectedEnvironment.AddinList.insert(dlg.FileName, lbAddins.SelectedIndex)
                'If lbAddins.SelectedIndex = -1 Then
                '    lbAddins.Items.Add(dlg.FileName)
                'Else
                '    lbAddins.Items.Insert(lbAddins.SelectedIndex, dlg.FileName)
                'End If
                Call enableAddinButtons()
            End If

        Catch ex As Exception

            MsgBox("Error inserting addin:" _
                 & vbCrLf & vbCrLf & ex.Message, _
                 MsgBoxStyle.OkOnly + MsgBoxStyle.Exclamation, _
                "QuantLibXL Error")

        End Try

    End Sub

    Private Sub btnAddinDelete_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles btnAddinDelete.Click

        Try

            If lbAddins.SelectedIndex = -1 Then Exit Sub
            SelectedEnvironment.AddinList.delete(lbAddins.SelectedItem.ToString())
            Dim i As Integer = lbAddins.SelectedIndex
            lbAddins.Items.RemoveAt(i)
            lbAddins.SelectedIndex = Math.Min(i, lbAddins.Items.Count - 1)
            Call enableAddinButtons()

        Catch ex As Exception

            MsgBox("Error deleting addin:" _
                 & vbCrLf & vbCrLf & ex.Message, _
                 MsgBoxStyle.OkOnly + MsgBoxStyle.Exclamation, _
                "QuantLibXL Error")

        End Try

    End Sub

    Private Sub btnAddinRename_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles btnAddinRename.Click

        Try

            If lbAddins.SelectedIndex = -1 Then Exit Sub
            Dim newName As String = InputBox("Edit Addin name:", _
                "Edit Addin Name", lbAddins.SelectedItem)
            If Len(newName) > 0 Then
                SelectedEnvironment.AddinList.update(lbAddins.SelectedItem.ToString(), newName)
                lbAddins.Items(lbAddins.SelectedIndex) = newName
            End If

        Catch ex As Exception

            MsgBox("Error renaming addin:" _
                 & vbCrLf & vbCrLf & ex.Message, _
                 MsgBoxStyle.OkOnly + MsgBoxStyle.Exclamation, _
                "QuantLibXL Error")

        End Try

    End Sub

    Private Sub btnAddinUp_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles btnAddinUp.Click

        Try

            If lbAddins.Items.Count <= 1 Then Exit Sub
            If lbAddins.SelectedIndex < 1 Then Exit Sub
            SelectedEnvironment.AddinList.up(lbAddins.SelectedIndex)
            Dim o As Object = lbAddins.SelectedItem
            Dim i As Integer = lbAddins.SelectedIndex
            lbAddins.Items.RemoveAt(i)
            lbAddins.Items.Insert(i - 1, o)
            lbAddins.SelectedIndex = i - 1

        Catch ex As Exception

            MsgBox("Error on addin up:" _
                 & vbCrLf & vbCrLf & ex.Message, _
                 MsgBoxStyle.OkOnly + MsgBoxStyle.Exclamation, _
                "QuantLibXL Error")

        End Try

    End Sub

    Private Sub btnAddinDown_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles btnAddinDown.Click

        Try

            If lbAddins.SelectedIndex = -1 Then Exit Sub
            If lbAddins.Items.Count <= 1 Then Exit Sub
            If lbAddins.SelectedIndex = (lbAddins.Items.Count - 1) Then Exit Sub
            SelectedEnvironment.AddinList.down(lbAddins.SelectedIndex)
            Dim o As Object = lbAddins.SelectedItem
            Dim i As Integer = lbAddins.SelectedIndex
            lbAddins.Items.RemoveAt(i)
            lbAddins.Items.Insert(i + 1, o)
            lbAddins.SelectedIndex = i + 1

        Catch ex As Exception

            MsgBox("Error on addin down:" _
                 & vbCrLf & vbCrLf & ex.Message, _
                 MsgBoxStyle.OkOnly + MsgBoxStyle.Exclamation, _
                "QuantLibXL Error")

        End Try

    End Sub

    ''''''''''''''''''''''''''''''''''''''''''
    ' Events - Feeds
    ''''''''''''''''''''''''''''''''''''''''''

    Private Sub cbReuters_CheckedChanged(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles cbReuters.CheckedChanged
        config_.ReutersSelected = cbReuters.Checked
    End Sub

    Private Sub cbBloomberg_CheckedChanged(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles cbBloomberg.CheckedChanged
        config_.BloombergSelected = cbBloomberg.Checked
    End Sub

    Private Sub txtBloomberg_TextChanged(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles txtBloomberg.TextChanged
        config_.BloombergPath = txtBloomberg.Text
        setBloombergPathEnabled()
    End Sub

    Private Sub txtReuters_TextChanged(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles txtReuters.TextChanged
        config_.ReutersPath = txtReuters.Text
        setReutersPathEnabled()
    End Sub

    Private Sub btnReuters_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles btnReuters.Click

        Try

            Dim dlg As New OpenFileDialog()
            dlg.InitialDirectory = deriveDefaultFile(txtReuters.Text)
            dlg.FileName = QuantLibXL.Configuration.REUTERS_XLA_DEFAULT
            dlg.Filter = "Reuters Addin (" & QuantLibXL.Configuration.REUTERS_XLA_DEFAULT & _
                ")|" & QuantLibXL.Configuration.REUTERS_XLA_DEFAULT & ""
            dlg.Title = "Select Reuters VBA Addin"
            If dlg.ShowDialog() = System.Windows.Forms.DialogResult.OK Then
                txtReuters.Text = dlg.FileName
            End If

        Catch ex As Exception

            MsgBox("Error processing Reuters path:" & vbCrLf & vbCrLf & ex.Message, _
                MsgBoxStyle.OkOnly + MsgBoxStyle.Exclamation, "QuantLibXL Error")

        End Try

    End Sub

    Private Sub btnBloomberg_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles btnBloomberg.Click

        Try

            Dim dlg As New OpenFileDialog()
            dlg.InitialDirectory = deriveDefaultFile(txtBloomberg.Text)
            dlg.FileName = QuantLibXL.Configuration.BLOOMBERG_XLA_DEFAULT
            dlg.Filter = "Bloomberg Addin (" & QuantLibXL.Configuration.BLOOMBERG_XLA_DEFAULT & _
                ")|" & QuantLibXL.Configuration.BLOOMBERG_XLA_DEFAULT & ""
            dlg.Title = "Select Bloomberg VBA Addin"
            If dlg.ShowDialog() = System.Windows.Forms.DialogResult.OK Then
                txtBloomberg.Text = dlg.FileName
            End If

        Catch ex As Exception

            MsgBox("Error processing Bloomberg path:" & vbCrLf & vbCrLf & ex.Message, _
                MsgBoxStyle.OkOnly + MsgBoxStyle.Exclamation, "QuantLibXL Error")

        End Try

    End Sub

    ''''''''''''''''''''''''''''''''''''''''''
    ' Events - Variables
    ''''''''''''''''''''''''''''''''''''''''''

    Private Sub lvVariables_SelectedIndexChanged(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles lvVariables.SelectedIndexChanged
        Call enableVariableMenu()
    End Sub

    Private Sub miVariableInsert_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles miVariableInsert.Click

        Try

            FormVariableEdit.tbVariable.Text = ""
            FormVariableEdit.tbValue.Text = ""

            If FormVariableEdit.ShowDialog() = System.Windows.Forms.DialogResult.OK Then
                Dim variableName As String = FormVariableEdit.tbVariable.Text
                Dim variableValue As String = FormVariableEdit.tbValue.Text
                selectedEnvironment_.VariableList.insert(variableName, variableValue)
                Dim listItem As New ListViewItem(variableName)
                listItem.SubItems.Add(variableValue)
                lvVariables.Items.Add(listItem)
            End If

        Catch ex As Exception

            MsgBox("Error inserting new environment variable:" _
                 & vbCrLf & vbCrLf & ex.Message, _
                 MsgBoxStyle.OkOnly + MsgBoxStyle.Exclamation, _
                "QuantLibXL Error")

        End Try

    End Sub

    Private Sub miVariableEdit_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles miVariableEdit.Click

        Try

            Dim variableNameOld As String = lvVariables.SelectedItems(0).Text
            Dim variableValueOld As String = lvVariables.SelectedItems(0).SubItems(1).Text
            FormVariableEdit.tbVariable.Text = variableNameOld
            FormVariableEdit.tbValue.Text = variableValueOld

            If FormVariableEdit.ShowDialog() = System.Windows.Forms.DialogResult.OK Then
                Dim variableName As String = FormVariableEdit.tbVariable.Text
                Dim variableValue As String = FormVariableEdit.tbValue.Text
                selectedEnvironment_.VariableList.update(variableNameOld, variableName, variableValue)
                lvVariables.SelectedItems(0).Text = variableName
                lvVariables.SelectedItems(0).SubItems(1).Text = variableValue
            End If

        Catch ex As Exception

            MsgBox("Error editing environment variable:" _
                 & vbCrLf & vbCrLf & ex.Message, _
                 MsgBoxStyle.OkOnly + MsgBoxStyle.Exclamation, _
                "QuantLibXL Error")

        End Try

    End Sub

    Private Sub miVariableDelete_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles miVariableDelete.Click

        Try

            selectedEnvironment_.VariableList.delete(lvVariables.SelectedItems(0).Text)
            lvVariables.Items.Remove(lvVariables.SelectedItems(0))

        Catch ex As Exception

            MsgBox("Error deleting environment variable:" _
                 & vbCrLf & vbCrLf & ex.Message, _
                 MsgBoxStyle.OkOnly + MsgBoxStyle.Exclamation, _
                "QuantLibXL Error")

        End Try

    End Sub

End Class
