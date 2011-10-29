
Imports System.IO
Imports System.Deployment.Application

Public Class FormMain

    ''''''''''''''''''''''''''''''''''''''''''
    ' Private members
    ''''''''''''''''''''''''''''''''''''''''''

    Private processingEvents_ As Boolean = True

    Private envPreconfigured_ As XL_Launcher.EnvironmentList = Nothing
    Private envUserconfigured_ As XL_Launcher.EnvironmentList = Nothing
    Private selectedEnvironment_ As XL_Launcher.Environment = Nothing
    Private selectedFramework_ As XL_Launcher.Framework = Nothing

    Private preConfigPath_ As String = Nothing
    Private userConfigPath_ As String = Nothing
    Private launcherXlaPath_ As String = Nothing
    Private sessionFileDir_ As String = Nothing
    Private userConfigError_ As Boolean

    ''''''''''''''''''''''''''''''''''''''''''
    ' Properties
    ''''''''''''''''''''''''''''''''''''''''''

    Private Property SelectedEnvironment() As XL_Launcher.Environment

        Get

            SelectedEnvironment = selectedEnvironment_
            If selectedEnvironment_ Is Nothing Then
                Throw New Exception("No environment selected.")
            End If

        End Get

        Set(ByVal value As XL_Launcher.Environment)

            selectedEnvironment_ = value
            btnLaunch.Enabled = Not selectedEnvironment_ Is Nothing
            btnGenerateSessionFile.Enabled = Not selectedEnvironment_ Is Nothing
            If value Is Nothing Then
                Me.Text = "XL-Launcher"
                Throw New Exception("No environment selected.")
            Else
                'resetControls()
                Me.Text = "XL-Launcher - " & selectedEnvironment_.Name
            End If

        End Set

    End Property

    Private Property SelectedFramework() As XL_Launcher.Framework

        Get

            SelectedFramework = selectedFramework_
            If selectedFramework_ Is Nothing Then
                Throw New Exception("No framework selected.")
            End If

        End Get

        Set(ByVal value As XL_Launcher.Framework)

            selectedFramework_ = value
            If value Is Nothing Then
                Throw New Exception("No framework selected.")
            End If

        End Set

    End Property

    ''''''''''''''''''''''''''''''''''''''''''
    ' Initialization
    ''''''''''''''''''''''''''''''''''''''''''

    Public Sub New()

        InitializeComponent()

        Try

            initialize()

        Catch ex As Exception

            XL_Launcher.Errors.displayError("Fatal Error", ex, MsgBoxStyle.OkOnly + MsgBoxStyle.Critical)
            Environment.Exit(1)

        End Try

    End Sub

    ''''''''''''''''''''''''''''''''''''''''''
    ' Private functions
    ''''''''''''''''''''''''''''''''''''''''''

    Private Sub initializeStaticLabels()

        ' About tab

        lblVersionValue.Text = buildNumber()
        Dim ap As AuthenticationParameters = deriveAuthenticationParameters()
        lblDomainValue.Text = ap.domainName
        lblUserNameValue.Text = ap.userName
        lblMachineValue.Text = ap.machineName

        ' Main dialog

        Dim helpFile As String = preConfigPath_ & "\help.txt"
        If System.IO.File.Exists(helpFile) Then
            rtHelp.LoadFile(helpFile, RichTextBoxStreamType.PlainText)
        End If

    End Sub

    Private Sub saveEnvironmentList(ByVal configPath As String, ByVal environmentList As XL_Launcher.EnvironmentList)

        Dim launcherConfigPath As String = configPath & "\LauncherConfig.xml"
        XL_Launcher.Serialization.saveObject(launcherConfigPath, environmentList, "LauncherConfig")

        Dim envPath As String
        For Each e As XL_Launcher.Environment In environmentList.Environments

            envPath = configPath & "\" & e.Name
            System.IO.Directory.CreateDirectory(envPath)

            Dim envConfigPath As String = envPath & "\_EnvironmentConfig.xml"
            XL_Launcher.Serialization.saveObject(envConfigPath, e, "EnvironmentConfig")

            For Each f As XL_Launcher.Framework In e.Frameworks

                Dim fwConfigPath As String = envPath & "\" & f.FileName & ".xml"
                XL_Launcher.Serialization.saveObject(fwConfigPath, f, "Framework")

            Next f

        Next e

    End Sub

    Private Sub saveConfiguration()

        deleteConfiguration()

        envPreconfigured_.ExcelPath = tbExcelPath.Text

        Dim usrConfigPath As String = userConfigPath_ & "\UserConfigured"
        Dim preConfigPath As String = userConfigPath_ & "\PreConfigured"

        System.IO.Directory.CreateDirectory(usrConfigPath)
        System.IO.Directory.CreateDirectory(preConfigPath)

        saveEnvironmentList(usrConfigPath, envUserconfigured_)
        saveEnvironmentList(preConfigPath, envPreconfigured_)

    End Sub

    Private Sub backupUserConfigDir()

        Try

            Dim s As String = userConfigPath_ & "_" & GetCurrentProcessId & "_" & _
                DateTime.Now.ToString("yyyy.MM.dd.HH.mm.ss.ffff")
            System.IO.Directory.Move(userConfigPath_, s)

            MsgBox("There were errors processing the User Config Directory:" & vbCrLf & vbCrLf & _
                vbTab & userConfigPath_ & vbCrLf & vbCrLf & _
                "This directory has been backed up to:" & vbCrLf & vbCrLf & _
                vbTab & s)

        Catch ex As System.IO.IOException

            MsgBox("There were errors processing the User Config Directory:" & vbCrLf & vbCrLf & _
                vbTab & userConfigPath_ & vbCrLf & vbCrLf & _
                "An attempt to take a backup of the User Config Directory" & vbCrLf & _
                "failed with the following error:" & vbCrLf & vbCrLf & _
                vbTab & ex.Message & vbCrLf & vbCrLf & _
                "(This error can occur if a file in the directory is locked," & vbCrLf & _
                "or if you have the directory open in Windows Explorer)." & vbCrLf & vbCrLf & _
                "User configuration has been lost.", _
                MsgBoxStyle.OkOnly + MsgBoxStyle.Exclamation, XL_Launcher.ERROR_MESSAGE)

        Catch ex As Exception

            MsgBox("There were errors processing the User Config Directory:" & vbCrLf & vbCrLf & _
                vbTab & userConfigPath_ & vbCrLf & vbCrLf & _
                "An attempt to take a backup of the User Config Directory" & vbCrLf & _
                "failed with the following error:" & vbCrLf & vbCrLf & _
                vbTab & ex.Message & vbCrLf & vbCrLf & _
                "User configuration has been lost.", _
                MsgBoxStyle.OkOnly + MsgBoxStyle.Exclamation, XL_Launcher.ERROR_MESSAGE)

        End Try

    End Sub

    Private Sub reloadConfiguration()

        userConfigError_ = False

        loadPreConfiguredEnvironments()
        loadUserConfiguredEnvironments()
        overridePreconfiguredParameters()
        displayPreConfiguredParameters()

        If userConfigError_ Then backupUserConfigDir()

    End Sub

    Private Sub deleteConfiguration()

        Try

            If System.IO.Directory.Exists(userConfigPath_) Then

                System.IO.Directory.Delete(userConfigPath_, True)

            End If

        Catch ex As System.IO.IOException

            Throw New Exception("Error deleting user configuration directory:" & vbCrLf & vbCrLf & _
                userConfigPath_ & vbCrLf & vbCrLf & _
                vbTab & ex.Message & vbCrLf & vbCrLf & _
                "(This error can occur if a file in the directory is locked," & vbCrLf & _
                "or if you have the directory open in Windows Explorer).")

        Catch ex As Exception

            Throw New Exception("Error deleting user configuration directory:" & vbCrLf & vbCrLf & _
                userConfigPath_ & vbCrLf & vbCrLf & _
                vbTab & ex.Message)

        End Try

    End Sub

    Private Function buildNumber() As String

        If (ApplicationDeployment.IsNetworkDeployed) Then
            buildNumber = My.Application.Deployment.CurrentVersion.ToString()
        Else
            buildNumber = "?.?.? (local build)"
        End If

    End Function

    Private Sub initializePaths()

        If (ApplicationDeployment.IsNetworkDeployed) Then

            Dim pathFile As String = ApplicationDeployment.CurrentDeployment.DataDirectory & "\site\paths.xml"
            Dim p As XL_Launcher.Paths = XL_Launcher.Serialization.loadObject(pathFile, "Paths")

            preConfigPath_ = p.PreConfigPath
            userConfigPath_ = Environment.GetFolderPath(Environment.SpecialFolder.LocalApplicationData) + "\XL-Launcher\UserConfigured"
            launcherXlaPath_ = ApplicationDeployment.CurrentDeployment.DataDirectory & "\Addin\Launcher.xla"

        Else

            Dim binDir As String = System.IO.Path.GetDirectoryName(System.IO.Path.GetDirectoryName(Application.ExecutablePath))

            preConfigPath_ = binDir + "\Configuration\PreConfigured"
            userConfigPath_ = binDir + "\Configuration\UserConfigured"
            launcherXlaPath_ = binDir + "\Addin\Launcher.xla"

        End If

        sessionFileDir_ = System.IO.Path.GetTempPath() & "XL-Launcher\"

        tbPreConfigured.Text = preConfigPath_
        tbUserConfigured.Text = userConfigPath_
        tbLauncherXla.Text = launcherXlaPath_
        tbSessionFileDir.Text = sessionFileDir_

    End Sub

    Private Sub loadPreConfiguredEnvironments()

        If System.IO.Directory.Exists(preConfigPath_) Then

            loadPreConfiguredEnvironmentsImpl()

        Else

            envPreconfigured_ = New XL_Launcher.EnvironmentList

        End If

    End Sub

    Private Sub loadPreConfiguredEnvironmentsImpl()

        Dim configPath As String = preConfigPath_ + "\LauncherConfig.xml"
        envPreconfigured_ = XL_Launcher.Serialization.loadObject(configPath, "LauncherConfig")
        envPreconfigured_.loadEnvironments(preConfigPath_)

        lstPreconfigured.Items.Clear()
        For Each e As XL_Launcher.Environment In envPreconfigured_.Environments

            lstPreconfigured.Items.Add(e.Name)

        Next e

        grpGlobalAddins.Controls.Clear()
        Dim y As Integer = 0
        For Each g As XL_Launcher.GlobalAddin In envPreconfigured_.GlobalAddins
            g.draw(grpGlobalAddins.Controls, y)
            y += XL_Launcher.GlobalAddin.HEIGHT
        Next g
        Const BOTTOM_MARGIN_HEIGHT As Integer = 22
        grpGlobalAddins.Height = y + BOTTOM_MARGIN_HEIGHT

    End Sub

    Private Sub restoreSelections(ByVal e As XL_Launcher.EnvironmentList)

        If XL_Launcher.EnvironmentList.PRECONFIGURED = e.SelectedEnvironmentType Then

            If lstPreconfigured.Items.Contains(e.SelectedEnvironmentName) Then

                lstPreconfigured.SelectedIndex = lstPreconfigured.Items.IndexOf(e.SelectedEnvironmentName)

            End If

        Else

            If lstUserConfigured.Items.Contains(e.SelectedEnvironmentName) Then

                lstUserConfigured.SelectedIndex = lstUserConfigured.Items.IndexOf(e.SelectedEnvironmentName)

            End If

        End If

        If lstFrameworks.Items.Contains(e.SelectedFrameworkName) Then

            lstFrameworks.SelectedIndex = lstFrameworks.Items.IndexOf(e.SelectedFrameworkName)

        End If

        tcFramework.SelectedIndex = e.FrameworkTabIndex
        tcEnvironment.SelectedIndex = e.EnvironmentTabIndex

        envPreconfigured_.FrameworkTabIndex = e.FrameworkTabIndex
        envPreconfigured_.EnvironmentTabIndex = e.EnvironmentTabIndex

    End Sub

    Private Sub overridePreconfiguredParameters()

        Try

            Dim configPath As String = userConfigPath_ + "\PreConfigured"

            If System.IO.Directory.Exists(configPath) Then

                Dim e As XL_Launcher.EnvironmentList = _
                    XL_Launcher.Serialization.loadObject(configPath + "\LauncherConfig.xml", "LauncherConfig")
                e.loadEnvironments(configPath)
                userConfigError_ = userConfigError_ Or e.LoadError

                envPreconfigured_.overrideParameters(e)
                restoreSelections(e)

            End If

        Catch ex As Exception

        End Try

    End Sub

    Private Sub displayPreConfiguredParameters()

        tbExcelPath.Text = envPreconfigured_.ExcelPath

    End Sub

    Private Sub loadUserConfiguredEnvironments()

        envUserconfigured_ = New XL_Launcher.EnvironmentList
        Dim path As String = userConfigPath_ + "\UserConfigured"
        lstUserConfigured.Items.Clear()

        If System.IO.Directory.Exists(path) Then

            envUserconfigured_.loadEnvironments(path)
            userConfigError_ = userConfigError_ Or envUserconfigured_.LoadError

            For Each e As XL_Launcher.Environment In envUserconfigured_.Environments

                lstUserConfigured.Items.Add(e.Name)

            Next e

        Else

            System.IO.Directory.CreateDirectory(path)

        End If

    End Sub

    Private Sub initialize()

        initializePaths()
        reloadConfiguration()
        initializeStaticLabels()
        enableVariableMenu()

    End Sub

    ''''''''''''''''''''''''''''''''''''''''''
    ' Events - Main Form
    ''''''''''''''''''''''''''''''''''''''''''

    Private Sub btnLaunch_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles btnLaunch.Click

        Try

            If Not System.IO.File.Exists(launcherXlaPath_) Then
                Throw New Exception("The path to Launcher.xla is invalid : '" & launcherXlaPath_ & "'")
            End If

            SelectedEnvironment.launch(tbExcelPath.Text, sessionFileDir_, launcherXlaPath_, envPreconfigured_.GlobalAddins)
            tbSessionFilePath.Text = SelectedEnvironment.SessionFilePath
            rtSessionFileContents.LoadFile(SelectedEnvironment.SessionFilePath, RichTextBoxStreamType.PlainText)

        Catch ex As Exception

            XL_Launcher.Errors.displayError("Error on launch command", ex)

        End Try

    End Sub

    ''''''''''''''''''''''''''''''''''''''''''
    ' Events - Addins
    ''''''''''''''''''''''''''''''''''''''''''

    Private Sub lbAddins_SelectedIndexChanged(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles lbComponents.SelectedIndexChanged

        Call enableAddinButtons()

    End Sub

    Private Function selectAddin(ByVal initialPath As String) As String

        selectAddin = ""
        Dim validResult As Boolean = False
        If initialPath Is Nothing Then
            initialPath = preConfigPath_
        ElseIf 0 = initialPath.Length Then
            initialPath = preConfigPath_
        End If

        Dim dlg As New OpenFileDialog()
        dlg.InitialDirectory = Path.GetDirectoryName(initialPath)
        dlg.FileName = Path.GetFileName(initialPath)
        dlg.Filter = "Excel Addins (*.xll;*.xla)|*.xll;*.xla"
        dlg.Title = "Select Addin"

        Do While Not validResult

            If System.Windows.Forms.DialogResult.OK = dlg.ShowDialog() Then

                If SelectedFramework.componentNameInUse(dlg.FileName) Then

                    XL_Launcher.Errors.displayError("The framework '" & SelectedFramework.Name & "'" & vbCrLf & vbCrLf & _
                        "already contains a component with path" & vbCrLf & vbCrLf & _
                        "'" & dlg.FileName & "'")

                Else

                    selectAddin = dlg.FileName
                    validResult = True

                End If

            Else

                validResult = True

            End If

        Loop

    End Function

    Private Sub renameAddin()

        Try

            Dim fileName As String = selectAddin(lbComponents.SelectedItem)
            If fileName.Length > 0 Then

                SelectedFramework.renameComponent(lbComponents.SelectedItem, fileName)
                lbComponents.Items(lbComponents.SelectedIndex) = fileName

            End If

        Catch ex As Exception

            XL_Launcher.Errors.displayError("Error renaming addin", ex)

        End Try

    End Sub

    Private Sub btnAddinInsert_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles btnAddinInsert.Click

        Try

            Dim fileName As String = selectAddin(lbComponents.SelectedItem)
            If fileName.Length > 0 Then

                Dim before As Integer = Math.Max(lbComponents.SelectedIndex, 0)
                SelectedFramework.addComponent(fileName, before + 1)
                lbComponents.Items.Insert(before, fileName)
                Call enableAddinButtons()

            End If

        Catch ex As Exception

            XL_Launcher.Errors.displayError("Error inserting addin", ex)

        End Try

    End Sub

    Private Sub btnAddinDelete_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles btnAddinDelete.Click

        Try

            If lbComponents.SelectedIndex = -1 Then Exit Sub
            SelectedFramework.deleteComponent(lbComponents.SelectedItem)
            Dim i As Integer = lbComponents.SelectedIndex
            lbComponents.Items.RemoveAt(i)
            lbComponents.SelectedIndex = Math.Min(i, lbComponents.Items.Count - 1)
            Call enableAddinButtons()

        Catch ex As Exception

            XL_Launcher.Errors.displayError("Error deleting addin", ex)

        End Try

    End Sub

    Private Sub btnAddinRename_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles btnAddinRename.Click

        renameAddin()

    End Sub

    Private Sub btnAddinUp_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles btnAddinUp.Click

        Try

            If lbComponents.Items.Count <= 1 Then Exit Sub
            If lbComponents.SelectedIndex < 1 Then Exit Sub
            SelectedFramework.moveComponentUp(lbComponents.SelectedIndex)
            Dim o As Object = lbComponents.SelectedItem
            Dim i As Integer = lbComponents.SelectedIndex
            lbComponents.Items.RemoveAt(i)
            lbComponents.Items.Insert(i - 1, o)
            lbComponents.SelectedIndex = i - 1

        Catch ex As Exception

            XL_Launcher.Errors.displayError("Error on addin up", ex)

        End Try

    End Sub

    Private Sub btnAddinDown_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles btnAddinDown.Click

        Try

            If lbComponents.SelectedIndex = -1 Then Exit Sub
            If lbComponents.Items.Count <= 1 Then Exit Sub
            If lbComponents.SelectedIndex = (lbComponents.Items.Count - 1) Then Exit Sub
            SelectedFramework.moveComponentDown(lbComponents.SelectedIndex)
            Dim o As Object = lbComponents.SelectedItem
            Dim i As Integer = lbComponents.SelectedIndex
            lbComponents.Items.RemoveAt(i)
            lbComponents.Items.Insert(i + 1, o)
            lbComponents.SelectedIndex = i + 1

        Catch ex As Exception

            XL_Launcher.Errors.displayError("Error on addin down", ex)

        End Try

    End Sub

    Private Sub lbComponents_MouseDoubleClick(ByVal sender As System.Object, ByVal e As System.Windows.Forms.MouseEventArgs) Handles lbComponents.MouseDoubleClick

        renameAddin()

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
                SelectedFramework.insertVariable(variableName, variableValue)
                Dim listItem As New ListViewItem(variableName)
                listItem.SubItems.Add(variableValue)
                lvVariables.Items.Add(listItem)
            End If

        Catch ex As Exception

            XL_Launcher.Errors.displayError("Error inserting new environment variable", ex)

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
                SelectedFramework.updateVariable(variableNameOld, variableName, variableValue)
                lvVariables.SelectedItems(0).Text = variableName
                lvVariables.SelectedItems(0).SubItems(1).Text = variableValue
            End If

        Catch ex As Exception

            XL_Launcher.Errors.displayError("Error editing environment variable", ex)

        End Try

    End Sub

    Private Sub miVariableDelete_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles miVariableDelete.Click

        Try

            SelectedFramework.deleteVariable(lvVariables.SelectedItems(0).Text)
            lvVariables.Items.Remove(lvVariables.SelectedItems(0))

        Catch ex As Exception

            XL_Launcher.Errors.displayError("Error deleting environment variable", ex)

        End Try

    End Sub

    Private Sub lstPreconfigured_SelectedIndexChanged(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles lstPreconfigured.SelectedIndexChanged

        If lstPreconfigured.SelectedIndex = -1 Then Exit Sub
        If processingEvents_ Then
            processingEvents_ = False
            lstUserConfigured.SelectedIndex = -1
            setEnabled(False)
            pnlStartupParameters.Controls.Clear()
            lbComponents.Items.Clear()
            lstFrameworks.Items.Clear()
            SelectedEnvironment = envPreconfigured_.nameToEnvironment(lstPreconfigured.Text)
            For Each f As XL_Launcher.Framework In SelectedEnvironment.Frameworks
                lstFrameworks.Items.Add(f.Name)
            Next f
            If lstFrameworks.Items.Count > 0 Then lstFrameworks.SelectedIndex = 0
            txtAuthenticationFile.Text = SelectedEnvironment.AuthenticationFile
            envPreconfigured_.SelectedEnvironmentType = XL_Launcher.EnvironmentList.PRECONFIGURED
            envPreconfigured_.SelectedEnvironmentName = lstPreconfigured.Text
            processingEvents_ = True
        End If

    End Sub

    Private Sub lstUserConfigured_SelectedIndexChanged(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles lstUserConfigured.SelectedIndexChanged

        If lstUserConfigured.SelectedIndex = -1 Then Exit Sub
        If processingEvents_ Then
            processingEvents_ = False
            lstPreconfigured.SelectedIndex = -1
            setEnabled(True)
            pnlStartupParameters.Controls.Clear()
            lbComponents.Items.Clear()
            lstFrameworks.Items.Clear()
            SelectedEnvironment = envUserconfigured_.nameToEnvironment(lstUserConfigured.Text)
            For Each f As XL_Launcher.Framework In SelectedEnvironment.Frameworks
                lstFrameworks.Items.Add(f.Name)
            Next f
            If lstFrameworks.Items.Count > 0 Then lstFrameworks.SelectedIndex = 0
            txtAuthenticationFile.Text = SelectedEnvironment.AuthenticationFile
            envPreconfigured_.SelectedEnvironmentType = XL_Launcher.EnvironmentList.USERCONFIGURED
            envPreconfigured_.SelectedEnvironmentName = lstUserConfigured.Text
            processingEvents_ = True
        End If

    End Sub

    Private Sub lstFrameworks_SelectedIndexChanged(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles lstFrameworks.SelectedIndexChanged

        pnlStartupParameters.Controls.Clear()
        lbComponents.Items.Clear()
        If lstFrameworks.SelectedIndex = -1 Then
            selectedFramework_ = Nothing
            Exit Sub
        End If
        SelectedFramework = SelectedEnvironment.nameToFramework(lstFrameworks.Text)
        envPreconfigured_.SelectedFrameworkName = lstFrameworks.Text

        Dim y As Integer = 0
        For Each g As XL_Launcher.GroupBox In SelectedFramework.GroupBoxes
            If Not g.visible Then Continue For
            g.draw(pnlStartupParameters.Controls, y)
            Dim y2 As Integer = 20
            For Each sp As XL_Launcher.StartupParameter In g.StartupParameterList
                If sp.visible Then
                    sp.draw(g.GroupBox.Controls, y2)
                    y2 += sp.height
                End If
            Next sp
            g.GroupBox.Height = y2 + 5
            y += y2 + 10
        Next g

        For Each c As XL_Launcher.Component In SelectedFramework.ComponentList
            lbComponents.Items.Add(c.path)
        Next c
        initializeVariableList()

    End Sub

    Private Sub btnCopy_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles btnCopy.Click

        Try

            Dim deriveCopyName As String = envUserconfigured_.deriveCopyName(SelectedEnvironment.Name)
            Dim newName As String = inputEnvironmentName(deriveCopyName)
            If 0 = newName.Length Then Exit Sub

            SelectedEnvironment = envUserconfigured_.copyEnvironment(SelectedEnvironment, newName)
            Dim newIndex As Integer = lstUserConfigured.Items.Add(newName)
            lstUserConfigured.SelectedIndex = newIndex

        Catch ex As Exception

            XL_Launcher.Errors.displayError("Error copying environment", ex)

        End Try

    End Sub

    Private Sub btnRename_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles btnRename.Click

        If lstUserConfigured.SelectedIndex = -1 Then Exit Sub

        Try

            Dim oldName As String = SelectedEnvironment.Name
            Dim newName As String = inputEnvironmentName(oldName)
            If 0 = newName.Length Or newName = oldName Then Exit Sub

            envUserconfigured_.renameEnvironment(oldName, newName)
            SelectedEnvironment.Name = newName
            processingEvents_ = False
            lstUserConfigured.Items.Remove(oldName)
            Dim newIndex As Integer = lstUserConfigured.Items.Add(newName)
            lstUserConfigured.SelectedIndex = newIndex
            processingEvents_ = True

        Catch ex As Exception

            XL_Launcher.Errors.displayError("Error renaming environment", ex)

        End Try

    End Sub

    Private Sub btnDelete_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles btnDelete.Click

        Try

            If lstUserConfigured.SelectedIndex = -1 Then
                XL_Launcher.Errors.displayError("No environment selected")
                Exit Sub
            End If

            envUserconfigured_.deleteEnvironment(lstUserConfigured.Text)
            Dim deletedIndex As Integer = lstUserConfigured.SelectedIndex
            lstUserConfigured.Items.RemoveAt(lstUserConfigured.SelectedIndex)

            If lstUserConfigured.Items.Count > 0 Then
                lstUserConfigured.SelectedIndex = Math.Min(deletedIndex, lstUserConfigured.Items.Count - 1)
            Else
                lstPreconfigured.SelectedIndex = 0
            End If

        Catch ex As Exception

            XL_Launcher.Errors.displayError("Error deleting environment", ex)

        End Try

    End Sub

    Private Sub FormMain_FormClosed(ByVal sender As System.Object, ByVal e As System.Windows.Forms.FormClosedEventArgs) Handles MyBase.FormClosed

        closeForm()

    End Sub

    Private Sub closeForm()

        Try

            saveConfiguration()

        Catch ex As Exception

            XL_Launcher.Errors.displayError("Error while closing launcher", ex)

        End Try

    End Sub

    Private Sub btnGenerateSessionFile_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles btnGenerateSessionFile.Click

        Try

            SelectedEnvironment.generateSessionFile(envPreconfigured_.GlobalAddins, sessionFileDir_)
            tbSessionFilePath.Text = SelectedEnvironment.SessionFilePath
            rtSessionFileContents.LoadFile(SelectedEnvironment.SessionFilePath, RichTextBoxStreamType.PlainText)

        Catch ex As Exception

            XL_Launcher.Errors.displayError("Error generating session file", ex)

        End Try

    End Sub

    Private Sub setEnabled(ByVal enabled As Boolean)

        'txtAuthenticationFile.ReadOnly = Not enabled
        lvVariables.Enabled = enabled
        btnRename.Enabled = enabled
        btnDelete.Enabled = enabled
        setAddinsEnabled(enabled)

    End Sub

    Private Sub tcEnvironment_SelectedIndexChanged(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles tcEnvironment.SelectedIndexChanged

        If Not envPreconfigured_ Is Nothing Then
            envPreconfigured_.EnvironmentTabIndex = tcEnvironment.SelectedIndex
        End If

    End Sub

    Private Sub tcFramework_SelectedIndexChanged(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles tcFramework.SelectedIndexChanged

        If Not envPreconfigured_ Is Nothing Then
            envPreconfigured_.FrameworkTabIndex = tcFramework.SelectedIndex
        End If

    End Sub

    Private Sub btnSaveConfiguration_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles btnSaveConfiguration.Click

        Try

            saveConfiguration()
            MsgBox("Configuration successfully saved", MsgBoxStyle.OkOnly + MsgBoxStyle.Information, XL_Launcher.INFO_MESSAGE)

        Catch ex As Exception

            XL_Launcher.Errors.displayError("Error saving configuration", ex)

        End Try

    End Sub

    Private Sub btnReloadConfiguration_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles btnReloadConfiguration.Click

        Try

            Dim msg As String = "This operation will discard any unsaved changes which you have made to the" & vbCrLf & _
                "configuration and reload your previously saved configuration from the following path:" & vbCrLf & vbCrLf & _
                userConfigPath_ & vbCrLf & vbCrLf & _
                "Are you sure you want to proceed?"

            Dim r As MsgBoxResult = MsgBox(msg, MsgBoxStyle.YesNo + MsgBoxStyle.Information, XL_Launcher.INFO_MESSAGE)

            If MsgBoxResult.Yes = r Then

                reloadConfiguration()

                MsgBox("Configuration successfully reloaded", MsgBoxStyle.OkOnly + MsgBoxStyle.Information, XL_Launcher.INFO_MESSAGE)

            ElseIf MsgBoxResult.No = r Then

                MsgBox("Operation cancelled", MsgBoxStyle.OkOnly + MsgBoxStyle.Information, XL_Launcher.INFO_MESSAGE)

            End If

        Catch ex As Exception

            XL_Launcher.Errors.displayError("Error reloading configuration", ex)

        End Try

    End Sub

    Private Sub btnDeleteConfiguration_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles btnDeleteConfiguration.Click

        Try

            Dim msg As String = "This operation will delete any configuration" & vbCrLf & _
                "which was saved to the following path:" & vbCrLf & vbCrLf & _
                userConfigPath_ & vbCrLf & vbCrLf & _
                "Are you sure you want to proceed?"

            Dim r As MsgBoxResult = MsgBox(msg, MsgBoxStyle.YesNo + MsgBoxStyle.Information, XL_Launcher.INFO_MESSAGE)

            If MsgBoxResult.Yes = r Then

                deleteConfiguration()
                reloadConfiguration()

                MsgBox("Configuration successfully deleted." & vbCrLf & vbCrLf & _
                    "Any further configuration changes which you make" & vbCrLf & _
                    "during the remainder of the current session" & vbCrLf & _
                    "will be saved on exit.", MsgBoxStyle.OkOnly + MsgBoxStyle.Information, XL_Launcher.INFO_MESSAGE)

            ElseIf MsgBoxResult.No = r Then

                MsgBox("Operation cancelled", MsgBoxStyle.OkOnly + MsgBoxStyle.Information, XL_Launcher.INFO_MESSAGE)

            End If

        Catch ex As Exception

            XL_Launcher.Errors.displayError("Error deleting configuration", ex)

        End Try


    End Sub

    Private Sub btnLaunchExcel_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles btnLaunchExcel.Click

        Try

            If Not System.IO.File.Exists(envPreconfigured_.ExcelPath) Then
                Throw New Exception("The specified Excel path:" & vbCrLf & vbCrLf & envPreconfigured_.ExcelPath & _
                vbCrLf & vbCrLf & "is invalid.")
            End If

            Shell(envPreconfigured_.ExcelPath, AppWinStyle.NormalFocus)

        Catch ex As Exception

            XL_Launcher.Errors.displayError("Error starting Excel", ex)

        End Try

    End Sub

    Private Sub btnExcelPath_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles btnExcelPath.Click

        Try

            Dim dlg As New OpenFileDialog()
            If System.IO.File.Exists(tbExcelPath.Text) Then
                dlg.InitialDirectory = tbExcelPath.Text
            Else
                dlg.InitialDirectory = deriveDefaultExcelPath()
            End If
            dlg.FileName = "EXCEL.EXE"
            dlg.Filter = "Excel executable (EXCEL.EXE)|EXCEL.EXE"
            dlg.Title = "Select Excel executable"
            If dlg.ShowDialog() = System.Windows.Forms.DialogResult.OK Then
                tbExcelPath.Text = dlg.FileName
                envPreconfigured_.ExcelPath = dlg.FileName
            End If

        Catch ex As Exception

            XL_Launcher.Errors.displayError("Error processing path to Excel", ex)

        End Try

    End Sub

End Class
