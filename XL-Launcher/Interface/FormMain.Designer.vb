<Global.Microsoft.VisualBasic.CompilerServices.DesignerGenerated()> _
Partial Class FormMain
    Inherits System.Windows.Forms.Form

    'Form overrides dispose to clean up the component list.
    <System.Diagnostics.DebuggerNonUserCode()> _
    Protected Overrides Sub Dispose(ByVal disposing As Boolean)
        If disposing AndAlso components IsNot Nothing Then
            components.Dispose()
        End If
        MyBase.Dispose(disposing)
    End Sub

    'Required by the Windows Form Designer
    Private components As System.ComponentModel.IContainer

    'NOTE: The following procedure is required by the Windows Form Designer
    'It can be modified using the Windows Form Designer.  
    'Do not modify it using the code editor.
    <System.Diagnostics.DebuggerStepThrough()> _
    Private Sub InitializeComponent()
        Me.components = New System.ComponentModel.Container
        Dim resources As System.ComponentModel.ComponentResourceManager = New System.ComponentModel.ComponentResourceManager(GetType(FormMain))
        Dim ListViewItem1 As System.Windows.Forms.ListViewItem = New System.Windows.Forms.ListViewItem(New String() {"EXAMPLE_DIR", "C:\Example\Dir"}, -1)
        Me.btnLaunch = New System.Windows.Forms.Button
        Me.cmVariables = New System.Windows.Forms.ContextMenuStrip(Me.components)
        Me.miVariableInsert = New System.Windows.Forms.ToolStripMenuItem
        Me.miVariableEdit = New System.Windows.Forms.ToolStripMenuItem
        Me.miVariableDelete = New System.Windows.Forms.ToolStripMenuItem
        Me.tpAbout = New System.Windows.Forms.TabPage
        Me.rtHelp = New System.Windows.Forms.RichTextBox
        Me.lblMachineValue = New System.Windows.Forms.Label
        Me.lblMachine = New System.Windows.Forms.Label
        Me.lblDomainValue = New System.Windows.Forms.Label
        Me.lblUserNameValue = New System.Windows.Forms.Label
        Me.lblVersionValue = New System.Windows.Forms.Label
        Me.lblDomain = New System.Windows.Forms.Label
        Me.lblUserName = New System.Windows.Forms.Label
        Me.lblVersion = New System.Windows.Forms.Label
        Me.tpPaths = New System.Windows.Forms.TabPage
        Me.grpLocalPaths = New System.Windows.Forms.GroupBox
        Me.btnLaunchExcel = New System.Windows.Forms.Button
        Me.btnExcelPath = New System.Windows.Forms.Button
        Me.lblExcelPath = New System.Windows.Forms.Label
        Me.tbExcelPath = New System.Windows.Forms.TextBox
        Me.grpAppPaths = New System.Windows.Forms.GroupBox
        Me.lblSessionFileDir = New System.Windows.Forms.Label
        Me.tbSessionFileDir = New System.Windows.Forms.TextBox
        Me.tscConfiguration = New System.Windows.Forms.ToolStripContainer
        Me.tstConfiguration = New System.Windows.Forms.ToolStrip
        Me.btnSaveConfiguration = New System.Windows.Forms.ToolStripButton
        Me.btnReloadConfiguration = New System.Windows.Forms.ToolStripButton
        Me.btnDeleteConfiguration = New System.Windows.Forms.ToolStripButton
        Me.lblPreConfigured = New System.Windows.Forms.Label
        Me.lblLauncherXla = New System.Windows.Forms.Label
        Me.tbUserConfigured = New System.Windows.Forms.TextBox
        Me.tbLauncherXla = New System.Windows.Forms.TextBox
        Me.lblUserConfigured = New System.Windows.Forms.Label
        Me.tbPreConfigured = New System.Windows.Forms.TextBox
        Me.grpGlobalAddins = New System.Windows.Forms.GroupBox
        Me.tpEnvironments2 = New System.Windows.Forms.TabPage
        Me.tcEnvironment = New System.Windows.Forms.TabControl
        Me.TabPage1 = New System.Windows.Forms.TabPage
        Me.tcFramework = New System.Windows.Forms.TabControl
        Me.TabPage3 = New System.Windows.Forms.TabPage
        Me.pnlStartupParameters = New System.Windows.Forms.Panel
        Me.TabPage4 = New System.Windows.Forms.TabPage
        Me.tscComponents = New System.Windows.Forms.ToolStripContainer
        Me.tstAddins = New System.Windows.Forms.ToolStrip
        Me.btnAddinInsert = New System.Windows.Forms.ToolStripButton
        Me.btnAddinDelete = New System.Windows.Forms.ToolStripButton
        Me.btnAddinRename = New System.Windows.Forms.ToolStripButton
        Me.btnAddinUp = New System.Windows.Forms.ToolStripButton
        Me.btnAddinDown = New System.Windows.Forms.ToolStripButton
        Me.lbComponents = New System.Windows.Forms.ListBox
        Me.TabPage5 = New System.Windows.Forms.TabPage
        Me.lvVariables = New System.Windows.Forms.ListView
        Me.ColumnHeader1 = New System.Windows.Forms.ColumnHeader
        Me.ColumnHeader2 = New System.Windows.Forms.ColumnHeader
        Me.lblFrameworks = New System.Windows.Forms.Label
        Me.lstFrameworks = New System.Windows.Forms.ListBox
        Me.TabPage2 = New System.Windows.Forms.TabPage
        Me.Label6 = New System.Windows.Forms.Label
        Me.tbSessionFilePath = New System.Windows.Forms.TextBox
        Me.btnGenerateSessionFile = New System.Windows.Forms.Button
        Me.rtSessionFileContents = New System.Windows.Forms.RichTextBox
        Me.txtAuthenticationFile = New System.Windows.Forms.TextBox
        Me.lblAuthenticationFile = New System.Windows.Forms.Label
        Me.grpEnvironments = New System.Windows.Forms.GroupBox
        Me.tscEnvironments = New System.Windows.Forms.ToolStripContainer
        Me.tstEnvironment = New System.Windows.Forms.ToolStrip
        Me.btnCopy = New System.Windows.Forms.ToolStripButton
        Me.btnRename = New System.Windows.Forms.ToolStripButton
        Me.btnDelete = New System.Windows.Forms.ToolStripButton
        Me.lstUserConfigured = New System.Windows.Forms.ListBox
        Me.lblUserConfigured2 = New System.Windows.Forms.Label
        Me.lstPreconfigured = New System.Windows.Forms.ListBox
        Me.lblPreconfigured2 = New System.Windows.Forms.Label
        Me.tcLauncher = New System.Windows.Forms.TabControl
        Me.chVariable = New System.Windows.Forms.ColumnHeader
        Me.chValue = New System.Windows.Forms.ColumnHeader
        Me.cmVariables.SuspendLayout()
        Me.tpAbout.SuspendLayout()
        Me.tpPaths.SuspendLayout()
        Me.grpLocalPaths.SuspendLayout()
        Me.grpAppPaths.SuspendLayout()
        Me.tscConfiguration.ContentPanel.SuspendLayout()
        Me.tscConfiguration.SuspendLayout()
        Me.tstConfiguration.SuspendLayout()
        Me.tpEnvironments2.SuspendLayout()
        Me.tcEnvironment.SuspendLayout()
        Me.TabPage1.SuspendLayout()
        Me.tcFramework.SuspendLayout()
        Me.TabPage3.SuspendLayout()
        Me.TabPage4.SuspendLayout()
        Me.tscComponents.ContentPanel.SuspendLayout()
        Me.tscComponents.SuspendLayout()
        Me.tstAddins.SuspendLayout()
        Me.TabPage5.SuspendLayout()
        Me.TabPage2.SuspendLayout()
        Me.grpEnvironments.SuspendLayout()
        Me.tscEnvironments.ContentPanel.SuspendLayout()
        Me.tscEnvironments.SuspendLayout()
        Me.tstEnvironment.SuspendLayout()
        Me.tcLauncher.SuspendLayout()
        Me.SuspendLayout()
        '
        'btnLaunch
        '
        Me.btnLaunch.Enabled = False
        Me.btnLaunch.Location = New System.Drawing.Point(9, 496)
        Me.btnLaunch.Name = "btnLaunch"
        Me.btnLaunch.Size = New System.Drawing.Size(75, 23)
        Me.btnLaunch.TabIndex = 20
        Me.btnLaunch.Text = "Launch"
        Me.btnLaunch.UseVisualStyleBackColor = True
        '
        'cmVariables
        '
        Me.cmVariables.Items.AddRange(New System.Windows.Forms.ToolStripItem() {Me.miVariableInsert, Me.miVariableEdit, Me.miVariableDelete})
        Me.cmVariables.Name = "cmVariables"
        Me.cmVariables.Size = New System.Drawing.Size(117, 70)
        '
        'miVariableInsert
        '
        Me.miVariableInsert.Name = "miVariableInsert"
        Me.miVariableInsert.Size = New System.Drawing.Size(116, 22)
        Me.miVariableInsert.Text = "Insert"
        '
        'miVariableEdit
        '
        Me.miVariableEdit.Name = "miVariableEdit"
        Me.miVariableEdit.Size = New System.Drawing.Size(116, 22)
        Me.miVariableEdit.Text = "Edit"
        '
        'miVariableDelete
        '
        Me.miVariableDelete.Name = "miVariableDelete"
        Me.miVariableDelete.Size = New System.Drawing.Size(116, 22)
        Me.miVariableDelete.Text = "Delete"
        '
        'tpAbout
        '
        Me.tpAbout.Controls.Add(Me.rtHelp)
        Me.tpAbout.Controls.Add(Me.lblMachineValue)
        Me.tpAbout.Controls.Add(Me.lblMachine)
        Me.tpAbout.Controls.Add(Me.lblDomainValue)
        Me.tpAbout.Controls.Add(Me.lblUserNameValue)
        Me.tpAbout.Controls.Add(Me.lblVersionValue)
        Me.tpAbout.Controls.Add(Me.lblDomain)
        Me.tpAbout.Controls.Add(Me.lblUserName)
        Me.tpAbout.Controls.Add(Me.lblVersion)
        Me.tpAbout.Location = New System.Drawing.Point(4, 22)
        Me.tpAbout.Name = "tpAbout"
        Me.tpAbout.Padding = New System.Windows.Forms.Padding(3)
        Me.tpAbout.Size = New System.Drawing.Size(913, 539)
        Me.tpAbout.TabIndex = 4
        Me.tpAbout.Text = "About"
        Me.tpAbout.UseVisualStyleBackColor = True
        '
        'rtHelp
        '
        Me.rtHelp.BorderStyle = System.Windows.Forms.BorderStyle.None
        Me.rtHelp.Location = New System.Drawing.Point(9, 103)
        Me.rtHelp.Name = "rtHelp"
        Me.rtHelp.ReadOnly = True
        Me.rtHelp.Size = New System.Drawing.Size(280, 399)
        Me.rtHelp.TabIndex = 9
        Me.rtHelp.Text = ""
        '
        'lblMachineValue
        '
        Me.lblMachineValue.AutoSize = True
        Me.lblMachineValue.Location = New System.Drawing.Point(155, 72)
        Me.lblMachineValue.Name = "lblMachineValue"
        Me.lblMachineValue.Size = New System.Drawing.Size(47, 13)
        Me.lblMachineValue.TabIndex = 8
        Me.lblMachineValue.Text = "machine"
        '
        'lblMachine
        '
        Me.lblMachine.AutoSize = True
        Me.lblMachine.Location = New System.Drawing.Point(6, 72)
        Me.lblMachine.Name = "lblMachine"
        Me.lblMachine.Size = New System.Drawing.Size(48, 13)
        Me.lblMachine.TabIndex = 7
        Me.lblMachine.Text = "Machine"
        '
        'lblDomainValue
        '
        Me.lblDomainValue.AutoSize = True
        Me.lblDomainValue.Location = New System.Drawing.Point(156, 32)
        Me.lblDomainValue.Name = "lblDomainValue"
        Me.lblDomainValue.Size = New System.Drawing.Size(41, 13)
        Me.lblDomainValue.TabIndex = 6
        Me.lblDomainValue.Text = "domain"
        '
        'lblUserNameValue
        '
        Me.lblUserNameValue.AutoSize = True
        Me.lblUserNameValue.Location = New System.Drawing.Point(156, 52)
        Me.lblUserNameValue.Name = "lblUserNameValue"
        Me.lblUserNameValue.Size = New System.Drawing.Size(27, 13)
        Me.lblUserNameValue.TabIndex = 5
        Me.lblUserNameValue.Text = "user"
        '
        'lblVersionValue
        '
        Me.lblVersionValue.AutoSize = True
        Me.lblVersionValue.Location = New System.Drawing.Point(155, 12)
        Me.lblVersionValue.Name = "lblVersionValue"
        Me.lblVersionValue.Size = New System.Drawing.Size(40, 13)
        Me.lblVersionValue.TabIndex = 4
        Me.lblVersionValue.Text = "0.0.0.0"
        '
        'lblDomain
        '
        Me.lblDomain.AutoSize = True
        Me.lblDomain.Location = New System.Drawing.Point(6, 32)
        Me.lblDomain.Name = "lblDomain"
        Me.lblDomain.Size = New System.Drawing.Size(43, 13)
        Me.lblDomain.TabIndex = 2
        Me.lblDomain.Text = "Domain"
        '
        'lblUserName
        '
        Me.lblUserName.AutoSize = True
        Me.lblUserName.Location = New System.Drawing.Point(6, 52)
        Me.lblUserName.Name = "lblUserName"
        Me.lblUserName.Size = New System.Drawing.Size(29, 13)
        Me.lblUserName.TabIndex = 1
        Me.lblUserName.Text = "User"
        '
        'lblVersion
        '
        Me.lblVersion.AutoSize = True
        Me.lblVersion.Location = New System.Drawing.Point(6, 12)
        Me.lblVersion.Name = "lblVersion"
        Me.lblVersion.Size = New System.Drawing.Size(68, 13)
        Me.lblVersion.TabIndex = 0
        Me.lblVersion.Text = "XL-Launcher"
        '
        'tpPaths
        '
        Me.tpPaths.Controls.Add(Me.grpLocalPaths)
        Me.tpPaths.Controls.Add(Me.grpAppPaths)
        Me.tpPaths.Controls.Add(Me.grpGlobalAddins)
        Me.tpPaths.Location = New System.Drawing.Point(4, 22)
        Me.tpPaths.Name = "tpPaths"
        Me.tpPaths.Padding = New System.Windows.Forms.Padding(3)
        Me.tpPaths.Size = New System.Drawing.Size(913, 539)
        Me.tpPaths.TabIndex = 1
        Me.tpPaths.Text = "Paths"
        Me.tpPaths.UseVisualStyleBackColor = True
        '
        'grpLocalPaths
        '
        Me.grpLocalPaths.Controls.Add(Me.btnLaunchExcel)
        Me.grpLocalPaths.Controls.Add(Me.btnExcelPath)
        Me.grpLocalPaths.Controls.Add(Me.lblExcelPath)
        Me.grpLocalPaths.Controls.Add(Me.tbExcelPath)
        Me.grpLocalPaths.Location = New System.Drawing.Point(5, 191)
        Me.grpLocalPaths.Name = "grpLocalPaths"
        Me.grpLocalPaths.Size = New System.Drawing.Size(901, 60)
        Me.grpLocalPaths.TabIndex = 46
        Me.grpLocalPaths.TabStop = False
        Me.grpLocalPaths.Text = "Local Paths"
        '
        'btnLaunchExcel
        '
        Me.btnLaunchExcel.Image = Global.launcher.My.Resources.Resources.excel
        Me.btnLaunchExcel.Location = New System.Drawing.Point(825, 33)
        Me.btnLaunchExcel.Name = "btnLaunchExcel"
        Me.btnLaunchExcel.Size = New System.Drawing.Size(32, 23)
        Me.btnLaunchExcel.TabIndex = 34
        Me.btnLaunchExcel.UseVisualStyleBackColor = True
        '
        'btnExcelPath
        '
        Me.btnExcelPath.Location = New System.Drawing.Point(863, 33)
        Me.btnExcelPath.Name = "btnExcelPath"
        Me.btnExcelPath.Size = New System.Drawing.Size(32, 23)
        Me.btnExcelPath.TabIndex = 33
        Me.btnExcelPath.Text = "..."
        Me.btnExcelPath.UseVisualStyleBackColor = True
        '
        'lblExcelPath
        '
        Me.lblExcelPath.AutoSize = True
        Me.lblExcelPath.Location = New System.Drawing.Point(7, 16)
        Me.lblExcelPath.Name = "lblExcelPath"
        Me.lblExcelPath.Size = New System.Drawing.Size(70, 13)
        Me.lblExcelPath.TabIndex = 30
        Me.lblExcelPath.Text = "Path to Excel"
        '
        'tbExcelPath
        '
        Me.tbExcelPath.Location = New System.Drawing.Point(20, 33)
        Me.tbExcelPath.Name = "tbExcelPath"
        Me.tbExcelPath.Size = New System.Drawing.Size(799, 20)
        Me.tbExcelPath.TabIndex = 32
        '
        'grpAppPaths
        '
        Me.grpAppPaths.Controls.Add(Me.lblSessionFileDir)
        Me.grpAppPaths.Controls.Add(Me.tbSessionFileDir)
        Me.grpAppPaths.Controls.Add(Me.tscConfiguration)
        Me.grpAppPaths.Controls.Add(Me.lblPreConfigured)
        Me.grpAppPaths.Controls.Add(Me.lblLauncherXla)
        Me.grpAppPaths.Controls.Add(Me.tbUserConfigured)
        Me.grpAppPaths.Controls.Add(Me.tbLauncherXla)
        Me.grpAppPaths.Controls.Add(Me.lblUserConfigured)
        Me.grpAppPaths.Controls.Add(Me.tbPreConfigured)
        Me.grpAppPaths.Location = New System.Drawing.Point(6, 6)
        Me.grpAppPaths.Name = "grpAppPaths"
        Me.grpAppPaths.Size = New System.Drawing.Size(901, 179)
        Me.grpAppPaths.TabIndex = 45
        Me.grpAppPaths.TabStop = False
        Me.grpAppPaths.Text = "Application Paths"
        '
        'lblSessionFileDir
        '
        Me.lblSessionFileDir.AutoSize = True
        Me.lblSessionFileDir.Location = New System.Drawing.Point(7, 133)
        Me.lblSessionFileDir.Name = "lblSessionFileDir"
        Me.lblSessionFileDir.Size = New System.Drawing.Size(108, 13)
        Me.lblSessionFileDir.TabIndex = 45
        Me.lblSessionFileDir.Text = "Session File Directory"
        '
        'tbSessionFileDir
        '
        Me.tbSessionFileDir.Location = New System.Drawing.Point(20, 149)
        Me.tbSessionFileDir.Name = "tbSessionFileDir"
        Me.tbSessionFileDir.ReadOnly = True
        Me.tbSessionFileDir.Size = New System.Drawing.Size(875, 20)
        Me.tbSessionFileDir.TabIndex = 46
        '
        'tscConfiguration
        '
        '
        'tscConfiguration.ContentPanel
        '
        Me.tscConfiguration.ContentPanel.Controls.Add(Me.tstConfiguration)
        Me.tscConfiguration.ContentPanel.Size = New System.Drawing.Size(74, 5)
        Me.tscConfiguration.Location = New System.Drawing.Point(821, 110)
        Me.tscConfiguration.Name = "tscConfiguration"
        Me.tscConfiguration.Size = New System.Drawing.Size(74, 30)
        Me.tscConfiguration.TabIndex = 44
        Me.tscConfiguration.Text = "ToolStripContainer1"
        '
        'tstConfiguration
        '
        Me.tstConfiguration.Dock = System.Windows.Forms.DockStyle.None
        Me.tstConfiguration.GripStyle = System.Windows.Forms.ToolStripGripStyle.Hidden
        Me.tstConfiguration.Items.AddRange(New System.Windows.Forms.ToolStripItem() {Me.btnSaveConfiguration, Me.btnReloadConfiguration, Me.btnDeleteConfiguration})
        Me.tstConfiguration.Location = New System.Drawing.Point(0, 0)
        Me.tstConfiguration.Name = "tstConfiguration"
        Me.tstConfiguration.RenderMode = System.Windows.Forms.ToolStripRenderMode.System
        Me.tstConfiguration.Size = New System.Drawing.Size(72, 25)
        Me.tstConfiguration.TabIndex = 0
        Me.tstConfiguration.Text = "ToolStrip1"
        '
        'btnSaveConfiguration
        '
        Me.btnSaveConfiguration.DisplayStyle = System.Windows.Forms.ToolStripItemDisplayStyle.Image
        Me.btnSaveConfiguration.Image = Global.launcher.My.Resources.Resources.save
        Me.btnSaveConfiguration.ImageTransparentColor = System.Drawing.Color.Magenta
        Me.btnSaveConfiguration.Name = "btnSaveConfiguration"
        Me.btnSaveConfiguration.Size = New System.Drawing.Size(23, 22)
        Me.btnSaveConfiguration.Text = "Save Configuration"
        '
        'btnReloadConfiguration
        '
        Me.btnReloadConfiguration.DisplayStyle = System.Windows.Forms.ToolStripItemDisplayStyle.Image
        Me.btnReloadConfiguration.Image = Global.launcher.My.Resources.Resources.reload
        Me.btnReloadConfiguration.ImageTransparentColor = System.Drawing.Color.Magenta
        Me.btnReloadConfiguration.Name = "btnReloadConfiguration"
        Me.btnReloadConfiguration.Size = New System.Drawing.Size(23, 22)
        Me.btnReloadConfiguration.Text = "Reload Configuration"
        '
        'btnDeleteConfiguration
        '
        Me.btnDeleteConfiguration.DisplayStyle = System.Windows.Forms.ToolStripItemDisplayStyle.Image
        Me.btnDeleteConfiguration.Image = Global.launcher.My.Resources.Resources.delete
        Me.btnDeleteConfiguration.ImageTransparentColor = System.Drawing.Color.Magenta
        Me.btnDeleteConfiguration.Name = "btnDeleteConfiguration"
        Me.btnDeleteConfiguration.Size = New System.Drawing.Size(23, 22)
        Me.btnDeleteConfiguration.Text = "Delete Configuration"
        '
        'lblPreConfigured
        '
        Me.lblPreConfigured.AutoSize = True
        Me.lblPreConfigured.Location = New System.Drawing.Point(7, 55)
        Me.lblPreConfigured.Name = "lblPreConfigured"
        Me.lblPreConfigured.Size = New System.Drawing.Size(122, 13)
        Me.lblPreConfigured.TabIndex = 40
        Me.lblPreConfigured.Text = "Pre-Configured Directory"
        '
        'lblLauncherXla
        '
        Me.lblLauncherXla.AutoSize = True
        Me.lblLauncherXla.Location = New System.Drawing.Point(7, 16)
        Me.lblLauncherXla.Name = "lblLauncherXla"
        Me.lblLauncherXla.Size = New System.Drawing.Size(105, 13)
        Me.lblLauncherXla.TabIndex = 31
        Me.lblLauncherXla.Text = "Path to Launcher.xla"
        '
        'tbUserConfigured
        '
        Me.tbUserConfigured.Location = New System.Drawing.Point(20, 110)
        Me.tbUserConfigured.Name = "tbUserConfigured"
        Me.tbUserConfigured.ReadOnly = True
        Me.tbUserConfigured.Size = New System.Drawing.Size(795, 20)
        Me.tbUserConfigured.TabIndex = 43
        '
        'tbLauncherXla
        '
        Me.tbLauncherXla.Location = New System.Drawing.Point(20, 33)
        Me.tbLauncherXla.Name = "tbLauncherXla"
        Me.tbLauncherXla.ReadOnly = True
        Me.tbLauncherXla.Size = New System.Drawing.Size(875, 20)
        Me.tbLauncherXla.TabIndex = 33
        '
        'lblUserConfigured
        '
        Me.lblUserConfigured.AutoSize = True
        Me.lblUserConfigured.Location = New System.Drawing.Point(7, 94)
        Me.lblUserConfigured.Name = "lblUserConfigured"
        Me.lblUserConfigured.Size = New System.Drawing.Size(128, 13)
        Me.lblUserConfigured.TabIndex = 42
        Me.lblUserConfigured.Text = "User-Configured Directory"
        '
        'tbPreConfigured
        '
        Me.tbPreConfigured.Location = New System.Drawing.Point(20, 71)
        Me.tbPreConfigured.Name = "tbPreConfigured"
        Me.tbPreConfigured.ReadOnly = True
        Me.tbPreConfigured.Size = New System.Drawing.Size(875, 20)
        Me.tbPreConfigured.TabIndex = 41
        '
        'grpGlobalAddins
        '
        Me.grpGlobalAddins.Location = New System.Drawing.Point(6, 257)
        Me.grpGlobalAddins.Name = "grpGlobalAddins"
        Me.grpGlobalAddins.Size = New System.Drawing.Size(901, 100)
        Me.grpGlobalAddins.TabIndex = 44
        Me.grpGlobalAddins.TabStop = False
        Me.grpGlobalAddins.Text = "Global Addins"
        '
        'tpEnvironments2
        '
        Me.tpEnvironments2.Controls.Add(Me.tcEnvironment)
        Me.tpEnvironments2.Controls.Add(Me.grpEnvironments)
        Me.tpEnvironments2.Location = New System.Drawing.Point(4, 22)
        Me.tpEnvironments2.Name = "tpEnvironments2"
        Me.tpEnvironments2.Padding = New System.Windows.Forms.Padding(3)
        Me.tpEnvironments2.Size = New System.Drawing.Size(913, 539)
        Me.tpEnvironments2.TabIndex = 6
        Me.tpEnvironments2.Text = "Environments"
        Me.tpEnvironments2.UseVisualStyleBackColor = True
        '
        'tcEnvironment
        '
        Me.tcEnvironment.Controls.Add(Me.TabPage1)
        Me.tcEnvironment.Controls.Add(Me.TabPage2)
        Me.tcEnvironment.Location = New System.Drawing.Point(201, 6)
        Me.tcEnvironment.Name = "tcEnvironment"
        Me.tcEnvironment.SelectedIndex = 0
        Me.tcEnvironment.Size = New System.Drawing.Size(702, 527)
        Me.tcEnvironment.TabIndex = 3
        '
        'TabPage1
        '
        Me.TabPage1.Controls.Add(Me.tcFramework)
        Me.TabPage1.Controls.Add(Me.lblFrameworks)
        Me.TabPage1.Controls.Add(Me.lstFrameworks)
        Me.TabPage1.Location = New System.Drawing.Point(4, 22)
        Me.TabPage1.Name = "TabPage1"
        Me.TabPage1.Padding = New System.Windows.Forms.Padding(3)
        Me.TabPage1.Size = New System.Drawing.Size(694, 501)
        Me.TabPage1.TabIndex = 0
        Me.TabPage1.Text = "Frameworks"
        Me.TabPage1.UseVisualStyleBackColor = True
        '
        'tcFramework
        '
        Me.tcFramework.Controls.Add(Me.TabPage3)
        Me.tcFramework.Controls.Add(Me.TabPage4)
        Me.tcFramework.Controls.Add(Me.TabPage5)
        Me.tcFramework.Location = New System.Drawing.Point(207, 10)
        Me.tcFramework.Name = "tcFramework"
        Me.tcFramework.SelectedIndex = 0
        Me.tcFramework.Size = New System.Drawing.Size(478, 485)
        Me.tcFramework.TabIndex = 2
        '
        'TabPage3
        '
        Me.TabPage3.Controls.Add(Me.pnlStartupParameters)
        Me.TabPage3.Location = New System.Drawing.Point(4, 22)
        Me.TabPage3.Name = "TabPage3"
        Me.TabPage3.Padding = New System.Windows.Forms.Padding(3)
        Me.TabPage3.Size = New System.Drawing.Size(470, 459)
        Me.TabPage3.TabIndex = 0
        Me.TabPage3.Text = "Startup Parameters"
        Me.TabPage3.UseVisualStyleBackColor = True
        '
        'pnlStartupParameters
        '
        Me.pnlStartupParameters.AutoScroll = True
        Me.pnlStartupParameters.Location = New System.Drawing.Point(6, 6)
        Me.pnlStartupParameters.Name = "pnlStartupParameters"
        Me.pnlStartupParameters.Size = New System.Drawing.Size(458, 447)
        Me.pnlStartupParameters.TabIndex = 4
        '
        'TabPage4
        '
        Me.TabPage4.Controls.Add(Me.tscComponents)
        Me.TabPage4.Controls.Add(Me.lbComponents)
        Me.TabPage4.Location = New System.Drawing.Point(4, 22)
        Me.TabPage4.Name = "TabPage4"
        Me.TabPage4.Padding = New System.Windows.Forms.Padding(3)
        Me.TabPage4.Size = New System.Drawing.Size(470, 459)
        Me.TabPage4.TabIndex = 1
        Me.TabPage4.Text = "Components"
        Me.TabPage4.UseVisualStyleBackColor = True
        '
        'tscComponents
        '
        '
        'tscComponents.ContentPanel
        '
        Me.tscComponents.ContentPanel.Controls.Add(Me.tstAddins)
        Me.tscComponents.ContentPanel.Size = New System.Drawing.Size(454, 3)
        Me.tscComponents.Location = New System.Drawing.Point(6, 425)
        Me.tscComponents.Name = "tscComponents"
        Me.tscComponents.Size = New System.Drawing.Size(454, 28)
        Me.tscComponents.TabIndex = 13
        Me.tscComponents.Text = "ToolStripContainer1"
        '
        'tstAddins
        '
        Me.tstAddins.BackColor = System.Drawing.SystemColors.Control
        Me.tstAddins.Dock = System.Windows.Forms.DockStyle.None
        Me.tstAddins.Enabled = False
        Me.tstAddins.GripStyle = System.Windows.Forms.ToolStripGripStyle.Hidden
        Me.tstAddins.Items.AddRange(New System.Windows.Forms.ToolStripItem() {Me.btnAddinInsert, Me.btnAddinDelete, Me.btnAddinRename, Me.btnAddinUp, Me.btnAddinDown})
        Me.tstAddins.Location = New System.Drawing.Point(0, 0)
        Me.tstAddins.Name = "tstAddins"
        Me.tstAddins.RenderMode = System.Windows.Forms.ToolStripRenderMode.System
        Me.tstAddins.Size = New System.Drawing.Size(118, 25)
        Me.tstAddins.TabIndex = 11
        Me.tstAddins.Text = "ToolStrip1"
        '
        'btnAddinInsert
        '
        Me.btnAddinInsert.DisplayStyle = System.Windows.Forms.ToolStripItemDisplayStyle.Image
        Me.btnAddinInsert.Image = Global.launcher.My.Resources.Resources.create
        Me.btnAddinInsert.ImageTransparentColor = System.Drawing.Color.Magenta
        Me.btnAddinInsert.Name = "btnAddinInsert"
        Me.btnAddinInsert.Size = New System.Drawing.Size(23, 22)
        Me.btnAddinInsert.Text = "Insert Addin"
        Me.btnAddinInsert.ToolTipText = "Insert Addin"
        '
        'btnAddinDelete
        '
        Me.btnAddinDelete.DisplayStyle = System.Windows.Forms.ToolStripItemDisplayStyle.Image
        Me.btnAddinDelete.Image = Global.launcher.My.Resources.Resources.delete
        Me.btnAddinDelete.ImageTransparentColor = System.Drawing.Color.Magenta
        Me.btnAddinDelete.Name = "btnAddinDelete"
        Me.btnAddinDelete.Size = New System.Drawing.Size(23, 22)
        Me.btnAddinDelete.Text = "Delete Selected Addin"
        '
        'btnAddinRename
        '
        Me.btnAddinRename.DisplayStyle = System.Windows.Forms.ToolStripItemDisplayStyle.Image
        Me.btnAddinRename.Image = CType(resources.GetObject("btnAddinRename.Image"), System.Drawing.Image)
        Me.btnAddinRename.ImageTransparentColor = System.Drawing.Color.Magenta
        Me.btnAddinRename.Name = "btnAddinRename"
        Me.btnAddinRename.Size = New System.Drawing.Size(23, 22)
        Me.btnAddinRename.Text = "Edit Selected Addin Name"
        '
        'btnAddinUp
        '
        Me.btnAddinUp.DisplayStyle = System.Windows.Forms.ToolStripItemDisplayStyle.Image
        Me.btnAddinUp.Image = Global.launcher.My.Resources.Resources.up
        Me.btnAddinUp.ImageTransparentColor = System.Drawing.Color.Magenta
        Me.btnAddinUp.Name = "btnAddinUp"
        Me.btnAddinUp.Size = New System.Drawing.Size(23, 22)
        Me.btnAddinUp.Text = "Move Up"
        '
        'btnAddinDown
        '
        Me.btnAddinDown.DisplayStyle = System.Windows.Forms.ToolStripItemDisplayStyle.Image
        Me.btnAddinDown.Image = Global.launcher.My.Resources.Resources.down
        Me.btnAddinDown.ImageTransparentColor = System.Drawing.Color.Magenta
        Me.btnAddinDown.Name = "btnAddinDown"
        Me.btnAddinDown.Size = New System.Drawing.Size(23, 22)
        Me.btnAddinDown.Text = "Move Down"
        '
        'lbComponents
        '
        Me.lbComponents.Enabled = False
        Me.lbComponents.FormattingEnabled = True
        Me.lbComponents.HorizontalScrollbar = True
        Me.lbComponents.Location = New System.Drawing.Point(6, 6)
        Me.lbComponents.Name = "lbComponents"
        Me.lbComponents.Size = New System.Drawing.Size(454, 407)
        Me.lbComponents.TabIndex = 12
        '
        'TabPage5
        '
        Me.TabPage5.Controls.Add(Me.lvVariables)
        Me.TabPage5.Location = New System.Drawing.Point(4, 22)
        Me.TabPage5.Name = "TabPage5"
        Me.TabPage5.Size = New System.Drawing.Size(470, 459)
        Me.TabPage5.TabIndex = 2
        Me.TabPage5.Text = "Variables"
        Me.TabPage5.UseVisualStyleBackColor = True
        '
        'lvVariables
        '
        Me.lvVariables.AutoArrange = False
        Me.lvVariables.CausesValidation = False
        Me.lvVariables.Columns.AddRange(New System.Windows.Forms.ColumnHeader() {Me.ColumnHeader1, Me.ColumnHeader2})
        Me.lvVariables.ContextMenuStrip = Me.cmVariables
        Me.lvVariables.FullRowSelect = True
        Me.lvVariables.HeaderStyle = System.Windows.Forms.ColumnHeaderStyle.Nonclickable
        Me.lvVariables.Items.AddRange(New System.Windows.Forms.ListViewItem() {ListViewItem1})
        Me.lvVariables.LabelWrap = False
        Me.lvVariables.Location = New System.Drawing.Point(3, 3)
        Me.lvVariables.MultiSelect = False
        Me.lvVariables.Name = "lvVariables"
        Me.lvVariables.Scrollable = False
        Me.lvVariables.Size = New System.Drawing.Size(464, 453)
        Me.lvVariables.Sorting = System.Windows.Forms.SortOrder.Ascending
        Me.lvVariables.TabIndex = 3
        Me.lvVariables.UseCompatibleStateImageBehavior = False
        Me.lvVariables.View = System.Windows.Forms.View.Details
        '
        'ColumnHeader1
        '
        Me.ColumnHeader1.Text = "Variable"
        Me.ColumnHeader1.Width = 150
        '
        'ColumnHeader2
        '
        Me.ColumnHeader2.Text = "Value"
        Me.ColumnHeader2.Width = 310
        '
        'lblFrameworks
        '
        Me.lblFrameworks.AutoSize = True
        Me.lblFrameworks.Location = New System.Drawing.Point(7, 19)
        Me.lblFrameworks.Name = "lblFrameworks"
        Me.lblFrameworks.Size = New System.Drawing.Size(64, 13)
        Me.lblFrameworks.TabIndex = 0
        Me.lblFrameworks.Text = "Frameworks"
        '
        'lstFrameworks
        '
        Me.lstFrameworks.FormattingEnabled = True
        Me.lstFrameworks.Location = New System.Drawing.Point(10, 35)
        Me.lstFrameworks.Name = "lstFrameworks"
        Me.lstFrameworks.Size = New System.Drawing.Size(191, 459)
        Me.lstFrameworks.TabIndex = 1
        '
        'TabPage2
        '
        Me.TabPage2.Controls.Add(Me.Label6)
        Me.TabPage2.Controls.Add(Me.tbSessionFilePath)
        Me.TabPage2.Controls.Add(Me.btnGenerateSessionFile)
        Me.TabPage2.Controls.Add(Me.rtSessionFileContents)
        Me.TabPage2.Controls.Add(Me.txtAuthenticationFile)
        Me.TabPage2.Controls.Add(Me.lblAuthenticationFile)
        Me.TabPage2.Location = New System.Drawing.Point(4, 22)
        Me.TabPage2.Name = "TabPage2"
        Me.TabPage2.Padding = New System.Windows.Forms.Padding(3)
        Me.TabPage2.Size = New System.Drawing.Size(694, 501)
        Me.TabPage2.TabIndex = 1
        Me.TabPage2.Text = "Settings"
        Me.TabPage2.UseVisualStyleBackColor = True
        '
        'Label6
        '
        Me.Label6.AutoSize = True
        Me.Label6.Location = New System.Drawing.Point(6, 78)
        Me.Label6.Name = "Label6"
        Me.Label6.Size = New System.Drawing.Size(188, 13)
        Me.Label6.TabIndex = 5
        Me.Label6.Text = "Session File (XL_LAUNCHER_PATH):"
        '
        'tbSessionFilePath
        '
        Me.tbSessionFilePath.Location = New System.Drawing.Point(9, 94)
        Me.tbSessionFilePath.Name = "tbSessionFilePath"
        Me.tbSessionFilePath.ReadOnly = True
        Me.tbSessionFilePath.Size = New System.Drawing.Size(679, 20)
        Me.tbSessionFilePath.TabIndex = 4
        '
        'btnGenerateSessionFile
        '
        Me.btnGenerateSessionFile.Enabled = False
        Me.btnGenerateSessionFile.Location = New System.Drawing.Point(9, 52)
        Me.btnGenerateSessionFile.Name = "btnGenerateSessionFile"
        Me.btnGenerateSessionFile.Size = New System.Drawing.Size(135, 23)
        Me.btnGenerateSessionFile.TabIndex = 3
        Me.btnGenerateSessionFile.Text = "Generate Session File"
        Me.btnGenerateSessionFile.UseVisualStyleBackColor = True
        '
        'rtSessionFileContents
        '
        Me.rtSessionFileContents.Location = New System.Drawing.Point(9, 120)
        Me.rtSessionFileContents.Name = "rtSessionFileContents"
        Me.rtSessionFileContents.ReadOnly = True
        Me.rtSessionFileContents.Size = New System.Drawing.Size(679, 375)
        Me.rtSessionFileContents.TabIndex = 2
        Me.rtSessionFileContents.Text = ""
        '
        'txtAuthenticationFile
        '
        Me.txtAuthenticationFile.Location = New System.Drawing.Point(9, 26)
        Me.txtAuthenticationFile.Name = "txtAuthenticationFile"
        Me.txtAuthenticationFile.ReadOnly = True
        Me.txtAuthenticationFile.Size = New System.Drawing.Size(679, 20)
        Me.txtAuthenticationFile.TabIndex = 1
        '
        'lblAuthenticationFile
        '
        Me.lblAuthenticationFile.AutoSize = True
        Me.lblAuthenticationFile.Location = New System.Drawing.Point(6, 10)
        Me.lblAuthenticationFile.Name = "lblAuthenticationFile"
        Me.lblAuthenticationFile.Size = New System.Drawing.Size(97, 13)
        Me.lblAuthenticationFile.TabIndex = 0
        Me.lblAuthenticationFile.Text = "Authentication File:"
        '
        'grpEnvironments
        '
        Me.grpEnvironments.Controls.Add(Me.tscEnvironments)
        Me.grpEnvironments.Controls.Add(Me.lstUserConfigured)
        Me.grpEnvironments.Controls.Add(Me.lblUserConfigured2)
        Me.grpEnvironments.Controls.Add(Me.lstPreconfigured)
        Me.grpEnvironments.Controls.Add(Me.btnLaunch)
        Me.grpEnvironments.Controls.Add(Me.lblPreconfigured2)
        Me.grpEnvironments.Location = New System.Drawing.Point(3, 6)
        Me.grpEnvironments.Name = "grpEnvironments"
        Me.grpEnvironments.Size = New System.Drawing.Size(192, 527)
        Me.grpEnvironments.TabIndex = 0
        Me.grpEnvironments.TabStop = False
        Me.grpEnvironments.Text = "Environments"
        '
        'tscEnvironments
        '
        '
        'tscEnvironments.ContentPanel
        '
        Me.tscEnvironments.ContentPanel.Controls.Add(Me.tstEnvironment)
        Me.tscEnvironments.ContentPanel.Size = New System.Drawing.Size(74, 0)
        Me.tscEnvironments.Location = New System.Drawing.Point(111, 496)
        Me.tscEnvironments.Name = "tscEnvironments"
        Me.tscEnvironments.Size = New System.Drawing.Size(74, 25)
        Me.tscEnvironments.TabIndex = 0
        Me.tscEnvironments.Text = "ToolStripContainer1"
        '
        'tstEnvironment
        '
        Me.tstEnvironment.Dock = System.Windows.Forms.DockStyle.None
        Me.tstEnvironment.GripStyle = System.Windows.Forms.ToolStripGripStyle.Hidden
        Me.tstEnvironment.Items.AddRange(New System.Windows.Forms.ToolStripItem() {Me.btnCopy, Me.btnRename, Me.btnDelete})
        Me.tstEnvironment.Location = New System.Drawing.Point(0, 0)
        Me.tstEnvironment.Name = "tstEnvironment"
        Me.tstEnvironment.RenderMode = System.Windows.Forms.ToolStripRenderMode.System
        Me.tstEnvironment.Size = New System.Drawing.Size(72, 25)
        Me.tstEnvironment.TabIndex = 4
        Me.tstEnvironment.Text = "ToolStrip1"
        '
        'btnCopy
        '
        Me.btnCopy.DisplayStyle = System.Windows.Forms.ToolStripItemDisplayStyle.Image
        Me.btnCopy.Image = Global.launcher.My.Resources.Resources.copy
        Me.btnCopy.ImageTransparentColor = System.Drawing.Color.Magenta
        Me.btnCopy.Name = "btnCopy"
        Me.btnCopy.Size = New System.Drawing.Size(23, 22)
        Me.btnCopy.Text = "Copy Environment"
        '
        'btnRename
        '
        Me.btnRename.DisplayStyle = System.Windows.Forms.ToolStripItemDisplayStyle.Image
        Me.btnRename.Image = CType(resources.GetObject("btnRename.Image"), System.Drawing.Image)
        Me.btnRename.ImageTransparentColor = System.Drawing.Color.Magenta
        Me.btnRename.Name = "btnRename"
        Me.btnRename.Size = New System.Drawing.Size(23, 22)
        Me.btnRename.Text = "Rename Environment"
        '
        'btnDelete
        '
        Me.btnDelete.DisplayStyle = System.Windows.Forms.ToolStripItemDisplayStyle.Image
        Me.btnDelete.Image = Global.launcher.My.Resources.Resources.delete
        Me.btnDelete.ImageTransparentColor = System.Drawing.Color.Magenta
        Me.btnDelete.Name = "btnDelete"
        Me.btnDelete.Size = New System.Drawing.Size(23, 22)
        Me.btnDelete.Text = "Delete Environment"
        '
        'lstUserConfigured
        '
        Me.lstUserConfigured.FormattingEnabled = True
        Me.lstUserConfigured.Location = New System.Drawing.Point(9, 278)
        Me.lstUserConfigured.Name = "lstUserConfigured"
        Me.lstUserConfigured.Size = New System.Drawing.Size(176, 212)
        Me.lstUserConfigured.TabIndex = 3
        '
        'lblUserConfigured2
        '
        Me.lblUserConfigured2.AutoSize = True
        Me.lblUserConfigured2.Location = New System.Drawing.Point(6, 262)
        Me.lblUserConfigured2.Name = "lblUserConfigured2"
        Me.lblUserConfigured2.Size = New System.Drawing.Size(83, 13)
        Me.lblUserConfigured2.TabIndex = 2
        Me.lblUserConfigured2.Text = "User-Configured"
        '
        'lstPreconfigured
        '
        Me.lstPreconfigured.FormattingEnabled = True
        Me.lstPreconfigured.Location = New System.Drawing.Point(9, 32)
        Me.lstPreconfigured.Name = "lstPreconfigured"
        Me.lstPreconfigured.Size = New System.Drawing.Size(176, 212)
        Me.lstPreconfigured.TabIndex = 1
        '
        'lblPreconfigured2
        '
        Me.lblPreconfigured2.AutoSize = True
        Me.lblPreconfigured2.Location = New System.Drawing.Point(6, 16)
        Me.lblPreconfigured2.Name = "lblPreconfigured2"
        Me.lblPreconfigured2.Size = New System.Drawing.Size(73, 13)
        Me.lblPreconfigured2.TabIndex = 0
        Me.lblPreconfigured2.Text = "Preconfigured"
        '
        'tcLauncher
        '
        Me.tcLauncher.Controls.Add(Me.tpEnvironments2)
        Me.tcLauncher.Controls.Add(Me.tpPaths)
        Me.tcLauncher.Controls.Add(Me.tpAbout)
        Me.tcLauncher.Location = New System.Drawing.Point(3, 3)
        Me.tcLauncher.Name = "tcLauncher"
        Me.tcLauncher.SelectedIndex = 0
        Me.tcLauncher.Size = New System.Drawing.Size(921, 565)
        Me.tcLauncher.TabIndex = 21
        '
        'chVariable
        '
        Me.chVariable.Text = "Variable"
        Me.chVariable.Width = 150
        '
        'chValue
        '
        Me.chValue.Text = "Value"
        Me.chValue.Width = 313
        '
        'FormMain
        '
        Me.AutoScaleDimensions = New System.Drawing.SizeF(6.0!, 13.0!)
        Me.AutoScaleMode = System.Windows.Forms.AutoScaleMode.Font
        Me.ClientSize = New System.Drawing.Size(934, 575)
        Me.Controls.Add(Me.tcLauncher)
        Me.FormBorderStyle = System.Windows.Forms.FormBorderStyle.FixedDialog
        Me.Icon = CType(resources.GetObject("$this.Icon"), System.Drawing.Icon)
        Me.Name = "FormMain"
        Me.Text = "XL-Launcher"
        Me.cmVariables.ResumeLayout(False)
        Me.tpAbout.ResumeLayout(False)
        Me.tpAbout.PerformLayout()
        Me.tpPaths.ResumeLayout(False)
        Me.grpLocalPaths.ResumeLayout(False)
        Me.grpLocalPaths.PerformLayout()
        Me.grpAppPaths.ResumeLayout(False)
        Me.grpAppPaths.PerformLayout()
        Me.tscConfiguration.ContentPanel.ResumeLayout(False)
        Me.tscConfiguration.ContentPanel.PerformLayout()
        Me.tscConfiguration.ResumeLayout(False)
        Me.tscConfiguration.PerformLayout()
        Me.tstConfiguration.ResumeLayout(False)
        Me.tstConfiguration.PerformLayout()
        Me.tpEnvironments2.ResumeLayout(False)
        Me.tcEnvironment.ResumeLayout(False)
        Me.TabPage1.ResumeLayout(False)
        Me.TabPage1.PerformLayout()
        Me.tcFramework.ResumeLayout(False)
        Me.TabPage3.ResumeLayout(False)
        Me.TabPage4.ResumeLayout(False)
        Me.tscComponents.ContentPanel.ResumeLayout(False)
        Me.tscComponents.ContentPanel.PerformLayout()
        Me.tscComponents.ResumeLayout(False)
        Me.tscComponents.PerformLayout()
        Me.tstAddins.ResumeLayout(False)
        Me.tstAddins.PerformLayout()
        Me.TabPage5.ResumeLayout(False)
        Me.TabPage2.ResumeLayout(False)
        Me.TabPage2.PerformLayout()
        Me.grpEnvironments.ResumeLayout(False)
        Me.grpEnvironments.PerformLayout()
        Me.tscEnvironments.ContentPanel.ResumeLayout(False)
        Me.tscEnvironments.ContentPanel.PerformLayout()
        Me.tscEnvironments.ResumeLayout(False)
        Me.tscEnvironments.PerformLayout()
        Me.tstEnvironment.ResumeLayout(False)
        Me.tstEnvironment.PerformLayout()
        Me.tcLauncher.ResumeLayout(False)
        Me.ResumeLayout(False)

    End Sub
    Friend WithEvents btnLaunch As System.Windows.Forms.Button
    Friend WithEvents cmVariables As System.Windows.Forms.ContextMenuStrip
    Friend WithEvents miVariableInsert As System.Windows.Forms.ToolStripMenuItem
    Friend WithEvents miVariableEdit As System.Windows.Forms.ToolStripMenuItem
    Friend WithEvents miVariableDelete As System.Windows.Forms.ToolStripMenuItem
    Friend WithEvents tpAbout As System.Windows.Forms.TabPage
    Friend WithEvents lblDomainValue As System.Windows.Forms.Label
    Friend WithEvents lblUserNameValue As System.Windows.Forms.Label
    Friend WithEvents lblVersionValue As System.Windows.Forms.Label
    Friend WithEvents lblDomain As System.Windows.Forms.Label
    Friend WithEvents lblUserName As System.Windows.Forms.Label
    Friend WithEvents lblVersion As System.Windows.Forms.Label
    Friend WithEvents tpPaths As System.Windows.Forms.TabPage
    Friend WithEvents tbLauncherXla As System.Windows.Forms.TextBox
    Friend WithEvents tbExcelPath As System.Windows.Forms.TextBox
    Friend WithEvents lblLauncherXla As System.Windows.Forms.Label
    Friend WithEvents lblExcelPath As System.Windows.Forms.Label
    Friend WithEvents tpEnvironments2 As System.Windows.Forms.TabPage
    Friend WithEvents tcEnvironment As System.Windows.Forms.TabControl
    Friend WithEvents TabPage1 As System.Windows.Forms.TabPage
    Friend WithEvents tcFramework As System.Windows.Forms.TabControl
    Friend WithEvents TabPage3 As System.Windows.Forms.TabPage
    Friend WithEvents pnlStartupParameters As System.Windows.Forms.Panel
    Friend WithEvents TabPage4 As System.Windows.Forms.TabPage
    Friend WithEvents lbComponents As System.Windows.Forms.ListBox
    Friend WithEvents tstAddins As System.Windows.Forms.ToolStrip
    Friend WithEvents btnAddinInsert As System.Windows.Forms.ToolStripButton
    Friend WithEvents btnAddinDelete As System.Windows.Forms.ToolStripButton
    Friend WithEvents btnAddinRename As System.Windows.Forms.ToolStripButton
    Friend WithEvents btnAddinUp As System.Windows.Forms.ToolStripButton
    Friend WithEvents btnAddinDown As System.Windows.Forms.ToolStripButton
    Friend WithEvents lblFrameworks As System.Windows.Forms.Label
    Friend WithEvents lstFrameworks As System.Windows.Forms.ListBox
    Friend WithEvents TabPage2 As System.Windows.Forms.TabPage
    Friend WithEvents txtAuthenticationFile As System.Windows.Forms.TextBox
    Friend WithEvents lblAuthenticationFile As System.Windows.Forms.Label
    Friend WithEvents grpEnvironments As System.Windows.Forms.GroupBox
    Friend WithEvents tstEnvironment As System.Windows.Forms.ToolStrip
    Friend WithEvents btnCopy As System.Windows.Forms.ToolStripButton
    Friend WithEvents btnRename As System.Windows.Forms.ToolStripButton
    Friend WithEvents btnDelete As System.Windows.Forms.ToolStripButton
    Friend WithEvents lstUserConfigured As System.Windows.Forms.ListBox
    Friend WithEvents lblUserConfigured2 As System.Windows.Forms.Label
    Friend WithEvents lstPreconfigured As System.Windows.Forms.ListBox
    Friend WithEvents lblPreconfigured2 As System.Windows.Forms.Label
    Friend WithEvents tcLauncher As System.Windows.Forms.TabControl
    Friend WithEvents chVariable As System.Windows.Forms.ColumnHeader
    Friend WithEvents chValue As System.Windows.Forms.ColumnHeader
    Friend WithEvents rtSessionFileContents As System.Windows.Forms.RichTextBox
    Friend WithEvents btnGenerateSessionFile As System.Windows.Forms.Button
    Friend WithEvents tbSessionFilePath As System.Windows.Forms.TextBox
    Friend WithEvents Label6 As System.Windows.Forms.Label
    Friend WithEvents lblPreConfigured As System.Windows.Forms.Label
    Friend WithEvents tbUserConfigured As System.Windows.Forms.TextBox
    Friend WithEvents lblUserConfigured As System.Windows.Forms.Label
    Friend WithEvents tbPreConfigured As System.Windows.Forms.TextBox
    Friend WithEvents grpGlobalAddins As System.Windows.Forms.GroupBox
    Friend WithEvents tscComponents As System.Windows.Forms.ToolStripContainer
    Friend WithEvents grpLocalPaths As System.Windows.Forms.GroupBox
    Friend WithEvents grpAppPaths As System.Windows.Forms.GroupBox
    Friend WithEvents TabPage5 As System.Windows.Forms.TabPage
    Friend WithEvents lvVariables As System.Windows.Forms.ListView
    Friend WithEvents ColumnHeader1 As System.Windows.Forms.ColumnHeader
    Friend WithEvents ColumnHeader2 As System.Windows.Forms.ColumnHeader
    Friend WithEvents btnLaunchExcel As System.Windows.Forms.Button
    Friend WithEvents btnExcelPath As System.Windows.Forms.Button
    Friend WithEvents lblMachine As System.Windows.Forms.Label
    Friend WithEvents lblMachineValue As System.Windows.Forms.Label
    Friend WithEvents tscConfiguration As System.Windows.Forms.ToolStripContainer
    Friend WithEvents tstConfiguration As System.Windows.Forms.ToolStrip
    Friend WithEvents btnSaveConfiguration As System.Windows.Forms.ToolStripButton
    Friend WithEvents btnReloadConfiguration As System.Windows.Forms.ToolStripButton
    Friend WithEvents btnDeleteConfiguration As System.Windows.Forms.ToolStripButton
    Friend WithEvents rtHelp As System.Windows.Forms.RichTextBox
    Friend WithEvents tscEnvironments As System.Windows.Forms.ToolStripContainer
    Friend WithEvents lblSessionFileDir As System.Windows.Forms.Label
    Friend WithEvents tbSessionFileDir As System.Windows.Forms.TextBox
End Class
