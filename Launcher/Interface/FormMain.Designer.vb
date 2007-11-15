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
        Dim ListViewItem1 As System.Windows.Forms.ListViewItem = New System.Windows.Forms.ListViewItem(New String() {"NAZCATECH_DIR", "X:\Apps\Appsscript\CabotoXL\SensitivityAnalysis"}, -1)
        Dim resources As System.ComponentModel.ComponentResourceManager = New System.ComponentModel.ComponentResourceManager(GetType(FormMain))
        Me.btnClose = New System.Windows.Forms.Button
        Me.lblPreconfigured = New System.Windows.Forms.Label
        Me.lblFramework = New System.Windows.Forms.Label
        Me.txtFramework = New System.Windows.Forms.TextBox
        Me.btnFrameworkSelect = New System.Windows.Forms.Button
        Me.btnWorkbooks = New System.Windows.Forms.Button
        Me.txtWorkbooks = New System.Windows.Forms.TextBox
        Me.lblWorkbooks = New System.Windows.Forms.Label
        Me.grpEnvironment = New System.Windows.Forms.GroupBox
        Me.tstEnvironment = New System.Windows.Forms.ToolStrip
        Me.btnNew = New System.Windows.Forms.ToolStripButton
        Me.btnCopy = New System.Windows.Forms.ToolStripButton
        Me.btnDelete = New System.Windows.Forms.ToolStripButton
        Me.btnClear = New System.Windows.Forms.ToolStripButton
        Me.btnRename = New System.Windows.Forms.ToolStripButton
        Me.lstUserconfigured = New System.Windows.Forms.ListBox
        Me.lblUserConfigured = New System.Windows.Forms.Label
        Me.lstPreconfigured = New System.Windows.Forms.ListBox
        Me.grpStartup = New System.Windows.Forms.GroupBox
        Me.dtEvaluationDate = New System.Windows.Forms.DateTimePicker
        Me.cbSetEvaluationDate = New System.Windows.Forms.CheckBox
        Me.cbCalibrateCms = New System.Windows.Forms.CheckBox
        Me.cbStaticData = New System.Windows.Forms.CheckBox
        Me.cbFitCMS = New System.Windows.Forms.CheckBox
        Me.cbLoadBonds = New System.Windows.Forms.CheckBox
        Me.cbMainChecks = New System.Windows.Forms.CheckBox
        Me.cbSwapSmileBootstrap = New System.Windows.Forms.CheckBox
        Me.cbIndexesTimeSeries = New System.Windows.Forms.CheckBox
        Me.cbCapVolBootstrap = New System.Windows.Forms.CheckBox
        Me.cbYCBootstrap = New System.Windows.Forms.CheckBox
        Me.btnLaunch = New System.Windows.Forms.Button
        Me.tcLauncher = New System.Windows.Forms.TabControl
        Me.tpEnvironments = New System.Windows.Forms.TabPage
        Me.grpInit = New System.Windows.Forms.GroupBox
        Me.rbXML = New System.Windows.Forms.RadioButton
        Me.rbExcel = New System.Windows.Forms.RadioButton
        Me.grpFeeds = New System.Windows.Forms.GroupBox
        Me.rbBloomberg = New System.Windows.Forms.RadioButton
        Me.rbReuters = New System.Windows.Forms.RadioButton
        Me.tpPaths = New System.Windows.Forms.TabPage
        Me.cbFrameworkVersion = New System.Windows.Forms.ComboBox
        Me.btnLaunchExcel = New System.Windows.Forms.Button
        Me.txtExcelPath = New System.Windows.Forms.TextBox
        Me.lblExcelPath = New System.Windows.Forms.Label
        Me.btnExcelPath = New System.Windows.Forms.Button
        Me.lblFrameworkVersion = New System.Windows.Forms.Label
        Me.txtUserConfig = New System.Windows.Forms.TextBox
        Me.lblUserConfig = New System.Windows.Forms.Label
        Me.btnUserConfig = New System.Windows.Forms.Button
        Me.txtXmlPath = New System.Windows.Forms.TextBox
        Me.lblXmlPath = New System.Windows.Forms.Label
        Me.btnXmlPath = New System.Windows.Forms.Button
        Me.txtHelpPath = New System.Windows.Forms.TextBox
        Me.lblHelpPath = New System.Windows.Forms.Label
        Me.btnHelpFile = New System.Windows.Forms.Button
        Me.tpAddins = New System.Windows.Forms.TabPage
        Me.lbAddins = New System.Windows.Forms.ListBox
        Me.tstAddins = New System.Windows.Forms.ToolStrip
        Me.btnAddinInsert = New System.Windows.Forms.ToolStripButton
        Me.btnAddinDelete = New System.Windows.Forms.ToolStripButton
        Me.btnAddinRename = New System.Windows.Forms.ToolStripButton
        Me.btnAddinUp = New System.Windows.Forms.ToolStripButton
        Me.btnAddinDown = New System.Windows.Forms.ToolStripButton
        Me.tpFeeds = New System.Windows.Forms.TabPage
        Me.cbBloomberg = New System.Windows.Forms.CheckBox
        Me.cbReuters = New System.Windows.Forms.CheckBox
        Me.txtBloomberg = New System.Windows.Forms.TextBox
        Me.lblBloomberg = New System.Windows.Forms.Label
        Me.btnBloomberg = New System.Windows.Forms.Button
        Me.btnReuters = New System.Windows.Forms.Button
        Me.txtReuters = New System.Windows.Forms.TextBox
        Me.lblReuters = New System.Windows.Forms.Label
        Me.tpVariables = New System.Windows.Forms.TabPage
        Me.lvVariables = New System.Windows.Forms.ListView
        Me.chVariable = New System.Windows.Forms.ColumnHeader
        Me.chValue = New System.Windows.Forms.ColumnHeader
        Me.cmVariables = New System.Windows.Forms.ContextMenuStrip(Me.components)
        Me.miVariableInsert = New System.Windows.Forms.ToolStripMenuItem
        Me.miVariableEdit = New System.Windows.Forms.ToolStripMenuItem
        Me.miVariableDelete = New System.Windows.Forms.ToolStripMenuItem
        Me.tpAbout = New System.Windows.Forms.TabPage
        Me.lblHardDiskValue = New System.Windows.Forms.Label
        Me.lblDomainValue = New System.Windows.Forms.Label
        Me.lblUserNameValue = New System.Windows.Forms.Label
        Me.lblVersionValue = New System.Windows.Forms.Label
        Me.lblHardDisk = New System.Windows.Forms.Label
        Me.lblDomain = New System.Windows.Forms.Label
        Me.lblUserName = New System.Windows.Forms.Label
        Me.lblVersion = New System.Windows.Forms.Label
        Me.lblBuildNumber = New System.Windows.Forms.Label
        Me.grpEnvironment.SuspendLayout()
        Me.tstEnvironment.SuspendLayout()
        Me.grpStartup.SuspendLayout()
        Me.tcLauncher.SuspendLayout()
        Me.tpEnvironments.SuspendLayout()
        Me.grpInit.SuspendLayout()
        Me.grpFeeds.SuspendLayout()
        Me.tpPaths.SuspendLayout()
        Me.tpAddins.SuspendLayout()
        Me.tstAddins.SuspendLayout()
        Me.tpFeeds.SuspendLayout()
        Me.tpVariables.SuspendLayout()
        Me.cmVariables.SuspendLayout()
        Me.tpAbout.SuspendLayout()
        Me.SuspendLayout()
        '
        'btnClose
        '
        Me.btnClose.Location = New System.Drawing.Point(251, 412)
        Me.btnClose.Name = "btnClose"
        Me.btnClose.Size = New System.Drawing.Size(75, 23)
        Me.btnClose.TabIndex = 1
        Me.btnClose.Text = "Close"
        Me.btnClose.UseVisualStyleBackColor = True
        '
        'lblPreconfigured
        '
        Me.lblPreconfigured.AutoSize = True
        Me.lblPreconfigured.Location = New System.Drawing.Point(6, 22)
        Me.lblPreconfigured.Name = "lblPreconfigured"
        Me.lblPreconfigured.Size = New System.Drawing.Size(73, 13)
        Me.lblPreconfigured.TabIndex = 3
        Me.lblPreconfigured.Text = "Preconfigured"
        '
        'lblFramework
        '
        Me.lblFramework.AutoSize = True
        Me.lblFramework.Location = New System.Drawing.Point(10, 10)
        Me.lblFramework.Name = "lblFramework"
        Me.lblFramework.Size = New System.Drawing.Size(59, 13)
        Me.lblFramework.TabIndex = 4
        Me.lblFramework.Text = "Framework"
        '
        'txtFramework
        '
        Me.txtFramework.Location = New System.Drawing.Point(30, 26)
        Me.txtFramework.Name = "txtFramework"
        Me.txtFramework.Size = New System.Drawing.Size(405, 20)
        Me.txtFramework.TabIndex = 5
        '
        'btnFrameworkSelect
        '
        Me.btnFrameworkSelect.Location = New System.Drawing.Point(441, 26)
        Me.btnFrameworkSelect.Name = "btnFrameworkSelect"
        Me.btnFrameworkSelect.Size = New System.Drawing.Size(32, 23)
        Me.btnFrameworkSelect.TabIndex = 11
        Me.btnFrameworkSelect.Text = "..."
        Me.btnFrameworkSelect.UseVisualStyleBackColor = True
        '
        'btnWorkbooks
        '
        Me.btnWorkbooks.Location = New System.Drawing.Point(441, 104)
        Me.btnWorkbooks.Name = "btnWorkbooks"
        Me.btnWorkbooks.Size = New System.Drawing.Size(32, 23)
        Me.btnWorkbooks.TabIndex = 15
        Me.btnWorkbooks.Text = "..."
        Me.btnWorkbooks.UseVisualStyleBackColor = True
        '
        'txtWorkbooks
        '
        Me.txtWorkbooks.Location = New System.Drawing.Point(30, 104)
        Me.txtWorkbooks.Name = "txtWorkbooks"
        Me.txtWorkbooks.Size = New System.Drawing.Size(405, 20)
        Me.txtWorkbooks.TabIndex = 14
        '
        'lblWorkbooks
        '
        Me.lblWorkbooks.AutoSize = True
        Me.lblWorkbooks.Location = New System.Drawing.Point(10, 88)
        Me.lblWorkbooks.Name = "lblWorkbooks"
        Me.lblWorkbooks.Size = New System.Drawing.Size(62, 13)
        Me.lblWorkbooks.TabIndex = 13
        Me.lblWorkbooks.Text = "Workbooks"
        '
        'grpEnvironment
        '
        Me.grpEnvironment.Controls.Add(Me.tstEnvironment)
        Me.grpEnvironment.Controls.Add(Me.lstUserconfigured)
        Me.grpEnvironment.Controls.Add(Me.lblUserConfigured)
        Me.grpEnvironment.Controls.Add(Me.lstPreconfigured)
        Me.grpEnvironment.Controls.Add(Me.lblPreconfigured)
        Me.grpEnvironment.Location = New System.Drawing.Point(6, 7)
        Me.grpEnvironment.Name = "grpEnvironment"
        Me.grpEnvironment.Size = New System.Drawing.Size(230, 355)
        Me.grpEnvironment.TabIndex = 16
        Me.grpEnvironment.TabStop = False
        Me.grpEnvironment.Text = "Environments"
        '
        'tstEnvironment
        '
        Me.tstEnvironment.BackColor = System.Drawing.SystemColors.Control
        Me.tstEnvironment.Dock = System.Windows.Forms.DockStyle.None
        Me.tstEnvironment.GripStyle = System.Windows.Forms.ToolStripGripStyle.Hidden
        Me.tstEnvironment.Items.AddRange(New System.Windows.Forms.ToolStripItem() {Me.btnNew, Me.btnCopy, Me.btnDelete, Me.btnClear, Me.btnRename})
        Me.tstEnvironment.Location = New System.Drawing.Point(19, 309)
        Me.tstEnvironment.Name = "tstEnvironment"
        Me.tstEnvironment.RenderMode = System.Windows.Forms.ToolStripRenderMode.System
        Me.tstEnvironment.Size = New System.Drawing.Size(118, 25)
        Me.tstEnvironment.TabIndex = 7
        Me.tstEnvironment.Text = "ToolStrip1"
        '
        'btnNew
        '
        Me.btnNew.DisplayStyle = System.Windows.Forms.ToolStripItemDisplayStyle.Image
        Me.btnNew.Image = Global.launcher.My.Resources.Resources.create
        Me.btnNew.ImageTransparentColor = System.Drawing.Color.Magenta
        Me.btnNew.Name = "btnNew"
        Me.btnNew.Size = New System.Drawing.Size(23, 22)
        Me.btnNew.Text = "New"
        '
        'btnCopy
        '
        Me.btnCopy.DisplayStyle = System.Windows.Forms.ToolStripItemDisplayStyle.Image
        Me.btnCopy.Image = Global.launcher.My.Resources.Resources.copy
        Me.btnCopy.ImageTransparentColor = System.Drawing.Color.Magenta
        Me.btnCopy.Name = "btnCopy"
        Me.btnCopy.Size = New System.Drawing.Size(23, 22)
        Me.btnCopy.Text = "Copy"
        '
        'btnDelete
        '
        Me.btnDelete.DisplayStyle = System.Windows.Forms.ToolStripItemDisplayStyle.Image
        Me.btnDelete.Image = Global.launcher.My.Resources.Resources.delete
        Me.btnDelete.ImageTransparentColor = System.Drawing.Color.Magenta
        Me.btnDelete.Name = "btnDelete"
        Me.btnDelete.Size = New System.Drawing.Size(23, 22)
        Me.btnDelete.Text = "Delete"
        '
        'btnClear
        '
        Me.btnClear.DisplayStyle = System.Windows.Forms.ToolStripItemDisplayStyle.Image
        Me.btnClear.Image = Global.launcher.My.Resources.Resources.clear
        Me.btnClear.ImageTransparentColor = System.Drawing.Color.Magenta
        Me.btnClear.Name = "btnClear"
        Me.btnClear.Size = New System.Drawing.Size(23, 22)
        Me.btnClear.Text = "Clear"
        '
        'btnRename
        '
        Me.btnRename.DisplayStyle = System.Windows.Forms.ToolStripItemDisplayStyle.Image
        Me.btnRename.Image = Global.launcher.My.Resources.Resources.rename
        Me.btnRename.ImageTransparentColor = System.Drawing.Color.Magenta
        Me.btnRename.Name = "btnRename"
        Me.btnRename.Size = New System.Drawing.Size(23, 22)
        Me.btnRename.Text = "Rename"
        '
        'lstUserconfigured
        '
        Me.lstUserconfigured.FormattingEnabled = True
        Me.lstUserconfigured.Location = New System.Drawing.Point(19, 167)
        Me.lstUserconfigured.Name = "lstUserconfigured"
        Me.lstUserconfigured.Size = New System.Drawing.Size(194, 134)
        Me.lstUserconfigured.Sorted = True
        Me.lstUserconfigured.TabIndex = 6
        '
        'lblUserConfigured
        '
        Me.lblUserConfigured.AutoSize = True
        Me.lblUserConfigured.Location = New System.Drawing.Point(6, 151)
        Me.lblUserConfigured.Name = "lblUserConfigured"
        Me.lblUserConfigured.Size = New System.Drawing.Size(83, 13)
        Me.lblUserConfigured.TabIndex = 5
        Me.lblUserConfigured.Text = "User Configured"
        '
        'lstPreconfigured
        '
        Me.lstPreconfigured.FormattingEnabled = True
        Me.lstPreconfigured.Location = New System.Drawing.Point(19, 38)
        Me.lstPreconfigured.Name = "lstPreconfigured"
        Me.lstPreconfigured.Size = New System.Drawing.Size(194, 95)
        Me.lstPreconfigured.Sorted = True
        Me.lstPreconfigured.TabIndex = 4
        '
        'grpStartup
        '
        Me.grpStartup.Controls.Add(Me.dtEvaluationDate)
        Me.grpStartup.Controls.Add(Me.cbSetEvaluationDate)
        Me.grpStartup.Controls.Add(Me.cbCalibrateCms)
        Me.grpStartup.Controls.Add(Me.cbStaticData)
        Me.grpStartup.Controls.Add(Me.cbFitCMS)
        Me.grpStartup.Controls.Add(Me.cbLoadBonds)
        Me.grpStartup.Controls.Add(Me.cbMainChecks)
        Me.grpStartup.Controls.Add(Me.cbSwapSmileBootstrap)
        Me.grpStartup.Controls.Add(Me.cbIndexesTimeSeries)
        Me.grpStartup.Controls.Add(Me.cbCapVolBootstrap)
        Me.grpStartup.Controls.Add(Me.cbYCBootstrap)
        Me.grpStartup.Location = New System.Drawing.Point(244, 6)
        Me.grpStartup.Name = "grpStartup"
        Me.grpStartup.Size = New System.Drawing.Size(230, 282)
        Me.grpStartup.TabIndex = 19
        Me.grpStartup.TabStop = False
        Me.grpStartup.Text = "Startup Actions"
        '
        'dtEvaluationDate
        '
        Me.dtEvaluationDate.CustomFormat = "dd-MMM-yyyy"
        Me.dtEvaluationDate.Format = System.Windows.Forms.DateTimePickerFormat.Custom
        Me.dtEvaluationDate.Location = New System.Drawing.Point(135, 26)
        Me.dtEvaluationDate.Name = "dtEvaluationDate"
        Me.dtEvaluationDate.Size = New System.Drawing.Size(90, 20)
        Me.dtEvaluationDate.TabIndex = 12
        '
        'cbSetEvaluationDate
        '
        Me.cbSetEvaluationDate.AutoSize = True
        Me.cbSetEvaluationDate.Location = New System.Drawing.Point(10, 29)
        Me.cbSetEvaluationDate.Name = "cbSetEvaluationDate"
        Me.cbSetEvaluationDate.Size = New System.Drawing.Size(121, 17)
        Me.cbSetEvaluationDate.TabIndex = 11
        Me.cbSetEvaluationDate.Text = "Set Evaluation Date"
        Me.cbSetEvaluationDate.UseVisualStyleBackColor = True
        '
        'cbCalibrateCms
        '
        Me.cbCalibrateCms.AutoSize = True
        Me.cbCalibrateCms.Location = New System.Drawing.Point(10, 121)
        Me.cbCalibrateCms.Name = "cbCalibrateCms"
        Me.cbCalibrateCms.Size = New System.Drawing.Size(129, 17)
        Me.cbCalibrateCms.TabIndex = 9
        Me.cbCalibrateCms.Text = "Calibrate CMS Market"
        Me.cbCalibrateCms.UseVisualStyleBackColor = True
        '
        'cbStaticData
        '
        Me.cbStaticData.AutoSize = True
        Me.cbStaticData.Location = New System.Drawing.Point(10, 236)
        Me.cbStaticData.Name = "cbStaticData"
        Me.cbStaticData.Size = New System.Drawing.Size(157, 17)
        Me.cbStaticData.TabIndex = 8
        Me.cbStaticData.Text = "Switch to static market data"
        Me.cbStaticData.UseVisualStyleBackColor = True
        '
        'cbFitCMS
        '
        Me.cbFitCMS.AutoSize = True
        Me.cbFitCMS.Location = New System.Drawing.Point(10, 144)
        Me.cbFitCMS.Name = "cbFitCMS"
        Me.cbFitCMS.Size = New System.Drawing.Size(162, 17)
        Me.cbFitCMS.TabIndex = 7
        Me.cbFitCMS.Text = "View CMS Market Current Fit"
        Me.cbFitCMS.UseVisualStyleBackColor = True
        '
        'cbLoadBonds
        '
        Me.cbLoadBonds.AutoSize = True
        Me.cbLoadBonds.Location = New System.Drawing.Point(10, 190)
        Me.cbLoadBonds.Name = "cbLoadBonds"
        Me.cbLoadBonds.Size = New System.Drawing.Size(83, 17)
        Me.cbLoadBonds.TabIndex = 6
        Me.cbLoadBonds.Text = "Load Bonds"
        Me.cbLoadBonds.UseVisualStyleBackColor = True
        '
        'cbMainChecks
        '
        Me.cbMainChecks.AutoSize = True
        Me.cbMainChecks.Location = New System.Drawing.Point(10, 213)
        Me.cbMainChecks.Name = "cbMainChecks"
        Me.cbMainChecks.Size = New System.Drawing.Size(117, 17)
        Me.cbMainChecks.TabIndex = 6
        Me.cbMainChecks.Text = "Open Main Checks"
        Me.cbMainChecks.UseVisualStyleBackColor = True
        '
        'cbSwapSmileBootstrap
        '
        Me.cbSwapSmileBootstrap.AutoSize = True
        Me.cbSwapSmileBootstrap.Location = New System.Drawing.Point(10, 98)
        Me.cbSwapSmileBootstrap.Name = "cbSwapSmileBootstrap"
        Me.cbSwapSmileBootstrap.Size = New System.Drawing.Size(173, 17)
        Me.cbSwapSmileBootstrap.TabIndex = 5
        Me.cbSwapSmileBootstrap.Text = "Create Swaption Volatility Cube"
        Me.cbSwapSmileBootstrap.UseVisualStyleBackColor = True
        '
        'cbIndexesTimeSeries
        '
        Me.cbIndexesTimeSeries.AutoSize = True
        Me.cbIndexesTimeSeries.Location = New System.Drawing.Point(10, 167)
        Me.cbIndexesTimeSeries.Name = "cbIndexesTimeSeries"
        Me.cbIndexesTimeSeries.Size = New System.Drawing.Size(148, 17)
        Me.cbIndexesTimeSeries.TabIndex = 4
        Me.cbIndexesTimeSeries.Text = "Load Indexes Time Series"
        Me.cbIndexesTimeSeries.UseVisualStyleBackColor = True
        '
        'cbCapVolBootstrap
        '
        Me.cbCapVolBootstrap.AutoSize = True
        Me.cbCapVolBootstrap.Location = New System.Drawing.Point(10, 75)
        Me.cbCapVolBootstrap.Name = "cbCapVolBootstrap"
        Me.cbCapVolBootstrap.Size = New System.Drawing.Size(142, 17)
        Me.cbCapVolBootstrap.TabIndex = 2
        Me.cbCapVolBootstrap.Text = "Bootstrap Cap Volatilities"
        Me.cbCapVolBootstrap.UseVisualStyleBackColor = True
        '
        'cbYCBootstrap
        '
        Me.cbYCBootstrap.AutoSize = True
        Me.cbYCBootstrap.Location = New System.Drawing.Point(10, 52)
        Me.cbYCBootstrap.Name = "cbYCBootstrap"
        Me.cbYCBootstrap.Size = New System.Drawing.Size(174, 17)
        Me.cbYCBootstrap.TabIndex = 0
        Me.cbYCBootstrap.Text = "Bootstrap Yield Curves"
        Me.cbYCBootstrap.UseVisualStyleBackColor = True
        '
        'btnLaunch
        '
        Me.btnLaunch.Location = New System.Drawing.Point(168, 412)
        Me.btnLaunch.Name = "btnLaunch"
        Me.btnLaunch.Size = New System.Drawing.Size(75, 23)
        Me.btnLaunch.TabIndex = 20
        Me.btnLaunch.Text = "Launch"
        Me.btnLaunch.UseVisualStyleBackColor = True
        '
        'tcLauncher
        '
        Me.tcLauncher.Controls.Add(Me.tpEnvironments)
        Me.tcLauncher.Controls.Add(Me.tpPaths)
        Me.tcLauncher.Controls.Add(Me.tpAddins)
        Me.tcLauncher.Controls.Add(Me.tpFeeds)
        Me.tcLauncher.Controls.Add(Me.tpVariables)
        Me.tcLauncher.Controls.Add(Me.tpAbout)
        Me.tcLauncher.Location = New System.Drawing.Point(3, 3)
        Me.tcLauncher.Name = "tcLauncher"
        Me.tcLauncher.SelectedIndex = 0
        Me.tcLauncher.Size = New System.Drawing.Size(487, 403)
        Me.tcLauncher.TabIndex = 21
        '
        'tpEnvironments
        '
        Me.tpEnvironments.Controls.Add(Me.grpInit)
        Me.tpEnvironments.Controls.Add(Me.grpFeeds)
        Me.tpEnvironments.Controls.Add(Me.grpEnvironment)
        Me.tpEnvironments.Controls.Add(Me.grpStartup)
        Me.tpEnvironments.Location = New System.Drawing.Point(4, 22)
        Me.tpEnvironments.Name = "tpEnvironments"
        Me.tpEnvironments.Padding = New System.Windows.Forms.Padding(3)
        Me.tpEnvironments.Size = New System.Drawing.Size(479, 377)
        Me.tpEnvironments.TabIndex = 0
        Me.tpEnvironments.Text = "Environments"
        Me.tpEnvironments.UseVisualStyleBackColor = True
        '
        'grpInit
        '
        Me.grpInit.Controls.Add(Me.rbXML)
        Me.grpInit.Controls.Add(Me.rbExcel)
        Me.grpInit.Location = New System.Drawing.Point(362, 293)
        Me.grpInit.Name = "grpInit"
        Me.grpInit.Size = New System.Drawing.Size(112, 69)
        Me.grpInit.TabIndex = 21
        Me.grpInit.TabStop = False
        Me.grpInit.Text = "Initialization Data"
        '
        'rbXML
        '
        Me.rbXML.AutoSize = True
        Me.rbXML.Location = New System.Drawing.Point(10, 45)
        Me.rbXML.Name = "rbXML"
        Me.rbXML.Size = New System.Drawing.Size(71, 17)
        Me.rbXML.TabIndex = 1
        Me.rbXML.TabStop = True
        Me.rbXML.Text = "XML Files"
        Me.rbXML.UseVisualStyleBackColor = True
        '
        'rbExcel
        '
        Me.rbExcel.AutoSize = True
        Me.rbExcel.Location = New System.Drawing.Point(10, 23)
        Me.rbExcel.Name = "rbExcel"
        Me.rbExcel.Size = New System.Drawing.Size(84, 17)
        Me.rbExcel.TabIndex = 0
        Me.rbExcel.TabStop = True
        Me.rbExcel.Text = "Excel Books"
        Me.rbExcel.UseVisualStyleBackColor = True
        '
        'grpFeeds
        '
        Me.grpFeeds.Controls.Add(Me.rbBloomberg)
        Me.grpFeeds.Controls.Add(Me.rbReuters)
        Me.grpFeeds.Location = New System.Drawing.Point(244, 293)
        Me.grpFeeds.Name = "grpFeeds"
        Me.grpFeeds.Size = New System.Drawing.Size(112, 69)
        Me.grpFeeds.TabIndex = 20
        Me.grpFeeds.TabStop = False
        Me.grpFeeds.Text = "Feed"
        '
        'rbBloomberg
        '
        Me.rbBloomberg.AutoSize = True
        Me.rbBloomberg.Location = New System.Drawing.Point(10, 45)
        Me.rbBloomberg.Name = "rbBloomberg"
        Me.rbBloomberg.Size = New System.Drawing.Size(75, 17)
        Me.rbBloomberg.TabIndex = 1
        Me.rbBloomberg.TabStop = True
        Me.rbBloomberg.Text = "Bloomberg"
        Me.rbBloomberg.UseVisualStyleBackColor = True
        '
        'rbReuters
        '
        Me.rbReuters.AutoSize = True
        Me.rbReuters.Location = New System.Drawing.Point(10, 23)
        Me.rbReuters.Name = "rbReuters"
        Me.rbReuters.Size = New System.Drawing.Size(62, 17)
        Me.rbReuters.TabIndex = 0
        Me.rbReuters.TabStop = True
        Me.rbReuters.Text = "Reuters"
        Me.rbReuters.UseVisualStyleBackColor = True
        '
        'tpPaths
        '
        Me.tpPaths.Controls.Add(Me.cbFrameworkVersion)
        Me.tpPaths.Controls.Add(Me.btnLaunchExcel)
        Me.tpPaths.Controls.Add(Me.txtExcelPath)
        Me.tpPaths.Controls.Add(Me.lblExcelPath)
        Me.tpPaths.Controls.Add(Me.btnExcelPath)
        Me.tpPaths.Controls.Add(Me.lblFrameworkVersion)
        Me.tpPaths.Controls.Add(Me.txtUserConfig)
        Me.tpPaths.Controls.Add(Me.lblUserConfig)
        Me.tpPaths.Controls.Add(Me.btnUserConfig)
        Me.tpPaths.Controls.Add(Me.txtXmlPath)
        Me.tpPaths.Controls.Add(Me.lblXmlPath)
        Me.tpPaths.Controls.Add(Me.btnXmlPath)
        Me.tpPaths.Controls.Add(Me.txtHelpPath)
        Me.tpPaths.Controls.Add(Me.lblHelpPath)
        Me.tpPaths.Controls.Add(Me.btnHelpFile)
        Me.tpPaths.Controls.Add(Me.txtFramework)
        Me.tpPaths.Controls.Add(Me.lblFramework)
        Me.tpPaths.Controls.Add(Me.btnWorkbooks)
        Me.tpPaths.Controls.Add(Me.btnFrameworkSelect)
        Me.tpPaths.Controls.Add(Me.txtWorkbooks)
        Me.tpPaths.Controls.Add(Me.lblWorkbooks)
        Me.tpPaths.Location = New System.Drawing.Point(4, 22)
        Me.tpPaths.Name = "tpPaths"
        Me.tpPaths.Padding = New System.Windows.Forms.Padding(3)
        Me.tpPaths.Size = New System.Drawing.Size(479, 377)
        Me.tpPaths.TabIndex = 1
        Me.tpPaths.Text = "Paths"
        Me.tpPaths.UseVisualStyleBackColor = True
        '
        'cbFrameworkVersion
        '
        Me.cbFrameworkVersion.DropDownStyle = System.Windows.Forms.ComboBoxStyle.DropDownList
        Me.cbFrameworkVersion.FormattingEnabled = True
        Me.cbFrameworkVersion.Items.AddRange(New Object() {"8", "9", "10"})
        Me.cbFrameworkVersion.Location = New System.Drawing.Point(161, 59)
        Me.cbFrameworkVersion.Name = "cbFrameworkVersion"
        Me.cbFrameworkVersion.Size = New System.Drawing.Size(47, 21)
        Me.cbFrameworkVersion.TabIndex = 29
        '
        'btnLaunchExcel
        '
        Me.btnLaunchExcel.Image = Global.launcher.My.Resources.Resources.excel
        Me.btnLaunchExcel.Location = New System.Drawing.Point(403, 260)
        Me.btnLaunchExcel.Name = "btnLaunchExcel"
        Me.btnLaunchExcel.Size = New System.Drawing.Size(32, 23)
        Me.btnLaunchExcel.TabIndex = 28
        Me.btnLaunchExcel.UseVisualStyleBackColor = True
        '
        'txtExcelPath
        '
        Me.txtExcelPath.Location = New System.Drawing.Point(31, 260)
        Me.txtExcelPath.Name = "txtExcelPath"
        Me.txtExcelPath.Size = New System.Drawing.Size(366, 20)
        Me.txtExcelPath.TabIndex = 27
        '
        'lblExcelPath
        '
        Me.lblExcelPath.AutoSize = True
        Me.lblExcelPath.Location = New System.Drawing.Point(11, 244)
        Me.lblExcelPath.Name = "lblExcelPath"
        Me.lblExcelPath.Size = New System.Drawing.Size(70, 13)
        Me.lblExcelPath.TabIndex = 26
        Me.lblExcelPath.Text = "Path to Excel"
        '
        'btnExcelPath
        '
        Me.btnExcelPath.Location = New System.Drawing.Point(441, 260)
        Me.btnExcelPath.Name = "btnExcelPath"
        Me.btnExcelPath.Size = New System.Drawing.Size(32, 23)
        Me.btnExcelPath.TabIndex = 25
        Me.btnExcelPath.Text = "..."
        Me.btnExcelPath.UseVisualStyleBackColor = True
        '
        'lblFrameworkVersion
        '
        Me.lblFrameworkVersion.AutoSize = True
        Me.lblFrameworkVersion.Location = New System.Drawing.Point(10, 59)
        Me.lblFrameworkVersion.Name = "lblFrameworkVersion"
        Me.lblFrameworkVersion.Size = New System.Drawing.Size(137, 13)
        Me.lblFrameworkVersion.TabIndex = 0
        Me.lblFrameworkVersion.Text = "Framework Version Number"
        '
        'txtUserConfig
        '
        Me.txtUserConfig.Location = New System.Drawing.Point(30, 221)
        Me.txtUserConfig.Name = "txtUserConfig"
        Me.txtUserConfig.Size = New System.Drawing.Size(405, 20)
        Me.txtUserConfig.TabIndex = 24
        '
        'lblUserConfig
        '
        Me.lblUserConfig.AutoSize = True
        Me.lblUserConfig.Location = New System.Drawing.Point(10, 205)
        Me.lblUserConfig.Name = "lblUserConfig"
        Me.lblUserConfig.Size = New System.Drawing.Size(113, 13)
        Me.lblUserConfig.TabIndex = 23
        Me.lblUserConfig.Text = "User Configuration File"
        '
        'btnUserConfig
        '
        Me.btnUserConfig.Location = New System.Drawing.Point(441, 221)
        Me.btnUserConfig.Name = "btnUserConfig"
        Me.btnUserConfig.Size = New System.Drawing.Size(32, 23)
        Me.btnUserConfig.TabIndex = 22
        Me.btnUserConfig.Text = "..."
        Me.btnUserConfig.UseVisualStyleBackColor = True
        '
        'txtXmlPath
        '
        Me.txtXmlPath.Location = New System.Drawing.Point(30, 182)
        Me.txtXmlPath.Name = "txtXmlPath"
        Me.txtXmlPath.Size = New System.Drawing.Size(405, 20)
        Me.txtXmlPath.TabIndex = 21
        '
        'lblXmlPath
        '
        Me.lblXmlPath.AutoSize = True
        Me.lblXmlPath.Location = New System.Drawing.Point(10, 166)
        Me.lblXmlPath.Name = "lblXmlPath"
        Me.lblXmlPath.Size = New System.Drawing.Size(96, 13)
        Me.lblXmlPath.TabIndex = 20
        Me.lblXmlPath.Text = "Function Metadata"
        '
        'btnXmlPath
        '
        Me.btnXmlPath.Location = New System.Drawing.Point(441, 182)
        Me.btnXmlPath.Name = "btnXmlPath"
        Me.btnXmlPath.Size = New System.Drawing.Size(32, 23)
        Me.btnXmlPath.TabIndex = 19
        Me.btnXmlPath.Text = "..."
        Me.btnXmlPath.UseVisualStyleBackColor = True
        '
        'txtHelpPath
        '
        Me.txtHelpPath.Location = New System.Drawing.Point(30, 143)
        Me.txtHelpPath.Name = "txtHelpPath"
        Me.txtHelpPath.Size = New System.Drawing.Size(405, 20)
        Me.txtHelpPath.TabIndex = 18
        '
        'lblHelpPath
        '
        Me.lblHelpPath.AutoSize = True
        Me.lblHelpPath.Location = New System.Drawing.Point(10, 127)
        Me.lblHelpPath.Name = "lblHelpPath"
        Me.lblHelpPath.Size = New System.Drawing.Size(61, 13)
        Me.lblHelpPath.TabIndex = 17
        Me.lblHelpPath.Text = "Help Folder"
        '
        'btnHelpFile
        '
        Me.btnHelpFile.Location = New System.Drawing.Point(441, 143)
        Me.btnHelpFile.Name = "btnHelpFile"
        Me.btnHelpFile.Size = New System.Drawing.Size(32, 23)
        Me.btnHelpFile.TabIndex = 16
        Me.btnHelpFile.Text = "..."
        Me.btnHelpFile.UseVisualStyleBackColor = True
        '
        'tpAddins
        '
        Me.tpAddins.Controls.Add(Me.lbAddins)
        Me.tpAddins.Controls.Add(Me.tstAddins)
        Me.tpAddins.Location = New System.Drawing.Point(4, 22)
        Me.tpAddins.Name = "tpAddins"
        Me.tpAddins.Padding = New System.Windows.Forms.Padding(3)
        Me.tpAddins.Size = New System.Drawing.Size(479, 377)
        Me.tpAddins.TabIndex = 2
        Me.tpAddins.Text = "Addins"
        Me.tpAddins.UseVisualStyleBackColor = True
        '
        'lbAddins
        '
        Me.lbAddins.FormattingEnabled = True
        Me.lbAddins.HorizontalScrollbar = True
        Me.lbAddins.Location = New System.Drawing.Point(6, 13)
        Me.lbAddins.Name = "lbAddins"
        Me.lbAddins.Size = New System.Drawing.Size(467, 173)
        Me.lbAddins.TabIndex = 10
        '
        'tstAddins
        '
        Me.tstAddins.BackColor = System.Drawing.SystemColors.Control
        Me.tstAddins.Dock = System.Windows.Forms.DockStyle.None
        Me.tstAddins.GripStyle = System.Windows.Forms.ToolStripGripStyle.Hidden
        Me.tstAddins.Items.AddRange(New System.Windows.Forms.ToolStripItem() {Me.btnAddinInsert, Me.btnAddinDelete, Me.btnAddinRename, Me.btnAddinUp, Me.btnAddinDown})
        Me.tstAddins.Location = New System.Drawing.Point(6, 189)
        Me.tstAddins.Name = "tstAddins"
        Me.tstAddins.RenderMode = System.Windows.Forms.ToolStripRenderMode.System
        Me.tstAddins.Size = New System.Drawing.Size(118, 25)
        Me.tstAddins.TabIndex = 8
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
        Me.btnAddinRename.Image = Global.launcher.My.Resources.Resources.rename
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
        'tpFeeds
        '
        Me.tpFeeds.Controls.Add(Me.cbBloomberg)
        Me.tpFeeds.Controls.Add(Me.cbReuters)
        Me.tpFeeds.Controls.Add(Me.txtBloomberg)
        Me.tpFeeds.Controls.Add(Me.lblBloomberg)
        Me.tpFeeds.Controls.Add(Me.btnBloomberg)
        Me.tpFeeds.Controls.Add(Me.btnReuters)
        Me.tpFeeds.Controls.Add(Me.txtReuters)
        Me.tpFeeds.Controls.Add(Me.lblReuters)
        Me.tpFeeds.Location = New System.Drawing.Point(4, 22)
        Me.tpFeeds.Name = "tpFeeds"
        Me.tpFeeds.Padding = New System.Windows.Forms.Padding(3)
        Me.tpFeeds.Size = New System.Drawing.Size(479, 377)
        Me.tpFeeds.TabIndex = 3
        Me.tpFeeds.Text = "Feeds"
        Me.tpFeeds.UseVisualStyleBackColor = True
        '
        'cbBloomberg
        '
        Me.cbBloomberg.AutoSize = True
        Me.cbBloomberg.Location = New System.Drawing.Point(30, 65)
        Me.cbBloomberg.Name = "cbBloomberg"
        Me.cbBloomberg.Size = New System.Drawing.Size(50, 17)
        Me.cbBloomberg.TabIndex = 26
        Me.cbBloomberg.Text = "Load"
        Me.cbBloomberg.UseVisualStyleBackColor = True
        '
        'cbReuters
        '
        Me.cbReuters.AutoSize = True
        Me.cbReuters.Location = New System.Drawing.Point(30, 26)
        Me.cbReuters.Name = "cbReuters"
        Me.cbReuters.Size = New System.Drawing.Size(50, 17)
        Me.cbReuters.TabIndex = 25
        Me.cbReuters.Text = "Load"
        Me.cbReuters.UseVisualStyleBackColor = True
        '
        'txtBloomberg
        '
        Me.txtBloomberg.Location = New System.Drawing.Point(81, 65)
        Me.txtBloomberg.Name = "txtBloomberg"
        Me.txtBloomberg.Size = New System.Drawing.Size(354, 20)
        Me.txtBloomberg.TabIndex = 24
        '
        'lblBloomberg
        '
        Me.lblBloomberg.AutoSize = True
        Me.lblBloomberg.Location = New System.Drawing.Point(10, 49)
        Me.lblBloomberg.Name = "lblBloomberg"
        Me.lblBloomberg.Size = New System.Drawing.Size(57, 13)
        Me.lblBloomberg.TabIndex = 23
        Me.lblBloomberg.Text = "Bloomberg"
        '
        'btnBloomberg
        '
        Me.btnBloomberg.Location = New System.Drawing.Point(441, 65)
        Me.btnBloomberg.Name = "btnBloomberg"
        Me.btnBloomberg.Size = New System.Drawing.Size(32, 23)
        Me.btnBloomberg.TabIndex = 22
        Me.btnBloomberg.Text = "..."
        Me.btnBloomberg.UseVisualStyleBackColor = True
        '
        'btnReuters
        '
        Me.btnReuters.Location = New System.Drawing.Point(441, 26)
        Me.btnReuters.Name = "btnReuters"
        Me.btnReuters.Size = New System.Drawing.Size(32, 23)
        Me.btnReuters.TabIndex = 21
        Me.btnReuters.Text = "..."
        Me.btnReuters.UseVisualStyleBackColor = True
        '
        'txtReuters
        '
        Me.txtReuters.Location = New System.Drawing.Point(81, 26)
        Me.txtReuters.Name = "txtReuters"
        Me.txtReuters.Size = New System.Drawing.Size(354, 20)
        Me.txtReuters.TabIndex = 20
        '
        'lblReuters
        '
        Me.lblReuters.AutoSize = True
        Me.lblReuters.Location = New System.Drawing.Point(10, 10)
        Me.lblReuters.Name = "lblReuters"
        Me.lblReuters.Size = New System.Drawing.Size(44, 13)
        Me.lblReuters.TabIndex = 19
        Me.lblReuters.Text = "Reuters"
        '
        'tpVariables
        '
        Me.tpVariables.Controls.Add(Me.lvVariables)
        Me.tpVariables.Location = New System.Drawing.Point(4, 22)
        Me.tpVariables.Name = "tpVariables"
        Me.tpVariables.Padding = New System.Windows.Forms.Padding(3)
        Me.tpVariables.Size = New System.Drawing.Size(479, 377)
        Me.tpVariables.TabIndex = 5
        Me.tpVariables.Text = "Variables"
        Me.tpVariables.UseVisualStyleBackColor = True
        '
        'lvVariables
        '
        Me.lvVariables.AutoArrange = False
        Me.lvVariables.CausesValidation = False
        Me.lvVariables.Columns.AddRange(New System.Windows.Forms.ColumnHeader() {Me.chVariable, Me.chValue})
        Me.lvVariables.ContextMenuStrip = Me.cmVariables
        Me.lvVariables.FullRowSelect = True
        Me.lvVariables.HeaderStyle = System.Windows.Forms.ColumnHeaderStyle.Nonclickable
        Me.lvVariables.Items.AddRange(New System.Windows.Forms.ListViewItem() {ListViewItem1})
        Me.lvVariables.LabelWrap = False
        Me.lvVariables.Location = New System.Drawing.Point(6, 6)
        Me.lvVariables.MultiSelect = False
        Me.lvVariables.Name = "lvVariables"
        Me.lvVariables.Scrollable = False
        Me.lvVariables.Size = New System.Drawing.Size(466, 365)
        Me.lvVariables.Sorting = System.Windows.Forms.SortOrder.Ascending
        Me.lvVariables.TabIndex = 0
        Me.lvVariables.UseCompatibleStateImageBehavior = False
        Me.lvVariables.View = System.Windows.Forms.View.Details
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
        Me.tpAbout.Controls.Add(Me.lblHardDiskValue)
        Me.tpAbout.Controls.Add(Me.lblDomainValue)
        Me.tpAbout.Controls.Add(Me.lblUserNameValue)
        Me.tpAbout.Controls.Add(Me.lblVersionValue)
        Me.tpAbout.Controls.Add(Me.lblHardDisk)
        Me.tpAbout.Controls.Add(Me.lblDomain)
        Me.tpAbout.Controls.Add(Me.lblUserName)
        Me.tpAbout.Controls.Add(Me.lblVersion)
        Me.tpAbout.Location = New System.Drawing.Point(4, 22)
        Me.tpAbout.Name = "tpAbout"
        Me.tpAbout.Padding = New System.Windows.Forms.Padding(3)
        Me.tpAbout.Size = New System.Drawing.Size(479, 377)
        Me.tpAbout.TabIndex = 4
        Me.tpAbout.Text = "About"
        Me.tpAbout.UseVisualStyleBackColor = True
        '
        'lblHardDiskValue
        '
        Me.lblHardDiskValue.AutoSize = True
        Me.lblHardDiskValue.Location = New System.Drawing.Point(155, 80)
        Me.lblHardDiskValue.Name = "lblHardDiskValue"
        Me.lblHardDiskValue.Size = New System.Drawing.Size(69, 13)
        Me.lblHardDiskValue.TabIndex = 7
        Me.lblHardDiskValue.Text = "serial number"
        '
        'lblDomainValue
        '
        Me.lblDomainValue.AutoSize = True
        Me.lblDomainValue.Location = New System.Drawing.Point(155, 58)
        Me.lblDomainValue.Name = "lblDomainValue"
        Me.lblDomainValue.Size = New System.Drawing.Size(41, 13)
        Me.lblDomainValue.TabIndex = 6
        Me.lblDomainValue.Text = "domain"
        '
        'lblUserNameValue
        '
        Me.lblUserNameValue.AutoSize = True
        Me.lblUserNameValue.Location = New System.Drawing.Point(156, 34)
        Me.lblUserNameValue.Name = "lblUserNameValue"
        Me.lblUserNameValue.Size = New System.Drawing.Size(53, 13)
        Me.lblUserNameValue.TabIndex = 5
        Me.lblUserNameValue.Text = "username"
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
        'lblHardDisk
        '
        Me.lblHardDisk.AutoSize = True
        Me.lblHardDisk.Location = New System.Drawing.Point(6, 80)
        Me.lblHardDisk.Name = "lblHardDisk"
        Me.lblHardDisk.Size = New System.Drawing.Size(123, 13)
        Me.lblHardDisk.TabIndex = 3
        Me.lblHardDisk.Text = "Hard Disk Serial Number"
        '
        'lblDomain
        '
        Me.lblDomain.AutoSize = True
        Me.lblDomain.Location = New System.Drawing.Point(6, 58)
        Me.lblDomain.Name = "lblDomain"
        Me.lblDomain.Size = New System.Drawing.Size(43, 13)
        Me.lblDomain.TabIndex = 2
        Me.lblDomain.Text = "Domain"
        '
        'lblUserName
        '
        Me.lblUserName.AutoSize = True
        Me.lblUserName.Location = New System.Drawing.Point(6, 34)
        Me.lblUserName.Name = "lblUserName"
        Me.lblUserName.Size = New System.Drawing.Size(60, 13)
        Me.lblUserName.TabIndex = 1
        Me.lblUserName.Text = "User Name"
        '
        'lblVersion
        '
        Me.lblVersion.AutoSize = True
        Me.lblVersion.Location = New System.Drawing.Point(6, 12)
        Me.lblVersion.Name = "lblVersion"
        Me.lblVersion.Size = New System.Drawing.Size(90, 13)
        Me.lblVersion.TabIndex = 0
        Me.lblVersion.Text = "Launcher 2008 Version"
        '
        'lblBuildNumber
        '
        Me.lblBuildNumber.AutoSize = True
        Me.lblBuildNumber.Font = New System.Drawing.Font("Microsoft Sans Serif", 6.75!, System.Drawing.FontStyle.Regular, System.Drawing.GraphicsUnit.Point, CType(0, Byte))
        Me.lblBuildNumber.ForeColor = System.Drawing.Color.DimGray
        Me.lblBuildNumber.Location = New System.Drawing.Point(370, 423)
        Me.lblBuildNumber.MinimumSize = New System.Drawing.Size(120, 0)
        Me.lblBuildNumber.Name = "lblBuildNumber"
        Me.lblBuildNumber.Size = New System.Drawing.Size(120, 12)
        Me.lblBuildNumber.TabIndex = 22
        Me.lblBuildNumber.Text = "version 0.0.0.0"
        Me.lblBuildNumber.TextAlign = System.Drawing.ContentAlignment.BottomRight
        '
        'FormMain
        '
        Me.AutoScaleDimensions = New System.Drawing.SizeF(6.0!, 13.0!)
        Me.AutoScaleMode = System.Windows.Forms.AutoScaleMode.Font
        Me.ClientSize = New System.Drawing.Size(491, 438)
        Me.Controls.Add(Me.lblBuildNumber)
        Me.Controls.Add(Me.tcLauncher)
        Me.Controls.Add(Me.btnLaunch)
        Me.Controls.Add(Me.btnClose)
        Me.FormBorderStyle = System.Windows.Forms.FormBorderStyle.FixedDialog
        Me.Icon = CType(resources.GetObject("$this.Icon"), System.Drawing.Icon)
        Me.Name = "FormMain"
        Me.Text = "QuantLibXL Launcher 2008"
        Me.grpEnvironment.ResumeLayout(False)
        Me.grpEnvironment.PerformLayout()
        Me.tstEnvironment.ResumeLayout(False)
        Me.tstEnvironment.PerformLayout()
        Me.grpStartup.ResumeLayout(False)
        Me.grpStartup.PerformLayout()
        Me.tcLauncher.ResumeLayout(False)
        Me.tpEnvironments.ResumeLayout(False)
        Me.grpInit.ResumeLayout(False)
        Me.grpInit.PerformLayout()
        Me.grpFeeds.ResumeLayout(False)
        Me.grpFeeds.PerformLayout()
        Me.tpPaths.ResumeLayout(False)
        Me.tpPaths.PerformLayout()
        Me.tpAddins.ResumeLayout(False)
        Me.tpAddins.PerformLayout()
        Me.tstAddins.ResumeLayout(False)
        Me.tstAddins.PerformLayout()
        Me.tpFeeds.ResumeLayout(False)
        Me.tpFeeds.PerformLayout()
        Me.tpVariables.ResumeLayout(False)
        Me.cmVariables.ResumeLayout(False)
        Me.tpAbout.ResumeLayout(False)
        Me.tpAbout.PerformLayout()
        Me.ResumeLayout(False)
        Me.PerformLayout()

    End Sub
    Friend WithEvents btnClose As System.Windows.Forms.Button
    Friend WithEvents lblPreconfigured As System.Windows.Forms.Label
    Friend WithEvents lblFramework As System.Windows.Forms.Label
    Friend WithEvents txtFramework As System.Windows.Forms.TextBox
    Friend WithEvents btnFrameworkSelect As System.Windows.Forms.Button
    Friend WithEvents btnWorkbooks As System.Windows.Forms.Button
    Friend WithEvents txtWorkbooks As System.Windows.Forms.TextBox
    Friend WithEvents lblWorkbooks As System.Windows.Forms.Label
    Friend WithEvents grpEnvironment As System.Windows.Forms.GroupBox
    Friend WithEvents lstPreconfigured As System.Windows.Forms.ListBox
    Friend WithEvents grpStartup As System.Windows.Forms.GroupBox
    Friend WithEvents cbYCBootstrap As System.Windows.Forms.CheckBox
    Friend WithEvents lblUserConfigured As System.Windows.Forms.Label
    Friend WithEvents lstUserconfigured As System.Windows.Forms.ListBox
    Friend WithEvents tstEnvironment As System.Windows.Forms.ToolStrip
    Friend WithEvents btnNew As System.Windows.Forms.ToolStripButton
    Friend WithEvents btnCopy As System.Windows.Forms.ToolStripButton
    Friend WithEvents btnDelete As System.Windows.Forms.ToolStripButton
    Friend WithEvents btnClear As System.Windows.Forms.ToolStripButton
    Friend WithEvents btnRename As System.Windows.Forms.ToolStripButton
    Friend WithEvents btnLaunch As System.Windows.Forms.Button
    Friend WithEvents cbIndexesTimeSeries As System.Windows.Forms.CheckBox
    Friend WithEvents cbCapVolBootstrap As System.Windows.Forms.CheckBox
    Friend WithEvents tcLauncher As System.Windows.Forms.TabControl
    Friend WithEvents tpEnvironments As System.Windows.Forms.TabPage
    Friend WithEvents tpPaths As System.Windows.Forms.TabPage
    Friend WithEvents cbFitCMS As System.Windows.Forms.CheckBox
    Friend WithEvents cbLoadBonds As System.Windows.Forms.CheckBox
    Friend WithEvents cbMainChecks As System.Windows.Forms.CheckBox
    Friend WithEvents cbStaticData As System.Windows.Forms.CheckBox
    Friend WithEvents cbSwapSmileBootstrap As System.Windows.Forms.CheckBox
    Friend WithEvents txtHelpPath As System.Windows.Forms.TextBox
    Friend WithEvents lblHelpPath As System.Windows.Forms.Label
    Friend WithEvents btnHelpFile As System.Windows.Forms.Button
    Friend WithEvents txtUserConfig As System.Windows.Forms.TextBox
    Friend WithEvents lblUserConfig As System.Windows.Forms.Label
    Friend WithEvents btnUserConfig As System.Windows.Forms.Button
    Friend WithEvents txtXmlPath As System.Windows.Forms.TextBox
    Friend WithEvents lblXmlPath As System.Windows.Forms.Label
    Friend WithEvents btnXmlPath As System.Windows.Forms.Button
    Friend WithEvents tpAddins As System.Windows.Forms.TabPage
    Friend WithEvents lblFrameworkVersion As System.Windows.Forms.Label
    Friend WithEvents tstAddins As System.Windows.Forms.ToolStrip
    Friend WithEvents btnAddinInsert As System.Windows.Forms.ToolStripButton
    Friend WithEvents btnAddinDelete As System.Windows.Forms.ToolStripButton
    Friend WithEvents btnAddinUp As System.Windows.Forms.ToolStripButton
    Friend WithEvents btnAddinDown As System.Windows.Forms.ToolStripButton
    Friend WithEvents lbAddins As System.Windows.Forms.ListBox
    Friend WithEvents btnAddinRename As System.Windows.Forms.ToolStripButton
    Friend WithEvents tpFeeds As System.Windows.Forms.TabPage
    Friend WithEvents txtBloomberg As System.Windows.Forms.TextBox
    Friend WithEvents lblBloomberg As System.Windows.Forms.Label
    Friend WithEvents btnBloomberg As System.Windows.Forms.Button
    Friend WithEvents btnReuters As System.Windows.Forms.Button
    Friend WithEvents txtReuters As System.Windows.Forms.TextBox
    Friend WithEvents lblReuters As System.Windows.Forms.Label
    Friend WithEvents grpFeeds As System.Windows.Forms.GroupBox
    Friend WithEvents lblBuildNumber As System.Windows.Forms.Label
    Friend WithEvents cbCalibrateCms As System.Windows.Forms.CheckBox
    Friend WithEvents txtExcelPath As System.Windows.Forms.TextBox
    Friend WithEvents lblExcelPath As System.Windows.Forms.Label
    Friend WithEvents btnExcelPath As System.Windows.Forms.Button
    Friend WithEvents tpAbout As System.Windows.Forms.TabPage
    Friend WithEvents lblUserName As System.Windows.Forms.Label
    Friend WithEvents lblVersion As System.Windows.Forms.Label
    Friend WithEvents lblHardDiskValue As System.Windows.Forms.Label
    Friend WithEvents lblDomainValue As System.Windows.Forms.Label
    Friend WithEvents lblUserNameValue As System.Windows.Forms.Label
    Friend WithEvents lblVersionValue As System.Windows.Forms.Label
    Friend WithEvents lblHardDisk As System.Windows.Forms.Label
    Friend WithEvents lblDomain As System.Windows.Forms.Label
    Friend WithEvents btnLaunchExcel As System.Windows.Forms.Button
    Friend WithEvents cbSetEvaluationDate As System.Windows.Forms.CheckBox
    Friend WithEvents dtEvaluationDate As System.Windows.Forms.DateTimePicker
    Friend WithEvents cbFrameworkVersion As System.Windows.Forms.ComboBox
    Friend WithEvents tpVariables As System.Windows.Forms.TabPage
    Friend WithEvents lvVariables As System.Windows.Forms.ListView
    Friend WithEvents chVariable As System.Windows.Forms.ColumnHeader
    Friend WithEvents chValue As System.Windows.Forms.ColumnHeader
    Friend WithEvents grpInit As System.Windows.Forms.GroupBox
    Friend WithEvents rbExcel As System.Windows.Forms.RadioButton
    Friend WithEvents rbXML As System.Windows.Forms.RadioButton
    Friend WithEvents cbBloomberg As System.Windows.Forms.CheckBox
    Friend WithEvents cbReuters As System.Windows.Forms.CheckBox
    Friend WithEvents rbBloomberg As System.Windows.Forms.RadioButton
    Friend WithEvents rbReuters As System.Windows.Forms.RadioButton
    Friend WithEvents cmVariables As System.Windows.Forms.ContextMenuStrip
    Friend WithEvents miVariableInsert As System.Windows.Forms.ToolStripMenuItem
    Friend WithEvents miVariableEdit As System.Windows.Forms.ToolStripMenuItem
    Friend WithEvents miVariableDelete As System.Windows.Forms.ToolStripMenuItem
End Class
