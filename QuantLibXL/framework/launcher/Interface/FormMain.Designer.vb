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
        Dim resources As System.ComponentModel.ComponentResourceManager = New System.ComponentModel.ComponentResourceManager(GetType(FormMain))
        Me.btnClose = New System.Windows.Forms.Button
        Me.lblPreconfigured = New System.Windows.Forms.Label
        Me.lblFramework = New System.Windows.Forms.Label
        Me.txtFramework = New System.Windows.Forms.TextBox
        Me.lblAddinDir = New System.Windows.Forms.Label
        Me.txtAddinDir = New System.Windows.Forms.TextBox
        Me.btnAddinNameSelect = New System.Windows.Forms.Button
        Me.lblAddinName = New System.Windows.Forms.Label
        Me.txtAddinName = New System.Windows.Forms.TextBox
        Me.btnFrameworkSelect = New System.Windows.Forms.Button
        Me.btnAddinDirSelect = New System.Windows.Forms.Button
        Me.btnWorkbooks = New System.Windows.Forms.Button
        Me.txtWorkbooks = New System.Windows.Forms.TextBox
        Me.lblWorkbooks = New System.Windows.Forms.Label
        Me.grpEnvironment = New System.Windows.Forms.GroupBox
        Me.ToolStrip1 = New System.Windows.Forms.ToolStrip
        Me.btnNew = New System.Windows.Forms.ToolStripButton
        Me.btnCopy = New System.Windows.Forms.ToolStripButton
        Me.btnDelete = New System.Windows.Forms.ToolStripButton
        Me.btnClear = New System.Windows.Forms.ToolStripButton
        Me.btnRename = New System.Windows.Forms.ToolStripButton
        Me.lstUserconfigured = New System.Windows.Forms.ListBox
        Me.lblUserConfigured = New System.Windows.Forms.Label
        Me.lstPreconfigured = New System.Windows.Forms.ListBox
        Me.grpStartup = New System.Windows.Forms.GroupBox
        Me.cbStaticData = New System.Windows.Forms.CheckBox
        Me.cbFitCMS = New System.Windows.Forms.CheckBox
        Me.cbLoadBonds = New System.Windows.Forms.CheckBox
        Me.cbSwapSmileBootstrap = New System.Windows.Forms.CheckBox
        Me.cbIndexesTimeSeries = New System.Windows.Forms.CheckBox
        Me.cbSwapVolBootstrap = New System.Windows.Forms.CheckBox
        Me.cbCapVolBootstrap = New System.Windows.Forms.CheckBox
        Me.cbLoadMurexYC = New System.Windows.Forms.CheckBox
        Me.cbYCBootstrap = New System.Windows.Forms.CheckBox
        Me.btnLaunch = New System.Windows.Forms.Button
        Me.TabControl1 = New System.Windows.Forms.TabControl
        Me.tbEnvironments = New System.Windows.Forms.TabPage
        Me.tpPaths = New System.Windows.Forms.TabPage
        Me.txtHelpPath = New System.Windows.Forms.TextBox
        Me.lblHelpPath = New System.Windows.Forms.Label
        Me.btnHelpFile = New System.Windows.Forms.Button
        Me.grpEnvironment.SuspendLayout()
        Me.ToolStrip1.SuspendLayout()
        Me.grpStartup.SuspendLayout()
        Me.TabControl1.SuspendLayout()
        Me.tbEnvironments.SuspendLayout()
        Me.tpPaths.SuspendLayout()
        Me.SuspendLayout()
        '
        'btnClose
        '
        Me.btnClose.Location = New System.Drawing.Point(207, 345)
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
        Me.txtFramework.Size = New System.Drawing.Size(315, 20)
        Me.txtFramework.TabIndex = 5
        '
        'lblAddinDir
        '
        Me.lblAddinDir.AutoSize = True
        Me.lblAddinDir.Location = New System.Drawing.Point(10, 88)
        Me.lblAddinDir.Name = "lblAddinDir"
        Me.lblAddinDir.Size = New System.Drawing.Size(79, 13)
        Me.lblAddinDir.TabIndex = 6
        Me.lblAddinDir.Text = "Addin Directory"
        '
        'txtAddinDir
        '
        Me.txtAddinDir.Location = New System.Drawing.Point(29, 104)
        Me.txtAddinDir.Name = "txtAddinDir"
        Me.txtAddinDir.Size = New System.Drawing.Size(316, 20)
        Me.txtAddinDir.TabIndex = 7
        '
        'btnAddinNameSelect
        '
        Me.btnAddinNameSelect.Location = New System.Drawing.Point(351, 143)
        Me.btnAddinNameSelect.Name = "btnAddinNameSelect"
        Me.btnAddinNameSelect.Size = New System.Drawing.Size(32, 23)
        Me.btnAddinNameSelect.TabIndex = 8
        Me.btnAddinNameSelect.Text = "..."
        Me.btnAddinNameSelect.UseVisualStyleBackColor = True
        '
        'lblAddinName
        '
        Me.lblAddinName.AutoSize = True
        Me.lblAddinName.Location = New System.Drawing.Point(10, 127)
        Me.lblAddinName.Name = "lblAddinName"
        Me.lblAddinName.Size = New System.Drawing.Size(65, 13)
        Me.lblAddinName.TabIndex = 9
        Me.lblAddinName.Text = "Addin Name"
        '
        'txtAddinName
        '
        Me.txtAddinName.Location = New System.Drawing.Point(29, 143)
        Me.txtAddinName.Name = "txtAddinName"
        Me.txtAddinName.Size = New System.Drawing.Size(316, 20)
        Me.txtAddinName.TabIndex = 10
        '
        'btnFrameworkSelect
        '
        Me.btnFrameworkSelect.Location = New System.Drawing.Point(351, 26)
        Me.btnFrameworkSelect.Name = "btnFrameworkSelect"
        Me.btnFrameworkSelect.Size = New System.Drawing.Size(32, 23)
        Me.btnFrameworkSelect.TabIndex = 11
        Me.btnFrameworkSelect.Text = "..."
        Me.btnFrameworkSelect.UseVisualStyleBackColor = True
        '
        'btnAddinDirSelect
        '
        Me.btnAddinDirSelect.Location = New System.Drawing.Point(351, 104)
        Me.btnAddinDirSelect.Name = "btnAddinDirSelect"
        Me.btnAddinDirSelect.Size = New System.Drawing.Size(32, 23)
        Me.btnAddinDirSelect.TabIndex = 12
        Me.btnAddinDirSelect.Text = "..."
        Me.btnAddinDirSelect.UseVisualStyleBackColor = True
        '
        'btnWorkbooks
        '
        Me.btnWorkbooks.Location = New System.Drawing.Point(351, 65)
        Me.btnWorkbooks.Name = "btnWorkbooks"
        Me.btnWorkbooks.Size = New System.Drawing.Size(32, 23)
        Me.btnWorkbooks.TabIndex = 15
        Me.btnWorkbooks.Text = "..."
        Me.btnWorkbooks.UseVisualStyleBackColor = True
        '
        'txtWorkbooks
        '
        Me.txtWorkbooks.Location = New System.Drawing.Point(30, 65)
        Me.txtWorkbooks.Name = "txtWorkbooks"
        Me.txtWorkbooks.Size = New System.Drawing.Size(315, 20)
        Me.txtWorkbooks.TabIndex = 14
        '
        'lblWorkbooks
        '
        Me.lblWorkbooks.AutoSize = True
        Me.lblWorkbooks.Location = New System.Drawing.Point(10, 49)
        Me.lblWorkbooks.Name = "lblWorkbooks"
        Me.lblWorkbooks.Size = New System.Drawing.Size(62, 13)
        Me.lblWorkbooks.TabIndex = 13
        Me.lblWorkbooks.Text = "Workbooks"
        '
        'grpEnvironment
        '
        Me.grpEnvironment.Controls.Add(Me.ToolStrip1)
        Me.grpEnvironment.Controls.Add(Me.lstUserconfigured)
        Me.grpEnvironment.Controls.Add(Me.lblUserConfigured)
        Me.grpEnvironment.Controls.Add(Me.lstPreconfigured)
        Me.grpEnvironment.Controls.Add(Me.lblPreconfigured)
        Me.grpEnvironment.Location = New System.Drawing.Point(6, 7)
        Me.grpEnvironment.Name = "grpEnvironment"
        Me.grpEnvironment.Size = New System.Drawing.Size(190, 300)
        Me.grpEnvironment.TabIndex = 16
        Me.grpEnvironment.TabStop = False
        Me.grpEnvironment.Text = "Environments"
        '
        'ToolStrip1
        '
        Me.ToolStrip1.BackColor = System.Drawing.SystemColors.Control
        Me.ToolStrip1.Dock = System.Windows.Forms.DockStyle.None
        Me.ToolStrip1.GripStyle = System.Windows.Forms.ToolStripGripStyle.Hidden
        Me.ToolStrip1.Items.AddRange(New System.Windows.Forms.ToolStripItem() {Me.btnNew, Me.btnCopy, Me.btnDelete, Me.btnClear, Me.btnRename})
        Me.ToolStrip1.Location = New System.Drawing.Point(9, 267)
        Me.ToolStrip1.Name = "ToolStrip1"
        Me.ToolStrip1.RenderMode = System.Windows.Forms.ToolStripRenderMode.System
        Me.ToolStrip1.Size = New System.Drawing.Size(118, 25)
        Me.ToolStrip1.TabIndex = 7
        Me.ToolStrip1.Text = "ToolStrip1"
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
        Me.lstUserconfigured.Location = New System.Drawing.Point(19, 126)
        Me.lstUserconfigured.Name = "lstUserconfigured"
        Me.lstUserconfigured.Size = New System.Drawing.Size(160, 134)
        Me.lstUserconfigured.Sorted = True
        Me.lstUserconfigured.TabIndex = 6
        '
        'lblUserConfigured
        '
        Me.lblUserConfigured.AutoSize = True
        Me.lblUserConfigured.Location = New System.Drawing.Point(6, 110)
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
        Me.lstPreconfigured.Size = New System.Drawing.Size(160, 69)
        Me.lstPreconfigured.Sorted = True
        Me.lstPreconfigured.TabIndex = 4
        '
        'grpStartup
        '
        Me.grpStartup.Controls.Add(Me.cbStaticData)
        Me.grpStartup.Controls.Add(Me.cbFitCMS)
        Me.grpStartup.Controls.Add(Me.cbLoadBonds)
        Me.grpStartup.Controls.Add(Me.cbSwapSmileBootstrap)
        Me.grpStartup.Controls.Add(Me.cbIndexesTimeSeries)
        Me.grpStartup.Controls.Add(Me.cbSwapVolBootstrap)
        Me.grpStartup.Controls.Add(Me.cbCapVolBootstrap)
        Me.grpStartup.Controls.Add(Me.cbLoadMurexYC)
        Me.grpStartup.Controls.Add(Me.cbYCBootstrap)
        Me.grpStartup.Location = New System.Drawing.Point(202, 6)
        Me.grpStartup.Name = "grpStartup"
        Me.grpStartup.Size = New System.Drawing.Size(190, 300)
        Me.grpStartup.TabIndex = 19
        Me.grpStartup.TabStop = False
        Me.grpStartup.Text = "Startup Actions"
        '
        'cbStaticData
        '
        Me.cbStaticData.AutoSize = True
        Me.cbStaticData.Location = New System.Drawing.Point(10, 210)
        Me.cbStaticData.Name = "cbStaticData"
        Me.cbStaticData.Size = New System.Drawing.Size(159, 17)
        Me.cbStaticData.TabIndex = 8
        Me.cbStaticData.Text = "Default to static market data"
        Me.cbStaticData.UseVisualStyleBackColor = True
        '
        'cbFitCMS
        '
        Me.cbFitCMS.AutoSize = True
        Me.cbFitCMS.Location = New System.Drawing.Point(10, 141)
        Me.cbFitCMS.Name = "cbFitCMS"
        Me.cbFitCMS.Size = New System.Drawing.Size(105, 17)
        Me.cbFitCMS.TabIndex = 7
        Me.cbFitCMS.Text = "Fit CMS Spreads"
        Me.cbFitCMS.UseVisualStyleBackColor = True
        '
        'cbLoadBonds
        '
        Me.cbLoadBonds.AutoSize = True
        Me.cbLoadBonds.Location = New System.Drawing.Point(10, 187)
        Me.cbLoadBonds.Name = "cbLoadBonds"
        Me.cbLoadBonds.Size = New System.Drawing.Size(83, 17)
        Me.cbLoadBonds.TabIndex = 6
        Me.cbLoadBonds.Text = "Load Bonds"
        Me.cbLoadBonds.UseVisualStyleBackColor = True
        '
        'cbSwapSmileBootstrap
        '
        Me.cbSwapSmileBootstrap.AutoSize = True
        Me.cbSwapSmileBootstrap.Location = New System.Drawing.Point(10, 118)
        Me.cbSwapSmileBootstrap.Name = "cbSwapSmileBootstrap"
        Me.cbSwapSmileBootstrap.Size = New System.Drawing.Size(173, 17)
        Me.cbSwapSmileBootstrap.TabIndex = 5
        Me.cbSwapSmileBootstrap.Text = "Create Swaption Smile Volatility"
        Me.cbSwapSmileBootstrap.UseVisualStyleBackColor = True
        '
        'cbIndexesTimeSeries
        '
        Me.cbIndexesTimeSeries.AutoSize = True
        Me.cbIndexesTimeSeries.Location = New System.Drawing.Point(10, 164)
        Me.cbIndexesTimeSeries.Name = "cbIndexesTimeSeries"
        Me.cbIndexesTimeSeries.Size = New System.Drawing.Size(148, 17)
        Me.cbIndexesTimeSeries.TabIndex = 4
        Me.cbIndexesTimeSeries.Text = "Load Indexes Time Series"
        Me.cbIndexesTimeSeries.UseVisualStyleBackColor = True
        '
        'cbSwapVolBootstrap
        '
        Me.cbSwapVolBootstrap.AutoSize = True
        Me.cbSwapVolBootstrap.Location = New System.Drawing.Point(10, 95)
        Me.cbSwapVolBootstrap.Name = "cbSwapVolBootstrap"
        Me.cbSwapVolBootstrap.Size = New System.Drawing.Size(171, 17)
        Me.cbSwapVolBootstrap.TabIndex = 3
        Me.cbSwapVolBootstrap.Text = "Create Swaption ATM Volatility"
        Me.cbSwapVolBootstrap.UseVisualStyleBackColor = True
        '
        'cbCapVolBootstrap
        '
        Me.cbCapVolBootstrap.AutoSize = True
        Me.cbCapVolBootstrap.Location = New System.Drawing.Point(10, 72)
        Me.cbCapVolBootstrap.Name = "cbCapVolBootstrap"
        Me.cbCapVolBootstrap.Size = New System.Drawing.Size(142, 17)
        Me.cbCapVolBootstrap.TabIndex = 2
        Me.cbCapVolBootstrap.Text = "Bootstrap Cap Volatilities"
        Me.cbCapVolBootstrap.UseVisualStyleBackColor = True
        '
        'cbLoadMurexYC
        '
        Me.cbLoadMurexYC.AutoSize = True
        Me.cbLoadMurexYC.Location = New System.Drawing.Point(10, 47)
        Me.cbLoadMurexYC.Name = "cbLoadMurexYC"
        Me.cbLoadMurexYC.Size = New System.Drawing.Size(160, 17)
        Me.cbLoadMurexYC.TabIndex = 1
        Me.cbLoadMurexYC.Text = "Bootstrap Murex Yield Curve"
        Me.cbLoadMurexYC.UseVisualStyleBackColor = True
        '
        'cbYCBootstrap
        '
        Me.cbYCBootstrap.AutoSize = True
        Me.cbYCBootstrap.Location = New System.Drawing.Point(10, 23)
        Me.cbYCBootstrap.Name = "cbYCBootstrap"
        Me.cbYCBootstrap.Size = New System.Drawing.Size(174, 17)
        Me.cbYCBootstrap.TabIndex = 0
        Me.cbYCBootstrap.Text = "Bootstrap QuantLib Yield Curve"
        Me.cbYCBootstrap.UseVisualStyleBackColor = True
        '
        'btnLaunch
        '
        Me.btnLaunch.Location = New System.Drawing.Point(126, 345)
        Me.btnLaunch.Name = "btnLaunch"
        Me.btnLaunch.Size = New System.Drawing.Size(75, 23)
        Me.btnLaunch.TabIndex = 20
        Me.btnLaunch.Text = "Launch"
        Me.btnLaunch.UseVisualStyleBackColor = True
        '
        'TabControl1
        '
        Me.TabControl1.Controls.Add(Me.tbEnvironments)
        Me.TabControl1.Controls.Add(Me.tpPaths)
        Me.TabControl1.Location = New System.Drawing.Point(3, 3)
        Me.TabControl1.Name = "TabControl1"
        Me.TabControl1.SelectedIndex = 0
        Me.TabControl1.Size = New System.Drawing.Size(403, 336)
        Me.TabControl1.TabIndex = 21
        '
        'tbEnvironments
        '
        Me.tbEnvironments.Controls.Add(Me.grpEnvironment)
        Me.tbEnvironments.Controls.Add(Me.grpStartup)
        Me.tbEnvironments.Location = New System.Drawing.Point(4, 22)
        Me.tbEnvironments.Name = "tbEnvironments"
        Me.tbEnvironments.Padding = New System.Windows.Forms.Padding(3)
        Me.tbEnvironments.Size = New System.Drawing.Size(395, 310)
        Me.tbEnvironments.TabIndex = 0
        Me.tbEnvironments.Text = "Environments"
        Me.tbEnvironments.UseVisualStyleBackColor = True
        '
        'tpPaths
        '
        Me.tpPaths.Controls.Add(Me.txtHelpPath)
        Me.tpPaths.Controls.Add(Me.lblHelpPath)
        Me.tpPaths.Controls.Add(Me.btnHelpFile)
        Me.tpPaths.Controls.Add(Me.txtFramework)
        Me.tpPaths.Controls.Add(Me.btnAddinDirSelect)
        Me.tpPaths.Controls.Add(Me.lblFramework)
        Me.tpPaths.Controls.Add(Me.txtAddinName)
        Me.tpPaths.Controls.Add(Me.btnWorkbooks)
        Me.tpPaths.Controls.Add(Me.btnFrameworkSelect)
        Me.tpPaths.Controls.Add(Me.lblAddinDir)
        Me.tpPaths.Controls.Add(Me.lblAddinName)
        Me.tpPaths.Controls.Add(Me.txtWorkbooks)
        Me.tpPaths.Controls.Add(Me.btnAddinNameSelect)
        Me.tpPaths.Controls.Add(Me.txtAddinDir)
        Me.tpPaths.Controls.Add(Me.lblWorkbooks)
        Me.tpPaths.Location = New System.Drawing.Point(4, 22)
        Me.tpPaths.Name = "tpPaths"
        Me.tpPaths.Padding = New System.Windows.Forms.Padding(3)
        Me.tpPaths.Size = New System.Drawing.Size(395, 310)
        Me.tpPaths.TabIndex = 1
        Me.tpPaths.Text = "Paths"
        Me.tpPaths.UseVisualStyleBackColor = True
        '
        'txtHelpPath
        '
        Me.txtHelpPath.Location = New System.Drawing.Point(29, 182)
        Me.txtHelpPath.Name = "txtHelpPath"
        Me.txtHelpPath.Size = New System.Drawing.Size(316, 20)
        Me.txtHelpPath.TabIndex = 18
        '
        'lblHelpPath
        '
        Me.lblHelpPath.AutoSize = True
        Me.lblHelpPath.Location = New System.Drawing.Point(10, 166)
        Me.lblHelpPath.Name = "lblHelpPath"
        Me.lblHelpPath.Size = New System.Drawing.Size(48, 13)
        Me.lblHelpPath.TabIndex = 17
        Me.lblHelpPath.Text = "Help Folder"
        '
        'btnHelpFile
        '
        Me.btnHelpFile.Location = New System.Drawing.Point(351, 182)
        Me.btnHelpFile.Name = "btnHelpFile"
        Me.btnHelpFile.Size = New System.Drawing.Size(32, 23)
        Me.btnHelpFile.TabIndex = 16
        Me.btnHelpFile.Text = "..."
        Me.btnHelpFile.UseVisualStyleBackColor = True
        '
        'FormMain
        '
        Me.AutoScaleDimensions = New System.Drawing.SizeF(6.0!, 13.0!)
        Me.AutoScaleMode = System.Windows.Forms.AutoScaleMode.Font
        Me.ClientSize = New System.Drawing.Size(407, 370)
        Me.Controls.Add(Me.TabControl1)
        Me.Controls.Add(Me.btnLaunch)
        Me.Controls.Add(Me.btnClose)
        Me.FormBorderStyle = System.Windows.Forms.FormBorderStyle.FixedDialog
        Me.Icon = CType(resources.GetObject("$this.Icon"), System.Drawing.Icon)
        Me.Name = "FormMain"
        Me.Text = "QuantLibXL Launcher"
        Me.grpEnvironment.ResumeLayout(False)
        Me.grpEnvironment.PerformLayout()
        Me.ToolStrip1.ResumeLayout(False)
        Me.ToolStrip1.PerformLayout()
        Me.grpStartup.ResumeLayout(False)
        Me.grpStartup.PerformLayout()
        Me.TabControl1.ResumeLayout(False)
        Me.tbEnvironments.ResumeLayout(False)
        Me.tpPaths.ResumeLayout(False)
        Me.tpPaths.PerformLayout()
        Me.ResumeLayout(False)

    End Sub
    Friend WithEvents btnClose As System.Windows.Forms.Button
    Friend WithEvents lblPreconfigured As System.Windows.Forms.Label
    Friend WithEvents lblFramework As System.Windows.Forms.Label
    Friend WithEvents txtFramework As System.Windows.Forms.TextBox
    Friend WithEvents lblAddinDir As System.Windows.Forms.Label
    Friend WithEvents txtAddinDir As System.Windows.Forms.TextBox
    Friend WithEvents btnAddinNameSelect As System.Windows.Forms.Button
    Friend WithEvents lblAddinName As System.Windows.Forms.Label
    Friend WithEvents txtAddinName As System.Windows.Forms.TextBox
    Friend WithEvents btnFrameworkSelect As System.Windows.Forms.Button
    Friend WithEvents btnAddinDirSelect As System.Windows.Forms.Button
    Friend WithEvents btnWorkbooks As System.Windows.Forms.Button
    Friend WithEvents txtWorkbooks As System.Windows.Forms.TextBox
    Friend WithEvents lblWorkbooks As System.Windows.Forms.Label
    Friend WithEvents grpEnvironment As System.Windows.Forms.GroupBox
    Friend WithEvents lstPreconfigured As System.Windows.Forms.ListBox
    Friend WithEvents grpStartup As System.Windows.Forms.GroupBox
    Friend WithEvents cbYCBootstrap As System.Windows.Forms.CheckBox
    Friend WithEvents lblUserConfigured As System.Windows.Forms.Label
    Friend WithEvents lstUserconfigured As System.Windows.Forms.ListBox
    Friend WithEvents ToolStrip1 As System.Windows.Forms.ToolStrip
    Friend WithEvents btnNew As System.Windows.Forms.ToolStripButton
    Friend WithEvents btnCopy As System.Windows.Forms.ToolStripButton
    Friend WithEvents btnDelete As System.Windows.Forms.ToolStripButton
    Friend WithEvents btnClear As System.Windows.Forms.ToolStripButton
    Friend WithEvents btnRename As System.Windows.Forms.ToolStripButton
    Friend WithEvents btnLaunch As System.Windows.Forms.Button
    Friend WithEvents cbIndexesTimeSeries As System.Windows.Forms.CheckBox
    Friend WithEvents cbSwapVolBootstrap As System.Windows.Forms.CheckBox
    Friend WithEvents cbCapVolBootstrap As System.Windows.Forms.CheckBox
    Friend WithEvents cbLoadMurexYC As System.Windows.Forms.CheckBox
    Friend WithEvents TabControl1 As System.Windows.Forms.TabControl
    Friend WithEvents tbEnvironments As System.Windows.Forms.TabPage
    Friend WithEvents tpPaths As System.Windows.Forms.TabPage
    Friend WithEvents cbFitCMS As System.Windows.Forms.CheckBox
    Friend WithEvents cbLoadBonds As System.Windows.Forms.CheckBox
    Friend WithEvents cbStaticData As System.Windows.Forms.CheckBox
    Friend WithEvents cbSwapSmileBootstrap As System.Windows.Forms.CheckBox
    Friend WithEvents txtHelpPath As System.Windows.Forms.TextBox
    Friend WithEvents lblHelpPath As System.Windows.Forms.Label
    Friend WithEvents btnHelpFile As System.Windows.Forms.Button
End Class
