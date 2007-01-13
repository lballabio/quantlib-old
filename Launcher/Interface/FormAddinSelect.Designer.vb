<Global.Microsoft.VisualBasic.CompilerServices.DesignerGenerated()> _
Partial Class FormAddinSelect
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
        Me.optVC7 = New System.Windows.Forms.RadioButton
        Me.optVC8 = New System.Windows.Forms.RadioButton
        Me.btnOK = New System.Windows.Forms.Button
        Me.btnCancel = New System.Windows.Forms.Button
        Me.lblAddinName = New System.Windows.Forms.Label
        Me.optSingle = New System.Windows.Forms.RadioButton
        Me.optMulti = New System.Windows.Forms.RadioButton
        Me.optStatic = New System.Windows.Forms.RadioButton
        Me.optDynamic = New System.Windows.Forms.RadioButton
        Me.optDebug = New System.Windows.Forms.RadioButton
        Me.optRelease = New System.Windows.Forms.RadioButton
        Me.lstAddinNames = New System.Windows.Forms.ListBox
        Me.grpCompiler = New System.Windows.Forms.GroupBox
        Me.grpThreading = New System.Windows.Forms.GroupBox
        Me.grpLinking = New System.Windows.Forms.GroupBox
        Me.grpEnvironment = New System.Windows.Forms.GroupBox
        Me.grpCompiler.SuspendLayout()
        Me.grpThreading.SuspendLayout()
        Me.grpLinking.SuspendLayout()
        Me.grpEnvironment.SuspendLayout()
        Me.SuspendLayout()
        '
        'optVC7
        '
        Me.optVC7.AutoSize = True
        Me.optVC7.Location = New System.Drawing.Point(94, 19)
        Me.optVC7.Name = "optVC7"
        Me.optVC7.Size = New System.Drawing.Size(45, 17)
        Me.optVC7.TabIndex = 1
        Me.optVC7.TabStop = True
        Me.optVC7.Text = "VC7"
        Me.optVC7.UseVisualStyleBackColor = True
        '
        'optVC8
        '
        Me.optVC8.AutoSize = True
        Me.optVC8.Location = New System.Drawing.Point(23, 19)
        Me.optVC8.Name = "optVC8"
        Me.optVC8.Size = New System.Drawing.Size(45, 17)
        Me.optVC8.TabIndex = 2
        Me.optVC8.TabStop = True
        Me.optVC8.Text = "VC8"
        Me.optVC8.UseVisualStyleBackColor = True
        '
        'btnOK
        '
        Me.btnOK.Location = New System.Drawing.Point(140, 222)
        Me.btnOK.Name = "btnOK"
        Me.btnOK.Size = New System.Drawing.Size(75, 23)
        Me.btnOK.TabIndex = 3
        Me.btnOK.Text = "OK"
        Me.btnOK.UseVisualStyleBackColor = True
        '
        'btnCancel
        '
        Me.btnCancel.Location = New System.Drawing.Point(222, 222)
        Me.btnCancel.Name = "btnCancel"
        Me.btnCancel.Size = New System.Drawing.Size(75, 23)
        Me.btnCancel.TabIndex = 4
        Me.btnCancel.Text = "Cancel"
        Me.btnCancel.UseVisualStyleBackColor = True
        '
        'lblAddinName
        '
        Me.lblAddinName.AutoSize = True
        Me.lblAddinName.Location = New System.Drawing.Point(200, 9)
        Me.lblAddinName.Name = "lblAddinName"
        Me.lblAddinName.Size = New System.Drawing.Size(65, 13)
        Me.lblAddinName.TabIndex = 8
        Me.lblAddinName.Text = "Addin Name"
        '
        'optSingle
        '
        Me.optSingle.AutoSize = True
        Me.optSingle.Location = New System.Drawing.Point(94, 17)
        Me.optSingle.Name = "optSingle"
        Me.optSingle.Size = New System.Drawing.Size(54, 17)
        Me.optSingle.TabIndex = 9
        Me.optSingle.TabStop = True
        Me.optSingle.Text = "Single"
        Me.optSingle.UseVisualStyleBackColor = True
        '
        'optMulti
        '
        Me.optMulti.AutoSize = True
        Me.optMulti.Location = New System.Drawing.Point(23, 17)
        Me.optMulti.Name = "optMulti"
        Me.optMulti.Size = New System.Drawing.Size(47, 17)
        Me.optMulti.TabIndex = 10
        Me.optMulti.TabStop = True
        Me.optMulti.Text = "Multi"
        Me.optMulti.UseVisualStyleBackColor = True
        '
        'optStatic
        '
        Me.optStatic.AutoSize = True
        Me.optStatic.Location = New System.Drawing.Point(23, 19)
        Me.optStatic.Name = "optStatic"
        Me.optStatic.Size = New System.Drawing.Size(52, 17)
        Me.optStatic.TabIndex = 11
        Me.optStatic.TabStop = True
        Me.optStatic.Text = "Static"
        Me.optStatic.UseVisualStyleBackColor = True
        '
        'optDynamic
        '
        Me.optDynamic.AutoSize = True
        Me.optDynamic.Location = New System.Drawing.Point(94, 19)
        Me.optDynamic.Name = "optDynamic"
        Me.optDynamic.Size = New System.Drawing.Size(66, 17)
        Me.optDynamic.TabIndex = 12
        Me.optDynamic.TabStop = True
        Me.optDynamic.Text = "Dynamic"
        Me.optDynamic.UseVisualStyleBackColor = True
        '
        'optDebug
        '
        Me.optDebug.AutoSize = True
        Me.optDebug.Location = New System.Drawing.Point(94, 17)
        Me.optDebug.Name = "optDebug"
        Me.optDebug.Size = New System.Drawing.Size(57, 17)
        Me.optDebug.TabIndex = 13
        Me.optDebug.TabStop = True
        Me.optDebug.Text = "Debug"
        Me.optDebug.UseVisualStyleBackColor = True
        '
        'optRelease
        '
        Me.optRelease.AutoSize = True
        Me.optRelease.Location = New System.Drawing.Point(23, 17)
        Me.optRelease.Name = "optRelease"
        Me.optRelease.Size = New System.Drawing.Size(64, 17)
        Me.optRelease.TabIndex = 14
        Me.optRelease.TabStop = True
        Me.optRelease.Text = "Release"
        Me.optRelease.UseVisualStyleBackColor = True
        '
        'lstAddinNames
        '
        Me.lstAddinNames.FormattingEnabled = True
        Me.lstAddinNames.Location = New System.Drawing.Point(213, 34)
        Me.lstAddinNames.Name = "lstAddinNames"
        Me.lstAddinNames.Size = New System.Drawing.Size(206, 173)
        Me.lstAddinNames.TabIndex = 15
        '
        'grpCompiler
        '
        Me.grpCompiler.Controls.Add(Me.optVC8)
        Me.grpCompiler.Controls.Add(Me.optVC7)
        Me.grpCompiler.Location = New System.Drawing.Point(16, 3)
        Me.grpCompiler.Name = "grpCompiler"
        Me.grpCompiler.Size = New System.Drawing.Size(167, 45)
        Me.grpCompiler.TabIndex = 16
        Me.grpCompiler.TabStop = False
        Me.grpCompiler.Text = "Compiler"
        '
        'grpThreading
        '
        Me.grpThreading.Controls.Add(Me.optMulti)
        Me.grpThreading.Controls.Add(Me.optSingle)
        Me.grpThreading.Location = New System.Drawing.Point(16, 56)
        Me.grpThreading.Name = "grpThreading"
        Me.grpThreading.Size = New System.Drawing.Size(167, 45)
        Me.grpThreading.TabIndex = 17
        Me.grpThreading.TabStop = False
        Me.grpThreading.Text = "Threading"
        '
        'grpLinking
        '
        Me.grpLinking.Controls.Add(Me.optDynamic)
        Me.grpLinking.Controls.Add(Me.optStatic)
        Me.grpLinking.Location = New System.Drawing.Point(16, 109)
        Me.grpLinking.Name = "grpLinking"
        Me.grpLinking.Size = New System.Drawing.Size(167, 45)
        Me.grpLinking.TabIndex = 18
        Me.grpLinking.TabStop = False
        Me.grpLinking.Text = "Linking"
        '
        'grpEnvironment
        '
        Me.grpEnvironment.Controls.Add(Me.optRelease)
        Me.grpEnvironment.Controls.Add(Me.optDebug)
        Me.grpEnvironment.Location = New System.Drawing.Point(16, 162)
        Me.grpEnvironment.Name = "grpEnvironment"
        Me.grpEnvironment.Size = New System.Drawing.Size(167, 45)
        Me.grpEnvironment.TabIndex = 19
        Me.grpEnvironment.TabStop = False
        Me.grpEnvironment.Text = "Environment"
        '
        'FormAddinSelect
        '
        Me.AutoScaleDimensions = New System.Drawing.SizeF(6.0!, 13.0!)
        Me.AutoScaleMode = System.Windows.Forms.AutoScaleMode.Font
        Me.ClientSize = New System.Drawing.Size(436, 253)
        Me.Controls.Add(Me.grpEnvironment)
        Me.Controls.Add(Me.grpLinking)
        Me.Controls.Add(Me.grpThreading)
        Me.Controls.Add(Me.grpCompiler)
        Me.Controls.Add(Me.lstAddinNames)
        Me.Controls.Add(Me.lblAddinName)
        Me.Controls.Add(Me.btnCancel)
        Me.Controls.Add(Me.btnOK)
        Me.FormBorderStyle = System.Windows.Forms.FormBorderStyle.FixedDialog
        Me.Name = "FormAddinSelect"
        Me.Text = "Specify Addin Name"
        Me.grpCompiler.ResumeLayout(False)
        Me.grpCompiler.PerformLayout()
        Me.grpThreading.ResumeLayout(False)
        Me.grpThreading.PerformLayout()
        Me.grpLinking.ResumeLayout(False)
        Me.grpLinking.PerformLayout()
        Me.grpEnvironment.ResumeLayout(False)
        Me.grpEnvironment.PerformLayout()
        Me.ResumeLayout(False)
        Me.PerformLayout()

    End Sub
    Friend WithEvents optVC7 As System.Windows.Forms.RadioButton
    Friend WithEvents optVC8 As System.Windows.Forms.RadioButton
    Friend WithEvents btnOK As System.Windows.Forms.Button
    Friend WithEvents btnCancel As System.Windows.Forms.Button
    Friend WithEvents lblAddinName As System.Windows.Forms.Label
    Friend WithEvents optSingle As System.Windows.Forms.RadioButton
    Friend WithEvents optMulti As System.Windows.Forms.RadioButton
    Friend WithEvents optStatic As System.Windows.Forms.RadioButton
    Friend WithEvents optDynamic As System.Windows.Forms.RadioButton
    Friend WithEvents optDebug As System.Windows.Forms.RadioButton
    Friend WithEvents optRelease As System.Windows.Forms.RadioButton
    Friend WithEvents lstAddinNames As System.Windows.Forms.ListBox
    Friend WithEvents grpCompiler As System.Windows.Forms.GroupBox
    Friend WithEvents grpThreading As System.Windows.Forms.GroupBox
    Friend WithEvents grpLinking As System.Windows.Forms.GroupBox
    Friend WithEvents grpEnvironment As System.Windows.Forms.GroupBox
End Class
