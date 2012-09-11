
Namespace XL_Launcher

    ''' <summary>
    ''' An Addin which is specific to an overall Excel session and not to any particular
    ''' Excecl VBA application, e.g. Reuters, Bloomberg, Oracle.
    ''' </summary>
    ''' <remarks></remarks>
    Public Class GlobalAddin

        Implements ISerializable

        ''''''''''''''''''''''''''''''''''''''''''
        ' Members
        ''''''''''''''''''''''''''''''''''''''''''

        Private lbName_ As New System.Windows.Forms.Label
        Private cbEnabled_ As New System.Windows.Forms.CheckBox
        Private tbPath_ As New System.Windows.Forms.TextBox
        Public Const HEIGHT As Integer = 39

        ''''''''''''''''''''''''''''''''''''''''''
        ' Properties
        ''''''''''''''''''''''''''''''''''''''''''

        Public ReadOnly Property nodeName() As String Implements ISerializable.nodeName

            Get
                nodeName = "GlobalAddin"
            End Get

        End Property

        Public Property Name() As String Implements ISerializable.Name

            Get
                Name = lbName_.Text
            End Get

            Set(ByVal value As String)
            End Set

        End Property

        Public Property Path() As String

            Get
                Path = tbPath_.Text
            End Get

            Set(ByVal value As String)
                tbPath_.Text = value
            End Set

        End Property

        Public Property Enabled() As Boolean

            Get
                Enabled = cbEnabled_.Checked
            End Get

            Set(ByVal value As Boolean)
                cbEnabled_.Checked = value
            End Set

        End Property

        ''''''''''''''''''''''''''''''''''''''''''
        ' Serializable Interface
        ''''''''''''''''''''''''''''''''''''''''''

        Public Sub serialize(ByRef serializer As ISerializer, ByVal versionNumber As Integer) Implements ISerializable.serialize

            serializer.serializeAttribute(lbName_.Text, "name")
            serializer.serializeAttribute(cbEnabled_.Checked, "enabled")
            serializer.serializeAttribute(tbPath_.Text, "path")

        End Sub

        ''''''''''''''''''''''''''''''''''''''''''
        ' Initialization
        ''''''''''''''''''''''''''''''''''''''''''

        Public Sub New()

            lbName_.Width = 200
            lbName_.Height = 13
            cbEnabled_.Width = 20
            tbPath_.Width = 855

        End Sub

        ''''''''''''''''''''''''''''''''''''''''''
        ' Public Interface
        ''''''''''''''''''''''''''''''''''''''''''

        Public Sub draw(ByRef c As System.Windows.Forms.Control.ControlCollection, ByVal y As Integer)

            lbName_.Location = New Point(7, y + 16)
            c.Add(lbName_)

            cbEnabled_.Location = New Point(20, y + 33)
            c.Add(cbEnabled_)

            tbPath_.Location = New Point(40, y + 33)
            c.Add(tbPath_)

        End Sub

    End Class

End Namespace
