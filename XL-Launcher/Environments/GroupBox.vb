
Namespace XL_Launcher

    ''' <summary>
    ''' Encapsulate a collection of Startup Parameter objects which are displayed
    ''' together in the XL-Launcher UI within a single GroupBox control.
    ''' </summary>
    ''' <remarks></remarks>
    Public Class GroupBox

        Implements ICloneable
        Implements ISerializable

        '''''''''''''''''''''''''''''''''''''''''''
        '' Members
        '''''''''''''''''''''''''''''''''''''''''''

        Private name_ As String
        Private text_ As String
        Private startupParameterNames() As String = {"Separator", "KeyValuePair", "CheckBox", "TextBox", "ComboBox", "DateTimePicker"}
        Private startupParameterList_ As Collection = New Collection
        Private groupBox_ As System.Windows.Forms.GroupBox

        '''''''''''''''''''''''''''''''''''''''''''
        '' Properties
        '''''''''''''''''''''''''''''''''''''''''''

        Public ReadOnly Property nodeName() As String Implements ISerializable.nodeName

            Get
                nodeName = "GroupBox"
            End Get

        End Property

        Public Property Name() As String Implements ISerializable.Name

            Get
                Name = name_
            End Get

            Set(ByVal value As String)
                name_ = value
            End Set

        End Property

        Public ReadOnly Property StartupParameterList() As Collection

            Get
                StartupParameterList = startupParameterList_
            End Get

        End Property

        Public Property GroupBox() As System.Windows.Forms.GroupBox

            Get
                GroupBox = groupBox_
            End Get

            Set(ByVal value As System.Windows.Forms.GroupBox)
                groupBox_ = value
            End Set

        End Property

        Public ReadOnly Property visible() As Boolean

            Get
                For Each s As StartupParameter In startupParameterList_
                    If s.visible Then
                        visible = True
                        Exit Property
                    End If
                Next s
            End Get

        End Property

        ''''''''''''''''''''''''''''''''''''''''''
        ' ICloneable Interface
        ''''''''''''''''''''''''''''''''''''''''''

        Public Function Clone() As Object Implements ICloneable.Clone

            Clone = New GroupBox(name_, text_)
            For Each s As StartupParameter In startupParameterList_
                Clone.StartupParameterList.Add(CType(s, StartupParameter).Clone, s.parameterName)
            Next

        End Function

        ''''''''''''''''''''''''''''''''''''''''''
        ' Serializable Interface
        ''''''''''''''''''''''''''''''''''''''''''

        Public Sub serialize(ByRef serializer As ISerializer, ByVal versionNumber As Integer) Implements ISerializable.serialize

            serializer.serializeAttribute(text_, "text", "")
            serializer.serializeObjectCollection(startupParameterList_, "StartupParameter", startupParameterNames, _
                versionNumber, False, False, "GroupBox", False)

        End Sub

        '''''''''''''''''''''''''''''''''''''''''''
        '' Initialization
        '''''''''''''''''''''''''''''''''''''''''''

        Public Sub New(ByVal name As String, ByVal text As String)

            groupBox_ = New System.Windows.Forms.GroupBox
            groupBox_.Width = 435
            name_ = name
            text_ = text

        End Sub

        Public Sub New()

            groupBox_ = New System.Windows.Forms.GroupBox
            groupBox_.Width = 435

        End Sub

        Sub draw(ByRef c As System.Windows.Forms.Control.ControlCollection, ByVal y As Integer)

            groupBox_.Location = New Point(0, y)
            groupBox_.Text = text_
            c.Add(groupBox_)

        End Sub

    End Class

End Namespace
