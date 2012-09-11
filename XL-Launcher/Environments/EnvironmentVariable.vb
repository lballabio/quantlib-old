
Namespace XL_Launcher

    ''' <summary>
    ''' An Environment Variable which is required by a given Framework and which is to be
    ''' set by XL-Launcher within the subprocess in which Excel is spawned.
    ''' </summary>
    ''' <remarks></remarks>
    Public Class EnvironmentVariable

        Implements ISerializable

        ''''''''''''''''''''''''''''''''''''''''''
        ' Members
        ''''''''''''''''''''''''''''''''''''''''''

        Private name_ As String
        Private value_ As String

        ''''''''''''''''''''''''''''''''''''''''''
        ' Properties
        ''''''''''''''''''''''''''''''''''''''''''

        Public ReadOnly Property nodeName() As String Implements ISerializable.nodeName

            Get
                nodeName = "EnvironmentVariable"
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

        Public Property Value() As String

            Get
                Value = value_
            End Get

            Set(ByVal value As String)
                value_ = value
            End Set

        End Property

        ''''''''''''''''''''''''''''''''''''''''''
        ' Initialization
        ''''''''''''''''''''''''''''''''''''''''''

        Public Sub New(ByVal name As String, ByVal value As String)

            name_ = name
            value_ = value

        End Sub

        Public Sub New()
        End Sub

        ''''''''''''''''''''''''''''''''''''''''''
        ' Serializable interface
        ''''''''''''''''''''''''''''''''''''''''''

        Public Sub serialize(ByRef serializer As ISerializer, ByVal versionNumber As Integer) Implements ISerializable.serialize

            serializer.serializeAttribute(name_, "name")
            serializer.serializeAttribute(value_, "value")

        End Sub

    End Class

End Namespace
