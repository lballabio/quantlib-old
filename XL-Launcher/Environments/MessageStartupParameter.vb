
Namespace XL_Launcher

    ''' <summary>
    ''' Take a Startup Parameter object and generate the relevant XML
    ''' for the session file.
    ''' </summary>
    ''' <remarks></remarks>
    Public Class MessageStartupParameter

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
                nodeName = "StartupParameter"
            End Get

        End Property

        Public Property Name() As String Implements ISerializable.Name

            Get
                Name = nodeName
            End Get

            Set(ByVal value As String)
            End Set

        End Property

        ''''''''''''''''''''''''''''''''''''''''''
        ' Initialization
        ''''''''''''''''''''''''''''''''''''''''''

        Public Sub New(ByVal sp As StartupParameter)

            name_ = sp.parameterName
            value_ = sp.transmitValue

        End Sub

        ''''''''''''''''''''''''''''''''''''''''''
        ' Serializable Interface
        ''''''''''''''''''''''''''''''''''''''''''

        Public Sub serialize(ByRef serializer As ISerializer, ByVal versionNumber As Integer) Implements ISerializable.serialize

            serializer.serializeAttribute(name_, "name")
            serializer.serializeAttribute(value_, "value")

        End Sub

    End Class

End Namespace
