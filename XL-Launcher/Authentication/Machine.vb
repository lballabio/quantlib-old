
Namespace XL_Launcher

    ''' <summary>
    ''' Encapsulate the information relating to the current machine
    ''' on which this instance of XL-Launcher is running.  Used for authentication.
    ''' </summary>
    ''' <remarks></remarks>
    Public Class Machine

        Implements ISerializable

        Private name_ As String

        Public ReadOnly Property nodeName() As String Implements ISerializable.nodeName

            Get
                nodeName = "Machine"
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

        Public Sub serialize(ByRef serializer As ISerializer, ByVal versionNumber As Integer) Implements ISerializable.serialize

            serializer.serializeAttribute(name_, "name")

        End Sub

    End Class

End Namespace
