
Namespace XL_Launcher

    ''' <summary>
    ''' Generate the XML for the session file.
    ''' </summary>
    ''' <remarks></remarks>
    Public Class Message

        Implements ISerializable

        ''''''''''''''''''''''''''''''''''''''''''
        ' Members
        ''''''''''''''''''''''''''''''''''''''''''

        Private messageFrameworkList_ As Collection = New Collection
        Private globalAddins_ As Collection

        ''''''''''''''''''''''''''''''''''''''''''
        ' Properties
        ''''''''''''''''''''''''''''''''''''''''''

        Public ReadOnly Property nodeName() As String Implements ISerializable.nodeName

            Get
                nodeName = "Environment"
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

        Public Sub New(ByVal frameworkList As Collection, ByVal globalAddins As Collection)

            globalAddins_ = globalAddins

            For Each f As Framework In frameworkList
                messageFrameworkList_.Add(New MessageFramework(f))
            Next f

        End Sub

        ''''''''''''''''''''''''''''''''''''''''''
        ' Serializable Interface
        ''''''''''''''''''''''''''''''''''''''''''

        Public Sub serialize(ByRef serializer As ISerializer, ByVal versionNumber As Integer) Implements ISerializable.serialize

            serializer.serializeObjectCollection(globalAddins_, "GlobalAddin", versionNumber)
            serializer.serializeObjectCollection(messageFrameworkList_, "Framework", versionNumber)

        End Sub

    End Class

End Namespace
