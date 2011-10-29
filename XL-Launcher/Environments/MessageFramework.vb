
Namespace XL_Launcher

    ''' <summary>
    ''' Take a Framework object and generate the relevant XML
    ''' for the session file.
    ''' </summary>
    ''' <remarks></remarks>
    Public Class MessageFramework

        Implements ISerializable

        ''''''''''''''''''''''''''''''''''''''''''
        ' Members
        ''''''''''''''''''''''''''''''''''''''''''

        Private name_ As String
        Private fileName_ As String
        Private displayName_ As String
        Private componentList_ As Collection = New Collection
        Private messageStartupParameterList_ As Collection = New Collection

        ''''''''''''''''''''''''''''''''''''''''''
        ' Properties
        ''''''''''''''''''''''''''''''''''''''''''

        Public ReadOnly Property nodeName() As String Implements ISerializable.nodeName

            Get
                nodeName = "Framework"
            End Get

        End Property

        Public Property Name() As String Implements ISerializable.Name

            Get
                Name = name_
            End Get

            Set(ByVal value As String)
            End Set

        End Property

        ''''''''''''''''''''''''''''''''''''''''''
        ' Initialization
        ''''''''''''''''''''''''''''''''''''''''''

        Public Sub New(ByVal f As Framework)

            name_ = f.Name
            displayName_ = f.DisplayName
            fileName_ = f.FileName
            componentList_ = f.ComponentList

            For Each g As GroupBox In f.GroupBoxes
                For Each sp As StartupParameter In g.StartupParameterList
                    messageStartupParameterList_.Add(New MessageStartupParameter(sp))
                Next sp
            Next g

        End Sub

        ''''''''''''''''''''''''''''''''''''''''''
        ' Serializable Interface
        ''''''''''''''''''''''''''''''''''''''''''

        Public Sub serialize(ByRef serializer As ISerializer, ByVal versionNumber As Integer) Implements ISerializable.serialize

            serializer.serializeAttribute(name_, "name")
            serializer.serializeAttribute(displayName_, "displayName")
            serializer.serializeAttribute(fileName_, "fileName")
            serializer.serializeObjectCollection(componentList_, "Component", versionNumber)
            serializer.serializeObjectCollection(messageStartupParameterList_, "StartupParameter", versionNumber)

        End Sub

    End Class

End Namespace
